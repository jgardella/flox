module Flox.Interpreter

open System.Collections.Generic
open Flox.Env
open Flox.Error
open Flox.Parser
open Flox.Scanner

exception Return of value : obj

type ILoxCallable = 
    abstract member Call: (Stmt [] -> Env -> unit) * obj [] -> obj
    abstract member Arity: int

let mutable locals = Dictionary()

let globals = Env()
globals.Define "clock" 
    { new ILoxCallable with
        member __.Arity = 0
        member __.Call(_, _) =
            (System.DateTime.Now.Ticks / System.TimeSpan.TicksPerMillisecond) / 1000L :> obj }

type LoxFunction (name : Token, functionParams : Token [], body : Stmt [], closure : Env, isInitializer : bool) =
    interface ILoxCallable with
        member __.Arity = functionParams.Length
        member __.Call(evaluateBlock, arguments) =
            try
                let environment = Env(closure)
                for i = 0 to functionParams.Length - 1 do
                    environment.Define (functionParams.[i].lexeme) (arguments.[i])

                evaluateBlock body environment
                if isInitializer then
                    closure.GetAt 0 "this"
                else
                    null
            with
                | Return returnValue ->
                    if isInitializer then
                        closure.GetAt 0 "this"
                    else
                        returnValue

    member __.Bind (instance : LoxInstance) =
        let env = Env(closure)
        env.Define "this" instance
        LoxFunction (name, functionParams, body, env, isInitializer)

    override __.ToString() =
        sprintf "<fn %s>" name.lexeme

and LoxInstance (klass : LoxClass) =
    let fields = Dictionary()

    member this.Get (name : Token) =
        match fields.TryGetValue name.lexeme with
        | (true, value) -> value
        | (false, _) -> 
            match klass.FindMethod name.lexeme with
            | Some (method : LoxFunction) -> 
                (method.Bind(this)) :> obj
            | None -> raise (RuntimeError (name, sprintf "Undefined property '%s'." name.lexeme))

    member __.Set (name : Token, value : obj) =
        fields.Add(name.lexeme, value)

    override __.ToString() =
        sprintf "%s instance" klass.Name

and LoxClass (name : string, methods : IDictionary<string, LoxFunction>) =
    interface ILoxCallable with
        member this.Arity = 
            match this.FindMethod("init") with
            | Some initializer -> (initializer :> ILoxCallable).Arity
            | None -> 0

        member this.Call(evaluateBlock, arguments) =
            let instance = LoxInstance(this)
            match this.FindMethod("init") with
            | Some initializer ->
                let boundInitializer = initializer.Bind(instance) :> ILoxCallable
                boundInitializer.Call(evaluateBlock, arguments) |> ignore
            | None -> ()
            instance :> obj

    member __.Name = name

    member __.FindMethod (name : string) =
        match methods.TryGetValue name with
        | (true, value) -> Some value
        | (false, _) -> None

    override __.ToString() = name

let mutable currentEnv = globals 

let isTruthy (object : obj) =
    match object with
    | null -> false
    | :? bool -> object :?> bool
    | _ -> true
    
let isEqual (a : obj) (b : obj) =
    // nil is only equal to nil.
    if isNull a && isNull b then true
    elif isNull a then false
    else a.Equals b
    
let stringify (object : obj) =
    match object with
    | null -> "nil"
    | :? double ->
        let text = object.ToString()
        if text.EndsWith ".0"
        then text.Substring(0, text.Length - 2)
        else text
    | :? bool ->
        object.ToString().ToLower()
    | _ ->
        object.ToString()
    
let checkNumberOperand (operator : Token) (object : obj) = 
    match object with
    | :? double -> true
    | _ -> raise (RuntimeError (operator, "Operand must be a number"))
    
let checkNumberOperands (operator : Token) (left : obj) (right : obj) = 
    match (left, right) with
    | (:? double, :? double) -> true
    | _ -> raise (RuntimeError (operator, "Operands must be numbers"))   

let lookUpVariable (name : Token) (expr : Expr) =
    match locals.TryGetValue expr with
    | (true, distance) ->
        currentEnv.GetAt distance name.lexeme
    | (false, _) ->
        globals.Get name

let rec evaluateExpr = function
    | Expr.Literal literal -> literal
    | Expr.Grouping expr -> evaluateExpr expr
    | Expr.Unary (operator, right) ->
        let right = evaluateExpr right
        
        match operator.tokenType with
        | TokenType.Minus when checkNumberOperand operator right ->
            right 
            :?> double
            |> ((*) -1.0)
            :> obj
        | TokenType.Bang ->
            right 
            |> isTruthy 
            |> not
            :> obj
        | _ ->
            null
    | Expr.Binary (left, operator, right) ->
        let left = evaluateExpr left
        let right = evaluateExpr right
        let checkNumberOperands = checkNumberOperands operator
        
        match operator.tokenType with
        | TokenType.Minus when checkNumberOperands left right ->
            (left :?> double) - (right :?> double)
            :> obj
        | TokenType.Slash when checkNumberOperands left right ->
            (left :?> double) / (right :?> double)
            :> obj
        | TokenType.Star when checkNumberOperands left right ->
            (left :?> double) * (right :?> double)
            :> obj
        | TokenType.Plus when (left :? double && right :? double) ->
            (left :?> double) + (right :?> double)
            :> obj
        | TokenType.Plus when (left :? string && right :? string) ->
            (left :?> string) + (right :?> string)
            :> obj
        | TokenType.Plus ->
            raise (RuntimeError(operator, "Operators must be two numbers or two strings."))
        | TokenType.Greater when checkNumberOperands left right ->
            (left :?> double) > (right :?> double)
            :> obj
        | TokenType.GreaterEqual when checkNumberOperands left right ->
            (left :?> double) >= (right :?> double)
            :> obj
        | TokenType.Less when checkNumberOperands left right ->
            (left :?> double) < (right :?> double)
            :> obj
        | TokenType.LessEqual when checkNumberOperands left right ->
            (left :?> double) <= (right :?> double)
            :> obj
        | TokenType.BangEqual -> (isEqual left right |> not) :> obj
        | TokenType.EqualEqual -> (isEqual left right) :> obj
        | _ -> null
    | Expr.Variable name as expr -> 
        lookUpVariable name expr
    | Expr.Assign (name, expr) as assignExpr ->
        let value = evaluateExpr expr
        match locals.TryGetValue assignExpr with
        | (true, distance) ->
            currentEnv.AssignAt distance name value
        | (false, _) ->
            globals.Assign name value
        value
    | Expr.Logical (leftExpr, operator, rightExpr) ->
        let left = evaluateExpr leftExpr
        match operator.tokenType with
        | TokenType.Or when isTruthy left ->
            left
        | _ when not (isTruthy left) ->
            left
        | _ ->
            evaluateExpr rightExpr
    | Expr.Call (callee, paren, arguments) ->
        let callee = evaluateExpr callee
        let evaluatedArguments = arguments |> Array.map evaluateExpr
        if not (callee :? ILoxCallable) then runtimeError paren "Can only call functions and classes."
        let loxFunction = callee :?> ILoxCallable
        if evaluatedArguments.Length <> loxFunction.Arity then
            runtimeError paren (sprintf "Expected %d arguments but got %d." (loxFunction.Arity) evaluatedArguments.Length)
        loxFunction.Call(evaluateBlock, evaluatedArguments)
    | Expr.Get (objectExpr, name) ->
        let object = evaluateExpr objectExpr
        if object :? LoxInstance then
            (object :?> LoxInstance).Get name
        else
            raise (RuntimeError (name, "Only instances have properties."))
    | Expr.Set (objectExpr, name, valueExpr) ->
        let object = evaluateExpr objectExpr
        if object :? LoxInstance then
            let value = evaluateExpr valueExpr
            (object :?> LoxInstance).Set(name, value)
            value
        else
            raise (RuntimeError (name, "Only instances have fields."))
    | Expr.This keyword as expr ->
        lookUpVariable keyword expr
       
and evaluateBlock (stmts : Stmt []) (env : Env) =
    let previousEnv = currentEnv
    try
        currentEnv <- env
        stmts |> Array.iter evaluateStmt
    finally
        currentEnv <- previousEnv
        
and evaluateStmt = function
    | Stmt.Expression expr -> evaluateExpr expr |> ignore
    | Stmt.If (condition, ifBranch, thenBranch) ->
        if isTruthy (evaluateExpr condition)
        then evaluateStmt ifBranch
        else 
            match thenBranch with
            | Some thenBranch -> evaluateStmt thenBranch
            | None -> ()
    | Stmt.Print stmt -> evaluateExpr stmt |> stringify |> printfn "%s"
    | Stmt.Var (name, initializer) ->
        let value = 
            initializer
            |> Option.map evaluateExpr 
            |> Option.defaultValue null
            
        currentEnv.Define name.lexeme value
    | Stmt.Block stmts ->
        evaluateBlock stmts (Env currentEnv)
    | Stmt.While (condition, body) ->
        while isTruthy (evaluateExpr condition) do
            evaluateStmt body
    | Stmt.Function (Func (name, functionParams, body)) ->
        let loxFunction = LoxFunction (name, functionParams, body, currentEnv, false)
        currentEnv.Define name.lexeme loxFunction
    | Stmt.Return (_, value) ->
        match value with
        | Some value ->
            raise (Return (evaluateExpr value))
        | None ->
            raise (Return null)
    | Stmt.Class (name, methods) ->
        currentEnv.Define name.lexeme null
        let methodMap =
            methods
            |> List.map (fun (Func (name, functionParams, body)) ->
                (name.lexeme, LoxFunction(name, functionParams, body, currentEnv, (name.lexeme = "init"))))
            |> dict

        let klass = LoxClass(name.lexeme, methodMap)
        currentEnv.Assign name klass

let interpret (resolutions : Dictionary<Expr, int>) (stmts : Stmt []) =
    try
        locals <- resolutions
        stmts |> Array.iter evaluateStmt
        Some ()
    with
        | RuntimeError (token, error) ->
            Error.runtimeError token error
            None
