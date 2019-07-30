module Flox.Interpreter

open Flox.Env
open Flox.Error
open Flox.Parser
open Flox.Scanner

exception Return of value : obj

type ILoxCallable = 
    abstract member Call: (Stmt [] -> Env -> unit) * obj [] -> obj
    abstract member Arity: int

let globals = Env()
globals.Define "clock" 
    { new ILoxCallable with
        member __.Arity = 0
        member __.Call(_, _) =
            (System.DateTime.Now.Ticks / System.TimeSpan.TicksPerMillisecond) / 1000L :> obj }

type LoxFunction (name : Token, functionParams : Token [], body : Stmt [], closure : Env) =
    interface ILoxCallable with
        member __.Arity = functionParams.Length
        member __.Call(evaluateBlock, arguments) =
            let environment = Env(closure)
            for i = 0 to functionParams.Length - 1 do
                environment.Define (functionParams.[i].lexeme) (arguments.[i])

            evaluateBlock body environment
            null

    override __.ToString() =
        sprintf "<fn %s>" name.lexeme

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
    | Expr.Variable name -> currentEnv.Get name
    | Expr.Assign (name, expr) ->
        let value = evaluateExpr expr
        currentEnv.Assign name value
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
        try
            loxFunction.Call(evaluateBlock, evaluatedArguments)
        with
            | Return returnValue ->
                returnValue
        
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
    | Stmt.Function (name, functionParams, body) ->
        let loxFunction = LoxFunction (name, functionParams, body, currentEnv)
        currentEnv.Define name.lexeme loxFunction
    | Stmt.Return (_, value) ->
        match value with
        | Some value ->
            raise (Return (evaluateExpr value))
        | None ->
            raise (Return null)

let interpret (stmts : Stmt []) =
    try
        stmts |> Array.iter evaluateStmt
        Some ()
    with
        | RuntimeError (token, error) ->
            Error.runtimeError token error
            None
