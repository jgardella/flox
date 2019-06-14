module Flox.Interpreter

open Flox.Env
open Flox.Error
open Flox.Parser
open Flox.Scanner

let mutable currentEnv = Env()

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
            (left :?> double) * (right :?> double)
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
        
let rec evaluateBlock (stmts : Stmt []) (env : Env) =
    let previousEnv = currentEnv
    try
        currentEnv <- env
        stmts |> Array.iter evaluateStmt
    finally
        currentEnv <- previousEnv
        
and evaluateStmt = function
    | Stmt.Expression expr -> evaluateExpr expr |> ignore
    | Stmt.Print stmt -> evaluateExpr stmt |> stringify |> printfn "%s"
    | Stmt.Var (name, initializer) ->
        let value = 
            initializer
            |> Option.map evaluateExpr 
            |> Option.defaultValue null
            
        currentEnv.Define name.lexeme value
    | Stmt.Block stmts ->
        evaluateBlock stmts (Env currentEnv)

let interpret (stmts : Stmt []) =
    try
        stmts |> Array.iter evaluateStmt
        Some ()
    with
        | RuntimeError (token, error) ->
            Error.runtimeError token error
            None