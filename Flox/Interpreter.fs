module Flox.Interpreter

open Flox.Scanner
open Flox.Parser

exception RuntimeError of token : Token * message : string

let isTruthy (object : obj) =
    match object with
    | null -> false
    | :? bool -> object :?> bool
    | _ -> true
    
let isEqual (a : obj) (b : obj) =
    // nil is only equal to nil.
    if a = null && b = null then true
    elif a = null then false
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

let rec evaluate = function
    | Expr.Literal literal -> literal
    | Expr.Grouping expr -> evaluate expr
    | Expr.Unary (operator, right) ->
        let right = evaluate right
        
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
        let left = evaluate left
        let right = evaluate right
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
        
let interpret (expr : Expr) =
    try
        let value = evaluate expr
        printfn "%s" (stringify value)
        Some value
    with
        | RuntimeError (token, error) ->
            Error.runtimeError token error
            None
