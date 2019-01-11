module Flox.Parser

open Flox.Scanner

type Expr =
    | Binary of left : Expr * operator : Token * right : Expr
    | Grouping of expression : Expr
    | Literal of value : obj
    | Unary of operator : Token * right : Expr
    
// Creates an unambiguous, if ugly, string representation of AST nodes.
let rec printAst = function
    | Binary (left, operator, right) -> 
        sprintf "(%s %s %s)" operator.lexeme (printAst left) (printAst right)
    | Grouping expression ->
        sprintf "(group %s)" (printAst expression)
    | Literal value ->
        value.ToString()
    | Unary (operator, right) ->
        sprintf "(%s %s)" operator.lexeme (printAst right)