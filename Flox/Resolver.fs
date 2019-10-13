module Flox.Resolver

open System.Collections.Generic
open Flox.Error
open Flox.Parser
open Flox.Scanner

type FunctionType =
    | NoFunction
    | Function

let mutable private scopes = Stack()
let mutable private resolutions = Dictionary() 
let mutable private currentFunctionType = FunctionType.NoFunction

let private beginScope () =
    scopes.Push(Dictionary())

let private endScope () =
    scopes.Pop() |> ignore
    ()

let private declare (name : Token) =
    if scopes.Count <> 0 then
        let scope = scopes.Peek()
        if scope.ContainsKey name.lexeme then
            raise (Error.compileError name "Variable with this name already declared in this scope.")
        scope.[name.lexeme] <- false

let private define (name : Token) =
    if scopes.Count <> 0 then
        scopes.Peek().[name.lexeme] <- true

let private resolveLocal (expr : Expr) (name : Token) =
    let arr = scopes.ToArray()
    let mutable found = false
    let mutable i = 0
    while i < scopes.Count && not found do
        if arr.[i].ContainsKey name.lexeme then
            resolutions.[expr] <- scopes.Count - 1 - i
        i <- i + 1
    
    // Not found, assume it is in global.

let rec private resolveFunction (funParams, body) functionType =
    let enclosingFunctionType = currentFunctionType
    currentFunctionType <- functionType

    beginScope()
    funParams
    |> Array.iter (fun funParam ->
        declare funParam
        define funParam)
    body |> Array.iter resolveStmt
    endScope()

    currentFunctionType <- enclosingFunctionType
and resolveExpr = function
    | Expr.Variable name as expr ->
        let value = ref false 
        if scopes.Peek().TryGetValue(name.lexeme, value) then
            if not !value then
                raise (Error.compileError name "Cannot read local variable in its own initializer.")

        resolveLocal expr name
    | Expr.Assign (name, value) as expr ->
        resolveExpr value
        resolveLocal expr name
    | Expr.Binary (left, _, right) ->
        resolveExpr left
        resolveExpr right
    | Expr.Call (callee, _, arguments) ->
        resolveExpr callee
        arguments |> Array.iter resolveExpr
    | Expr.Grouping expr ->
        resolveExpr expr
    | Expr.Literal _ -> ()
    | Expr.Logical (left, _, right) ->
        resolveExpr left
        resolveExpr right
    | Expr.Unary (_, expr) ->
        resolveExpr expr
and resolveStmt = function
    | Stmt.Block stmts ->
        beginScope()
        stmts |> Array.iter resolveStmt
        endScope()
        ()
    | Stmt.Var (name, initializer) ->
        declare name
        initializer |> Option.iter resolveExpr
        define name
        ()
    | Stmt.Function (name, functionParams, body) ->
        declare name
        define name
        resolveFunction (functionParams, body) FunctionType.Function
    | Stmt.Expression expr ->
        resolveExpr expr
    | Stmt.If (condition, thenBranch, elseBranch) ->
        resolveExpr condition
        resolveStmt thenBranch
        elseBranch |> Option.iter resolveStmt
    | Stmt.Print expr ->
        resolveExpr expr
    | Stmt.Return (keyword, value) ->
        if currentFunctionType = FunctionType.NoFunction then
            raise (Error.compileError keyword "Cannot return from top-level code.")
        value |> Option.iter resolveExpr
    | Stmt.While (condition, body) ->
        resolveExpr condition
        resolveStmt body

let resolve (stmts : Stmt []) =
    try
        scopes <- Stack()
        resolutions <- Dictionary()
        currentFunctionType <- FunctionType.NoFunction
        stmts |> Array.iter resolveStmt
        Some resolutions
    with
    | Error.CompileError -> None
