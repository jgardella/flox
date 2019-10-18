module Flox.Parser

open Flox.Scanner

type FunctionKind =
    | Function
    | Method
with
    override this.ToString() =
        match this with
        | Function -> "function"
        | Method -> "method"

type Expr =
    | Binary of left : Expr * operator : Token * right : Expr
    | Grouping of expression : Expr
    | Call of callee : Expr * paren : Token * arguments : Expr []
    | Literal of value : obj
    | Logical of exprLeft : Expr * operator : Token * exprRight : Expr
    | Unary of operator : Token * right : Expr
    | Variable of name : Token
    | Assign of name : Token * value : Expr
    | Get of object : Expr * name : Token
    | Set of object : Expr * name : Token * value : Expr
    | This of keyword : Token
    | Super of keyword : Token * method : Token

type Func = Func of name : Token * functionParams : Token [] * body : Stmt []
and Stmt =
    | Expression of expr : Expr
    | Function of func : Func
    | If of condition : Expr * thenBrach : Stmt * elseBranch : Stmt option
    | Print of expr : Expr
    | Return of keyword : Token * value : Expr option
    | Var of name : Token * initializer : Expr option
    | Block of Stmt []
    | While of condition : Expr * body : Stmt
    | Class of name : Token * superclass : Token option * methods : Func list
   
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
    | Assign (name, value) ->
        sprintf "(assign %s %s)" name.lexeme (printAst value)
    | Variable name ->
        sprintf "(var %s)" name.lexeme
    | Logical (exprLeft, operator, exprRight) ->
        sprintf "(logical %s %s %s)" operator.lexeme (printAst exprLeft) (printAst exprRight)
    | Call (callee, _, _) ->
        sprintf "(call %s)" (printAst callee) 
    | Get (object, name) ->
        sprintf "(get %s %s)" (printAst object) name.lexeme
    | Set (object, name, value) ->
        sprintf "(set %s %s %s)" (printAst object) name.lexeme (printAst value)
    | This keyword ->
        sprintf "(this %s)" keyword.lexeme
             
let parse (tokens : Token []) =
    let mutable current = 0
    
    let peekToken () =
        tokens.[current]
        
    let previousToken () =
        tokens.[current - 1]
    
    let isAtEnd () =
        peekToken().tokenType = TokenType.Eof
    
    let advance () =
        if not <| isAtEnd () then current <- current + 1
        previousToken()
    
    let checkToken (tokenType : TokenType) =
        if isAtEnd()
        then false
        else peekToken().tokenType = tokenType
    
    let matchToken (tokenTypes : TokenType list) =
        let mutable matchedToken = false
        let mutable i = 0
        while i < tokenTypes.Length && not matchedToken do
            if checkToken (tokenTypes.[i]) then
                advance() |> ignore
                matchedToken <- true
            i <- i + 1
                
        matchedToken
               
    let consumeToken (tokenType : TokenType) (message : string) =
        if checkToken tokenType
        then advance()
        else raise (Error.compileError (peekToken()) message)
        
    let synchronize () =
        advance() |> ignore
        
        let mutable foundStatementEnd = false
        while not <| isAtEnd() && not foundStatementEnd do
            if previousToken().tokenType = TokenType.Semicolon
            then foundStatementEnd <- true
            else
                match peekToken().tokenType with
                | TokenType.Class
                | TokenType.Fun
                | TokenType.Var
                | TokenType.For
                | TokenType.If
                | TokenType.While
                | TokenType.Print
                | TokenType.Return ->
                    foundStatementEnd <- true
                | _ -> advance () |> ignore
        
    let rec parsePrimary () =
        if matchToken [TokenType.False] then Expr.Literal false
        elif matchToken [TokenType.True] then Expr.Literal true
        elif matchToken [TokenType.Nil] then Expr.Literal null 
        elif matchToken [TokenType.Number; TokenType.String] then
            Expr.Literal (previousToken().literal)
        elif matchToken [TokenType.Super] then
            let keyword = previousToken()
            consumeToken TokenType.Dot "Expect '.' after 'super'." |> ignore
            let method = consumeToken TokenType.Identifier "Expect superclass method name."
            Expr.Super (keyword, method)
        elif matchToken [TokenType.This] then
            Expr.This (previousToken())
        elif matchToken [TokenType.Identifier] then
            Expr.Variable (previousToken())
        elif matchToken [TokenType.LeftParen] then
            let expr = parseExpression()
            consumeToken TokenType.RightParen "Expect ')' after expression." |> ignore
            Expr.Grouping expr
        else
            raise (Error.compileError (peekToken()) "Expect expression.")
        
    and finishCall (callee : Expr) =
        let arguments = ResizeArray()
        if not (checkToken TokenType.RightParen) then
            let mutable loop = true
            while loop do
                if arguments.Count >= 255 then (Error.compileError (peekToken()) "Cannot have more than 255 arguments.") |> ignore
                arguments.Add(parseExpression())
                loop <- matchToken [TokenType.Comma]
        
        let paren = consumeToken TokenType.RightParen "Expect ',' after arguments"

        Expr.Call (callee, paren, arguments.ToArray())

    and parseCall () =
        let mutable expr = parsePrimary()

        let mutable loop = true
        while loop do
            if matchToken [TokenType.LeftParen] then 
                expr <- finishCall(expr)
            elif matchToken [TokenType.Dot] then
                let name = consumeToken TokenType.Identifier "Expect property name after '.'."
                expr <- Expr.Get(expr, name)
            else 
                loop <- false

        expr

    and parseUnary () =
        if matchToken [TokenType.Bang; TokenType.Minus] then
            let operator = previousToken()
            let right = parseUnary()
            Expr.Unary (operator, right)
        else
            parseCall()
    
    and parseMultiplication () =
        let mutable expr = parseUnary()
       
        while matchToken [TokenType.Star; TokenType.Slash] do
            let operator = previousToken()
            let right = parseUnary()
            expr <- Expr.Binary (expr, operator, right)
           
        expr      
    
    and parseAddition () =
        let mutable expr = parseMultiplication()
        
        while matchToken [TokenType.Minus; TokenType.Plus] do
            let operator = previousToken()
            let right = parseMultiplication()
            expr <- Expr.Binary (expr, operator, right)
            
        expr   
        
    and parseComparison () =
        let mutable expr = parseAddition()
        
        while matchToken [TokenType.Greater; TokenType.GreaterEqual; TokenType.Less; TokenType.LessEqual] do
            let operator = previousToken()
            let right = parseAddition()
            expr <- Expr.Binary (expr, operator, right)
            
        expr
    
    and parseEquality () =
        let mutable expr = parseComparison()
        
        while matchToken [TokenType.BangEqual; TokenType.EqualEqual] do
            let operator = previousToken()
            let right = parseComparison()
            expr <- Expr.Binary(expr, operator, right)
            
        expr

    and parseAnd () =
        let mutable expr = parseEquality()

        while matchToken [TokenType.And] do
            let operator = previousToken()
            let right = parseAnd()
            expr <- Expr.Logical (expr, operator, right)

        expr

    and parseOr () =
        let mutable expr = parseAnd()

        while matchToken [TokenType.Or] do
            let operator = previousToken()
            let right = parseAnd()
            expr <- Expr.Logical (expr, operator, right)

        expr
        
    and parseAssignment () =
        let mutable expr = parseOr()
        
        if matchToken [TokenType.Equal] then
            let equals = previousToken()
            let value = parseAssignment()
            
            match expr with
            | Expr.Variable name ->
                expr <- Expr.Assign(name, value)
            | Expr.Get (object, name) ->
                expr <- Expr.Set (object, name, value)
            | _ ->
                Error.compileError equals "Invalid assignment target." |> ignore
                
        expr
        
    and parseExpression () = 
        parseAssignment()

    and parseForStatement () =
        consumeToken TokenType.LeftParen "Expect '(' after 'for'." |> ignore
        let initializer =
            if matchToken [TokenType.Semicolon] then
                None
            elif matchToken [TokenType.Var] then
                Some (parseVarDeclaration())
            else
                Some (parseExpressionStatement())

        let condition =
            if not (checkToken TokenType.Semicolon)
            then parseExpression()
            else Expr.Literal true
        consumeToken TokenType.Semicolon "Expect ';' after loop condition." |> ignore

        let increment =
            if not (checkToken TokenType.RightParen)
            then Some (parseExpression())
            else None
        consumeToken TokenType.RightParen "Expect ')' after for clauses." |> ignore

        let baseBody = parseStatement()

        let withIncrement =
            match increment with
            | Some increment ->
                Stmt.Block [|
                    baseBody
                    Stmt.Expression increment
                |]
            | None ->
                baseBody

        let withCondition = Stmt.While (condition, withIncrement)

        match initializer with
        | Some initializer ->
            Stmt.Block [|
                initializer
                withCondition
            |]
        | None -> withCondition

    and parseIfStatement () =
        consumeToken TokenType.LeftParen "Expect '(' after 'if'." |> ignore
        let condition = parseExpression()
        consumeToken TokenType.RightParen "Expect ')' after if condition." |> ignore

        let thenBranch = parseStatement()
        let elseBranch =
            if matchToken [TokenType.Else] then
                Some (parseStatement())
            else
                None

        Stmt.If (condition, thenBranch, elseBranch)
        
    and parsePrintStatement () =
        let value = parseExpression()
        consumeToken TokenType.Semicolon "Expect ';' after value." |> ignore
        Stmt.Print value
        
    and parseExpressionStatement () =
        let expr = parseExpression()
        consumeToken TokenType.Semicolon "Expect ';' after expression." |> ignore
        Stmt.Expression expr
        
    and parseBlockStatement () =
        let statements = new ResizeArray<Stmt option>()
        
        while not (checkToken TokenType.RightBrace) && not (isAtEnd()) do
            statements.Add(parseDeclaration())
        
        consumeToken TokenType.RightBrace "Expect '}' after block." |> ignore
        
        statements.ToArray() 
        |> Array.choose id

    and parseReturnStatement () =
        let keyword = previousToken()
        let value =
            if not (checkToken TokenType.Semicolon) then
                Some (parseExpression())
            else
                None

        consumeToken TokenType.Semicolon "Expect ';' after return value." |> ignore
        Stmt.Return (keyword, value)

    and parseWhileStatement () =
        consumeToken TokenType.LeftParen "Expect '(' after 'while'." |> ignore
        let condition = parseExpression()
        consumeToken TokenType.RightParen "Expect ')' after condition." |> ignore
        let body = parseStatement()
        Stmt.While (condition, body)

    and parseStatement () =
        if matchToken [TokenType.For] then parseForStatement()
        elif matchToken [TokenType.If] then parseIfStatement()
        elif matchToken [TokenType.Print] then parsePrintStatement()
        elif matchToken [TokenType.Return] then parseReturnStatement()
        elif matchToken [TokenType.While] then parseWhileStatement()
        elif matchToken [TokenType.LeftBrace] then Stmt.Block (parseBlockStatement())
        else parseExpressionStatement()
        
    and parseFunction (functionKind : FunctionKind) =
        let name = consumeToken TokenType.Identifier (sprintf "Expect %O name." functionKind)
        consumeToken TokenType.LeftParen (sprintf "Expect '(' after %O name." functionKind) |> ignore
        let parameters = ResizeArray()
        if not (checkToken TokenType.RightParen) then
            let mutable loop = true
            while loop do
                if parameters.Count >= 8 then
                    Error.compileError (peekToken()) "Cannot have more than 8 parameters." |> ignore
                parameters.Add (consumeToken TokenType.Identifier "Expect parameter name.")
                loop <- matchToken [TokenType.Comma]
        consumeToken TokenType.RightParen "Expect ')' after arguments." |> ignore

        consumeToken TokenType.LeftBrace (sprintf "Expect '{' before %O body." functionKind) |> ignore
        let body = parseBlockStatement()
        Func (name, parameters.ToArray(), body)

    and parseVarDeclaration () =
        let name = consumeToken TokenType.Identifier "Expect variable name."
        
        let initializer =
            if matchToken [TokenType.Equal]
            then Some (parseExpression())
            else None
            
        consumeToken TokenType.Semicolon "Expect ';' after variable declaration." |> ignore
        Stmt.Var (name, initializer)

    and parseClassDeclaration () =
        let name = consumeToken TokenType.Identifier "Expect class name."
        let superclass = 
            if matchToken [TokenType.Less] then
                consumeToken TokenType.Identifier "Expect superclass name." |> ignore
                Some (previousToken())
            else
                None
        consumeToken TokenType.LeftBrace "Expect '{' before class body." |> ignore

        let methods = ResizeArray()
        while not (checkToken TokenType.RightBrace) && not (isAtEnd()) do
            methods.Add(parseFunction FunctionKind.Method)
        
        consumeToken TokenType.RightBrace "Expect '}' after class body." |> ignore

        Stmt.Class (name, superclass, Array.toList (methods.ToArray()))

    and parseDeclaration () =
        try
            if matchToken [TokenType.Class] then Some (parseClassDeclaration())
            elif matchToken [TokenType.Fun] then Some (parseFunction FunctionKind.Function |> Stmt.Function)
            elif matchToken [TokenType.Var] then Some (parseVarDeclaration())
            else Some (parseStatement())
        with
            | Error.CompileError ->
                synchronize()
                None
    
    try
        let statements = new ResizeArray<Stmt option>()
        while not <| isAtEnd() do
            statements.Add(parseDeclaration())
        statements.ToArray()
        |> Array.choose id
        |> Some
    with 
    | Error.CompileError -> None
