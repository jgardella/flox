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
        
exception ParseError
      
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
        
    let error (token : Token) (message : string) =
        Error.tokenError token message 
        ParseError
        
    let consumeToken (tokenType : TokenType) (message : string) =
        if checkToken tokenType
        then advance()
        else raise (error (peekToken()) message)
        
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
        elif matchToken [TokenType.LeftParen] then
            let expr = parseExpression()
            consumeToken TokenType.RightParen "Expect ')' after expression." |> ignore
            Expr.Grouping expr
        else
            raise (error (peekToken()) "Expect expression.")
        
    and parseUnary () =
        if matchToken [TokenType.Bang; TokenType.Minus] then
            let operator = previousToken()
            let right = parseUnary()
            Expr.Unary (operator, right)
        else
            parsePrimary()
    
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
        
    and parseExpression () = 
        parseEquality()
    
    try
        parseExpression() |> Some
    with 
    | ParseError -> None
