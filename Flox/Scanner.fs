module Flox.Scanner
open System

type TokenType =
    // Single-character tokens.
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star
    
    // One or two character tokens.
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    
    // Literals.
    | Identifier
    | String
    | Number
    
    // Keywords.
    | And
    | Class
    | Else
    | False
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True
    | Var
    | While
    
    | Eof

type Token = {
    tokenType : TokenType
    lexeme : string
    literal : obj
    line : int
} with
    override this.ToString() =
        sprintf "%O %s %O" this.tokenType this.lexeme this.literal

type ScannerError = {
    line : int
    message : string
}

let token (tokenType : TokenType) (lexeme : string) (literal : obj) (line : int) = {
    Token.tokenType = tokenType
    lexeme = lexeme
    literal = literal
    line = line
}

let reservedKeywords = 
    [
        ("and", TokenType.And)
        ("class", TokenType.Class)
        ("else", TokenType.Else)
        ("false", TokenType.False)
        ("fun", TokenType.Fun)
        ("for", TokenType.For)
        ("if", TokenType.If)
        ("nil", TokenType.Nil)
        ("or", TokenType.Or)
        ("print", TokenType.Print)
        ("return", TokenType.Return)
        ("super", TokenType.Super)
        ("this", TokenType.This)
        ("true", TokenType.True)
        ("var", TokenType.Var)
        ("while", TokenType.While)
    ] |> Map.ofList

let scanTokens (source : string) =
    let tokens = ResizeArray<Token>()
    let errors = ResizeArray<ScannerError>()
    let mutable start = 0
    let mutable current = 0
    let mutable line = 1
    
    let isAtEnd () =
        current >= source.Length
    
    let advance () =
        current <- current + 1
        source.Chars (current - 1)
        
    let addTokenLiteral (tokenType : TokenType) (literal : obj) =
        let text = source.Substring(start, current - start)
        tokens.Add({ Token.tokenType = tokenType; lexeme = text; literal = literal; line = line })
        
    let addToken (tokenType : TokenType) =
        addTokenLiteral tokenType null
        
    let matchToken (expected : char) =
        if isAtEnd () || source.Chars current <> expected
        then false
        else 
            current <- current + 1
            true
            
    let peekToken () =
        if isAtEnd () 
        then '\u0000'
        else source.Chars current
        
    let stringToken () =
        while peekToken() <> '"' && not <| isAtEnd() do
            if peekToken () = '\n' then line <- line + 1
            advance() |> ignore
            
        // Unterminated string.
        if isAtEnd() 
        then errors.Add({ ScannerError.line = line; message = "Unterminated string." })
        
        // The closing ".
        advance() |> ignore
        
        // Trim the surrounding quotes.
        source.Substring(start + 1, current - start - 1) 
        |> addTokenLiteral TokenType.String
        
    let isDigit (c : char) =
        c >= '0' && c <= '9'
        
    let peekNextToken () =
        if current + 1 >= source.Length
        then '\u0000'
        else source.Chars(current + 1)
        
    let numberToken () =
        while peekToken() |> isDigit do 
            advance() |> ignore
        
        // Look for a fractional part.
        if peekToken() = '.' && peekNextToken() |> isDigit then
            // Consume the "."
            advance() |> ignore
            
            while peekToken() |> isDigit do 
                advance() |> ignore
            
        let literal = source.Substring(start, current - start) |> Double.Parse 
        addTokenLiteral TokenType.Number literal
        
    let isAlpha (c : char) =
        (c >= 'a' && c <= 'z') || 
        (c >= 'A' && c <= 'Z') ||
        c = '_'
        
    let isAlphaNumeric (c : char) =
        isAlpha c || isDigit c
        
    let identifierToken () =
        while peekToken() |> isAlphaNumeric do 
            advance() |> ignore
        
        reservedKeywords
        |> Map.tryFind (source.Substring(start, current - start))
        |> Option.defaultValue TokenType.Identifier
        |> addToken
    
    let scanToken () =
        match advance() with
        | '(' -> addToken TokenType.LeftParen
        | ')' -> addToken TokenType.RightParen
        | '{' -> addToken TokenType.LeftBrace
        | '}' -> addToken TokenType.RightBrace
        | ',' -> addToken TokenType.Comma
        | '.' -> addToken TokenType.Dot
        | '-' -> addToken TokenType.Minus
        | '+' -> addToken TokenType.Plus
        | ';' -> addToken TokenType.Semicolon
        | '*' -> addToken TokenType.Star
        | '!' -> (if matchToken '=' then TokenType.BangEqual else TokenType.Bang) |> addToken
        | '=' -> (if matchToken '=' then TokenType.EqualEqual else TokenType.Equal) |> addToken
        | '<' -> (if matchToken '=' then TokenType.LessEqual else TokenType.Less) |> addToken
        | '>' -> (if matchToken '=' then TokenType.GreaterEqual else TokenType.Greater) |> addToken
        | '/' ->
            if matchToken '/'
            then 
                while peekToken() <> '\n' && not <| isAtEnd() do 
                    advance() |> ignore
            else addToken TokenType.Slash
        | ' '
        | '\r'
        | '\t' ->
            // Ignore whitespace.
            ()
        | '\n' -> line <- line + 1
        | '"' -> stringToken()
        | c when isDigit c -> numberToken()
        | c when isAlpha c -> identifierToken()
        | _ -> errors.Add({ ScannerError.line = line; message = "Unexpected character."})
        
    while not <| isAtEnd() do
        start <- current
        scanToken()
    
    tokens.Add({ Token.tokenType = TokenType.Eof; lexeme = ""; literal = null; line = line })
    (tokens.ToArray(), errors.ToArray())