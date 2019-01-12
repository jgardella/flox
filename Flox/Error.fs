module Flox.Error

open Flox.Scanner

let report (line : int) (where : string) (message : string) =
    eprintfn "[line %d] Error%s: %s" line where message
        
let tokenError (token : Token) (message : string) =
    if token.tokenType = TokenType.Eof
    then report token.line " at end" message
    else report token.line (sprintf " at '%s'"  token.lexeme) message

let error (line : int) (message : string) =
    report line "" message

let runtimeError (token : Token) (message : string) =
    eprintfn "%s\n[line %d]" message token.line