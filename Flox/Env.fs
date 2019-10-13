module Flox.Env

open System.Collections.Generic
open Flox.Error
open Flox.Scanner

[<AllowNullLiteral>]
type Env(enclosing : Env) =
    let dict = Dictionary<string, obj>()
    new() = Env(null)
    member this.Dict = dict
    member private __.Enclosing = enclosing
    member this.Define (name : string) (value : obj) =
        dict.Add(name, value)
    member this.Get (name : Token) =
        let value = ref null
        if dict.TryGetValue(name.lexeme, value)
        then !value
        elif not (isNull this.Enclosing)
        then this.Enclosing.Get name
        else raise (RuntimeError (name, sprintf "Undefined variable '%s'." name.lexeme))
    member this.Ancestor (distance : int) =
        let mutable env = this
        for i = 1 to distance do
            env <- env.Enclosing
        env
    member this.GetAt (distance : int) (name : Token) =
        this.Ancestor(distance).Dict.[name.lexeme]
    member this.Assign (name : Token) (value : obj) =
        if dict.ContainsKey name.lexeme
        then dict.[name.lexeme] <- value
        elif not (isNull this.Enclosing)
        then this.Enclosing.Assign name value
        else raise (RuntimeError (name, sprintf "Undefined variable '%s'." name.lexeme))
    member this.AssignAt (distance : int) (name : Token) (value : obj) =
        this.Ancestor(distance).Dict.[name.lexeme] <- value
