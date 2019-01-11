open System
open System.IO
open Flox

let mutable hadError = false

let report (line : int) (where : string) (message : string) =
    eprintfn "[line %d] Error%s: %s" line where message
    hadError <- true
        
let error (line : int) (message : string) =
    report line "" message

let run (source : string) =
    let (tokens, errors) = Scanner.scanTokens source
    tokens
    |> Array.iter (fun token -> printfn "%O" token)
    errors
    |> Array.iter (fun { Scanner.ScannerError.line = line; message = message } -> error line message)

let [<Literal>] USAGE = "Usage: flox [script]"

let runFile (path : string) =
    File.ReadAllText path
    |> run

let runPrompt () =
    while true do
        printf "%s" "> "
        Console.ReadLine() |> run
        hadError <- false

[<EntryPoint>]
let main argv =
    match argv with
    | [||] ->
        runPrompt()
    | [| fileName |] ->
        runFile fileName
    | _ ->
        printfn "%s" USAGE
    if hadError
    then 65
    else 0