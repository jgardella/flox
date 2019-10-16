open System
open System.IO
open Flox
open Flox.Parser

let mutable hadError = false
let mutable hadRuntimeError = false

let run (source : string) =
    let (tokens, errors) = Scanner.scanTokens source
    match errors with
    | [||] ->
        match Parser.parse tokens with
        | Some parseResult ->
            match Resolver.resolve parseResult with
            | Some resolutions ->
                match parseResult with
                | [|Stmt.Expression expr|] ->
                  Interpreter.evaluateExpr expr
                  |> Interpreter.stringify
                  |> printfn "%s"
                | stmts ->
                    match Interpreter.interpret resolutions stmts with
                    | Some _ -> ()
                    | None -> hadRuntimeError <- true
            | None -> 
                hadError <- true
        | None ->
            hadError <- true
    | scannerErrors ->
        hadError <- true
        scannerErrors
        |> Array.iter (fun { Scanner.ScannerError.line = line; message = message } -> 
            Error.error line message)

let [<Literal>] USAGE = "Usage: flox [script]"

let test = """
class Thing {
  getCallback() {
    fun localFunction() {
      print this;
    }

    return localFunction;
  }
}

var callback = Thing().getCallback();
callback();
"""

let runFile (path : string) =
    File.ReadAllText path |> run

let runPrompt () =
    while true do
        printf "%s" "> "
        Console.ReadLine() |> run
        hadError <- false

[<EntryPoint>]
let main argv =
    match argv with
    | [||] ->
//        runPrompt()
        run test
    | [| fileName |] ->
        runFile fileName
    | _ ->
        printfn "%s" USAGE
    if hadError then 65
    elif hadRuntimeError then 70
    else 0