open Evaluator
open System
open Parser

[<EntryPoint>]
let main args =
    let file = args[0]
    let text = IO.File.ReadAllText file
    match parse text with
    | Some ast ->
        let svg = evalPlaybook ast
        printfn "%s" svg
        0
    | None ->
        printfn "%s" text
        printfn "Invalid program."
        1