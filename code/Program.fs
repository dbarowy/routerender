open Evaluator
open System.IO
open Parser

[<EntryPoint>]
let main args =
    let file = args[0]
    let text = File.ReadAllText file
    match parse text with
    | Some ast ->
        let svg = eval ast
        let path = __SOURCE_DIRECTORY__
        let fullPath = Path.Combine (path, Path.ChangeExtension (file, "svg"))
        File.WriteAllText (fullPath, svg)
        0
    | None ->
        printfn "Invalid program."
        1