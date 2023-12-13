﻿open Evaluator
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
        printfn "Invalid program. Usage: (43, cover2)power[(x,curl,1), (y,comeback,2),(z,slant,3)](shotgun, 1x2);"
        1