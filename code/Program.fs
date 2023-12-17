// Tim Forth & Eric Gage c. 2023

open Evaluator
open System.IO
open Parser


(*
 * Running 'dotnet run <filename>' at the command line will create an svg file
 * inside the 'code' directory. You can open this file using Google Chrome or an 
 * svg viewer of your choice.
 *)
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
        printfn "Invalid program. Usage: dotnet run <filename> where <filename> is a valid RouteRender program. See specification for more details."
        1