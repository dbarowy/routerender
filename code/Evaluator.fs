module Evaluator

open Parser

module Evaluator

open Parser
open AST


let evalDefense (cov: string, box: string) =

    let three_four = "<text x=\"510\" y=\"385\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">F</text>\n
    <text x=\"670\" y=\"385\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">N</text>\n
    <text x=\"830\" y=\"385\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">R</text>\n
    <text x=\"590\" y=\"285\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">B</text>\n
    <text x=\"750\" y=\"285\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">M</text>\n
    <text x=\"400\" y=\"335\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">W</text>\n
    <text x=\"940\" y=\"335\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">S</text>\n"
    
    let four_three = "<text x=\"480\" y=\"385\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">F</text>\n
    <text x=\"590\" y=\"385\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">E</text>\n
    <text x=\"750\" y=\"385\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">N</text>\n
    <text x=\"870\" y=\"385\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">R</text>\n
    <text x=\"670\" y=\"285\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">M</text>\n
    <text x=\"800\" y=\"285\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">S</text>\n
    <text x=\"540\" y=\"285\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">W</text>\n"

    let cover_2 = "<text x=\"380\" y=\"100\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">S</text>\n
  <text x=\"970\" y=\"100\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">FS</text>\n
  <text x=\"150\" y=\"385\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">C</text>\n
  <text x=\"1190\" y=\"385\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">C</text>\n"
    match box, cov with
    | "43", cover2 -> four_three + cover_2
    | _, _-> three_four + cover_2
    
let rec evalCanvas (canvas: Canvas) =
    match canvas with
    | [] -> ""
    | l::ls -> (evalDefense l) + (evalCanvas ls)


let eval (canvas: Canvas) : string =
    let csz = CANVAS_SZ |> string

    let OLine ="<circle cx=\"530\" cy=\"425\" r=\"30\" stroke=\"black\" stroke-width=\"2\" fill=\"none\"/>\n
  <circle cx=\"610\" cy=\"425\" r=\"30\" stroke=\"black\" stroke-width=\"2\" fill=\"none\"/>\n
  <rect x=\"660\" y=\"395\" width=\"60\" height=\"60\" stroke=\"black\" stroke-width=\"2\" fill=\"none\"/>\n
  <circle cx=\"770\" cy=\"425\" r=\"30\" stroke=\"black\" stroke-width=\"2\" fill=\"none\"/>\n
  <circle cx=\"850\" cy=\"425\" r=\"30\" stroke=\"black\" stroke-width=\"2\" fill=\"none\"/>\n"

    
    "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +

    OLine +

    (evalCanvas canvas)
    + "</svg>\n"