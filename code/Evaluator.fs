module Evaluator

open Parser
open AST

let evalScheme(schm: string) =
    let pwr = "<!--Power Scheme against 3-4 Defense-->\n
    <!--Labeling Keys-->\n
    <text x=\"417\" y=\"285\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
    <text x=\"597\" y=\"235\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
    <!--Gap Hinge-->\n
    <line x1=\"530\" y1=\"395\" x2=\"580\" y2=\"395\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"580\" y1=\"395\" x2=\"530\" y2=\"485\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"530\" y1=\"465\" x2=\"530\" y2=\"505\" stroke=\"black\" stroke-width=\"3\"/>\n
    <!--Pull for -1-->\n
    <line x1=\"610\" y1=\"455\" x2=\"610\" y2=\"495\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"610\" y1=\"495\" x2=\"790\" y2=\"495\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"790\" y1=\"495\" x2=\"790\" y2=\"300\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"770\" y1=\"300\" x2=\"810\" y2=\"300\" stroke=\"black\" stroke-width=\"3\"/>\n
    <!--Down Block on Frank-->\n
    <line x1=\"690\" y1=\"395\" x2=\"560\" y2=\"375\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"555\" y1=\"390\" x2=\"565\" y2=\"355\" stroke=\"black\" stroke-width=\"3\"/>\n
    <!--Down Block on Nose-->\n
    <line x1=\"770\" y1=\"395\" x2=\"730\" y2=\"375\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"725\" y1=\"390\" x2=\"735\" y2=\"355\" stroke=\"black\" stroke-width=\"3\"/>\n
    <!--Block to Key-->\n
    <line x1=\"850\" y1=\"395\" x2=\"640\" y2=\"290\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"630\" y1=\"305\" x2=\"650\" y2=\"275\" stroke=\"black\" stroke-width=\"3\"/>\n"

    match schm with
    | "power" -> pwr
    | _ -> ""

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