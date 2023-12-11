module Evaluator

open Parser
open AST

let x_cords = (0,0)
let y_cords = (0,0)
let z_cords = (0,0)
let a_cords = (0,0)


let evalRoutes(routes: Routes) = 
    let rec extractPlayers(routes: Routes) = 
      let position_list= []
      match routes with
      | [] -> position_list
      | (p, _, _)::ls -> p::position_list @ extractPlayers(ls)

    let rec extractMovements(routes: Routes) =
      let movement_list = []
      match routes with
      | [] -> movement_list
      | (_,m,_)::ls -> m::movement_list @ extractMovements(ls)

    let rec extractReads(routes: Routes) =
      let read_list = []
      match routes with
      | [] -> read_list
      | (_,_,r)::ls -> r::read_list @ extractReads(ls)

    let players = extractPlayers(routes)
    

    let movements = extractMovements(routes)
    let reads = extractReads(routes)
    ""

let evalFormation(unit: Unit, rs: Receivers) =
  
    let getRec(r: Receivers) = 
      match rs with
      | YesReceivers (fld, bnd) -> (int fld, int bnd) 
      | NoReceivers -> (0,0)
  
    let field, boundry = getRec(rs)

    let fieldTup = (1, field)
    let boundryTup = (1, boundry)

    let rec drawField(x: int, c: int) = 
        if x = c then
            ""
        else
            // need better way to extract position from Routes list 
            let pos = ['Y';'Z';'A'][x]
            // Update cords here for routes 
            let Xval = 600 - 150 * x
            let mutable Yval = 425
            if x > 1 then
                Yval <- 485
            "<circle cx=\"" + string Xval + "\" cy=\"" + string Yval + "\" r=\"30\" stroke=\"black\" stroke-width=\"3\" fill=\"none\"/> \n<text x=\"" + string (Xval - 13) + "\" y=\"" + string (Yval + 15) + "\" font-size=\"45\" font-family=\"Arial, Helvetica, sans-serif\">" + string pos + "</text>\n" + drawField(x + 1, c)
    let rec drawBoundry(x: int, c: int) = 
        if x = c then
            ""
        else
            let pos = ['X','A']
            let Xval = 930 + 150 * x
            let mutable Yval = 425
            if x > 1 then
                Yval <- 485
            "<circle cx=\"" + string Xval + "\" cy=\"" + string Yval + "\" r=\"30\" stroke=\"black\" stroke-width=\"3\" fill=\"none\"/> \n<text x=\"" + string (Xval - 13) + "\" y=\"" + string (Yval + 15) + "\" font-size=\"45\" font-family=\"Arial, Helvetica, sans-serif\">" + string pos + "</text>\n" + drawBoundry(x + 1, c)
 
    drawField fieldTup + drawBoundry boundryTup

let evalScheme(schm: Scheme) =
    let pwr34 = "<!--Power Scheme against 3-4 Defense-->\n
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

    let pwr43 = "<!--Power Scheme against 4-3 Defense-->\n
<!--Labeling Keys-->\n
<text x=\"555\" y=\"225\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
<!--Block F-->\n
<line x1=\"530\" y1=\"395\" x2=\"500\" y2=\"375\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"490\" y1=\"390\" x2=\"510\" y2=\"360\" stroke=\"black\" stroke-width=\"3\"/>\n
<!--Pull for -1-->\n
<line x1=\"610\" y1=\"455\" x2=\"610\" y2=\"495\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"610\" y1=\"495\" x2=\"790\" y2=\"495\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"790\" y1=\"495\" x2=\"790\" y2=\"300\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"770\" y1=\"300\" x2=\"810\" y2=\"300\" stroke=\"black\" stroke-width=\"3\"/>\n
<!--Down Block on Eagle-->\n
<line x1=\"690\" y1=\"395\" x2=\"640\" y2=\"375\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"635\" y1=\"390\" x2=\"645\" y2=\"360\" stroke=\"black\" stroke-width=\"3\"/>\n
<!--B to K-->\n
<line x1=\"770\" y1=\"395\" x2=\"770\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"750\" y1=\"380\" x2=\"790\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"750\" y1=\"355\" x2=\"600\" y2=\"275\" stroke=\"black\" stroke-dasharray=\"4\" stroke-width=\"3\"/>\n
<line x1=\"590\" y1=\"285\" x2=\"610\" y2=\"260\" stroke=\"black\" stroke-width=\"3\"/>\n
<!--B to K-->\n
<line x1=\"850\" y1=\"395\" x2=\"800\" y2=\"375\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"795\" y1=\"390\" x2=\"805\" y2=\"360\" stroke=\"black\" stroke-width=\"3\"/>\n
"

    let ctr34 = "<!--Counter Scheme against 3-4 Defense-->\n
    <!--Labeling Keys-->\n
    <text x=\"417\" y=\"285\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
    <text x=\"597\" y=\"235\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
    <!--Pull for -1-->\n
    <line x1=\"530\" y1=\"455\" x2=\"545\" y2=\"495\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"545\" y1=\"495\" x2=\"790\" y2=\"495\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"790\" y1=\"495\" x2=\"790\" y2=\"300\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"770\" y1=\"300\" x2=\"810\" y2=\"300\" stroke=\"black\" stroke-width=\"3\"/>\n
    <!--Kickout-->\n
    <line x1=\"610\" y1=\"455\" x2=\"610\" y2=\"485\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"610\" y1=\"485\" x2=\"870\" y2=\"480\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"870\" y1=\"460\" x2=\"870\" y2=\"500\" stroke=\"black\" stroke-width=\"3\"/>\n
    <!--Down Block on Frank-->\n
    <line x1=\"690\" y1=\"395\" x2=\"560\" y2=\"375\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"555\" y1=\"390\" x2=\"565\" y2=\"355\" stroke=\"black\" stroke-width=\"3\"/>\n
    <!--Down Block on Nose-->\n
    <line x1=\"770\" y1=\"395\" x2=\"730\" y2=\"375\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"725\" y1=\"390\" x2=\"735\" y2=\"355\" stroke=\"black\" stroke-width=\"3\"/>\n
    <!--Block to BS Key-->\n
    <line x1=\"850\" y1=\"395\" x2=\"640\" y2=\"290\" stroke=\"black\" stroke-width=\"3\"/>\n
    <line x1=\"630\" y1=\"305\" x2=\"650\" y2=\"275\" stroke=\"black\" stroke-width=\"3\"/>\n"

    let ctr43 = "<!--Counter Scheme against 4-3 Defense-->\n
  <!--Labeling Keys-->\n
  <text x=\"555\" y=\"225\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
  <!--Pull for -1-->\n
  <line x1=\"530\" y1=\"455\" x2=\"545\" y2=\"495\" stroke=\"black\" stroke-width=\"3\"/>\n
  <line x1=\"545\" y1=\"495\" x2=\"790\" y2=\"495\" stroke=\"black\" stroke-width=\"3\"/>\n
  <line x1=\"790\" y1=\"495\" x2=\"790\" y2=\"300\" stroke=\"black\" stroke-width=\"3\"/>\n
  <line x1=\"770\" y1=\"300\" x2=\"810\" y2=\"300\" stroke=\"black\" stroke-width=\"3\"/>\n
  <!--Kickout-->\n
  <line x1=\"610\" y1=\"455\" x2=\"610\" y2=\"485\" stroke=\"black\" stroke-width=\"3\"/>\n
  <line x1=\"610\" y1=\"485\" x2=\"870\" y2=\"480\" stroke=\"black\" stroke-width=\"3\"/>\n
  <line x1=\"870\" y1=\"460\" x2=\"870\" y2=\"500\" stroke=\"black\" stroke-width=\"3\"/>\n
  <!--Down Block on Eagle-->\n
  <line x1=\"690\" y1=\"395\" x2=\"640\" y2=\"375\" stroke=\"black\" stroke-width=\"3\"/>\n
  <line x1=\"635\" y1=\"390\" x2=\"645\" y2=\"360\" stroke=\"black\" stroke-width=\"3\"/>\n
  <!--B to K-->\n
  <line x1=\"770\" y1=\"395\" x2=\"770\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
  <line x1=\"750\" y1=\"380\" x2=\"790\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
  <line x1=\"750\" y1=\"355\" x2=\"600\" y2=\"275\" stroke=\"black\" stroke-dasharray=\"4\" stroke-width=\"3\"/>\n
  <line x1=\"590\" y1=\"285\" x2=\"610\" y2=\"260\" stroke=\"black\" stroke-width=\"3\"/>\n
  <!--B to K-->\n
  <line x1=\"850\" y1=\"395\" x2=\"800\" y2=\"375\" stroke=\"black\" stroke-width=\"3\"/>\n
  <line x1=\"795\" y1=\"390\" x2=\"805\" y2=\"360\" stroke=\"black\" stroke-width=\"3\"/>\n"


    match schm with
    | Power -> pwr34
    | Counter -> ctr34
    | _ -> ""

let evalDefense (box: Box, cov: Coverage) =

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
    | FourThree, cover2 -> four_three + cover_2
    | _, _-> three_four + cover_2
    
let evalPlay (play: Play) =
    match play with
    | (a,b,c,d) -> evalDefense a + evalFormation b + evalScheme c + evalRoutes d


let eval (play: Play) : string =
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

    (evalPlay play)
    + "</svg>\n"


let rec evalPlaybook(playbook: Playbook) =
  match playbook with
  | [] -> ""
  | l::ls -> eval l + evalPlaybook ls