// Tim Forth & Eric Gage c. 2023

module Evaluator

open Parser
open AST
(*
  Interprets and extracts the Routes input

  Returns: 3-tuple of the (players,movements,reads)
*)
let evalRoutes(routes: Routes) = 
    
    // creates a list of the players in the passing formation
    let rec extractPlayers(routes: Routes) = 
      let position_list= []
      match routes with
      | [] -> position_list
      | (p, _, _)::ls -> p::position_list @ extractPlayers(ls)
    
    // creates a list of the routes each player is running
    let rec extractMovements(routes: Routes) =
      let movement_list = []
      match routes with
      | [] -> movement_list
      | (_,m,_)::ls -> m::movement_list @ extractMovements(ls)

    // creates a list of the quarterbacks read for each route
    let rec extractReads(routes: Routes) =
      let read_list = []
      match routes with
      | [] -> read_list
      | (_,_,r)::ls -> r::read_list @ extractReads(ls)

    // extracts the Routes input into lists
    let players = extractPlayers(routes)
    let movements = extractMovements(routes)
    let reads = extractReads(routes)
    
    // returns the Routes input in list format
    (players,movements,reads)

(*
  Draws the offensive skill side of the ball

  Returns: string of svg code
*)
let evalFormation(unit: Unit, rs: Receivers,players,movements,reads) =

    // returns the char of the Player
    let playerToChar player = 
      match player with
      | X -> 'X'
      | Y -> 'Y'
      | Z -> 'Z'
      | H -> 'H'
      | A -> 'A'

    // returns the string representation of the read
    let readToChar read = 
      match read with
      | First -> "1"
      | Second -> "2"
      | Third -> "3"
      | Fourth -> "4"
      | Fifth -> "5"
    
    // list of all the inputted receivers
    let charsList = List.map playerToChar players

    // returns the number of receivers and on the field and boundry side of the field
    let getRec(r: Receivers) = 
      match r with
      | YesReceivers(fld: char, bnd: char) -> (int fld - int('0'), int bnd - int('0')) 
      | NoReceivers -> (0,0)
  
    // field, boundry = number x number
    let field, boundary = getRec(rs)

    // creates a tuple for ascending recursion 
    let fieldTup = (0, field)
    let boundaryTup = (0, boundary)
 
    // draws the routes for each receiver on the field side
    let rec drawField(x: int, c: int) = 
        if x = c then
            ""
        else
            // the position of which route will be drawn 
            let pos = ['Y';'Z';'A'][x]
            
            // update the coordinates for each receiver 
            let Xval = 600 - 150 * (x+1)
            let mutable Yval = 425

            // vars holding the movement and read
            let m = charsList|> List.findIndex (fun x -> x = pos)
            let p = m |> (fun index -> (List.item index movements))
            let r = m |> (fun index -> (List.item index reads))

            
            // TE (Y) on the line of scrimmage all other receivers off the line 
            if x > 0 then Yval <- 485

            // draws the receiver
            "<circle cx=\"" + string Xval + "\" cy=\"" + string Yval + "\" r=\"30\" stroke=\"black\" stroke-width=\"3\" fill=\"none\"/> \n
            <text x=\"" + string (Xval - 13) + "\" y=\"" + string (Yval + 15) + "\" font-size=\"45\" font-family=\"Arial, Helvetica, sans-serif\">" + string pos + "</text>\n" 
            +

            // draws the route and read
            match p with
            | Go -> 
              "<!--Go Route-->\n
               <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30)+ "\" x2=\"" + string Xval + "\" y2=\"100\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\" />\n
               <text x=\"" + string (Xval + 15) + "\" y=\"90\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | Slant -> 
              "<!--Slant Route-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2=\"" + string Xval + "\" y2=\"" + string (Yval - 75) + "\" stroke=\"black\" stroke-width=\"2\"/>\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 75) + "\" x2=\"" + string (Xval + 210) + "\" y2=\"" + string (Yval - 155) + "\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval + 215) + "\" y=\"" + string (Yval + - 155)+ "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | Out ->
              "<!--Out Route-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2=\"" + string Xval + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\"/>\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 255) + "\" x2=\"" + string (Xval - 100) + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval - 85) + "\" y=\"" + string(Yval - 265) + "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | In ->
              "<!--In Route-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2=\"" + string Xval + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\"/>\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 255) + "\" x2=\"" + string (Xval + 100) + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval + 115) + "\" y=\"" + string(Yval - 265) + "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>"
            | Corner ->
              "<!--Corner Route-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2=\"" + string Xval + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\"/>\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 255) + "\" x2=\"" + string (Xval - 200) + "\" y2=\"" + string (Yval - 355) + "\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval - 185) + "\" y=\"" + string (Yval - 365) + "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | Post ->
             "<!--Post Route-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2=\"" + string Xval + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\"/>\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 255) + "\" x2=\"" + string (Xval + 200) + "\" y2=\"" + string (Yval - 355) + "\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval + 185) + "\" y=\"" + string (Yval - 365) + "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | Curl ->
             "<!--Curl Route-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2=\"" + string Xval + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\"/>\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 255) + "\" x2=\"" + string (Xval + 50) + "\" y2=\"" + string (Yval - 205) + "\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval + 65) + "\" y=\"" + string (Yval - 215) + "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | Comeback ->
             "<!--Comeback Route-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2=\"" + string Xval + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\"/>\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 255) + "\" x2=\"" + string (Xval - 50) + "\" y2=\"" + string (Yval - 205) + "\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval - 65) + "\" y=\"" + string (Yval - 215) + "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | Block ->
             "<!--WR Block-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2 =\"" + string Xval + "\" y2=\"" + string (Yval - 80) + "\" stroke=\"black\" stroke-width=\"3\"/>\n
              <line x1=\"" + string (Xval - 20) + "\" y1=\"" + string (Yval - 80) + "\" x2=\"" + string (Xval + 20) + "\" y2=\"" + string (Yval - 80) + "\" stroke=\"black\" stroke-width=\"3\"/>\n"
            | Screen ->
              "<!--WR Screen-->
              <path d=\"M" + string Xval + "," + string (Yval + 30) + " Q" + string (Xval + 50) + "," + string (Yval + 90) + " " + string (Xval + 100) + "," + string (Yval + 100) + "\" stroke=\"black\" fill=\"none\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval + 115) + "\" y=\"" + string (Yval + 90) + "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | _ -> ""
            + drawField(x + 1, c)

    // var holding whether or not the RB (A) is lined up as a receiver
    let mutable aRec = false
    
    // draws the routes for each receiver on the boundry side
    let rec drawBoundary(x: int, c: int) = 
     
        if x = c then
            ""
        else
            // the position of which route will be drawn
            let pos = ['X'; 'H'; 'A'][x]

            // update the coordinates for each receiver 
            let Xval = 930 + 150 * (x+1)
            let mutable Yval = 425

            // updates aRec if the RB (A) is lined up in the receiver position
            if pos = 'A' then aRec <- true

            // vars holding the movement and read
            let m = charsList|> List.findIndex (fun x -> x = pos)
            let p = m |> (fun index -> (List.item index movements))
            let r = m |> (fun index -> (List.item index reads))
    
            // X on the line of scrimmage all other receivers off the line
            if x > 0 then
                Yval <- 485

            // draws the receiver
            "<circle cx=\"" + string Xval + "\" cy=\"" + string Yval + "\" r=\"30\" stroke=\"black\" stroke-width=\"3\" fill=\"none\"/>\n
            <text x=\"" + string (Xval - 13) + "\" y=\"" + string (Yval + 15) + "\" font-size=\"45\" font-family=\"Arial, Helvetica, sans-serif\">" + string pos + "</text>\n"
            +

            // draws the route and read
            match p with
            | Go -> 
              "<!--Go Route-->\n
               <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30)+ "\" x2=\"" + string Xval + "\" y2=\"100\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\" />\n
               <text x=\"" + string (Xval + 15) + "\" y=\"90\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | Slant -> 
              "<!--Slant Route-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2=\"" + string Xval + "\" y2=\"" + string (Yval - 75) + "\" stroke=\"black\" stroke-width=\"2\"/>\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 75) + "\" x2=\"" + string (Xval - 210) + "\" y2=\"" + string (Yval - 155) + "\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval - 215) + "\" y=\"" + string (Yval + - 155)+ "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | In ->
              "<!--In Route-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2=\"" + string Xval + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\"/>\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 255) + "\" x2=\"" + string (Xval - 100) + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval - 85) + "\" y=\"" + string(Yval - 265) + "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | Out ->
              "<!--Out Route-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2=\"" + string Xval + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\"/>\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 255) + "\" x2=\"" + string (Xval + 100) + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval + 115) + "\" y=\"" + string(Yval - 265) + "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>"
            | Post ->
              "<!--Post Route-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2=\"" + string Xval + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\"/>\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 255) + "\" x2=\"" + string (Xval - 200) + "\" y2=\"" + string (Yval - 355) + "\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval - 185) + "\" y=\"" + string (Yval - 365) + "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | Corner ->
             "<!--Corner Route-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2=\"" + string Xval + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\"/>\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 255) + "\" x2=\"" + string (Xval + 200) + "\" y2=\"" + string (Yval - 355) + "\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval + 185) + "\" y=\"" + string (Yval - 365) + "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | Comeback ->
             "<!--Comeback Route-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2=\"" + string Xval + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\"/>\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 255) + "\" x2=\"" + string (Xval + 50) + "\" y2=\"" + string (Yval - 205) + "\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval + 65) + "\" y=\"" + string (Yval - 215) + "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | Curl ->
             "<!--Curl Route-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2=\"" + string Xval + "\" y2=\"" + string (Yval - 255) + "\" stroke=\"black\" stroke-width=\"2\"/>\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 255) + "\" x2=\"" + string (Xval - 50) + "\" y2=\"" + string (Yval - 205) + "\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval - 65) + "\" y=\"" + string (Yval - 215) + "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | Block ->
             "<!--WR Block-->\n
              <line x1=\"" + string Xval + "\" y1=\"" + string (Yval - 30) + "\" x2 =\"" + string Xval + "\" y2=\"" + string (Yval - 80) + "\" stroke=\"black\" stroke-width=\"3\"/>\n
              <line x1=\"" + string (Xval - 20) + "\" y1=\"" + string (Yval - 80) + "\" x2=\"" + string (Xval + 20) + "\" y2=\"" + string (Yval - 80) + "\" stroke=\"black\" stroke-width=\"3\"/>\n"
            | Screen ->
             "<!--WR Screen-->
              <path d=\"M" + string Xval + "," + string (Yval + 30) + " Q" + string (Xval - 50) + "," + string (Yval + 90) + " " + string (Xval - 100) + "," + string (Yval + 100) + "\" stroke=\"black\" fill=\"none\" marker-end=\"url(#arrow)\"/>\n
              <text x=\"" + string (Xval - 115) + "\" y=\"" + string (Yval + 90) + "\" stroke=\"black\" font-family=\"Arial, Helvetica, sans-serif\">" + readToChar r + "</text>\n"
            | _ -> ""
            + drawBoundary(x + 1, c)

    // draws the backfield
    let drawFormation (unit) =

      // var that holds whether or not the RB will run the ball or block
      let mutable run = false

      // updates run
      if List.contains 'A' charsList then
        let m = charsList|> List.findIndex (fun x -> x = 'A')
        let p = m |> (fun index -> (List.item index movements))
        run <- if p = Run then true else false


      // the RB cannot be lined up in the backfield and receiver position
      //          -> this cannot happen with the parser, but is included due to the
      //             incomplete pattern matching below
      if run = true && aRec = true then printfn "Usage Error: A cannot run the ball and go out for a pass"

      printfn "%b" aRec
      // draws the backfield 
      match unit, run, aRec with
      | Shotgun, false, false -> 
        "<!--Shotgun Formation-->\n
        <text x=\"668\" y=\"635\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">Q</text>\n
        <circle cx=\"770\" cy=\"615\" r=\"30\" stroke=\"black\" stroke-width=\"3\" fill=\"none\"/>\n
        <text x=\"757\" y=\"630\" font-size=\"45\" font-family=\"Arial, Helvetica, sans-serif\">A</text>\n
        <!--RB Block-->\n
        <line x1=\"770\" y1=\"585\" x2 =\"770\" y2=\"505\" stroke=\"black\" stroke-width=\"3\"/>\n
        <line x1=\"750\" y1=\"505\" x2=\"790\" y2=\"505\" stroke=\"black\" stroke-width=\"3\"/>\n"
      | Under, false, false -> 
        "<!--Under Formation-->\n
        <text x=\"668\" y=\"510\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">Q</text>\n
        <circle cx=\"690\" cy=\"660\" r=\"30\" stroke=\"black\" stroke-width=\"3\" fill=\"none\"/>\n
        <text x=\"677\" y=\"675\" font-size=\"45\" font-family=\"Arial, Helvetica, sans-serif\">A</text>\n
        <!--RB Block-->\n
        <line x1=\"690\" y1=\"630\" x2 =\"580\" y2=\"505\" stroke=\"black\" stroke-width=\"3\"/>\n
        <line x1=\"560\" y1=\"505\" x2=\"600\" y2=\"505\" stroke=\"black\" stroke-width=\"3\"/>\n"
      | Shotgun, true, false -> 
        "<!--Shotgun Run Formation-->\n
        <text x=\"668\" y=\"635\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">Q</text>\n
        <circle cx=\"770\" cy=\"615\" r=\"30\" stroke=\"black\" stroke-width=\"3\" fill=\"none\"/>\n
        <text x=\"757\" y=\"630\" font-size=\"45\" font-family=\"Arial, Helvetica, sans-serif\">A</text>\n
        <line x1=\"770\" y1=\"585\" x2=\"715\" y2=\"575\" stroke=\"black\" stroke-width=\"2\"/>\n
        <path d=\"M715,575 Q705,560 730,520\" stroke=\"black\" fill=\"none\" marker-end=\"url(#arrow)\" stroke-width=\"2\"/>\n"
      | Under, true, false ->
        "<!--Under Formation-->\n
        <text x=\"668\" y=\"510\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">Q</text>\n
        <circle cx=\"690\" cy=\"660\" r=\"30\" stroke=\"black\" stroke-width=\"3\" fill=\"none\"/>\n
        <text x=\"677\" y=\"675\" font-size=\"45\" font-family=\"Arial, Helvetica, sans-serif\">A</text>\n
        <line x1=\"690\" y1=\"630\" x2=\"740\" y2=\"520\" stroke=\"black\" stroke-width=\"2\" marker-end=\"url(#arrow)\"/>\n"
      | Shotgun, false, true -> "<!--Shotgun Formation-->\n
        <text x=\"668\" y=\"635\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">Q</text>\n"
      | Under, false, true -> "<!--Under Formation-->\n
        <text x=\"668\" y=\"510\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">Q</text>\n"
      | _, _, _ -> 
        "<!--Error in Input: A cannot run the ball and -->"

    drawField fieldTup + drawBoundary boundaryTup + drawFormation unit

(*
  Draws the offensive line assignments

  Returns: string of svg code
*)
let evalScheme(front: Box, schm: Scheme) =
    let pwr34 = "<!--Power Scheme against 3-4 Defense-->\n
    <!--Labeling Keys-->\n
    <text x=\"417\" y=\"275\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
    <text x=\"597\" y=\"225\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
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
<text x=\"555\" y=\"215\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
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
    <text x=\"417\" y=\"275\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
    <text x=\"597\" y=\"225\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
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
  <text x=\"555\" y=\"215\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
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
      

    let iz34 = "<!--Inside Zone against 3-4 Defense-->\n
<!--Labeling Keys-->\n
<text x=\"948\" y=\"275\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
<text x=\"763\" y=\"225\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
<!--C to -1-->\n
<line x1=\"530\" y1=\"395\" x2=\"530\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"510\" y1=\"380\" x2=\"550\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"550\" y1=\"330\" x2=\"580\" y2=\"280\" stroke=\"black\" stroke-dasharray=\"4\" stroke-width=\"3\"/>\n
<line x1=\"575\" y1=\"265\" x2=\"590\" y2=\"290\" stroke=\"black\" stroke-width=\"3\"/>\n
<!--Single to BSK-->\n
<line x1=\"610\" y1=\"395\" x2=\"650\" y2=\"370\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"645\" y1=\"355\" x2=\"655\" y2=\"385\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"690\" y1=\"395\" x2=\"690\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"670\" y1=\"380\" x2=\"710\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"710\" y1=\"330\" x2=\"750\" y2=\"285\" stroke=\"black\" stroke-dasharray=\"4\" stroke-width=\"3\"/>\n
<line x1=\"740\" y1=\"275\" x2=\"760\" y2=\"295\" stroke=\"black\" stroke-width=\"3\"/>\n
<!--Double to PSK-->\n
<line x1=\"770\" y1=\"395\" x2=\"810\" y2=\"375\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"800\" y1=\"355\" x2=\"820\" y2=\"390\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"850\" y1=\"395\" x2=\"850\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"830\" y1=\"380\" x2=\"870\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"870\" y1=\"330\" x2=\"930\" y2=\"320\" stroke=\"black\" stroke-dasharray=\"4\" stroke-width=\"3\"/>\n
<line x1=\"925\" y1=\"300\" x2=\"935\" y2=\"340\" stroke=\"black\" stroke-width=\"3\"/>\n"

    let oz34 = "<!--Outside Zone against 3-4 Defense-->\n
<!--Labeling Keys-->\n
<text x=\"597\" y=\"225\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
<text x=\"763\" y=\"225\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
<!--C to -1-->\n
<line x1=\"530\" y1=\"395\" x2=\"530\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"510\" y1=\"380\" x2=\"550\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"530\" y1=\"320\" x2=\"530\" y2=\"290\" stroke=\"black\" stroke-dasharray=\"4\" stroke-width=\"3\"/>\n
<line x1=\"510\" y1=\"290\" x2=\"550\" y2=\"290\" stroke=\"black\" stroke-width=\"3\"/>\n
<!--Single to BSK-->\n
<line x1=\"610\" y1=\"395\" x2=\"650\" y2=\"370\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"645\" y1=\"355\" x2=\"655\" y2=\"385\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"690\" y1=\"395\" x2=\"690\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"670\" y1=\"380\" x2=\"710\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"690\" y1=\"320\" x2=\"690\" y2=\"290\" stroke=\"black\" stroke-dasharray=\"4\" stroke-width=\"3\"/>\n
<line x1=\"670\" y1=\"290\" x2=\"710\" y2=\"290\" stroke=\"black\" stroke-width=\"3\"/>\n
<!--Double to PSK-->\n
<line x1=\"770\" y1=\"395\" x2=\"810\" y2=\"375\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"800\" y1=\"355\" x2=\"820\" y2=\"390\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"850\" y1=\"395\" x2=\"850\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"830\" y1=\"380\" x2=\"870\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"850\" y1=\"320\" x2=\"850\" y2=\"290\" stroke=\"black\" stroke-dasharray=\"4\" stroke-width=\"3\"/>\n
<line x1=\"830\" y1=\"290\" x2=\"870\" y2=\"290\" stroke=\"black\" stroke-width=\"3\"/>\n
"
    //iz and oz against a 43 are the same
    let oz43 ="<!--Outside Zone against 4-3 Defense-->\n
<!--Labeling Keys-->\n
<text x=\"683\" y=\"225\" font-size=\"30\" stroke=\"red\" font-family=\"Arial, Helvetica, sans-serif\">K</text>\n
<!--B to -1-->\n
<line x1=\"530\" y1=\"395\" x2=\"570\" y2=\"370\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"565\" y1=\"355\" x2=\"575\" y2=\"385\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"610\" y1=\"395\" x2=\"610\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"590\" y1=\"380\" x2=\"630\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"610\" y1=\"320\" x2=\"610\" y2=\"290\" stroke=\"black\" stroke-dasharray=\"4\" stroke-width=\"3\"/>\n
<line x1=\"590\" y1=\"290\" x2=\"630\" y2=\"290\" stroke=\"black\" stroke-width=\"3\"/>\n
<!--A to K-->\n
<line x1=\"770\" y1=\"395\" x2=\"770\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"750\" y1=\"380\" x2=\"790\" y2=\"380\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"690\" y1=\"395\" x2=\"730\" y2=\"370\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"720\" y1=\"355\" x2=\"740\" y2=\"390\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"770\" y1=\"320\" x2=\"770\" y2=\"290\" stroke=\"black\" stroke-dasharray=\"4\" stroke-width=\"3\"/>\n
<line x1=\"750\" y1=\"290\" x2=\"790\" y2=\"290\" stroke=\"black\" stroke-width=\"3\"/>\n
<!--Solo on R-->\n
<line x1=\"850\" y1=\"395\" x2=\"865\" y2=\"370\" stroke=\"black\" stroke-width=\"3\"/>\n
<line x1=\"855\" y1=\"355\" x2=\"875\" y2=\"390\" stroke=\"black\" stroke-width=\"3\"/>\n
"
    // draws the right scheme for the right front
    match front, schm with
    | ThreeFour, Power -> pwr34
    | ThreeFour, Counter -> ctr34
    | FourThree, Power -> pwr43
    | FourThree, Counter -> ctr43
    | ThreeFour, InsideZone -> iz34
    | ThreeFour, OutsideZone -> oz34
    | FourThree, InsideZone -> oz43
    | FourThree, OutsideZone -> oz43
    | _, Pass -> ""

(*
  Draws the offensive line assignments

  Returns: string of svg code
*)
let evalDefense (box: Box, cov: Coverage) =

    let three_four = "<!--Three Four Box-->
    <text x=\"510\" y=\"375\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">F</text>\n
    <text x=\"670\" y=\"375\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">N</text>\n
    <text x=\"830\" y=\"375\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">R</text>\n
    <text x=\"590\" y=\"275\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">B</text>\n
    <text x=\"750\" y=\"275\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">M</text>\n
    <text x=\"400\" y=\"325\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">W</text>\n
    <text x=\"940\" y=\"325\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">S</text>\n"
    
    let four_three = "<!--Four Three Box-->
    <text x=\"480\" y=\"375\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">F</text>\n
    <text x=\"590\" y=\"375\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">E</text>\n
    <text x=\"750\" y=\"375\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">N</text>\n
    <text x=\"870\" y=\"375\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">R</text>\n
    <text x=\"670\" y=\"275\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">M</text>\n
    <text x=\"800\" y=\"275\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">S</text>\n
    <text x=\"540\" y=\"275\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">W</text>\n"

    let cover_2 = "<!--Cover 2-->
    <text x=\"370\" y=\"100\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">S</text>\n
    <text x=\"970\" y=\"100\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">FS</text>\n
    <text x=\"150\" y=\"375\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">C</text>\n
    <text x=\"1190\" y=\"375\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">C</text>\n"
    let cover_1 = "<!--Cover 1-->\n
    <text x=\"460\" y=\"250\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">S</text>\n
    <text x=\"590\" y=\"100\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">FS</text>\n
    <text x=\"150\" y=\"375\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">C</text>\n
    <text x=\"1190\" y=\"375\" font-size=\"60\" font-family=\"Arial, Helvetica, sans-serif\">C</text>\n"

    // draws the right box and coverage
    match box, cov with
    | FourThree, Cover2 -> four_three + cover_2
    | FourThree, Cover1-> four_three + cover_1
    | ThreeFour, Cover2 -> three_four + cover_2
    | ThreeFour, Cover1-> three_four + cover_1
     
(*
  Evaluates the play input 

  Returns: string of svg code
*)
let evalPlay (play: Play) =
    match play with
    | (a,b,c,d) -> 
      let players, routes, movements = evalRoutes c
      let unit, Routes = d
      let front = fst a
      evalDefense a + evalScheme (front, b) + evalFormation (unit, Routes, players, routes, movements)

(*
  Combines the entire play

  Returns: string of svg code
*)
let eval (play: Play) : string =
    let csz = CANVAS_SZ |> string

    let OLine ="<!--OLine-->\n
  <circle cx=\"530\" cy=\"425\" r=\"30\" stroke=\"black\" stroke-width=\"3\" fill=\"none\"/>\n
  <circle cx=\"610\" cy=\"425\" r=\"30\" stroke=\"black\" stroke-width=\"3\" fill=\"none\"/>\n
  <rect x=\"660\" y=\"395\" width=\"60\" height=\"60\" stroke=\"black\" stroke-width=\"3\" fill=\"none\"/>\n
  <circle cx=\"770\" cy=\"425\" r=\"30\" stroke=\"black\" stroke-width=\"3\" fill=\"none\"/>\n
  <circle cx=\"850\" cy=\"425\" r=\"30\" stroke=\"black\" stroke-width=\"3\" fill=\"none\"/>\n"

    
    "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n
    <defs>\n
      <marker id=\"arrow\" markerWidth=\"10\" markerHeight=\"10\" refX=\"8\" refY=\"3\" orient=\"auto\">\n
        <path d=\"M0,0 L0,6 L9,3 z\" fill=\"black\" />\n
      </marker>\n
    </defs>\n"

    + OLine 

    + (evalPlay play)
    + "</svg>\n"