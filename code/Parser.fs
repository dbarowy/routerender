module Parser

open Combinator
open AST

let pad p = pbetween pws0 p pws0

// Explicit data types that the parser will need to recognize.

// Coverages
let coverage = 
    (pstr "man" |>> (fun _ -> Man)) <|> 
    (pstr "cover1" |>> (fun _ -> Cover1)) <|>
    (pstr "cover2" |>> (fun _ -> Cover2)) <!>
    "coverage"

//Box
let box = 
    (pstr "34" |>> (fun _ -> ThreeFour)) <|> 
    (pstr "43" |>> (fun _ -> FourThree)) <!>
    "box"

//Defense
let defense =
    pbetween
        (pstr "(")
        (pseq (pleft box (pad (pstr ","))) coverage (fun (b, c) -> Defense(b,c)))
        (pstr ")")
    <!> "defense"

//Reads
let read = 
    (pchar '1' |>> (fun _ -> First)) <|>
    (pchar '2' |>> (fun _ -> Second)) <|>
    (pchar '3' |>> (fun _ -> Third)) <|>
    (pchar '4' |>> (fun _ -> Fourth)) <|>
    (pchar '5' |>> (fun _ -> Fifth)) <!>
    "read"

//Movements
let movement =
    (pstr "run" |>> (fun _ -> Run)) <|>
    (pstr "go" |>> (fun _ -> Go)) <|>
    (pstr "slant" |>> (fun _ -> Slant)) <|>
    (pstr "out" |>> (fun _ -> Out)) <|>
    (pstr "in" |>> (fun _ -> In)) <|>
    (pstr "post" |>> (fun _ -> Post)) <|>
    (pstr "corner" |>> (fun _ -> Corner)) <|>
    (pstr "curl" |>> (fun _ -> Curl)) <|>
    (pstr "dig" |>> (fun _ -> Dig)) <|>
    (pstr "comeback" |>> (fun _ -> Comeback)) <|>
    (pstr "block" |>> (fun _ -> Block)) <|>
    (pstr "screen" |>> (fun _ -> Screen)) <!>
    "movement"

//Player
let player = 
    ((pchar 'x' <|> pchar 'X') |>> (fun _ -> X)) <|> 
    ((pchar 'y' <|> pchar 'Y') |>> (fun _ -> Y)) <|> 
    ((pchar 'z' <|> pchar 'Z') |>> (fun _ -> Z)) <|> 
    ((pchar 'h' <|> pchar 'H') |>> (fun _ -> H)) <|> 
    ((pchar 'a' <|> pchar 'A') |>> (fun _ -> A)) <!>
    "player"
    

//Routes
let route = 
    pbetween
        (pchar '(')
        (pseq 
            (pleft player (pad (pchar ','))) 
            (pseq 
                (pleft movement (pad (pchar ',')))
                (read)
                (fun (m, r) -> (m,r))
            )
            (fun (p, (m, r)) -> Route(p,m,r))
        )
        (pchar ')')
    <!> "route"

let routes: Parser<Routes> =
    pbetween
        (pchar '[')
        (pseq
            (pmany0 (pleft route (pad (pchar ','))))
            (route)
            (fun (rs, r) -> r::rs)
        )
        (pchar ']')
    <!> "routes"

//Schemes
let scheme = 
    (pad (pstr "power") |>> (fun _ -> Power)) <|>
    (pad (pstr "counter") |>> (fun _ -> Counter)) <|>
    (pad (pstr "inside zone" <|> pstr "insidezone" <|> pstr "insideZone" <|> pstr "iz" <|> pstr "IZ") |>> (fun _ -> InsideZone)) <|>
    (pad (pstr "outside zone" <|> pstr "outsidezone" <|> pstr "outsideZone"  <|> pstr "oz" <|> pstr "OZ") |>> (fun _ -> OutsideZone)) <!>
    "scheme"

//Receivers
let noReceivers = (pad (pchar '_') |>> (fun _ -> NoReceivers)) <!> "noRec"
let yesReceivers =
    pad (pseq
        (pleft pdigit (pad (pchar 'x')))
        pdigit
        (fun (d1,d2) ->
            
            if (int(d1) - int('0')) + (int(d2) - int('0')) < 6 then YesReceivers(d1,d2) else NoReceivers)) <!> "yesRec"
let receivers = noReceivers <|> yesReceivers <!> "receivers"

//Units
let unit =
    (pstr "under" |>> (fun _ -> Under)) <|>
    (pstr "shotgun" |>> (fun _ -> Shotgun)) <!>
    "unit"

//Formations
let formation =
    pbetween
        (pchar '(')
        (pseq
            (pleft unit (pad (pchar ',')))
            (receivers)
            (fun (u, r) -> Formation(u,r))
        )
        (pchar ')')
    <!> "formation"

//Plays
let firstHalf =
    pseq defense scheme (fun (d,s) -> (d,s)) <!> "firstHalf"
let secondHalf =
    pseq routes formation (fun (rs,f) -> (rs,f)) <!> "secondHalf"
let fullPlay: Parser<Play> =
    pseq firstHalf secondHalf (fun (f,s) -> (fst f, snd f, fst s, snd s)) <!> "fullPlay"
let play = pleft fullPlay (pchar ';') <!> "play"

//Grammar
let grammar = pleft play peof <!> "grammar"

let parse (input: string): Play option= 
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_,_) -> None