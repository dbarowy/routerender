module Parser

open Combinator
open AST

let pad p = pbetween pws0 p pws0

// Explicit data types that the parser will need to recognize.

// Coverages
let coverage = 
    (pstr "man" |>> (fun _ -> Man)) <|> 
    (pstr "cover1" |>> (fun _ -> Cover1)) <|>
    (pstr "cover2" |>> (fun _ -> Cover2)) <|>
    (pstr "cover3" |>> (fun _ -> Cover3)) <|>
    (pstr "cover4" |>> (fun _ -> Cover4)) <|>
    (pstr "cover6" |>> (fun _ -> Cover6))

//Box
let box = 
    (pstr "34" |>> (fun _ -> ThreeFour)) <|> 
    (pstr "43" |>> (fun _ -> FourThree))

//Defense
let defense =
    pbetween
        (pstr "(")
        (pseq (pleft box (pad (pstr ","))) coverage (fun (b, c) -> Defense(b,c)))
        (pstr ")")

//Reads
let read = 
    (pchar '1' |>> (fun _ -> First)) <|>
    (pchar '2' |>> (fun _ -> Second)) <|>
    (pchar '3' |>> (fun _ -> Third)) <|>
    (pchar '4' |>> (fun _ -> Fourth)) <|>
    (pchar '5' |>> (fun _ -> Fifth))

//Movements
let movement =
    (pstr "go" |>> (fun _ -> Go)) <|>
    (pstr "slant" |>> (fun _ -> Slant)) <|>
    (pstr "out" |>> (fun _ -> Out)) <|>
    (pstr "in" |>> (fun _ -> In)) <|>
    (pstr "post" |>> (fun _ -> Post)) <|>
    (pstr "corner" |>> (fun _ -> Corner)) <|>
    (pstr "curl" |>> (fun _ -> Curl)) <|>
    (pstr "dig" |>> (fun _ -> Dig)) <|>
    (pstr "hitch" |>> (fun _ -> Hitch)) <|>
    (pstr "comeback" |>> (fun _ -> Comeback)) <|>
    (pstr "wheel" |>> (fun _ -> Wheel)) <|>
    ((pstr "postcorner" <|> pstr "post-corner" <|> pstr "post corner") |>> (fun _ -> PostCorner)) <|>
    (pstr "fade" |>> (fun _ -> Fade)) <|>
    (pstr "screen" |>> (fun _ -> Screen))

//Player
let player = 
    ((pchar 'x' <|> pchar 'X') |>> (fun _ -> X)) <|> 
    ((pchar 'y' <|> pchar 'Y') |>> (fun _ -> Y)) <|> 
    ((pchar 'z' <|> pchar 'Z') |>> (fun _ -> Z)) <|> 
    ((pchar 'h' <|> pchar 'H') |>> (fun _ -> H)) <|> 
    ((pchar 'a' <|> pchar 'A') |>> (fun _ -> A)) 
    

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

let routes =
    pbetween
        (pchar '[')
        (pseq
            (pmany0 (pleft route (pad (pchar ','))))
            (route)
            (fun (rs, r) -> r::rs)
        )
        (pchar ']')

//Schemes
let scheme = 
    (pad (pstr "power") |>> (fun _ -> Power)) <|>
    (pad (pstr "counter") |>> (fun _ -> Counter)) <|>
    (pad (pstr "inside zone" <|> pstr "insidezone" <|> pstr "insideZone") |>> (fun _ -> InsideZone)) <|>
    (pad (pstr "outside zone" <|> pstr "outsidezone" <|> pstr "outsideZone") |>> (fun _ -> OutsideZone))

//Receivers
let noReceivers = (pad (pchar '_') |>> (fun _ -> NoReceivers))
let yesReceivers =
    pad (pseq
        (pleft pdigit (pad (pchar 'x')))
        pdigit
        (fun (d1,d2) ->
            if int(d1) + int(d2) <= 6 then YesReceivers(d1,d2) else NoReceivers))
let receivers = noReceivers <|> yesReceivers

//Units
let unit =
    (pstr "iformation" |>> (fun _ -> IForm)) <|>
    (pstr "empty" |>> (fun _ -> Empty)) <|>
    (pstr "singleback" |>> (fun _ -> Singleback)) <|>
    (pstr "shotgun" |>> (fun _ -> Shotgun))

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

//Plays
let firstHalf =
    pseq defense formation (fun (d,f) -> (d,f))
let secondHalf =
    pseq scheme routes (fun (s,rs) -> (s,rs))

let play =
    pseq firstHalf secondHalf (fun (f,s) -> (fst f, snd f, fst s, snd s))

//Grammar
let grammar = pleft play peof

let parse (input: string) : Canvas option = 
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some [ast]
    | Failure(_,_) -> None