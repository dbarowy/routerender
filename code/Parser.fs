module Parser

open Combinator
open AST

let pad p = pbetween pws0 p pws0

// Explicit data types that the parser will need to recognize.
let symbol = (pchar 'x') <|> (pchar 'o')

// Coverages
let man = pstr "man"
let cover1 = pstr "cover1"
let cover2 = pstr "cover2"
let cover3 = pstr "cover3"
let cover4 = pstr "cover4"
let cover6 = pstr "cover6"
let coverage = man <|> cover1 <|> cover2 <|> cover3 <|> cover4 <|> cover6

//Box
let box = pstr "34" <|> pstr "43"

//Defense
let defense =
    pbetween
        (pstr "(")
        (pseq (pleft box (pad (pstr ","))) coverage (fun (b, c) -> Defense(b,c)))
        (pstr ")")

//Reads
let read = pchar '1' <|> pchar '2' <|> pchar '3' <|> pchar '4' <|> pchar '5'

//Movements
let go = pstr "go"
let slant = pstr "slant"
let out = pstr "out"
let inRoute = pstr "in"
let post = pstr "post"
let corner = pstr "corner"
let curl = pstr "curl"
let dig = pstr "dig"
let hitch = pstr "hitch"
let comeback = pstr "comeback"
let wheel = pstr "wheel"
let postCorner = (pstr "post corner") <|> (pstr "post-corner")
let fade = pstr "fade"
let screen = pstr "screen"
let movement = go <|> slant <|> out <|> inRoute <|> post <|> corner <|> curl <|> dig <|> hitch <|> comeback <|> wheel <|> postCorner <|> fade <|> screen

//Player
let player = pchar 'x' <|> pchar 'y' <|> pchar 'z' <|> pchar 'h' <|> pchar 'a' <|> pchar 'q'

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
let power = pad (pstr "power")
let counter = pad (pstr "counter")
let insideZone = pad (pstr "inside zone")
let outsideZone = pad (pstr "outside zone")
let pro = pad (pstr "pro")
let scheme = power <|> counter <|> insideZone <|> outsideZone <|> pro

//Receivers
let noReceivers = pstr "_"
let yesReceivers =
    pseq
        (pleft pdigit (pad (pchar 'x')))
        pdigit
        (fun (d1,d2) ->
            if int(d1) + int(d2) <= 6 then stringify [d1;d2] else "")
let receivers = noReceivers <|> yesReceivers

let grammar = pleft defense peof

let parse (input: string) : Canvas option = 
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some [ast]
    | Failure(_,_) -> None