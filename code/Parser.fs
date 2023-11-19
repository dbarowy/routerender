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
let read = pstr "1" <|> pstr "2" <|> pstr "3" <|> pstr "4"

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