module Parser

open Combinator

type Program =
| Play of 


// Explicit data types that the parser will need to recognize.
let symbol = (pchar 'x') <|> (pchar 'o')