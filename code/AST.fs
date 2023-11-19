module AST

type Defense = string * string
type Route = char * string * char
type Canvas = Defense list

let CANVAS_SZ = 400