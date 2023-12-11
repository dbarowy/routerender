module AST

type Box =
| ThreeFour
| FourThree
type Coverage =
| Man
| Cover1
| Cover2
| Cover3
| Cover4
| Cover6
type Defense = Box * Coverage

type Player =
| X
| Y
| Z
| H
| A
type Movement =
| Go
| Slant
| Out
| In
| Post
| Corner
| Curl
| Dig
| Hitch
| Comeback
| Wheel
| PostCorner
| Fade
| Screen
type Read =
| First
| Second
| Third
| Fourth
| Fifth
type Route = Player * Movement * Read
type Routes =  Route list

type Scheme =
| Power
| Counter
| InsideZone
| OutsideZone

type Receivers =
| NoReceivers
| YesReceivers of char * char
type Unit =
| IForm
| Empty
| Singleback
| Shotgun
type Formation = Unit * Receivers


type Play = Defense * Formation * Scheme * Routes
let CANVAS_SZ = 1500