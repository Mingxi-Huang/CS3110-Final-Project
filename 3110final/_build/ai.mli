open State
open Piece
open Board

type piece = Piece.t

val get_side : unit -> Piece.side

val make_command : piece -> string

(**1. 楚河汉界 2. 米字格 3. 马腿，相腿*)
