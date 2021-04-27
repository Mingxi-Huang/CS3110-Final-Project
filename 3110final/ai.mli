open State
open Piece
open Board

type piece = Piece.t

val available_piece : Board.t -> Piece.t list

val choose_piece : Board.t -> Piece.t

val get_coordinate : Piece.t -> Piece.coord * (int * int)

val make_command : State.t -> string

(**1. 楚河汉界 2. 米字格 3. 马腿，相腿*)
