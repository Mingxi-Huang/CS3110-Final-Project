open Pieces

(** [t] is a 2d array that represents the entire board, with every
    piece's prperty *)
type t = piece array

val board_array : char array array

val update_board :
  char array array -> (int * int) * (int * int) -> char -> unit
