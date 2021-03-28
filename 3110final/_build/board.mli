open Piece

(** [t] is a 2d array that represents the entire board, with every
    piece's prperty *)

val board_array : piece option array array

val update_board :
  char array array -> (int * int) * (int * int) -> char -> unit
