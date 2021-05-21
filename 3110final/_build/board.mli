open Piece

(** [t] is a 2d array that represents the entire board, with every
    piece's property *)

type t

type graveyard

val generate_board : unit -> t

(** [get_piece board coord] is the piece that locates on the coordinate
    [coord] on the board [board]*)
val get_piece : t -> coord -> Piece.t option

val print_board : t -> unit

val print_rev_board : t -> unit

(** [turned_board board] is the resulting board after rotating the board
    [board] for 180 degree *)
val turned_board : t -> t

(** [update_board board start dest] is the updated board [board] by
    moving the piece at [start] to [dest] and setting the postion
    [start] to None *)
val update_board : t -> coord -> coord -> t
