open Piece

(** [t] is a 2d array that represents the entire board, with every
    piece's property *)

type t

type graveyard = {
  red_graveyard : Piece.t list;
  black_graveyard : Piece.t list;
}

val generate_graveyard : unit -> graveyard

val get_red_g : graveyard -> Piece.t list

val get_black_g : graveyard -> Piece.t list

val count_pieces : Piece.rank -> graveyard -> Piece.side -> int

val generate_board : unit -> t

(** [get_piece board coord] is the piece that locates on the coordinate
    [coord] on the board [board]*)
val get_piece : t -> coord -> Piece.t option

val print_board : t -> graveyard -> unit

val print_rev_board : t -> graveyard -> unit

(** [turned_board board] is the resulting board after rotating the board
    [board] for 180 degree *)
val turned_board : t -> t

(** [update_board board start dest] is the updated board [board] by
    moving the piece at [start] to [dest] and setting the postion
    [start] to None *)
val update_board : t -> coord -> coord -> t
