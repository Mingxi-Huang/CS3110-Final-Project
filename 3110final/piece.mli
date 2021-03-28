(** Representation of static pieces data.

    This module represents the data stored in adventure files, including
    the states and moving rules. *)

type rank =
  | General
  | Advisor
  | Elephant
  | Horse
  | Rook
  | Cannon
  | Soldier

type piece

val get_c : piece -> rank

val get_coord : piece -> int * int

val get_side : piece -> string

val create_piece : rank -> string -> int * int -> piece

val init_pieces : piece list
