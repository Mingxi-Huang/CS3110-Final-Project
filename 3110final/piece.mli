(** Representation of static pieces data.

    This module represents the data stored in adventure files, including
    the states and moving rules. *)
type side =
  | Red
  | Black

type rank =
  | General
  | Advisor
  | Elephant
  | Horse
  | Rook
  | Cannon
  | Soldier

type coord = int * int

type t

val create_piece : rank -> side -> coord -> t

val change_coord : t -> coord -> t

val extract : t option -> t

val get_c : t -> rank

val get_coord : t -> coord

val get_side : t -> side

val char_of_piece : t option -> char

val init_pieces : t list
