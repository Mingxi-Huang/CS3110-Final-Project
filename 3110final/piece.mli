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

type piece

val get_c : piece -> rank

val get_coord : piece -> coord

val get_side : piece -> side

val create_piece : rank -> side -> coord -> piece

val char_of_piece : piece option -> char

val init_pieces : piece list
