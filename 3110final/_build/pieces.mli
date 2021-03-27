(** Representation of static pieces data.

    This module represents the data stored in adventure files, including
    the states and moving rules. *)
open State

type piece

val get_c : piece -> string

val get_n : piece -> string

val get_side : piece -> string

val get_id : piece -> string

val get_plabel : piece -> string

val create_piece :
  string -> string -> string -> string -> string -> piece

val init_pieces : piece list
