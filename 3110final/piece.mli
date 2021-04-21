(** Representation of static pieces.

    Character/Rank of pieces:

    General: The general may move and capture one point orthogonally and
    may not leave the palace;

    Advisor: move and capture one point diagonally and may not leave the
    palace, which confines them to five points on the board;

    Elephant: move and capture exactly two points diagonally and may not
    jump over intervening pieces;

    Horse: moves and captures one point orthogonally and then one point
    diagonally away from its former position;

    Rook: moves and captures any distance orthogonally, but may not jump
    over intervening pieces;

    Cannon: move any distance orthogonally, but only capture by jumping
    a single piece of either colour along the path of attack; Soldier:
    move and capture by advancing one point;

    Side of pieces: Red: Go first; Black: Wait for red to Go *)
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

val string_of_side : side -> string

val change_coord : t -> coord -> t

(** [extract piece_opt] is a piece extracted from a piece option
    [piece_opt]*)
val extract : t option -> t

(** [get_c piece] is the rank of piece [piece]*)
val get_c : t -> rank

val get_coord : t -> coord

val get_side : t -> side

val char_of_piece : t option -> char

(** [string_of_piece piece] is the string containing information about
    its rank, color, and coordinate *)
val string_of_piece : t -> string

(** [rules] is a bool, judging whether a move of piece [p] to
    destination coordinate [c] is legal, currently for player's red
    side, without eating pieces, before acrossing the river. *)
val rules : t -> coord -> bool
