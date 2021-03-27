(** Representation of static pieces.

    This module represents the checkers information, including the
    states and moving rules.

    Character of pieces: General: The general may move and capture one
    point orthogonally and may not leave the palace; Advisor: move and
    capture one point diagonally and may not leave the palace, which
    confines them to five points on the board; Elephant: move and
    capture exactly two points diagonally and may not jump over
    intervening pieces; Horse: moves and captures one point orthogonally
    and then one point diagonally away from its former position;
    Chariot: moves and captures any distance orthogonally, but may not
    jump over intervening pieces; Cannon: move any distance
    orthogonally, but only capture by jumping a single piece of either
    colour along the path of attack; Soldier: move and capture by
    advancing one point;

    Side of pieces: Red: Go first Black: Wait for red to Go

    id: string representation of character, number, and side of a
    specific piece*)
open State

type piece = {
  character : string;
  number : string;
  side : string;
  id : string;
  plabel : string;
}

let get_c piece = piece.character

let get_n piece = piece.number

let get_side piece = piece.side

let get_id piece = piece.id

let get_plabel piece = piece.plabel

let init_pieces = []

let create_piece c n side i p =
  { character = c; number = n; side; id = i; plabel = p }
