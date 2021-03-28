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

type piece = {
  character : rank;
  side : side;
  coordinate : coord;
}

let get_c piece = piece.character

let get_side piece = piece.side

let get_coord piece = piece.coordinate

let extract = function
  | Some x -> x
  | None -> raise (Invalid_argument "extract None")

let char_of_piece piece =
  if piece = None then '+'
  else
    match (extract piece).character with
    | General -> 'G'
    | Advisor -> 'A'
    | Elephant -> 'E'
    | Horse -> 'H'
    | Rook -> 'R'
    | Cannon -> 'C'
    | Soldier -> 'S'

let init_pieces = []

let create_piece r s c = { character = r; side = s; coordinate = c }
