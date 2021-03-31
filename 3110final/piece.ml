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

(** coord (2, 3) means the i (y-coordinate) is 2 and j (x-coordinate) is
    3. *)
type coord = int * int

type piece = {
  character : rank;
  side : side;
  coordinate : coord;
}

type t = piece

let create_piece r s c = { character = r; side = s; coordinate = c }

let get_c piece = piece.character

let get_side piece = piece.side

let string_of_side side = if side = Red then "Red" else "Black"

let change_coord piece coord =
  create_piece (get_c piece) (get_side piece) coord

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

(** [get_i] extract y-coordinate of the coordinate tuple*)
let get_i (a, _) = a

(** [get_j] extract x-coordinate of the coordinate tuple*)
let get_j (_, a) = a

(** [rules] is a bool, judging whether a move of piece [p] to
    destination coordinate [c] is legal, currently for player's red
    side, without eating pieces, before acrossing the river. *)
let rules p c2 =
  let c1_i = get_i p.coordinate in
  let c1_j = get_j p.coordinate in
  let c2_i = get_i c2 in
  let c2_j = get_j c2 in
  match get_c p with
  | General ->
      if
        c2 = (c1_i, c1_j - 1)
        || c2 = (c1_i, c1_j + 1)
        || c2 = (c1_i - 1, c1_j)
        || c2 = (c1_i + 1, c1_j)
      then true
      else false
  | Advisor ->
      if
        c2 = (c1_i + 1, c1_j + 1)
        || c2 = (c1_i - 1, c1_j + 1)
        || c2 = (c1_i + 1, c1_j - 1)
        || c2 = (c1_i - 1, c1_j - 1)
      then true
      else false
  | Elephant ->
      if
        c2 = (c1_i + 2, c1_j + 2)
        || c2 = (c1_i - 2, c1_j + 2)
        || c2 = (c1_i + 2, c1_j - 2)
        || c2 = (c1_i - 2, c1_j - 2)
      then true
      else false
  | Horse ->
      if
        c2 = (c1_i + 2, c1_j + 1)
        || c2 = (c1_i - 2, c1_j + 1)
        || c2 = (c1_i + 2, c1_j - 1)
        || c2 = (c1_i - 2, c1_j - 1)
        || c2 = (c1_i + 1, c1_j + 2)
        || c2 = (c1_i + 1, c1_j + -2)
        || c2 = (c1_i + -1, c1_j + 2)
        || c2 = (c1_i + -1, c1_j + -2)
      then true
      else false
  | Rook ->
      if (c2_i = c1_i || c2_j = c1_j) && (c1_i, c1_j) <> c2 then true
      else false
  | Cannon ->
      if (c2_i = c1_i || c2_j = c1_j) && (c1_i, c1_j) <> c2 then true
      else false
  | Soldier -> if c2 = (c1_i - 1, c1_j) then true else false
