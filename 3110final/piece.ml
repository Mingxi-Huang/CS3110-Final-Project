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

let string_of_piece piece =
  let col = if get_side piece = Red then "Red" else "Black" in
  match get_c piece with
  | General -> col ^ " General"
  | Advisor -> col ^ " Advisor"
  | Elephant -> col ^ " Elephant"
  | Horse -> col ^ " Horse"
  | Rook -> col ^ " Rook"
  | Cannon -> col ^ " Cannon"
  | Soldier -> col ^ " Soldier"

(** [get_i] extract y-coordinate of the coordinate tuple*)
let get_i (a, _) = a

(** [get_j] extract x-coordinate of the coordinate tuple*)
let get_j (_, a) = a

let rules p c2 =
  let c1_i = get_i p.coordinate in
  let c1_j = get_j p.coordinate in
  let c2_i = get_i c2 in
  let c2_j = get_j c2 in
  match get_c p with
  (** General and Advisor cannot move outside the 2x2 palace. *)
  | General ->
      if
        c2 = (c1_i, c1_j - 1)
        || c2 = (c1_i, c1_j + 1)
        || c2 = (c1_i - 1, c1_j)
        || c2 = (c1_i + 1, c1_j) && c2_i > 14 && (c2_j > 4 && c2_j < 6)
      then true
      else false
  | Advisor ->
      if
        c2 = (c1_i + 1, c1_j + 1)
        || c2 = (c1_i - 1, c1_j + 1)
        || c2 = (c1_i + 1, c1_j - 1)
        || c2 = (c1_i - 1, c1_j - 1) && c2_i > 14 && (c2_j > 4 || c2_j < 6)
      then true
      else false
  (** Elephant cannot cross the river. *)
  | Elephant ->
      if 
        c2 = (c1_i + 2, c1_j + 2)
        || c2 = (c1_i - 2, c1_j + 2)
        || c2 = (c1_i + 2, c1_j - 2)
        || c2 = (c1_i - 2, c1_j - 2) && c2_i > 10
      then true
      else false
  (** Tripping horse: illegal move if the coord one step towards the moving direction of horse is occupied. *)
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
        (* (occupied_coord (get_current_board st) (c1_i, c1_j - 1))) *)
      then true
      else false
  | Rook ->
      if (c2_i = c1_i || c2_j = c1_j) && (c1_i, c1_j) <> c2 then true
      else false
  | Cannon ->
      if (c2_i = c1_i || c2_j = c1_j) && (c1_i, c1_j) <> c2 then true
      else false
  (** Before crossing the river: Soldier can move one step forward;
    * After crossing the riverL Soldier can move one step forward, leftward, or rightward;
    * Soldier can never move backwards. *)
  | Soldier ->     
    if c1_i > 10 && c2 = (c1_i - 1, c1_j) then true
    else if c1_i < 10 && c2 = (c1_i - 1, c1_j) || c2 = (c1_i, c1_j + 1) || c2 = (c1_i, c1_j - 1) then true 
    else false