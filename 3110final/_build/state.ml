open Board
open Piece

type state = {
  current_board : Board.t;
  turn : side;
  red_graveyard : Piece.t list;
  black_graveyard : Piece.t list;
}

type t = state

let init_state =
  {
    current_board = Board.generate_board ();
    turn = Red;
    red_graveyard = [];
    black_graveyard = [];
  }

type result =
  | Legal of t
  | Illegal
      (** [get_result_state result] is the state from result [result],
          if result is legal, then gives state; otherwise, raise
          Illegal_state exception *)

let next_turn = function Red -> Black | Black -> Red

let get_current_board st = st.current_board

let get_current_red_g st = st.red_graveyard

let get_current_black_g st = st.black_graveyard

let get_current_turn st = st.turn

(**[non_empty_coord] evaluates if the selected piece have actual piece
   on it*)
let non_empty_coord piece_option =
  match piece_option with None -> false | Some _ -> true

(**[occupied_coord] evaluates if the coordinate of the specific board is
   occipied or not; if there is piece on it, then [true] else [false]*)
let occupied_coord board coord =
  match get_piece board coord with None -> false | Some _ -> true

(**[illegal_side] evaluates if the piece on the selected coordinate is
   of same side with [turn] and p is non empty*)
let is_legal_side board coord turn =
  let p = get_piece board coord in
  if non_empty_coord p && p |> extract |> get_side = turn then true
  else false

(** [inbound destiny] is whether the coordinate [destiny] is on the
    board or not*)
let inbound destiny =
  if
    fst destiny < 0
    || fst destiny > 9
    || snd destiny < 0
    || snd destiny > 8
  then false
  else true

(** get the opponent's color*)
let oppo_turn turn = if turn = Red then Black else Red

(* [get_i] extract y-coordinate of the coordinate tuple*)
let get_i (a, _) = a

(* [get_j] extract x-coordinate of the coordinate tuple*)
let get_j (_, a) = a

(*no_barricade takes a [start] and [destiny] coordinate (which has
  either the same i or j coordinate); the state [st]; and outputs
  whether there is no lying-there piece between the two coordinates on
  the board *)
let no_barricade c1 c2 st =
  let no_barr = ref true in
  if get_i c1 = get_i c2 then
    (*Same i, Diff j*)
    for
      j = min (get_j c1) (get_j c2) + 1 to max (get_j c1) (get_j c2) - 1
    do
      (*for the between route if there is piece on it --> occupied, set
        no_barr to false*)
      if occupied_coord st.current_board (get_i c1, j) then
        no_barr := false
    done
  else
    (*Diff i, Same j*)
    for
      i = min (get_i c1) (get_i c2) + 1 to max (get_i c1) (get_i c2) - 1
    do
      if occupied_coord st.current_board (i, get_j c1) then
        no_barr := false
    done;
  !no_barr

(*one_barricade takes a [start] and [destiny] coordinate (which has
  either the same i or j coordinate); the state [st]; and outputs
  whether there is exactly on occupied piece between the two coordinates
  on the board *)
let one_barricade c1 c2 st =
  let counter = ref 0 in
  if get_i c1 = get_i c2 then
    (*Same i, Diff j*)
    for
      j = min (get_j c1) (get_j c2) + 1 to max (get_j c1) (get_j c2) - 1
    do
      (*for the between route if there is piece on it --> occupied, set
        no_barr to false*)
      if occupied_coord st.current_board (get_i c1, j) then
        counter := !counter + 1
    done
  else
    (*Diff i, Same j*)
    for
      i = min (get_i c1) (get_i c2) + 1 to max (get_i c1) (get_i c2) - 1
    do
      if occupied_coord st.current_board (i, get_j c1) then
        counter := !counter + 1
    done;
  !counter = 1

(*Currently general, advisor, elephant, soldier moving rules solid and
  completed; horse, rook, canon involves checking whether other pieces
  block the way, which includes another parameter state [st], and
  currently causes circular reference issue; --> to be fixed*)
let rules p c2 st =
  let c1_i = get_i (get_coord p) in
  let c1_j = get_j (get_coord p) in
  let c2_i = get_i c2 in
  let c2_j = get_j c2 in
  match get_c p with
  (* General and Advisor cannot move outside the 2x2 palace, customized
     for both sides*)
  | General ->
      if
        (c2 = (c1_i, c1_j - 1)
        || c2 = (c1_i, c1_j + 1)
        || c2 = (c1_i - 1, c1_j)
        || c2 = (c1_i + 1, c1_j))
        && c2_j >= 3 && c2_j <= 5
        &&
        if get_side p = Red then c2_i >= 7 && c2_i <= 9
        else c2_i >= 0 && c2_i <= 2
      then true
      else false
  | Advisor ->
      if
        (c2 = (c1_i + 1, c1_j + 1)
        || c2 = (c1_i - 1, c1_j + 1)
        || c2 = (c1_i + 1, c1_j - 1)
        || c2 = (c1_i - 1, c1_j - 1))
        && c2_j >= 3 && c2_j <= 5
        &&
        if get_side p = Red then c2_i >= 7 && c2_i <= 9
        else c2_i >= 0 && c2_i <= 2
      then true
      else false
  (* Elephant cannot cross the river, customized for both sides, no
     barricade present in mid point of the diagonal*)
  | Elephant ->
      if
        (c2 = (c1_i + 2, c1_j + 2)
         && occupied_coord st.current_board (c1_i + 1, c1_j + 1) = false
        || c2 = (c1_i - 2, c1_j + 2)
           && occupied_coord st.current_board (c1_i - 1, c1_j + 1)
              = false
        || c2 = (c1_i + 2, c1_j - 2)
           && occupied_coord st.current_board (c1_i + 1, c1_j - 1)
              = false
        || c2 = (c1_i - 2, c1_j - 2)
           && occupied_coord st.current_board (c1_i - 1, c1_j - 1)
              = false)
        && if get_side p = Red then c2_i >= 5 else c2_i <= 4
      then true
      else false
  (* Tripping horse: illegal move if the coord one step towards the
     moving direction of horse is occupied; rules integrated and fixed
     here!*)
  | Horse ->
      if
        c2 = (c1_i + 2, c1_j + 1)
        && occupied_coord st.current_board (c1_i + 1, c1_j) = false
        || c2 = (c1_i - 2, c1_j + 1)
           && occupied_coord st.current_board (c1_i - 1, c1_j) = false
        || c2 = (c1_i + 2, c1_j - 1)
           && occupied_coord st.current_board (c1_i + 1, c1_j) = false
        || c2 = (c1_i - 2, c1_j - 1)
           && occupied_coord st.current_board (c1_i - 1, c1_j) = false
        || c2 = (c1_i + 1, c1_j + 2)
           && occupied_coord st.current_board (c1_i, c1_j + 1) = false
        || c2 = (c1_i + 1, c1_j + -2)
           && occupied_coord st.current_board (c1_i, c1_j - 1) = false
        || c2 = (c1_i + -1, c1_j + 2)
           && occupied_coord st.current_board (c1_i, c1_j + 1) = false
        || c2 = (c1_i + -1, c1_j + -2)
           && occupied_coord st.current_board (c1_i, c1_j - 1) = false
      then true
      else false
  | Rook ->
      (*Must have no_barricade between*)
      if
        let h_aline =
          c2_i = c1_i && c2_j <> c1_j && no_barricade (c1_i, c1_j) c2 st
        in
        let v_aline =
          c2_j = c1_j && c2_i <> c1_i && no_barricade (c1_i, c1_j) c2 st
        in
        h_aline || v_aline
      then true
      else false
  | Cannon ->
      (*either no_barricade between and destination emty, or exactly
        one_barricade between, and destination occupied*)
      if
        let h_aline =
          c2_i = c1_i && c2_j <> c1_j
          && no_barricade (c1_i, c1_j) c2 st
          && occupied_coord st.current_board c2 = false
          || c2_i = c1_i && c2_j <> c1_j
             && one_barricade (c1_i, c1_j) c2 st
             && occupied_coord st.current_board c2 = true
        in
        let v_aline =
          c2_j = c1_j && c2_i <> c1_i
          && no_barricade (c1_i, c1_j) c2 st
          && occupied_coord st.current_board c2 = false
          || c2_j = c1_j && c2_i <> c1_i
             && one_barricade (c1_i, c1_j) c2 st
             && occupied_coord st.current_board c2 = true
        in
        h_aline || v_aline
      then true
      else false
  (* Before crossing the river: Soldier can move one step forward; *
     After crossing the riverL Soldier can move one step forward,
     leftward, or rightward; * Soldier can never move backwards.

     *Soldier rule customized for both sides*)
  | Soldier -> (
      match get_side p with
      | Red ->
          if c1_i >= 5 then c2 = (c1_i - 1, c1_j)
          else if
            (*Crossing the river*)
            c2 = (c1_i - 1, c1_j)
            || c2 = (c1_i, c1_j + 1)
            || c2 = (c1_i, c1_j - 1)
          then true
          else false
      | Black ->
          if c1_i <= 4 then c2 = (c1_i + 1, c1_j)
          else if
            (*Crossing the river*)
            c2 = (c1_i + 1, c1_j)
            || c2 = (c1_i, c1_j + 1)
            || c2 = (c1_i, c1_j - 1)
          then true
          else false)

(** [go] evaluated if the input movement is legal. *)
let move start destiny st =
  let cur_board = get_current_board st in
  let cur_turn = get_current_turn st in
  let opponent_turn = oppo_turn cur_turn in
  (* don't move opponent's piece; don't move empty piece *)
  if is_legal_side cur_board start cur_turn then
    let piece = extract (get_piece cur_board start) in
    (* don't move out of the board *)
    if inbound destiny = false then (
      print_int 1;
      Illegal)
    else if rules piece destiny st then
      (* don't move your other piece *)
      if is_legal_side cur_board destiny cur_turn then Illegal
      else if is_legal_side cur_board destiny opponent_turn then
        let captured_piece = extract (get_piece cur_board destiny) in
        if get_side captured_piece = Red then
          Legal
            {
              current_board = update_board cur_board start destiny;
              turn = next_turn cur_turn;
              red_graveyard = captured_piece :: get_current_red_g st;
              black_graveyard = get_current_black_g st;
            }
        else
          Legal
            {
              current_board = update_board cur_board start destiny;
              turn = next_turn cur_turn;
              red_graveyard = get_current_red_g st;
              black_graveyard = captured_piece :: get_current_black_g st;
            }
      else
        Legal
          {
            current_board = update_board cur_board start destiny;
            turn = next_turn cur_turn;
            red_graveyard = get_current_red_g st;
            black_graveyard = get_current_black_g st;
          }
    else (
      print_int 5;
      Illegal)
  else (
    print_int 4;
    Illegal)
