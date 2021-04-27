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

let occupied_coord board coord =
  match get_piece board coord with None -> false | Some _ -> true

(**[illegal_side] evaluates if the piece on the selected coordinate is
   of same side with [turn]*)
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
    else if rules piece destiny then
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
