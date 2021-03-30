open Board
open Piece

type state = {
  current_board : Board.t;
  turn : side;
}

type t = state

let init_state = { current_board = Board.generate_board (); turn = Red }

type result =
  | Legal of t
  | Illegal

let next_turn = function Red -> Black | Black -> Red

let get_current_board st = st.current_board

(**[non_empty_coord] evaluates if the selected coordinate have piece on
   it*)
let get_current_turn st = st.turn

let create_state board turn = { current_board = board; turn }

let non_empty_coord piece_option =
  match piece_option with None -> false | Some _ -> true

(**[illegal_side] evaluates if the piece on the selected coordinate is
   of same side with [turn]*)
let illegal_side board coord turn =
  let p = get_piece board coord in
  if non_empty_coord p && p |> extract |> get_side = turn then true
  else false

(** [go] evaluated if the input movement is legal. *)
let go start destiny st =
  let cur_board = get_current_board st in
  let cur_turn = get_current_turn st in
  if illegal_side cur_board start cur_turn then
    let piece = extract (get_piece cur_board start) in
    if get_side piece <> cur_turn then (
      print_int 0;
      Illegal )
    else if
      fst destiny < 0
      || fst destiny > 9
      || snd destiny < 0
      || snd destiny > 8
    then (
      print_int 1;
      Illegal )
    else if rules piece destiny then
      if illegal_side cur_board destiny cur_turn then Illegal
      else
        Legal
          {
            current_board = update_board cur_board start destiny;
            turn = next_turn cur_turn;
          }
    else (
      print_int 5;
      Illegal )
  else (
    print_int 4;
    Illegal )
