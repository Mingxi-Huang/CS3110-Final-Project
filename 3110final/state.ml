open Board
open Piece

type turn =
  | Red
  | Black

let init_state = Black

type result =
  | Legal
  | Illegal

let next_turn turn = function Red -> Black | Black -> Red

(**[non_empty_coord] evaluates if the selected coordinate have piece on
   it*)
let non_empty_coord piece_option =
  match piece_option with None -> false | Some _ -> true

(**[illegal_side] evaluates if the piece on the selected coordinate is
   of same side with [turn]*)
let illegal_side board coord turn =
  let p = get_piece board coord in
  if non_empty_coord p && p |> extract |> get_side = turn then true
  else false

(** [go] evaluated if the input movement is legal. *)
let go turn start destiny board =
  if illegal_side board start turn then
    let piece = extract (get_piece board start) in
    if get_side piece <> turn then (
      print_int 0;
      Illegal)
    else if
      fst destiny < 0
      || fst destiny > 9
      || snd destiny < 0
      || snd destiny > 8
    then (
      print_int 1;
      Illegal)
    else if rules piece destiny then
      if illegal_side board destiny turn then Illegal
      else (*turn = next_turn turn;*)
        Legal
    else (
      print_int 5;
      Illegal)
  else (
    print_int 4;
    Illegal)
