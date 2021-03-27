(**Command.ml*)

(*Requires: message to be strictly following format "Go horse1me 6";
  players know the basic rules: won't make illegal moves / move piece
  into a state with another player piece occupied*)

open State
open Piece

(**Input: message, current piecelist; Output: selected piece*)
let selected_piece message plist =
  let lst = String.split_on_char ' ' message in
  let id = List.nth lst 1 in
  List.find (fun x -> get_id x = id) plist

(**Input: message, current statelist; Output: destination state *)
let destination_state message slist =
  let lst = String.split_on_char ' ' message in
  let label = List.nth lst 2 in
  List.find (fun x -> get_label x = label) slist

(**Input: selected piece, destination state; Output: renewed selected
   piece, with [plabel] changed*)
let update_piece piece state =
  create_piece (get_c piece) (get_n piece) (get_side piece)
    (get_id piece) (get_label state)

(**Input: selected piece, destination state, enable: bool; Output:
   renewed destination/old state -- with [occupied] changed*)
let update_state state enable =
  create_state (get_label state) (get_coord state) enable

(**Input: current piece list, selected piece, destination state; Output:
   the updated piece list with : updated selected piece, potentially
   deleted destination piece*)
let update_plist piecelist piece state =
  let lst0 =
    if get_occupy state then
      List.filter (fun x -> get_plabel x <> get_label state) piecelist
    else piecelist
  in
  let updated_piece = update_piece piece state in
  let lst1 = List.filter (fun x -> x <> piece) lst0 in
  updated_piece :: lst1

(**Input: current piece list, selected piece, destination state; Output:
   the updated piece list after moving things around*)
let update_slist slist piece state =
  let state0 =
    List.find (fun x -> get_label x = get_plabel piece) slist
  in
  let updated_state0 = update_state state0 false in
  let updated_state1 = update_state state true in
  let lst = List.filter (fun x -> x <> state0 && x <> state) slist in
  updated_state0 :: updated_state1 :: lst
