open Piece

let rank_array = [| Rook; Horse; Elephant; Advisor |]

let rev_rank_array = [| Advisor; Elephant; Horse; Rook |]

(** [bottom_row side] is an array containing all pieces in the bottom or
    top row of side [side] of the board*)
let bottom_row side =
  let general =
    create_piece General side ((if side = Red then 9 else 0), 4)
  in
  let row = Array.make 9 None in
  for i = 0 to 3 do
    let piece =
      create_piece rank_array.(i) side ((if side = Red then 9 else 0), i)
    in
    row.(i) <- Some piece
  done;
  row.(4) <- Some general;
  for i = 5 to 8 do
    let piece =
      create_piece
        rev_rank_array.(i - 5)
        side
        ((if side = Red then 9 else 0), i)
    in
    row.(i) <- Some piece
  done;
  row

(** [soldier_row side] is an array containing all pieces in the soldier
    row of side [side] of the board*)
let soldier_row side =
  let row = Array.make 9 None in
  for i = 0 to 8 do
    if i mod 2 = 0 then
      row.(i) <-
        Some
          (create_piece Soldier side ((if side = Red then 6 else 3), i))
  done;
  row

(** [cannon_row side] is an array containing all pieces in the cannon
    row of side [side] of the board*)
let cannon_row side =
  let row = Array.make 9 None in
  let cannon1 =
    create_piece Cannon side ((if side = Red then 7 else 2), 1)
  in
  let cannon2 =
    create_piece Cannon side ((if side = Red then 7 else 2), 7)
  in
  row.(1) <- Some cannon1;
  row.(7) <- Some cannon2;
  row

type board = Piece.t option array array

type t = board

(* [board_array] is the initial state of board *)
let generate_board () =
  [|
    bottom_row Black;
    Array.make 9 None;
    cannon_row Black;
    soldier_row Black;
    Array.make 9 None;
    Array.make 9 None;
    soldier_row Red;
    cannon_row Red;
    Array.make 9 None;
    bottom_row Red;
  |]

(**[print_board board] prints the representation of the board [board] *)
let print_board board =
  for i = 0 to 19 do
    if i = 0 then
      for j = 0 to 8 do
        if j = 0 then print_string " " else print_int (j - 1);
        print_string "   ";
        if j = 8 then (
          print_int 8;
          print_char '\n' )
      done
    else if (i + 1) mod 2 <> 0 then
      for j = 0 to 8 do
        if j <> 0 then (
          print_char '|';
          print_string "   ";
          if j = 8 then (
            print_char '|';
            print_char '\n' ) )
        else print_string "    "
      done
    else
      for j = 0 to 8 do
        if j = 0 then (
          print_int (i / 2);
          if i = 19 then print_string "   " else print_string "   " )
        else (
          print_char (char_of_piece board.(i / 2).(j - 1));
          print_string "---";
          if j = 8 then (
            print_char (char_of_piece board.(i / 2).(j));
            print_char '\n' ) )
      done
  done

let get_piece board coord = board.(fst coord).(snd coord)

(* let set_piece board piece coord = board.(fst coord).(snd coord) <-
   Some piece *)
let matrix_copy m = Array.map Array.copy m

let update_board board start dest =
  let new_board = matrix_copy board in
  let cur_piece = get_piece new_board start in
  new_board.(fst start).(snd start) <- None;
  new_board.(fst dest).(snd dest) <-
    Some (change_coord (extract cur_piece) dest);
  new_board
