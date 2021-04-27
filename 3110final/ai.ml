open State
open Piece
open Board

type piece = Piece.t

let s () = match Random.int 1 with 0 -> Black | 1 -> Red

let get_side = s

let available_piece (board : Board.t) : Piece.t list =
  let list = ref [] in
  for i = 0 to 9 do
    for j = 0 to 9 do
      let piece = get_piece board (i, j) in
      match piece with
      | None -> ()
      | Some x ->
          if Piece.get_side x = s () then list := x :: !list else ()
    done
  done;
  !list

let choose_piece board =
  let list_piece = available_piece board in
  list_piece |> List.length |> ( - ) 1 |> Random.int
  |> List.nth list_piece

let make_command = failwith "unimplemented"

let get_coordinate piece =
  let current_coord = get_coord piece in
  match get_c piece with
  | Soldier ->
      (current_coord, (fst current_coord - 1, snd current_coord))
  | Cannon -> failwith "unimplemented"
  | Rook -> failwith "unimplemented"
  | Horse -> failwith "unimplemented"
  | Elephant -> failwith "unimplemented"
  | Advisor -> failwith "unimplemented"
  | General -> failwith "unimplemented"

(* let rules p c2 = let c1_i = get_i p.coordinate in let c1_j = get_j
   p.coordinate in let c2_i = get_i c2 in let c2_j = get_j c2 in match
   get_c p with | General -> if c2 = (c1_i, c1_j - 1) || c2 = (c1_i,
   c1_j + 1) || c2 = (c1_i - 1, c1_j) || c2 = (c1_i + 1, c1_j) then true
   else false | Advisor -> if c2 = (c1_i + 1, c1_j + 1) || c2 = (c1_i -
   1, c1_j + 1) || c2 = (c1_i + 1, c1_j - 1) || c2 = (c1_i - 1, c1_j -
   1) then true else false | Elephant -> if c2 = (c1_i + 2, c1_j + 2) ||
   c2 = (c1_i - 2, c1_j + 2) || c2 = (c1_i + 2, c1_j - 2) || c2 = (c1_i
   - 2, c1_j - 2) then true else false | Horse -> if c2 = (c1_i + 2,
   c1_j + 1) || c2 = (c1_i - 2, c1_j + 1) || c2 = (c1_i + 2, c1_j - 1)
   || c2 = (c1_i - 2, c1_j - 1) || c2 = (c1_i + 1, c1_j + 2) || c2 =
   (c1_i + 1, c1_j + -2) || c2 = (c1_i + -1, c1_j + 2) || c2 = (c1_i +
   -1, c1_j + -2) then true else false | Rook -> if (c2_i = c1_i || c2_j
   = c1_j) && (c1_i, c1_j) <> c2 then true else false | Cannon -> if
   (c2_i = c1_i || c2_j = c1_j) && (c1_i, c1_j) <> c2 then true else
   false *)
