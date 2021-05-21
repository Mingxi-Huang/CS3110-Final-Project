open State
open Piece
open Board

type piece = Piece.t

let s = Black

let available_piece (board : Board.t) : Piece.t list =
  let list = ref [] in
  for i = 0 to 8 do
    for j = 0 to 8 do
      let piece = get_piece board (i, j) in
      match piece with
      | None -> ()
      | Some x ->
          if Piece.get_side x = Black then list := x :: !list else ()
    done
  done;
  !list

let choose_piece board list_piece =
  list_piece |> List.length |> ( + ) (-1) |> Random.int
  |> List.nth list_piece

let get_coordinate piece =
  let c = get_coord piece in
  match get_c piece with
  | Soldier -> (c, (fst c - 1, snd c))
  | Cannon ->
      ( c,
        List.nth
          [
            (fst c, snd c + Random.int (8 - snd c));
            (fst c + Random.int (8 - fst c), snd c);
          ]
          (Random.int 1) )
  | Rook ->
      ( c,
        List.nth
          [
            (fst c, snd c + Random.int (8 - snd c));
            (fst c + Random.int (8 - fst c), snd c);
          ]
          (Random.int 1) )
  | Horse ->
      ( c,
        List.nth
          [
            (fst c + 2, snd c + 1);
            (fst c - 2, snd c + 1);
            (fst c + 2, snd c - 1);
            (fst c - 2, snd c - 1);
            (fst c + 1, snd c + 2);
            (fst c + 1, snd c - 2);
            (fst c - 1, snd c + 2);
            (fst c - 1, snd c - 2);
          ]
          (Random.int 7) )
  | Elephant ->
      ( c,
        List.nth
          [
            (fst c + 2, snd c + 2);
            (fst c - 2, snd c + 2);
            (fst c + 2, snd c - 2);
            (fst c - 2, snd c - 2);
          ]
          (Random.int 3) )
  | Advisor ->
      ( c,
        List.nth
          [
            (fst c + 1, snd c + 1);
            (fst c - 1, snd c + 1);
            (fst c + 1, snd c - 1);
            (fst c - 1, snd c - 1);
          ]
          (Random.int 3) )
  | General ->
      ( c,
        List.nth
          [
            (fst c, snd c - 1);
            (fst c, snd c + 1);
            (fst c + 1, snd c);
            (fst c - 1, snd c);
          ]
          (Random.int 3) )

let rec make_legal_move state list_piece =
  let piece = choose_piece (State.get_current_board state) list_piece in
  let coord = get_coordinate piece in
  match State.move (fst coord) (snd coord) state with
  | Illegal -> make_legal_move state list_piece
  | Legal _ -> coord

let make_command state =
  let list_piece = available_piece (State.get_current_board state) in
  (* let piece = choose_piece (State.get_current_board state) list_piece
     in *)
  let coord = make_legal_move state list_piece in
  match coord with
  | (x, y), (x', y') ->
      "move " ^ string_of_int x ^ "," ^ string_of_int y ^ " "
      ^ string_of_int x' ^ "," ^ string_of_int y'
