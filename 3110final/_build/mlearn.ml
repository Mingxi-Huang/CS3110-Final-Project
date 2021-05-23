open Board
open State

let filename = "datasource/moves.csv"

let num_rows = 672376

let dummy_array = Array.make num_rows (Array.make 4 "")

let populate_train filename train_data =
  try
    let channel = open_in filename in
    let line = ref "" in
    line := input_line channel;
    for x = 0 to Array.length train_data - 1 do
      (* print_int x; print_endline ""; *)
      line := input_line channel;
      let cols = String.split_on_char ',' !line in
      train_data.(x) <- Array.of_list cols
      (* match cols with | [ game_id; turn_num; side; move ] ->
         train_data.(x).(0) <- game_id; train_data.(x).(1) <- turn_num;
         train_data.(x).(2) <- side; train_data.(x).(3) <- move | _ ->
         failwith "impossible" *)
    done;
    train_data
  with End_of_file -> train_data

(* let train_data = populate_train filename dummy_array *)

type vectorized_board_state = int array array array

type move = (int * int) * (int * int)

(*[G;A;E;H;R;C;S;g;a;e;h;r;c;s] red first half, black last half*)
let vec_piece rank side =
  match rank with
  | Piece.General ->
      if side = Piece.Red then
        [ 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
      else [ 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0 ]
  | Piece.Advisor ->
      if side = Piece.Red then
        [ 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
      else [ 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0 ]
  | Piece.Elephant ->
      if side = Piece.Red then
        [ 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
      else [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0 ]
  | Piece.Horse ->
      if side = Piece.Red then
        [ 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
      else [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0 ]
  | Piece.Rook ->
      if side = Piece.Red then
        [ 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
      else [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0 ]
  | Piece.Cannon ->
      if side = Piece.Red then
        [ 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0 ]
      else [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0 ]
  | Piece.Soldier ->
      if side = Piece.Red then
        [ 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0 ]
      else [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1 ]

let translate_lines board y_axis =
  let parray = Array.make 9 (Array.make 14 0) in
  for x = 0 to 8 do
    match Board.get_piece board (y_axis, x) with
    | Some piece ->
        let rank = Piece.get_c piece in
        let side = Piece.get_side piece in
        parray.(x) <- Array.of_list (vec_piece rank side)
    | None -> ()
  done;
  parray

(** [translate_board] takes a reference of board and turn it into
    vectorized board. *)
let translate_board board : vectorized_board_state =
  let vec = Array.make 10 (Array.make 9 (Array.make 14 0)) in
  for y = 0 to 9 do
    vec.(y) <- translate_lines board y
  done;
  vec

let s = State.init_state

let b = State.get_current_board s

let vec = translate_board b

let split s =
  let first = String.sub s 0 1 in
  let second = String.sub s 1 1 in
  let fourth = String.sub s 3 1 in
  let third = String.sub s 2 1 in
  (first, second, fourth, third)

(** return (Piece.rank, Piece.side) *)
let get_rank rank =
  match rank with
  | "K" -> (Piece.General, Piece.Red)
  | "A" -> (Piece.Advisor, Piece.Red)
  | "E" -> (Piece.Elephant, Piece.Red)
  | "H" -> (Piece.Horse, Piece.Red)
  | "R" -> (Piece.Rook, Piece.Red)
  | "C" -> (Piece.Cannon, Piece.Red)
  | "P" -> (Piece.Soldier, Piece.Red)
  | "k" -> (Piece.General, Piece.Black)
  | "a" -> (Piece.Advisor, Piece.Black)
  | "e" -> (Piece.Elephant, Piece.Black)
  | "h" -> (Piece.Horse, Piece.Black)
  | "r" -> (Piece.Rook, Piece.Black)
  | "c" -> (Piece.Cannon, Piece.Black)
  | "p" -> (Piece.Soldier, Piece.Black)
  | _ -> failwith "impossible"

(** 有的第二位数是+-？？？*)
let special_treatment (str : string) : int =
  match str with
  | "+" -> failwith "unimplemented"
  | "-" -> failwith "unimplemented"
  | _ -> int_of_string str

let get_start_coord r board start_x =
  let start_x = 9 - special_treatment start_x in
  let coord = ref (0, 0) in
  for y = 0 to 9 do
    match Board.get_piece board (y, start_x) with
    | Some piece ->
        let rank = Piece.get_c piece in
        let side = Piece.get_side piece in
        if (rank, side) = get_rank r then coord := (y, start_x)
    | None -> ()
  done;
  (* print_int (fst !coord); print_int (snd !coord); *)
  !coord

let legal s e state =
  match State.move s e state with Legal t -> true | Illegal -> false

let get_end_coord start state oper side end_x =
  let coord = ref (0, 0) in
  ( if oper = "." then
    let end_x = 9 - end_x in
    coord := (fst start, end_x)
  else if oper = "+" then
    let () = print_endline "in oper + branch" in
    let multiplier = if side = "Black" then -1 else 1 in
    (* let () = print_int (fst start - (multiplier * end_x)) in let () =
       print_int (snd start) in *)
    match
      State.move start
        (fst start - (multiplier * end_x), snd start)
        state
    with
    | Legal t ->
        let () = print_endline "legal branch" in
        (* rook, cannon, soldier*)
        coord := (fst start - (multiplier * end_x), snd start)
    | Illegal ->
        (*horse elephant advisor*)
        let () = print_endline "illegal branch" in

        if side = "Red" then
          let end_x = 9 - end_x in
          for y = 0 to fst start do
            if legal start (y, end_x) state then coord := (y, end_x)
          done
        else
          let end_x = end_x - 1 in
          let () = print_endline "in black branch " in
          for y = fst start + 1 to 9 do
            let () = print_int y in
            let () = print_int end_x in
            let () = print_endline "" in
            if legal start (y, end_x) state then
              let () = print_endline "in black branch legal" in
              coord := (y, end_x)
          done
  else
    (* - *)
    let () = print_endline "in oper - branch" in
    let multiplier = if side = "Black" then -1 else 1 in
    (* let () = print_int (fst start - (multiplier * end_x)) in let () =
       print_int (snd start) in *)
    match
      State.move start
        (fst start + (multiplier * end_x), snd start)
        state
    with
    | Legal t ->
        let () = print_endline "legal branch" in
        (* rook, cannon, soldier*)
        coord := (fst start + (multiplier * end_x), snd start)
    | Illegal ->
        (*horse elephant advisor*)
        let () = print_endline "illegal branch" in

        if side = "Red" then
          let end_x = 9 - end_x in
          for y = fst start + 1 to 9 do
            if legal start (y, end_x) state then coord := (y, end_x)
          done
        else
          let end_x = end_x - 1 in
          let () = print_endline "in black branch " in
          for y = 0 to fst start do
            let () = print_int y in
            let () = print_int end_x in
            let () = print_endline "" in
            if legal start (y, end_x) state then
              let () = print_endline "in black branch legal" in
              coord := (y, end_x)
          done );
  !coord

let translate_coord state_ref (s : string) : move =
  match split s with
  | rank, start_x, end_x, operation ->
      let start =
        get_start_coord rank
          (State.get_current_board !state_ref)
          start_x
      in
      let e =
        let side = snd (get_rank rank) in
        let side_str =
          match side with Red -> "Red" | Black -> "Black"
        in
        get_end_coord start !state_ref operation side_str
          (int_of_string end_x)
      in
      (start, e)

(** [simulate_round] takes all data for a round of play, simulate the
    round, and output an array of vectorized board with its move of
    black side. Required: the raw input data must include every move in
    the round in the correct order*)
let simulate_round (raw : string array array) :
    (vectorized_board_state * move) array =
  let state = ref State.init_state in
  let l = Array.length raw in
  let array =
    Array.make (l / 2)
      ( translate_board (State.get_current_board !state),
        translate_coord state "" )
  in
  try
    for x = 0 to l - 1 do
      let data = raw.(x) in
      let m = translate_coord state data.(3) in
      let result = State.move (fst m) (snd m) !state in
      match result with
      | Legal t ->
          state := t;
          let board = State.get_current_board t in
          let vector_board = translate_board board in
          if data.(2) == "black" then array.(x / 2) <- (vector_board, m)
      | Illegal -> failwith "something is wrong"
    done;
    array
  with e -> array

(** [data_processing] takes in raw data and turns it into the form of
    (vectorized_board_state * move) array *)
let data_processing (train_data : string array array) :
    (vectorized_board_state * move) array =
  failwith "unimplemented"
