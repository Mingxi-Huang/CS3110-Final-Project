open Board
open State

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let filename = "datasource/moves.csv"

(* let num_rows = 672374 *)
let num_rows = 672372

let first_gid = 57380690

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

let train_data = populate_train filename dummy_array

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

let string_of_int_tuple tp =
  "(" ^ string_of_int (fst tp) ^ ", " ^ string_of_int (snd tp) ^ ")"

let special_treatment board first second =
  match second with
  | "+" ->
      let r, s = get_rank first in
      (* let () = print_endline "before if s = RED" in *)
      if s = Piece.Red then (
        let final_coord = ref (9, 8) in
        for y = 9 downto 0 do
          for x = 0 to 8 do
            (* let () = print_endline (string_of_int_tuple (y, x)) in *)
            let piece = Board.get_piece board (y, x) in
            match piece with
            | Some p ->
                if Piece.get_c p = r && Piece.get_side p = Piece.Red
                then final_coord := (y, x)
            | None -> ()
          done
        done;
        !final_coord)
      else
        (*black*)
        (* let () = print_endline "in black branch" in *)
        let final_coord = ref (0, 0) in
        for y = 0 to 9 do
          for x = 0 to 8 do
            let piece = Board.get_piece board (y, x) in
            match piece with
            | Some p ->
                if Piece.get_c p = r && Piece.get_side p = Piece.Black
                then final_coord := (y, x)
            | None -> ()
          done
        done;
        !final_coord
  | "-" ->
      (* let () = print_endline "in - branch" in *)
      let r, s = get_rank first in
      if s = Piece.Black then (
        let final_coord = ref (9, 8) in
        for y = 9 downto 0 do
          for x = 0 to 8 do
            let piece = Board.get_piece board (y, x) in
            match piece with
            | Some p ->
                if Piece.get_c p = r && Piece.get_side p = Piece.Black
                then final_coord := (y, x)
            | None -> ()
          done
        done;
        !final_coord)
      else
        (*red*)
        let final_coord = ref (0, 0) in
        for y = 0 to 9 do
          for x = 0 to 8 do
            let piece = Board.get_piece board (y, x) in
            match piece with
            | Some p ->
                if Piece.get_c p = r && Piece.get_side p = Piece.Red
                then final_coord := (y, x)
            | None -> ()
          done
        done;
        !final_coord
  | _ -> failwith "deal with this in next step"

let get_start_coord r board start_x =
  if start_x = "+" then special_treatment board r start_x
  else if start_x = "-" then special_treatment board r start_x
  else
    let r, s = get_rank r in
    let start_x =
      if s = Piece.Red then 9 - int_of_string start_x
      else int_of_string start_x - 1
    in
    let coord = ref (0, 0) in
    for y = 0 to 9 do
      match Board.get_piece board (y, start_x) with
      | Some piece ->
          let rank = Piece.get_c piece in
          let side = Piece.get_side piece in
          if (rank, side) = (r, s) then coord := (y, start_x)
      | None -> ()
    done;
    (* print_int (fst !coord); print_int (snd !coord); *)
    !coord

let legal s e state =
  match State.move s e state with Legal t -> true | Illegal -> false

let get_end_coord start state oper side end_x =
  let coord = ref (0, 0) in
  (if oper = "." then
   let end_x =
     if side = "Red" then 9 - int_of_string end_x
     else int_of_string end_x - 1
   in
   coord := (fst start, end_x)
  else if oper = "+" then
    (* let () = print_endline "in oper + branch" in *)
    let multiplier = if side = "Black" then -1 else 1 in
    (* let () = print_int (fst start - (multiplier * end_x)) in let () =
       print_int (snd start) in *)
    match
      State.move start
        (fst start - (multiplier * int_of_string end_x), snd start)
        state
    with
    | Legal t ->
        (* let () = print_endline "legal branch" in *)
        (* rook, cannon, soldier*)
        coord :=
          (fst start - (multiplier * int_of_string end_x), snd start)
    | Illegal ->
        (*horse elephant advisor*)
        (* let () = print_endline "illegal branch" in *)
        if side = "Red" then
          let end_x = 9 - int_of_string end_x in
          for y = 0 to fst start do
            if legal start (y, end_x) state then coord := (y, end_x)
          done
        else
          let end_x = int_of_string end_x - 1 in
          (* let () = print_endline "in black branch " in *)
          for y = fst start + 1 to 9 do
            (* let () = print_int y in let () = print_int end_x in let
               () = print_endline "" in *)
            if legal start (y, end_x) state then
              (* let () = print_endline "in black branch legal" in *)
              coord := (y, end_x)
          done
  else
    (* - *)
    (* let () = print_endline "in oper - branch" in *)
    let multiplier = if side = "Black" then -1 else 1 in
    (* let () = print_int (fst start - (multiplier * end_x)) in let () =
       print_int (snd start) in *)
    match
      State.move start
        (fst start + (multiplier * int_of_string end_x), snd start)
        state
    with
    | Legal t ->
        (* let () = print_endline "legal branch" in *)
        (* rook, cannon, soldier*)
        coord :=
          (fst start + (multiplier * int_of_string end_x), snd start)
    | Illegal ->
        (*horse elephant advisor*)
        (* let () = print_endline "illegal branch" in *)
        if side = "Red" then
          let end_x = 9 - int_of_string end_x in
          for y = fst start + 1 to 9 do
            if legal start (y, end_x) state then coord := (y, end_x)
          done
        else
          let end_x = int_of_string end_x - 1 in
          (* let () = print_endline "in black branch " in *)
          for y = 0 to fst start do
            (* let () = print_int y in let () = print_int end_x in let
               () = print_endline "" in *)
            if legal start (y, end_x) state then
              (* let () = print_endline "in black branch legal" in *)
              coord := (y, end_x)
          done);
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
        get_end_coord start !state_ref operation side_str end_x
      in
      (start, e)

let string_of_move move =
  "("
  ^ string_of_int_tuple (fst move)
  ^ ", "
  ^ string_of_int_tuple (snd move)
  ^ ")"

(** [simulate_round] takes all data for a game of play, simulate the
    game, and output an array of vectorized board with its move of black
    side. Required: the raw input data must include every move in the
    game in the correct order*)
let simulate_round (raw : string array array) :
    (vectorized_board_state * move) array =
  let state_ref = ref State.init_state in
  let l = Array.length raw in
  let list = ref [] in
  for x = 0 to l - 1 do
    let row = raw.(x) in
    let m = translate_coord state_ref row.(3) in
    let board = State.get_current_board !state_ref in
    let vector_board = translate_board board in
    let result = State.move (fst m) (snd m) !state_ref in
    print_endline row.(3);
    print_endline (string_of_move m);
    print_board
      (State.get_current_board !state_ref)
      (Board.generate_graveyard ())
      (Board.generate_score ());
    match result with
    | Legal t ->
        state_ref := t;
        list := (vector_board, m) :: !list
    | _ -> failwith "something wrong"
  done;
  Array.of_list (List.rev !list)

let comp arr1 arr2 =
  compare (int_of_string arr1.(0)) (int_of_string arr2.(0))

let order_array array : string array array =
  let r = ref 0 in
  let b = ref 1 in
  for i = 0 to Array.length array - 1 do
    if array.(i).(2) = "red" then (
      array.(i) <-
        Array.of_list
          [
            Int.to_string !r;
            array.(i).(1);
            array.(i).(2);
            array.(i).(3);
          ];
      (* print_endline ""; print_string array.(i).(0); *)
      r := !r + 2)
    else (
      array.(i) <-
        Array.of_list
          [
            Int.to_string !b;
            array.(i).(1);
            array.(i).(2);
            array.(i).(3);
          ];
      (* print_endline ""; print_string array.(i).(0); *)
      b := !b + 2)
  done;
  Array.sort comp array;
  array

let test = Array.sub train_data 0 73

(** return a list of game length of the dataset*)
let cal_game_length df =
  let result = ref [] in
  let gid = ref first_gid in
  let length = ref 0 in
  for i = 0 to Array.length df - 1 do
    if i = Array.length df - 1 then (
      length := !length + 1;
      result := !length :: !result)
    else if df.(i).(0) = string_of_int !gid then length := !length + 1
    else (
      (*next game*)
      result := !length :: !result;
      length := 1;
      gid := int_of_string df.(i).(0))
  done;
  List.rev !result

(** [data_processing] takes in raw data and turns it into the form of
    (vectorized_board_state * move) array *)

let data_processing train_data =
  let final_data = ref [] in
  let lst_of_lengths = cal_game_length train_data in
  let num_games = List.length lst_of_lengths in
  (* let () = print_int num_games in let () = print_endline "" in *)
  let start_line = ref 0 in
  for n = 0 to num_games - 1 do
    let game_length = List.nth lst_of_lengths n in
    let one_game =
      order_array (Array.sub train_data !start_line game_length)
    in
    start_line := !start_line + game_length;
    let data_for_one_game = simulate_round one_game in
    final_data := Array.to_list data_for_one_game :: !final_data
  done;
  List.rev !final_data

let vectorized_data =
  (* print_endline "passed test"; *)
  data_processing test

let flatten_data
    (vectorized_data : (vectorized_board_state * move) list list) :
    (vectorized_board_state * move) list =
  List.flatten vectorized_data

let get_x_y vectorized_data =
  let v = flatten_data vectorized_data in
  let independent_variable = ref [] in
  let dependent_variable = ref [] in
  let get_x tuple =
    independent_variable := fst tuple :: !independent_variable
  in
  let get_y tuple =
    dependent_variable := snd tuple :: !dependent_variable
  in
  List.iter get_x v;
  List.iter get_y v;
  (List.rev !independent_variable, List.rev !dependent_variable)

let x, y = get_x_y vectorized_data

let train data = failwith "unimplemented"

(* let open Sklearn.Linear_model in  *)
let clf = Sklearn.Linear_model.LogisticRegression()
    (random_state=0).fit ~ x y () in
   LogisticRegression.predict x[:2 :] clf; LogisticRegression.score ~x y
   clf;

(* TEST TODO let%expect_test "LogisticRegression" = let open
   Sklearn.Linear_model in let x, y = load_iris ~return_X_y:true () in
   let clf = LogisticRegression(random_state=0).fit ~x y () in
   print_ndarray @@ LogisticRegression.predict x[:2 :] clf; [%expect {|
   array([0, 0]) |}] print_ndarray @@ LogisticRegression.predict_proba
   x[:2 :] clf; [%expect {| array([[9.8...e-01, 1.8...e-02, 1.4...e-08],
   [9.7...e-01, 2.8...e-02, ...e-08]]) |}] print_ndarray @@
   LogisticRegression.score ~x y clf; [%expect {| |}] *)
