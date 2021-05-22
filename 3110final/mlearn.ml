let filename = "datasource/moves.csv"

let num_rows = 672376

let dummy_array = Array.make num_rows (Array.make 4 "")

let populate_train filename train_data =
  try
    let channel = open_in filename in
    let line = ref "" in
    for x = 0 to Array.length train_data - 1 do
      print_int x;
      print_endline "";
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

(** [vectorized_board_state] is a 3D int array that represent the board
    state: [\[0, 0, … , 1\] * 9]*9 len[0][0] = 32, and is an one-hot
    representation of piece. *)
type vectorized_board_state = int array array array

(** [move] represent a move in a certain round. It has the form ((x1,
    y1),(x2, y2)), the first coordinate is the start of move and the
    second coordinate is the destiny. *)
type move = (int * int) * (int * int)

(** [translate_board] takes a reference of board and turn it into
    vectorized board. *)
let translate_board board : vectorized_board_state =
  failwith "unimplemented"

let translate_coord (s : string) : move = failwith "unimplemented"

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
        translate_coord "" )
  in
  try
    for x = 0 to l - 1 do
      let data = raw.(x) in
      let m = translate_coord data.(3) in
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
