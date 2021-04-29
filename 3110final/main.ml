open Stdlib
open Printf
open State
open Command

exception Illegal_state

(** [get_result_state result] is the state from result [result], if
    result is legal, then gives state; otherwise, raise Illegal_state
    exception *)
let get_result_state result =
  match result with
  | State.Legal t -> t
  | State.Illegal -> raise Illegal_state

(** [valid_command] is an recursive function that handles exceptions
    with mal inputs. If the input is legal, return parse command, else
    return a warning message and ask for new input. This step is
    repeated until a legal input is received. *)
let rec valid_command command =
  try parse command with
  | Empty -> (
      print_endline "Command is Empty";

      match read_line () with input -> valid_command input )
  | Malformed -> (
      print_endline
        "Command is Malformed, should be: 'quit' or 'move x1,y1 x2,y2' \
         Please try again";
      print_string "> ";
      match read_line () with input -> valid_command input )
  | Illegal_state -> (
      print_endline "This is an illegal move, try again! \n";
      print_string "> ";
      match read_line () with input -> valid_command input )

(** [play_game_help] is the helper function that updates each move
    according to the command and pass the turn to the other side*)

let rec play_game_help st mode =
  let cur_board = State.get_current_board st in
  let cur_turn = State.get_current_turn st in
  Printf.printf "\027[36;1m%s\027[0m" "\nCurrent Board:\n ";
  if cur_turn = Red then Board.print_board cur_board
  else Board.print_rev_board (Board.turned_board cur_board);
  print_endline "\027[36;1m\nCurrent Turn: \027[0m";
  if mode = 2 || (mode = 1 && cur_turn = Red) then (
    if Piece.string_of_side cur_turn = "Red" then
      Printf.printf "\027[31;1m\n%s\n\027[0m"
        (Piece.string_of_side cur_turn)
    else
      Printf.printf "\027[34;1m\n%s\n\027[0m"
        (Piece.string_of_side cur_turn);
    (* print_endline ("\n" ^ Piece.string_of_side cur_turn); *)
    print_endline
      "\n\
       What do you want to do next? (you can move or quit)\n\
       Example: 'move 9,4 8,4' moves the red General up one step.";
    print_string "> ";
    let msg = read_line () in
    let command = valid_command msg in
    try
      match command with
      | Move [ (x1, y1); (x2, y2) ] ->
          let start = (x1, y1) in
          let dest = (x2, y2) in
          let new_st_result = State.move start dest st in
          let new_st = get_result_state new_st_result in
          let win_cond = State.check_winner new_st in
          if win_cond <> None then
            let winner = Option.get win_cond |> Piece.string_of_side in
            let () =
              print_endline ("Congratulations! " ^ winner ^ " win!")
            in
            exit 0
          else play_game_help new_st mode
      | Quit ->
          print_endline "bye bye";
          exit 0
      | _ ->
          print_endline "unknown command!";
          play_game_help st mode
    with
    | Illegal_state ->
        print_endline "This is an illegal move, try again! \n";
        play_game_help st mode
    | Invalid_argument _ ->
        print_endline
          "please enter the coordinate within the board, Please try \
           again!";
        print_string "> ";
        play_game_help st mode )
  else
    Printf.printf "\027[34;1m\n%s\n\027[0m"
      (Piece.string_of_side cur_turn);
  (* print_endline "1"; *)
  let c = Ai.make_command st in
  (* print_endline "2"; *)
  (* print_endline c; *)
  let command = Command.parse c in
  match command with
  | Move [ (x1, y1); (x2, y2) ] ->
      let start = (x1, y1) in
      let dest = (x2, y2) in
      let new_st_result = State.move start dest st in
      play_game_help (get_result_state new_st_result) mode
  | _ -> failwith "ai module error"

(** [play_game f] starts the adventure in file [f]. *)
let play_game mode =
  let init_st = State.init_state in
  play_game_help init_st mode

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "\n\nWelcome  to the 3110 Chinese Chess Game engine.\n";
  print_endline
    "\n\none player or two players mode: 1 for one, 2 for two";
  print_string "> ";
  let msg = read_line () in
  let mode = ref 1 in
  if msg = string_of_int 1 then mode := 1 else mode := 2;
  print_string "Start playing, You are the red side\n";
  play_game !mode

(* Execute the game engine. *)

let () = main ()
