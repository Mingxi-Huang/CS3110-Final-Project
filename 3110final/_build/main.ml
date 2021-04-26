open Stdlib
open Printf
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
let rec play_game_help st =
  let cur_board = State.get_current_board st in
  let cur_turn = State.get_current_turn st in
  print_string "\nCurrent Board:\n ";
  Board.print_board cur_board;
  print_endline "\nCurrent Turn: ";
  print_endline ("\n" ^ Piece.string_of_side cur_turn);
  print_endline
    "\n\
     What do you want to do next?\n\
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
        play_game_help (get_result_state new_st_result)
    | Quit ->
        print_endline "bye bye";
        exit 0
    | _ ->
        print_endline "unknown command!";
        play_game_help st
  with
  | Illegal_state ->
      print_endline "This is an illegal move, try again! \n";
      play_game_help st
  | Invalid_argument _ ->
      print_endline
        "please enter the coordinate within the board, Please try \
         again!";
      print_string "> ";
      play_game_help st

(* let commands = ref valid_command input in while commands <> Quit do
   play_game_help st; Board.print_board (State.get_current_board st
   commands); commands := valid_command read_line () done; p *)

(** [play_game f] starts the adventure in file [f]. *)
let play_game =
  let init_st = State.init_state in
  play_game_help init_st

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "\n\nWelcome  to the 3110 Chinese Chess Game engine.\n";
  print_string "Start playing, You are the red side\n";
  print_string "> ";
  play_game

(* Execute the game engine. *)

let () = main ()