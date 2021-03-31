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

(** [play_game_help] the REPL (Read Eval Print Loop)*)

(** [valid_command] is an recursive function that handles exceptions
    with mal inputs. If the input is legal, return parse command, else
    return a warning message and ask for new input. This step is
    repeated until a legal input is received. *)
let rec valid_command command =
  try parse command with
  | Empty -> (
      print_endline "Command is Empty";
      match read_line () with input -> valid_command input)
  | Malformed -> (
      print_endline
        "Command is Malformed, should be: 'quit' or 'move x1,y1 x2,y2'";
      match read_line () with input -> valid_command input)

(** [play_game_help] is the helper function that updates each move
    according to the command and pass the turn to the other side*)
let rec play_game_help st commands = failwith "unimplemented"

(** [play_game f] starts the adventure in file [f]. *)
let play_game input =
  let commands = ref valid_command input in
  while commands <> Quit do
    play_game_help st;
    Board.print_board (State.get_current_board st commands);
    commands := valid_command read_line ()
  done;
  print_endline "bye bye";
  exit 0

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Chinese Chess Game engine.\n";
  print_endline "Start playing, You are the red side\n";
  print_string "> ";
  let st = State.init_state in
  Board.print_board (State.get_current_board st);
  match read_line () with input -> play_game input

(* Execute the game engine. *)
let () = main ()
