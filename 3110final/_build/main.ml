open Stdlib
open Printf

exception Illegal_state

(** [get_result_state result] is the state from result [result], if
    result is legal, then gives state; otherwise, raise Illegal_state
    exception *)
let get_result_state result =
  match result with
  | State.Legal t -> t
  | State.Illegal -> raise Illegal_state

(** [play_game_help] the REPL (Read Eval Print Loop)*)
let rec play_game_help st = failwith "unimplemented"

(** [play_game f] starts the adventure in file [f]. *)
let play_game =
  let st = State.init_state in
  Board.print_board (State.get_current_board st)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Chinese Chess Game engine.\n";
  print_endline "Start playing, You are the red side\n";
  print_string "> ";
  match read_line () with _ -> play_game

(* Execute the game engine. *)
let () = main ()
