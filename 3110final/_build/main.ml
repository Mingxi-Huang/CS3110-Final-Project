open Stdlib
open Printf

let play_game = failwith "unimplemented"

let main () =
  print_string "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game

(* Execute the game engine. *)
let () = main ()
