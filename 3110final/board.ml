open Pieces

(* let print_board = for i = 0 to 5 do if i mod 2 <> 0 then for i = 0 to
   8 do print_char '|'; print_string " "; if i = 9 then print_char '\n'
   done else for i = 0 to 9 do print_char 'A'; print_string "--"; if i =
   9 then print_char '\n' done done *)

type t = piece array

(* let board_array = [| [| |]; [| 1 |]; [| 1 |]; [| 1 |]; [| 1 |]; [| 1
   |]; [| 1 |]; [| 1 |]; |] *)
