open Pieces
open State

(* let board_array = [| [| |]; [| 1 |]; [| 1 |]; [| 1 |]; [| 1 |]; [| 1
   |]; [| 1 |]; [| 1 |]; |] *)
let board_array =
  [|
    [| 'A'; 'B'; 'C'; 'D'; 'E'; 'D'; 'C'; 'B'; 'A' |];
    [| '+'; '+'; '+'; '+'; '+'; '+'; '+'; '+'; '+' |];
    [| '+'; 'F'; '+'; '+'; '+'; '+'; '+'; 'F'; '+' |];
    [| 'G'; '+'; 'G'; '+'; 'G'; '+'; 'G'; '+'; 'G' |];
    [| '+'; '+'; '+'; '+'; '+'; '+'; '+'; '+'; '+' |];
  |]

(* let print_board = for i = 0 to 5 do if i mod 2 <> 0 then for i = 0 to
   8 do print_char '|'; print_string " "; if i = 9 then print_char '\n'
   done else for i = 0 to 9 do print_char 'A'; print_string "--"; if i =
   9 then print_char '\n' done done *)

let print_board =
  for i = 0 to 8 do
    if i mod 2 <> 0 then
      for j = 0 to 7 do
        print_char '|';
        print_string "   ";
        if j = 7 then (
          print_char '|';
          print_char '\n')
      done
    else
      for j = 0 to 7 do
        print_char board_array.(i / 2).(j);
        print_string "---";
        if j = 7 then (
          print_char board_array.(i / 2).(j + 1);
          print_char '\n')
      done
  done

type t = piece array

(*update board array according to last step; barray is the current
  board; position is a tuple that records the change of update: first
  term be the previous position vector and the second term be the
  updated position vector; name is the name of the piece that is moved.
  updated board array and returns unit*)
let update_board
    (barray : 'a array)
    (position : (int * int) * (int * int))
    (name : char) : unit =
  barray.(position |> fst |> fst).(position |> fst |> snd) <- '+';
  barray.(position |> snd |> fst).(position |> snd |> snd) <- name
