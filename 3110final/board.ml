open Piece

(* initial state of board *)
let board_array =
  [|
    [| 'A'; 'B'; 'C'; 'D'; 'E'; 'D'; 'C'; 'B'; 'A' |];
    [| '+'; '+'; '+'; '+'; '+'; '+'; '+'; '+'; '+' |];
    [| '+'; 'F'; '+'; '+'; '+'; '+'; '+'; 'F'; '+' |];
    [| 'G'; '+'; 'G'; '+'; 'G'; '+'; 'G'; '+'; 'G' |];
    [| '+'; '+'; '+'; '+'; '+'; '+'; '+'; '+'; '+' |];
    [| '+'; '+'; '+'; '+'; '+'; '+'; '+'; '+'; '+' |];
    [| 'g'; '+'; 'g'; '+'; 'g'; '+'; 'g'; '+'; 'g' |];
    [| '+'; 'f'; '+'; '+'; '+'; '+'; '+'; 'f'; '+' |];
    [| '+'; '+'; '+'; '+'; '+'; '+'; '+'; '+'; '+' |];
    [| 'a'; 'b'; 'c'; 'd'; 'e'; 'd'; 'c'; 'b'; 'a' |];
  |]

let print_board =
  for i = 0 to 19 do
    if i = 0 then
      for j = 0 to 8 do
        print_int j;
        print_string "   ";
        if j = 8 then (
          print_int 9;
          print_char '\n')
      done
    else if (i + 1) mod 2 <> 0 then
      for j = 0 to 8 do
        if j <> 0 then (
          print_char '|';
          print_string "   ";
          if j = 8 then (
            print_char '|';
            print_char '\n'))
        else print_string "    "
      done
    else
      for j = 0 to 8 do
        if j = 0 then (
          print_int ((i / 2) + 1);
          if i = 19 then print_string "  " else print_string "   ")
        else (
          print_char board_array.(i / 2).(j - 1);
          print_string "---";
          if j = 8 then (
            print_char board_array.(i / 2).(j);
            print_char '\n'))
      done
  done

type t = piece list

(*update board array according to last step; input: barray is the
  current board; position is a tuple that records the change of update:
  first term be the previous position vector and the second term be the
  updated position vector; name is the name of the piece that is moved.
  updated board array and returns unit*)
let update_board
    (barray : 'a array)
    (position : (int * int) * (int * int))
    (name : char) : unit =
  barray.(position |> fst |> fst |> ( - ) 1).(position |> fst |> snd
                                              |> ( - ) 1) <- '+';
  barray.(position |> snd |> fst |> ( - ) 1).((position |> snd |> snd)
                                              - 1) <- name
