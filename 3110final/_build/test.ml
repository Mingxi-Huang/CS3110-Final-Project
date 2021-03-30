open OUnit2
open Piece
open Command
open State
open Board

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

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

let string_of_piece_op piece_op =
  let rank = Char.escaped (char_of_piece piece_op) in
  let coord = piece_op |> extract |> Piece.get_coord in
  rank ^ " at "
  ^ string_of_int (fst coord)
  ^ ","
  ^ string_of_int (snd coord)

let advisor = create_piece Advisor Red (9, 3)

let moved_advisor = create_piece Advisor Red (8, 4)

let start_board = generate_board ()

let updated_board = update_board start_board (9, 3) (8, 4)

let start_state = init_state

let second_state = create_state updated_board Black

let piece_tests =
  [
    ("get advisor" >:: fun _ -> assert_equal (get_c advisor) Advisor);
    ("get side" >:: fun _ -> assert_equal (get_side advisor) Red);
  ]

let state_tests =
  [
    ( "Legal move" >:: fun _ ->
      assert_equal (Legal second_state) (move (9, 3) (8, 4) init_state)
    );
    ( "Illegal turn" >:: fun _ ->
      assert_equal Illegal (move (3, 0) (2, 0) init_state) );
  ]

let command_err name str excep =
  name >:: fun ctxt -> assert_raises excep (fun () -> parse str)

let command_tests =
  [
    ("quit" >:: fun _ -> assert_equal Quit (parse "quit"));
    ( "move good" >:: fun _ ->
      assert_equal (Move [ (2, 3); (4, 5) ]) (parse "move 2,3 4,5") );
    command_err "empty" "" Empty;
    command_err "coordinate wrong" "move 1 2" Malformed;
    command_err "coordinate space" "move 1, 3 2, 5" Malformed;
    command_err "coordinate length wrong" "move 1,2 3,4 5,6" Malformed;
  ]

let board_tests =
  [
    ( "get piece test" >:: fun _ ->
      assert_equal (Some advisor)
        (get_piece start_board (9, 3))
        ~printer:string_of_piece_op );
    ( "update board test" >:: fun _ ->
      assert_equal (Some moved_advisor)
        (get_piece updated_board (8, 4))
        ~printer:string_of_piece_op );
  ]

let ai_tests = [ (* TODO: add tests for the State module here *) ]

let suite =
  "test suite forProject"
  >::: List.flatten
         [ state_tests; command_tests; piece_tests; board_tests ]

let _ = run_test_tt_main suite
