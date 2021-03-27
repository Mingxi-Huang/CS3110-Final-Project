open OUnit2
open Piece
open Command
open State

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

let advisor = Piece.create_piece Advisor "number" "side" "id" "plable"

let piece_tests =
  [
    ("get advisor" >:: fun _ -> assert_equal (get_c advisor) Advisor);
    ("get side" >:: fun _ -> assert_equal (get_side advisor) "side");
  ]

let state_tests =
  [ (* TODO: add tests for the Adventure module here *) ]

let command_tests =
  [ (* TODO: add tests for the Command module here *) ]

let board_tests = [ (* TODO: add tests for the State module here *) ]

let ai_tests = [ (* TODO: add tests for the State module here *) ]

let suite =
  "test suite forProject"
  >::: List.flatten
         [ state_tests; command_tests; piece_tests; board_tests ]

let _ = run_test_tt_main suite
