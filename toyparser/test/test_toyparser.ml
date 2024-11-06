open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = 9

(* YOUR TESTS HERE *)

let%test "test_eval_1" = 
  parse "1 + 2 + 3 + (1 + 2)" |> eval = 9

let%test "test_eval_2" = 
  parse "5 - 3" |> eval = 2

let%test "test_eval_3" = 
  parse "2 * 3 + 4" |> eval = 10

let%test "test_eval_4" = 
  parse "(5 + 5) * 2" |> eval = 20

let%test "test_eval_5" = 
  parse "10 / 2" |> eval = 5

let%test "test_eval_6" = 
  parse "(10 + 2) / 4" |> eval = 3

let%test "test_eval_7" = 
  parse "2 * (3 + 4)" |> eval = 14

let%test "test_eval_8" = 
  parse "2 * (3 + 4) - 5" |> eval = 9

let%test "test_eval_9" = 
  parse "100 / 4 + 2" |> eval = 27

let%test "test_eval_10" = 
  parse "(100 / 4) + (2 * 3)" |> eval = 31

let%test "test_eval_11" = 
  parse "5 + 5 * 2" |> eval = 15

let%test "test_eval_12" = 
  parse "10 - 2 * (3 + 1)" |> eval = 2

let%test "test_eval_13" = 
  parse "100 + (50 - 25)" |> eval = 125

let%test "test_eval_14" = 
  parse "2 + 2 * 2" |> eval = 6

let%test "test_eval_15" = 
  parse "10 / (2 + 3)" |> eval = 2

let%test "test_eval_16" = 
  parse "(5 + 5) * (10 / 2)" |> eval = 50

let%test "test_eval_17" = 
  parse "(1 + 2) * (3 + 4) + 5" |> eval = 26

let%test "test_eval_18" = 
  parse "3 + 2 * 5 - 1" |> eval = 12

let%test "test_eval_19" = 
  parse "100 * (2 + 3)" |> eval = 500

let%test "test_eval_20" = 
  parse "(4 + 6) * 2 - 5" |> eval = 15

(* Tests for edge cases *)
let%test "test_eval_edge_case_1" = 
  parse "0" |> eval = 0

let%test "test_eval_edge_case_2" = 
  parse "1 + 0" |> eval = 1

let%test "test_eval_edge_case_3" = 
  parse "100 - 100" |> eval = 0

let%test "test_eval_edge_case_4" = 
  parse "50 / 5" |> eval = 10

let%test "test_eval_edge_case_5" = 
  parse "(2 + 3) * (4 - 1)" |> eval = 15
