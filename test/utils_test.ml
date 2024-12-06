open OUnit2
open Utils

let test_shuffle _ =
  let lst = [1; 2; 3; 4; 5] in
  let shuffled_lst = Utils.shuffle lst in
  assert_equal ~msg:"Shuffle maintains the same length" (List.length lst) (List.length shuffled_lst);
  assert_bool "Shuffle is random" (lst <> shuffled_lst)

let test_split_nth _ =
  let lst = [1; 2; 3; 4; 5] in
  let elem, rest = Utils.split_nth lst 2 in
  assert_equal ~msg:"Split element" 3 elem;
  assert_equal ~msg:"Rest of the list" [1; 2; 4; 5] rest

let suite =
  "Utils Tests" >::: [
    "test_shuffle" >:: test_shuffle;
    "test_split_nth" >:: test_split_nth;
  ]

let () =
  run_test_tt_main suite
