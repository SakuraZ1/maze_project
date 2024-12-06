(* test/cell_test.ml *)

open OUnit2
open Cell

let test_create_cell _ =
  let cell = { x = 1; y = 2; walls = [(North, true); (South, true)] } in
  assert_equal cell.x 1;
  assert_equal cell.y 2;
  assert_equal (List.length cell.walls) 2

let suite =
  "Cell Test Suite" >::: [
    "test_create_cell" >:: test_create_cell;
  ]

let () =
  run_test_tt_main suite
