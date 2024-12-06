open OUnit2
open Cell

let test_create_cell _ =
  let cell = Cell.create 1 2 in
  assert_equal ~msg:"Cell X coordinate" 1 cell.x;
  assert_equal ~msg:"Cell Y coordinate" 2 cell.y;
  assert_equal ~msg:"Cell walls initialized correctly" [(North, true); (South, true); (East, true); (West, true)] cell.walls

let test_set_wall _ =
  let cell = Cell.create 0 0 in
  let cell = Cell.set_wall cell North false in
  assert_equal ~msg:"North wall should be removed" false (List.assoc North cell.walls)

let suite =
  "Cell Tests" >::: [
    "test_create_cell" >:: test_create_cell;
    "test_set_wall" >:: test_set_wall;
  ]

let () =
  run_test_tt_main suite
