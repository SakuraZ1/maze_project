open OUnit2
open Maze

let test_create_maze _ =
  let maze = Maze.create 3 3 in
  assert_equal ~msg:"Maze width" 3 (Maze.get_width maze);
  assert_equal ~msg:"Maze height" 3 (Maze.get_height maze);
  assert_bool "Grid is initialized correctly" (Array.length (Maze.get_grid maze) = 3)

let test_get_cell _ =
  let maze = Maze.create 3 3 in
  let cell = Maze.get_cell maze 1 1 in
  assert_equal ~msg:"Cell X coordinate" 1 cell.x;
  assert_equal ~msg:"Cell Y coordinate" 1 cell.y

let suite =
  "Maze Tests" >::: [
    "test_create_maze" >:: test_create_maze;
    "test_get_cell" >:: test_get_cell;
  ]

let () =
  run_test_tt_main suite
