(* test/maze_test.ml *)

open OUnit2
open Maze

let test_create_maze _ =
  let maze = Maze.create 3 3 in
  assert_equal (Maze.get_width maze) 3;
  assert_equal (Maze.get_height maze) 3;
  assert_bool "Grid is initialized correctly" (List.length (Maze.get_grid maze) = 3);
  assert_bool "Each row has correct number of cells" (
    List.for_all (fun row -> List.length row = 3) (Maze.get_grid maze)
  )

let test_get_cell _ =
  let maze = Maze.create 3 3 in
  let cell = Maze.get_cell maze 1 1 in
  assert_equal cell.x 1;
  assert_equal cell.y 1

let suite =
  "Maze Test Suite" >::: [
    "test_create_maze" >:: test_create_maze;
    "test_get_cell" >:: test_get_cell;
  ]

let () =
  run_test_tt_main suite

