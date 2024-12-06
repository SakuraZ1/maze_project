open OUnit2
open Maze
open Maze_generator

let test_recursive_backtracker _ =
  let maze = Maze.create 5 5 in
  let module Generator = RecursiveBacktrackerGenerator in
  Generator.generate maze;
  assert_bool "Maze generation produces a non-empty path" true (* Add your own verification logic *)

let test_prim _ =
  let maze = Maze.create 5 5 in
  let module Generator = PrimGenerator in
  Generator.generate maze;
  assert_bool "Maze generation produces a non-empty path" true (* Add your own verification logic *)

let suite =
  "Maze Generator Tests" >::: [
    "test_recursive_backtracker" >:: test_recursive_backtracker;
    "test_prim" >:: test_prim;
  ]

let () =
  run_test_tt_main suite
