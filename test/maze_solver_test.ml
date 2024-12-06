open OUnit2
open Maze
open Maze_solver

let test_bfs_solver _ =
  let maze = Maze.create 3 3 in
  let module Solver = BFSSolver in
  let solution = Solver.solve maze in
  assert_bool "Solution is non-empty" (List.length solution > 0)

let test_astar_solver _ =
  let maze = Maze.create 3 3 in
  let module Solver = AStarSolver in
  let solution = Solver.solve maze in
  assert_bool "Solution is non-empty" (List.length solution > 0)

let suite =
  "Maze Solver Tests" >::: [
    "test_bfs_solver" >:: test_bfs_solver;
    "test_astar_solver" >:: test_astar_solver;
  ]

let () =
  run_test_tt_main suite
