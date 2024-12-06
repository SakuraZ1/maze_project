

open OUnit2
open Maze
open Maze_solver
open Maze_generator

let test_bfs_solver _ =
  (* Generate a simple connected maze using the RecursiveBacktrackerGenerator *)
  let maze = Maze.create 5 5 in
  let module Generator = RecursiveBacktrackerGenerator in
  let maze = Generator.generate maze in
  let solution = BFSSolver.solve maze in

  (* Assert that the solution is non-empty and ends at the goal (bottom-right corner) *)
  assert_bool "Solution is non-empty" (List.length solution > 0);
  match List.rev solution with
  | [] -> assert_failure "Solution should not be empty"
  | end_cell :: _ -> 
      let (ex, ey) = end_cell in
      assert_equal (maze.width - 1, maze.height - 1) (ex, ey)

let test_astar_solver _ =
  (* Generate a simple connected maze using the RecursiveBacktrackerGenerator *)
  let maze = Maze.create 5 5 in
  let module Generator = RecursiveBacktrackerGenerator in
  let maze = Generator.generate maze in
  let solution = AStarSolver.solve maze in

  (* Assert that the solution is non-empty and ends at the goal (bottom-right corner) *)
  assert_bool "Solution is non-empty" (List.length solution > 0);
  match List.rev solution with
  | [] -> assert_failure "Solution should not be empty"
  | end_cell :: _ ->
      let (ex, ey) = end_cell in
      assert_equal (maze.width - 1, maze.height - 1) (ex, ey)

let suite =
  "Maze Solver Tests" >:::
  [
    "test_bfs_solver" >:: test_bfs_solver;
    "test_astar_solver" >:: test_astar_solver;
  ]

let () =
  run_test_tt_main suite
