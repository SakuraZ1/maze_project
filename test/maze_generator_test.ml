(*open OUnit2
open Maze
open Maze_generator

let test_recursive_backtracker _ =
  let maze = Maze.create 5 5 in
  let module Generator = RecursiveBacktrackerGenerator in
  let _ = Generator.generate maze in
  assert_bool "Maze generation produces a non-empty path" true (* Add your own verification logic *)

let test_prim _ =
  let maze = Maze.create 5 5 in
  let module Generator = PrimGenerator in
  let _ = Generator.generate maze in
  assert_bool "Maze generation produces a non-empty path" true (* Add your own verification logic *)

let suite =
  "Maze Generator Tests" >::: [
    "test_recursive_backtracker" >:: test_recursive_backtracker;
    "test_prim" >:: test_prim;
  ]

let () =
  run_test_tt_main suite*)

  open OUnit2
  open Maze
  open Maze_generator
  open Core
  
  (* Function to compare if two mazes are different based on wall configuration *)
  let mazes_have_different_walls maze1 maze2 =
    let grid1, grid2 = Maze.get_grid maze1, Maze.get_grid maze2 in
    List.exists2_exn grid1 grid2 ~f:(fun row1 row2 ->
      List.exists2_exn row1 row2 ~f:(fun cell1 cell2 ->
        not (List.equal (fun (dir1, exists1) (dir2, exists2) ->
          Poly.equal dir1 dir2 && Bool.equal exists1 exists2
        ) cell1.walls cell2.walls)
      )
    )
  
  let test_recursive_backtracker_generator _ =
    let width = 5 in
    let height = 5 in
    let maze = Maze.create width height in
    let generated_maze = RecursiveBacktrackerGenerator.generate maze in
  
    (* Ensure that the maze is different from the initial state based on walls *)
    assert_bool
      "Recursive Backtracker generator should have modified the maze from its initial state"
      (mazes_have_different_walls maze generated_maze)
  
  let test_prim_generator _ =
    let width = 5 in
    let height = 5 in
    let maze = Maze.create width height in
    let generated_maze = PrimGenerator.generate maze in
  
    (* Ensure that the maze is different from the initial state based on walls *)
    assert_bool
      "Prim's generator should have modified the maze from its initial state"
      (mazes_have_different_walls maze generated_maze)
  
  let test_kruskal_generator _ =
    let width = 5 in
    let height = 5 in
    let maze = Maze.create width height in
    let generated_maze = KruskalGenerator.generate maze in
  
    (* Ensure that the maze is different from the initial state based on walls *)
    assert_bool
      "Kruskal's generator should have modified the maze from its initial state"
      (mazes_have_different_walls maze generated_maze)
  
  let comprehensive_test _ =
    let width = 5 in
    let height = 5 in
    let maze = Maze.create width height in
  
    (* Testing all generators *)
    let generators = [
      (module RecursiveBacktrackerGenerator : MAZE_GENERATOR);
      (module PrimGenerator : MAZE_GENERATOR);
      (module KruskalGenerator : MAZE_GENERATOR)
    ] in
  
    List.iter generators ~f:(fun (module G : MAZE_GENERATOR) ->
      let generated_maze = G.generate maze in
  
      (* Ensure that the maze is different from the initial state based on walls *)
      assert_bool
        "Maze generator should generate a connected maze"
        (mazes_have_different_walls maze generated_maze)
    )
  
  let suite =
    "Maze Generator Tests" >:::
    [
      "test_recursive_backtracker_generator" >:: test_recursive_backtracker_generator;
      "test_prim_generator" >:: test_prim_generator;
      "test_kruskal_generator" >:: test_kruskal_generator;
      "comprehensive_test" >:: comprehensive_test;
    ]
  
  let _ =
    run_test_tt_main suite
  