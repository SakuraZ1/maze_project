open Maze
open Utils
open Generator
open Solver

(* Parse command-line arguments for maze dimensions and algorithms *)
let parse_args () =
  let width = ref 10 in
  let height = ref 10 in
  let generator = ref "recursive" in
  let solver = ref "bfs" in
  let specs = [
    ("-w", Arg.Set_int width, "Width of the maze (default: 10)");
    ("-h", Arg.Set_int height, "Height of the maze (default: 10)");
    ("-g", Arg.Set_string generator, "Maze generator: recursive, prim, or kruskal (default: recursive)");
    ("-s", Arg.Set_string solver, "Maze solver: bfs or astar (default: bfs)")
  ] in
  Arg.parse specs (fun _ -> ()) "Maze generator and solver";
  (!width, !height, !generator, !solver)

(* Select the appropriate generator module *)
let select_generator generator_name =
  match generator_name with
  | "recursive" -> (module RecursiveBacktrackerGenerator : MAZE_GENERATOR)
  | "prim" -> (module PrimGenerator : MAZE_GENERATOR)
  | "kruskal" -> (module KruskalGenerator : MAZE_GENERATOR)
  | _ -> failwith "Invalid generator. Choose from: recursive, prim, kruskal."

(* Select the appropriate solver module *)
let select_solver solver_name =
  match solver_name with
  | "bfs" -> (module BFSSolver : MAZE_SOLVER)
  | "astar" -> (module AStarSolver : MAZE_SOLVER)
  | _ -> failwith "Invalid solver. Choose from: bfs, astar."

(* Main function *)
let () =
  (* Parse arguments *)
  let width, height, generator_name, solver_name = parse_args () in

  (* Create the maze *)
  let maze = Maze.create width height in

  (* Select generator and generate the maze *)
  let module Generator = (val select_generator generator_name : MAZE_GENERATOR) in
  Generator.generate maze;

  (* Display the generated maze *)
  Printf.printf "Generated Maze:\n";
  Maze.display maze;

  (* Select solver and solve the maze *)
  let module Solver = (val select_solver solver_name : MAZE_SOLVER) in
  let solution = Solver.solve maze in

  (* Display the solved maze with the solution path *)
  Printf.printf "Solved Maze:\n";
  let maze_with_solution = Utils.overlay_solution maze solution in
  Maze.display maze_with_solution

