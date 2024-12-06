open Maze
open Maze_generator
open Maze_solver
open Core

(* Command-line options type *)
type options = {
  width : int;
  height : int;
  generator : string;
  solver : string;
}

(* Default options *)
let default_options = {
  width = 10;
  height = 10;
  generator = "recursive";
  solver = "bfs";
}

(* Argument parsing with better error handling *)
let parse_args () =
  let args = Array.to_list (Sys.get_argv ()) |> List.tl_exn in
  let rec parse args options =
    match args with
    | [] -> options
    | "-w" :: value :: rest ->
        let width =
          try int_of_string value with Failure _ -> failwith "Invalid width value"
        in
        parse rest { options with width }
    | "-h" :: value :: rest ->
        let height =
          try int_of_string value with Failure _ -> failwith "Invalid height value"
        in
        parse rest { options with height }
    | "-g" :: value :: rest -> parse rest { options with generator = value }
    | "-s" :: value :: rest -> parse rest { options with solver = value }
    | flag :: [] -> failwith ("Missing value for flag: " ^ flag)
    | unknown :: _ -> failwith ("Unknown argument: " ^ unknown)
  in
  parse args default_options

(* Select the appropriate generator module *)
let select_generator generator_name =
  match generator_name with
  | "recursive" -> (module RecursiveBacktrackerGenerator : MAZE_GENERATOR)
  | "prim" -> (module PrimGenerator : MAZE_GENERATOR)
  | "kruskal" -> (module KruskalGenerator : MAZE_GENERATOR)
  | _ ->
      Printf.printf "Invalid generator. Choose from: recursive, prim, kruskal.\n";
      exit 1

(* Select the appropriate solver module *)
let select_solver solver_name =
  match solver_name with
  | "bfs" -> (module BFSSolver : MAZE_SOLVER)
  | "astar" -> (module AStarSolver : MAZE_SOLVER)
  | _ ->
      Printf.printf "Invalid solver. Choose from: bfs, astar.\n";
      exit 1

(* Main function *)
let () =
  (* Parse arguments *)
  let {width; height; generator; solver} = parse_args () in

  (* Create the maze *)
  let maze = Maze.create width height in

  (* Select generator and generate the maze *)
  let module Generator = (val select_generator generator : MAZE_GENERATOR) in
  let maze = Generator.generate maze in

  (* Display the generated maze *)
  Printf.printf "Generated Maze:\n";
  Maze.display maze;

  (* Select solver and solve the maze *)
  let module Solver = (val select_solver solver : MAZE_SOLVER) in
  let solution = Solver.solve maze in

  (* Display the solved maze with the solution path *)
  Printf.printf "Solved Maze:\n";
  let maze_with_solution = Utils.overlay_solution maze solution in
  Maze.display maze_with_solution
