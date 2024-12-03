open Maze
open Utils
open Maze_generator
open Maze_solver
open Core

(* Parse command-line arguments for maze dimensions and algorithms *)
type options = {
  width : int;
  height : int;
  generator : string;
  solver : string;
}

let default_options = {
  width = 10;
  height = 10;
  generator = "recursive";
  solver = "bfs";
}

let rec parse_args_rec args options =
  match args with
  | [] -> options
  | "-w" :: value :: rest ->
      (try
         let width = int_of_string value in
         let options = { options with width } in
         parse_args_rec rest options
       with Failure _ ->
         failwith "Invalid width value")
  | "-h" :: value :: rest ->
      (try
         let height = int_of_string value in
         let options = { options with height } in
         parse_args_rec rest options
       with Failure _ ->
         failwith "Invalid height value")
  | "-g" :: value :: rest ->
      let options = { options with generator = value } in
      parse_args_rec rest options
  | "-s" :: value :: rest ->
      let options = { options with solver = value } in
      parse_args_rec rest options
  | flag :: [] ->
      failwith ("Missing value for flag: " ^ flag)
  | unknown :: _ ->
      failwith ("Unknown argument: " ^ unknown)

let parse_args () =
  let args = Array.to_list Sys.argv |> List.tl in
  parse_args_rec args default_options



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

