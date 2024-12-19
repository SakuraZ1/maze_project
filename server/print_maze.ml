open Core
open Maze
open Maze_generator
open Maze_solver
open Cell


(* [display_with_solution] displays the maze and overlays the solution path.
   It prints each cell of the maze along with visual indicators for the path.

   - [maze] : The maze to display.
   - [solution] : A list of (x, y) positions representing the solution path.
*)

let display_with_solution maze solution =
  (* [is_on_solution x y] checks whether a given (x, y) cell is part of the solution path. *)
        let is_on_solution x y =
          List.exists solution ~f:(fun (sx, sy) -> sx = x && sy = y)
        in
      
        let width = Maze.get_width maze in
      
         (* [top_boundary] creates the top boundary of the maze as "+---" repeated. *)
        let top_boundary =
          String.concat ~sep:"" (List.init width ~f:(fun _ -> "+---")) ^ "+\n"
        in
      
        (* [maze_string] generates the entire maze with the solution overlay. *)
        let maze_string =
          List.foldi (Maze.get_grid maze) ~init:top_boundary ~f:(fun _ acc row ->

            (* [horizontal_line] represents the horizontal boundaries for each row. *)
            let horizontal_line =
              String.concat ~sep:"" (List.map row ~f:(fun _ -> "+---")) ^ "+\n"
            in

            (* [vertical_line] represents the vertical boundaries and cell contents. *)
            let vertical_line =
              String.concat ~sep:"" (List.map row ~f:(fun cell ->
                if is_on_solution cell.x cell.y then "| ■ " (* Solid square path *)
                else "|   ")) ^ "|\n"
            in

            (* Append the vertical and horizontal lines to the accumulator string. *)
            acc ^ vertical_line ^ horizontal_line
          )
        in
        print_string maze_string
      

(* Helper function to normalize user input (ignore case and whitespace) *)
let normalize_input input =
  String.strip input |> String.lowercase


(* [read_positive_int prompt] safely reads a positive integer from the user.
   It prompts until a valid integer in the range [3, 19] is provided. *)
let read_positive_int prompt =
  Printf.printf "%s" prompt;
  Out_channel.flush stdout;
  try
    let input = In_channel.input_line_exn In_channel.stdin in
    let value = Int.of_string input in
    if value > 2 && value < 20 then value
    else failwith "Invalid input: Please enter a positive integer and be in > 2 and < 20, Or you will not see a nice Maze!"
  with
  | Failure _ -> failwith "Invalid input: Please enter a positive integer and be in > 2 and < 20, Or you will not see a nice Maze!"
  | exn -> raise exn


(* [read_algorithm prompt valid_algorithms] reads and validates a user's choice of algorithm.
   - [prompt]: The prompt displayed to the user.
   - [valid_algorithms]: A list of valid algorithm names to compare against.
*)
let read_algorithm prompt valid_algorithms =
  Printf.printf "%s" prompt;
  Out_channel.flush stdout;
  let rec validate_input () =
    match In_channel.input_line In_channel.stdin with
    | Some input ->
      (let normalized = normalize_input input in
      let check_algo = List.find valid_algorithms ~f:(fun algo -> String.equal algo normalized) in
      match check_algo with
       | Some algo -> algo
       | None ->
        print_endline "Invalid choice. Please select a valid algorithm.\n";
        validate_input ())
    | None -> failwith "No input received."
  in
  validate_input ()


  
let () =
  (* Safely read maze dimensions *)
  let width =
    try read_positive_int "Enter maze width (positive integer): "
    with exn ->
      Printf.printf "Error: %s\n" (Exn.to_string exn);
      exit 1
  in

  let height =
    try read_positive_int "Enter maze height (positive integer): "
    with exn ->
      Printf.printf "Error: %s\n" (Exn.to_string exn);
      exit 1
  in

  (* Read and validate maze generation algorithm *)
  let generator_name =
    read_algorithm
      "Choose maze generation algorithm (Kruskal, Prim, Recursive): "
      ["kruskal"; "prim"; "recursive"]
  in

  (* Read and validate maze solving algorithm *)
  let solver_name =
    read_algorithm
      "Choose maze solving algorithm (BFS, AStar): "
      ["bfs"; "astar"]
  in

  (* Create an empty maze *)
  let maze = Maze.create width height in

  (* Select the generator and generate the maze *)
  let module Generator =
    (val (match generator_name with
          | "recursive" -> (module RecursiveBacktrackerGenerator : MAZE_GENERATOR)
          | "prim" -> (module PrimGenerator : MAZE_GENERATOR)
          | "kruskal" -> (module KruskalGenerator : MAZE_GENERATOR)
          | _ -> failwith "Invalid generator"))
  in
  let generated_maze = Generator.generate maze in

  (* Remove walls for entry and exit *)
  let start_cell = Maze.get_cell generated_maze 0 0 in
  let end_cell = Maze.get_cell generated_maze (width - 1) (height - 1) in

  let start_cell = { start_cell with walls = 
    List.map start_cell.walls ~f:(fun (dir, exists) -> 
      if Poly.equal dir West then (dir, false) else (dir, exists)) 
  } in

  let end_cell = { end_cell with walls = 
    List.map end_cell.walls ~f:(fun (dir, exists) -> 
      if Poly.equal dir East || Poly.equal dir South then (dir, false) else (dir, exists)) 
  } in

  let generated_maze = Maze.set_cell (Maze.set_cell generated_maze start_cell) end_cell in

  (* Print the generated maze *)
  Printf.printf "\nGenerated Maze (%s algorithm):\n" generator_name;
  Maze.display generated_maze;

  (* Select the solver algorithm *)
  let module Solver =
    (val (match solver_name with
          | "bfs" -> (module BFSSolver : MAZE_SOLVER)
          | "astar" -> (module AStarSolver : MAZE_SOLVER)
          | _ -> failwith "Invalid solver"))
  in

  (* Solve the maze *)
  let solution = Solver.solve generated_maze in

  (* Overlay the solution path and print the solved maze *)

Printf.printf "\nSolution (with %s path):\n" (String.uppercase solver_name);
display_with_solution generated_maze solution;
