open Core
open Maze
open Maze_generator
open Maze_solver
open Cell

(* Display the maze with a solution overlay while keeping the original maze intact. *)
(** [display_with_solution maze solution] displays the maze with the solution path overlaid,
    without modifying the original maze structure. *)
    let display_with_solution maze solution =
      let is_on_solution x y =
        List.exists solution ~f:(fun (sx, sy) -> sx = x && sy = y)
      in
    
      (* Generate horizontal walls *)
      let horizontal_walls =
        List.map (Maze.get_grid maze) ~f:(fun row ->
          List.map row ~f:(fun cell ->
            if List.Assoc.find_exn cell.walls North ~equal:Poly.equal then "+---"
            else "+   "
          )
        )
      in
    
      (* Generate vertical walls with solution path overlaid *)
      let vertical_walls =
        List.map (Maze.get_grid maze) ~f:(fun row ->
          List.map row ~f:(fun cell ->
            let cell_content =
              if is_on_solution cell.x cell.y then " * " else "   "
            in
            if List.Assoc.find_exn cell.walls West ~equal:Poly.equal then "|" ^ cell_content
            else " " ^ cell_content
          )
        )
      in
    
      (* Build the top boundary *)
      let top_boundary =
        String.concat ~sep:"" (List.init (Maze.get_width maze) ~f:(fun _ -> "+---")) ^ "+\n"
      in
    
      (* Combine horizontal and vertical walls row by row *)
      let maze_string =
        List.fold2_exn horizontal_walls vertical_walls ~init:top_boundary ~f:(fun acc horiz vert ->
          let horizontal_line = String.concat ~sep:"" horiz ^ "+\n" in
          let vertical_line = String.concat ~sep:"" vert ^ "|\n" in
          acc ^ vertical_line ^ horizontal_line
        )
      in
    
      print_string maze_string
    


(* Helper function to normalize user input (ignore case and whitespace) *)
let normalize_input input =
  String.strip input |> String.lowercase

(* Function to read and validate positive integer inputs *)
let read_positive_int prompt =
  Printf.printf "%s" prompt;
  Out_channel.flush stdout;
  try
    let input = In_channel.input_line_exn In_channel.stdin in
    let value = Int.of_string input in
    if value > 0 then value
    else failwith "Dimension must be a positive integer."
  with
  | Failure _ -> failwith "Invalid input: Please enter a positive integer, Or you will not see a nice Maze!"
  | exn -> raise exn

(* Function to read and validate algorithm selection *)
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

Printf.printf "\nSolved Maze (with %s path):\n" (String.uppercase solver_name);
display_with_solution generated_maze solution;
