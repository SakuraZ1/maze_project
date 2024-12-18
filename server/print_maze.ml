open Core
open Maze
open Maze_generator
open Maze_solver

let () =
  (* Set the maze dimensions *)
  let width = 3 in
  let height = 3 in

  (* Choose a generator: "recursive", "prim", or "kruskal" *)
  let generator_name = "recursive" in

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
    let open Cell in
    let start_cell = { start_cell with walls = List.map start_cell.walls ~f:(fun (dir, exists) ->
      if Poly.equal dir West then (dir, false) else (dir, exists)) } in
    
    let end_cell = { end_cell with walls = List.map end_cell.walls ~f:(fun (dir, exists) ->
      if Poly.equal dir East then (dir, false) else (dir, exists)) } in
    
    let generated_maze = Maze.set_cell (Maze.set_cell generated_maze start_cell) end_cell in
  
  (* Print the generated maze *)
  Printf.printf "Generated Maze (%s algorithm):\n" generator_name;
  Maze.display generated_maze;

 (* Solve the maze using BFS Solver *)
 let solution = BFSSolver.solve generated_maze  in 

 (* Overlay the solution path onto the maze *)
 (*let maze_with_solution = Utils.overlay_solution generated_maze solution in

(* Print the solved maze with solution path *)
Printf.printf "\nSolved Maze (with BFS path):\n";
Maze.display_with_solution maze_with_solution solution;*)
 (* Print the solution path *)
 Printf.printf "\nSolution Path (BFS):\n";
 List.iter solution ~f:(fun (x, y) ->
   Printf.printf "  -> (%d, %d)\n" x y
 );

 (* Overlay the solution path onto the maze *)
 let maze_with_solution = Utils.overlay_solution generated_maze solution in

 (* Print the solved maze with solution path *)
 Printf.printf "\nSolved Maze (with BFS path):\n";
Maze.display_with_solution maze_with_solution solution;
