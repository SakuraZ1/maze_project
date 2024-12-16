open Dream
open Core
open Maze_generator
open Maze_solver
open Utils
open Cell
open Maze
open Lwt
open Lwt.Infix


(* Utility functions for direction serialization/deserialization *)
module Direction = struct
  let to_string = function
    | North -> "North"
    | South -> "South"
    | East -> "East"
    | West -> "West"

  let of_string = function
    | "North" -> North
    | "South" -> South
    | "East" -> East
    | "West" -> West
    | _ -> failwith "Invalid direction"
end

(* Utility functions to serialize and deserialize a maze *)
module MazeSerialization = struct
  let to_json (maze: Maze.maze) =
    let grid_to_json = List.map maze.grid ~f:(fun row ->
      Printf.sprintf "[%s]" (String.concat ~sep:"," (List.map row ~f:(fun cell ->
        Printf.sprintf
          "{\"x\":%d,\"y\":%d,\"walls\":[%s]}"
          cell.Cell.x
          cell.Cell.y
          (String.concat ~sep:"," (List.map cell.Cell.walls ~f:(fun (dir, has_wall) ->
            Printf.sprintf
              "{\"direction\":\"%s\",\"has_wall\":%b}"
              (Direction.to_string dir)
              has_wall
          )))
      ))))
    in
    Printf.sprintf
      "{\"width\":%d,\"height\":%d,\"grid\":[%s]}"
      maze.width
      maze.height
      (String.concat ~sep:"," grid_to_json)

let of_json json =
  let extract_value key json =
    let key_pattern = Printf.sprintf "\"%s\":" key in
    match String.substr_index json ~pattern:key_pattern with
    | None -> failwith (Printf.sprintf "Key '%s' not found in JSON" key)
    | Some start ->
      let value_start = start + String.length key_pattern in
      let value_end =
        Option.value
          ~default:(String.length json)
          (String.lfindi_from json ~pos:value_start ~f:(fun _ c -> c = ',' || c = '}'))
      in
      String.sub json ~pos:value_start ~len:(value_end - value_start)
  in
  let width = Int.of_string (extract_value "width" json) in
  let height = Int.of_string (extract_value "height" json) in
  let grid_start = String.index_exn json '[' + 1 in
  let grid_end = String.rindex_exn json ']' in
  let grid_json = String.sub json ~pos:grid_start ~len:(grid_end - grid_start) in
  let grid =
    String.split grid_json ~on:']'
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:(fun row_json ->
         String.split row_json ~on:'}'
         |> List.filter ~f:(Fn.non String.is_empty)
         |> List.map ~f:(fun cell_json ->
              let x = Int.of_string (extract_value "x" cell_json) in
              let y = Int.of_string (extract_value "y" cell_json) in
              let walls_start = String.index_exn cell_json '[' + 1 in
              let walls_end = String.index_exn cell_json ']' in
              let walls_json = String.sub cell_json ~pos:walls_start ~len:(walls_end - walls_start) in
              let walls =
                String.split walls_json ~on:'}'
                |> List.filter ~f:(Fn.non String.is_empty)
                |> List.map ~f:(fun wall_json ->
                     let dir = Direction.of_string (extract_value "direction" wall_json) in
                     let has_wall = Bool.of_string (extract_value "has_wall" wall_json) in
                     (dir, has_wall))
              in
              { Cell.x; y; walls }))
  in
  { Maze.width; height; grid }


let generate_maze generator_type =
  match generator_type with
  | "recursive_backtracker" -> RecursiveBacktrackerGenerator.generate { Maze.width = 10; height = 10; grid = [] }
  | "prim" -> PrimGenerator.generate { Maze.width = 10; height = 10; grid = [] }
  | "kruskal" -> KruskalGenerator.generate { Maze.width = 10; height = 10; grid = [] }
  | _ -> failwith "Invalid generator type"

let bfs_solution maze =
  let bfs_path = BFSSolver.solve maze in
  Printf.sprintf
    "{\"path\":[%s]}"
    (String.concat ~sep:"," (List.map bfs_path ~f:(fun (x, y) -> Printf.sprintf "[%d,%d]" x y)))

let astar_solution maze =
  let astar_path = AStarSolver.solve maze in
  Printf.sprintf
    "{\"path\":[%s]}"
    (String.concat ~sep:"," (List.map astar_path ~f:(fun (x, y) -> Printf.sprintf "[%d,%d]" x y)))

let generate_maze_handler req =
  match Dream.query req "type" with
  | None -> Dream.respond ~status:`Bad_Request "{\"error\":\"Generator type missing\"}"
  | Some generator_type ->
    let maze = generate_maze generator_type in
    let maze_data = MazeSerialization.to_json maze in
    Dream.respond maze_data

let bfs_solution_handler req =
  match%lwt Dream.body req with
  | "" -> Dream.respond ~status:`Bad_Request "{\"error\":\"Maze data missing\"}"
  | body ->
    let maze = MazeSerialization.of_json body in
    let response = bfs_solution maze in
    Dream.respond response

let astar_solution_handler req =
  match%lwt Dream.body req with
  | "" -> Dream.respond ~status:`Bad_Request "{\"error\":\"Maze data missing\"}"
  | body ->
    let maze = MazeSerialization.of_json body in
    let response = astar_solution maze in
    Dream.respond response
let homepage_handler _req =
  Lwt.return
    (Dream.html
       {|
       <!DOCTYPE html>
       <html lang="en">
       <head>
         <meta charset="UTF-8">
         <meta name="viewport" content="width=device-width, initial-scale=1.0">
         <title>Maze Solver</title>
       </head>
       <body>
         <div id="root"></div>
         <script src="/static/frontend_maze_solver.bs.js"></script>
       </body>
       </html>
       |})

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
       Dream.get "/generate" generate_maze_handler;
       Dream.post "/solve/bfs" bfs_solution_handler;
       Dream.post "/solve/astar" astar_solution_handler;
       Dream.get "/static/:path" (Dream.static "./src");
       Dream.get "/" homepage_handler;
     ]
  @@ Dream.not_found


