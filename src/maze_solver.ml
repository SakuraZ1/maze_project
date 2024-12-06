open Core
open Maze
open Cell

module type MAZE_SOLVER = sig
  type direction = Cell.direction
  type cell = Cell.t
  type maze = Maze.maze

  (* Solves the maze and returns a list of (x, y) positions representing the path. *)
  val solve : Maze.maze -> (int * int) list
end

(* BFS Maze Solver *)
module BFSSolver : MAZE_SOLVER = struct
  type direction = Cell.direction
  type cell = Cell.t
  type maze = Maze.maze

  module Path_map = struct
    type t = (int * int) [@@deriving compare, sexp]
  end

  module Pathmap = Map.Make(Path_map)

  module Visitset = Set.Make(Path_map)



  let solve maze =
    let width = Maze.get_width maze in
    let height = Maze.get_height maze in

    let rec bfs visited prev queue =
      match queue with
      | [] -> []
      | (x, y) :: rest ->
        (* Goal reached, reconstruct the path *)
        if x = width - 1 && y = height - 1 then
          let rec reconstruct_path x y acc =
            match Map.find prev (x, y) with
            | None -> (x, y) :: acc
            | Some (px, py) -> reconstruct_path px py ((x, y) :: acc)
          in
          reconstruct_path x y []
        else if Set.mem visited (x, y) then
          bfs visited prev rest
        else
          let visited = Set.add visited (x, y) in
          let cell = Maze.get_cell maze x y in
          let neighbors = Maze.get_passable_neighbors maze cell in
          let next_queue, next_prev =
            List.fold neighbors ~init:(rest, prev) ~f:(fun (q, p) neighbor ->
              let nx, ny = neighbor.x, neighbor.y in
              if not (Set.mem visited (nx, ny)) then
                ((nx, ny) :: q, Map.set p ~key:(nx, ny) ~data:(x, y))
              else
                (q, p)
            )
          in
          bfs visited next_prev next_queue
    in
    bfs Visitset.empty Pathmap.empty [(0, 0)]
end

(* A* Search Algorithm Maze Solver *)
module AStarSolver : MAZE_SOLVER = struct
  type direction = Cell.direction
  type cell = Cell.t
  type maze = Maze.maze

  module Score_map = struct
    type t = (int * int) [@@deriving compare, sexp]
  end

  module Scoremap = Map.Make(Score_map)
  let solve maze =
    let width = Maze.get_width maze in
    let height = Maze.get_height maze in

    let start, goal = (0, 0), (width - 1, height - 1) in

    let heuristic (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) in

    let rec insert_by_priority queue element priority =
      match queue with
      | [] -> [(element, priority)]
      | (e, p) :: rest ->
        if priority <= p then
          (element, priority) :: queue
        else
          (e, p) :: insert_by_priority rest element priority in

    let rec a_star_search (open_set: ((int * int) * int) list) g_score f_score came_from : (int * int) list =
      match open_set with
      | [] -> []
      | (current, _) :: rest_open_set ->
        let (current_x, current_y) = current in
        if current_x = width - 1 && current_y = height - 1 then
          let rec reconstruct_path current acc =
            match Map.find came_from current with
            | None -> current :: acc
            | Some prev -> reconstruct_path prev (current :: acc)
          in
          reconstruct_path current []
        else
          let (x, y) = current in
          let cell = Maze.get_cell maze x y in
          let neighbors = Maze.get_passable_neighbors maze cell in
          let updated_data =
            List.fold neighbors ~init:(rest_open_set, g_score, f_score, came_from)
              ~f:(fun (os, g, f, c) neighbor ->
                let nx, ny = neighbor.x, neighbor.y in
                let neighbor_pos = (nx, ny) in
                let tentative_g_score =
                  (Map.find g current |> Option.value ~default:Int.max_value) + 1
                in
                let neighbor_g_score =
                  Map.find g neighbor_pos |> Option.value ~default:Int.max_value
                in
                if tentative_g_score < neighbor_g_score then
                  let g = Map.set g ~key:neighbor_pos ~data:tentative_g_score in
                  let f = Map.set f ~key:neighbor_pos ~data:(tentative_g_score + heuristic neighbor_pos goal) in
                  let c = Map.set c ~key:neighbor_pos ~data:current in
                  let os = insert_by_priority os neighbor_pos (tentative_g_score + heuristic neighbor_pos goal) in
                  (os, g, f, c)
                else
                  (os, g, f, c)
              )
          in
          let new_open_set, new_g_score, new_f_score, new_came_from = updated_data in
          a_star_search new_open_set new_g_score new_f_score new_came_from
    in
    let initial_g_score = Map.set Scoremap.empty ~key:start ~data:0 in
    let initial_f_score = Map.set Scoremap.empty ~key:start ~data:(heuristic start goal) in
    a_star_search [(start, heuristic start goal)] initial_g_score initial_f_score Scoremap.empty
end


(* Functor to create a solver *)
module MakeSolver (M : MAZE_SOLVER) : MAZE_SOLVER = struct
  include M
  let solve = M.solve
end
