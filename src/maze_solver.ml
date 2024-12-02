open Core
open Maze
open Utils

(* BFS Maze Solver *)
module BFSSolver : MAZE_SOLVER = struct
  let solve maze =
    let width, height = maze.width, maze.height in
    let visited = Array.make_matrix width height false in
    let prev = Array.make_matrix width height None in

    let rec bfs queue =
      match queue with
      | [] -> []
      | (x, y) :: rest ->
        if x = width - 1 && y = height - 1 then
          (* Goal reached, reconstruct the path *)
          let rec reconstruct_path x y acc =
            match prev.(x).(y) with
            | None -> (x, y) :: acc
            | Some (px, py) -> reconstruct_path px py ((x, y) :: acc)
          in
          reconstruct_path (width - 1) (height - 1) []
        else if visited.(x).(y) then
          bfs rest
        else
          let cell = Maze.get_cell maze x y in
          visited.(x).(y) <- true;
          let neighbors = Maze.get_passable_neighbors maze cell in
          let next_queue =
            List.fold_left
              (fun acc neighbor ->
                let nx, ny = neighbor.x, neighbor.y in
                if not visited.(nx).(ny) then (
                  prev.(nx).(ny) <- Some (x, y);
                  (nx, ny) :: acc
                ) else acc)
              rest neighbors
          in
          bfs next_queue
    in
    bfs [(0, 0)]
end

(* A* Search Algorithm Maze Solver *)
module AStarSolver : MAZE_SOLVER = struct
  let solve maze =
    let width, height = maze.width, maze.height in
    let open_set = PriorityQueue.empty in
    let came_from = Hashtbl.create (width * height) in
    let g_score = Hashtbl.create (width * height) in
    let f_score = Hashtbl.create (width * height) in

    let start, goal = (0, 0), (width - 1, height - 1) in
    let heuristic (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) in

    Hashtbl.add g_score start 0;
    Hashtbl.add f_score start (heuristic start goal);

    let rec a_star_search open_set =
      match PriorityQueue.extract_min open_set with
      | None -> [] (* No path found *)
      | Some (current, rest) ->
        if current = goal then
          (* Reconstruct the path *)
          let rec reconstruct_path current acc =
            if Hashtbl.mem came_from current then
              let prev = Hashtbl.find came_from current in
              reconstruct_path prev (current :: acc)
            else current :: acc
          in
          reconstruct_path current []
        else
          let (x, y) = current in
          let cell = Maze.get_cell maze x y in
          let neighbors = Maze.get_passable_neighbors maze cell in
          let updated_open_set =
            List.fold_left
              (fun acc neighbor ->
                let nx, ny = neighbor.x, neighbor.y in
                let neighbor_pos = (nx, ny) in
                let tentative_g_score =
                  (Hashtbl.find g_score current) + 1
                in
                let neighbor_g_score =
                  Hashtbl.find_opt g_score neighbor_pos
                  |> Option.value ~default:max_int
                in
                if tentative_g_score < neighbor_g_score then (
                  Hashtbl.replace came_from neighbor_pos current;
                  Hashtbl.replace g_score neighbor_pos tentative_g_score;
                  let f = tentative_g_score + heuristic neighbor_pos goal in
                  Hashtbl.replace f_score neighbor_pos f;
                  PriorityQueue.insert acc neighbor_pos f
                ) else acc)
              rest neighbors
          in
          a_star_search updated_open_set
    in
    a_star_search (PriorityQueue.insert open_set start (heuristic start goal))
end

(* Functor to create a solver *)
module MakeSolver (M : MAZE_SOLVER) : MAZE_SOLVER = struct
  let solve = M.solve
end
