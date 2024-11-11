
open Maze  
open Utils  


(* BFS Maze Solver *)
module BFSSolver : MAZE_SOLVER = struct
  let solve maze =
    let width = maze.width in
    let height = maze.height in
    let visited = Array.make_matrix width height false in
    let prev = Array.make_matrix width height None in
    let queue = Queue.create () in

    (* Starting position *)
    let x0, y0 = 0, 0 in  (* Assuming the start is at (0, 0) *)
    visited.(x0).(y0) <- true;
    Queue.add (x0, y0) queue;

    let found = ref false in

    while not (Queue.is_empty queue) && not !found do
      let x, y = Queue.take queue in
      let cell = Maze.get_cell maze x y in

      if x = maze.width - 1 && y = maze.height - 1 then
        found := true
      else
        let neighbors = Maze.get_passable_neighbors maze cell in
        List.iter (fun neighbor ->
          let nx = neighbor.x in
          let ny = neighbor.y in
          if not visited.(nx).(ny) then
            begin
              visited.(nx).(ny) <- true;
              prev.(nx).(ny) <- Some (x, y);
              Queue.add (nx, ny) queue;
            end
        ) neighbors
    done;

    if not !found then
      []  (* No path found *)
    else
      (* Reconstruct the path *)
      let rec reconstruct_path x y acc =
        match prev.(x).(y) with
        | None -> (x, y) :: acc
        | Some (px, py) -> reconstruct_path px py ((x, y) :: acc)
      in
      reconstruct_path (maze.width - 1) (maze.height - 1) []
end

(* A\* Search Algorithm Maze Solver *)
module AStarSolver : MAZE_SOLVER = struct
  let solve maze =
    let width = maze.width in
    let height = maze.height in
    let open_set = PriorityQueue.empty in
    let came_from = Hashtbl.create (width * height) in
    let g_score = Hashtbl.create (width * height) in
    let f_score = Hashtbl.create (width * height) in

    let start = (0, 0) in  (* Start position *)
    let goal = (width - 1, height - 1) in  (* Goal position *)

    let heuristic (x1, y1) (x2, y2) =
      abs (x1 - x2) + abs (y1 - y2)  (* Manhattan distance *)
    in

    Hashtbl.add g_score start 0;
    Hashtbl.add f_score start (heuristic start goal);
    let open_set = PriorityQueue.insert open_set start (Hashtbl.find f_score start) in

    let rec a_star_search open_set =
      if PriorityQueue.is_empty open_set then
        []  (* No path found *)
      else
        let (current, open_set) = match PriorityQueue.extract_min open_set with
          | Some (node, rest) -> (node, rest)
          | None -> failwith "Priority queue is empty"
        in
        let (x, y) = current in
        if current = goal then
          (* Reconstruct the path *)
          let rec reconstruct_path current acc =
            if Hashtbl.mem came_from current then
              let prev = Hashtbl.find came_from current in
              reconstruct_path prev (current :: acc)
            else
              current :: acc
          in
          reconstruct_path current []
        else
          let cell = Maze.get_cell maze x y in
          let neighbors = Maze.get_passable_neighbors maze cell in
          let open_set = List.fold_left (fun open_set neighbor ->
            let nx, ny = neighbor.x, neighbor.y in
            let neighbor_pos = (nx, ny) in
            let tentative_g_score = (Hashtbl.find g_score current) + 1 in
            let neighbor_g_score = Hashtbl.find_opt g_score neighbor_pos |> Option.value ~default:max_int in
            if tentative_g_score < neighbor_g_score then
              begin
                Hashtbl.replace came_from neighbor_pos current;
                Hashtbl.replace g_score neighbor_pos tentative_g_score;
                let f = tentative_g_score + heuristic neighbor_pos goal in
                Hashtbl.replace f_score neighbor_pos f;
                PriorityQueue.insert open_set neighbor_pos f
              end
            else
              open_set
          ) open_set neighbors in
          a_star_search open_set
    in
    a_star_search open_set
end

(* Functor to create a solver *)
module MakeSolver (M : MAZE_SOLVER) : MAZE_SOLVER = struct
  let solve = M.solve
end

