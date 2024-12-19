open Maze
open Utils
open Core
open Cell

module type MAZE_GENERATOR = sig
  type cell 
  type maze

  val generate : Maze.maze -> Maze.maze
end

module MakeGenerator (M : MAZE_GENERATOR) : MAZE_GENERATOR = struct
  include M
  type maze = M.maze
  type cell = M.cell
  let generate = M.generate
end

(* Recursive Backtracking Maze Generator *)
module RecursiveBacktrackerGenerator : MAZE_GENERATOR = struct
  type maze = Maze.maze 
  type cell = Maze.cell

  let rec generate maze =
    let maze = mark_entrance maze in

    let width = Maze.get_width maze in
    let height = Maze.get_height maze in
    let visited = Utils.make_matrix_as_list width height false in

   
    let rec carve_passages_from maze visited x y =
      if List.nth_exn (List.nth_exn visited y) x then maze, visited
      else
        let visited = List.mapi ~f:(fun j row ->
          if j = y then
            List.mapi ~f:(fun i v -> if i = x then true else v) row
          else
            row
        ) visited in
        let directions = Utils.shuffle [Cell.North; Cell.South; Cell.East; Cell.West] in
        List.fold directions ~init:(maze, visited) ~f:(fun (maze, visited) direction ->
          let nx, ny = match direction with
            | North -> (x, y - 1)
            | South -> (x, y + 1)
            | East  -> (x + 1, y)
            | West  -> (x - 1, y)
          in
          if Maze.in_bounds maze nx ny && not (List.nth_exn (List.nth_exn visited ny) nx) then
            let cell1 = Maze.get_cell maze x y in
            let cell2 = Maze.get_cell maze nx ny in
            let maze = Maze.remove_wall maze cell1 cell2 in
            carve_passages_from maze visited nx ny
          else
            (maze, visited)
        )
    in
    let maze, _ = carve_passages_from maze visited 0 0 in

    
    if path_exists maze (0, 0) (width - 1, height - 1) then
      begin
        maze
      end
    else
      generate maze 
end



(* Prim's Algorithm Maze Generator *)
module PrimGenerator : MAZE_GENERATOR = struct
  type maze = Maze.maze
  type cell = Maze.cell

  let rec generate maze =
    let width = Maze.get_width maze in
    let height = Maze.get_height maze in

    let visited = make_matrix_as_list width height false in

    let add_neighbors_to_frontier frontier x y visited =
      let visited = List.mapi ~f:(fun j row ->
        if j = y then
          List.mapi ~f:(fun i v -> if i = x then true else v) row
        else
          row
      ) visited in
      Utils.add_neighbors_to_frontier maze visited x y frontier, visited
    in

    let rec process_frontier maze visited frontier =
      match frontier with
      | [] -> maze  
      | (x1, y1, x2, y2) :: rest -> 
        if List.nth_exn (List.nth_exn visited y2) x2 then
          process_frontier maze visited rest
        else
          let cell1 = Maze.get_cell maze x1 y1 in
          let cell2 = Maze.get_cell maze x2 y2 in
          let maze = Maze.remove_wall maze cell1 cell2 in 
          let frontier, visited = add_neighbors_to_frontier rest x2 y2 visited in
          process_frontier maze visited frontier
    in

    let x0 = Random.int width in
    let y0 = Random.int height in
    let frontier, visited = add_neighbors_to_frontier [] x0 y0 visited in
    let maze = process_frontier maze visited frontier in

    (* Ensure there is a path between start (0, 0) and end (width - 1, height - 1) *)
    if path_exists maze (0, 0) (width - 1, height - 1) then
      maze
    else
      generate maze (* Retry if no path exists *)
end

(* Kruskal's Algorithm Maze Generator *)
module KruskalGenerator : MAZE_GENERATOR = struct
  type cell = Maze.cell
  type maze = Maze.maze

  module Component_map = struct
    type t = Cell.t [@@deriving compare, sexp]
  end

  module C_map = Map.Make(Component_map)

  let initialize_components maze =
    let cells = Maze.get_grid maze |> List.concat in
    List.foldi cells ~init:C_map.empty ~f:(fun i acc cell ->
      Map.set acc ~key:cell ~data:i
    )

  let find_component components cell =
    Map.find_exn components cell

  let merge_components components id1 id2 =
    Map.map components ~f:(fun id -> if id = id2 then id1 else id)

  let collect_walls maze =
    let width = Maze.get_width maze in
    let height = Maze.get_height maze in
    let rec collect x y acc =
      if x < width then
        if y < height then
          let cell = Maze.get_cell maze x y in
          let neighbors = Maze.get_neighbors maze cell in
          let valid_neighbors = List.filter neighbors ~f:(fun (dir, neighbor) ->
            match dir with
            | Cell.East when neighbor.x > x -> true
            | Cell.South when neighbor.y > y -> true
            | _ -> false
          ) in
          let new_walls = List.map valid_neighbors ~f:(fun (_, neighbor) -> (cell, neighbor)) in
          collect x (y + 1) (new_walls @ acc)
        else
          collect (x + 1) 0 acc
      else
        acc
    in
    collect 0 0 []

  let rec process_walls maze walls components =
    match walls with
    | [] -> maze
    | (cell1, cell2) :: rest ->
      let id1 = find_component components cell1 in
      let id2 = find_component components cell2 in
      if id1 <> id2 then
        let maze = Maze.remove_wall maze cell1 cell2 in
        let components = merge_components components id1 id2 in
        process_walls maze rest components
      else
        process_walls maze rest components

  let rec generate maze =
    let components = initialize_components maze in
    let walls = collect_walls maze |> Utils.shuffle in
    let maze = process_walls maze walls components in

    (* Ensure there is a path between start (0, 0) and end (width - 1, height - 1) *)
    if path_exists maze (0, 0) (Maze.get_width maze - 1, Maze.get_height maze - 1) then
      maze
    else
      generate maze (* Retry if no path exists *)
end
