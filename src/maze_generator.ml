open Maze
open Utils
open Core

module type MAZE_GENERATOR = sig

  type cell 
  type maze

  val generate : Maze.maze -> unit
end

module MakeGenerator (M : MAZE_GENERATOR) : MAZE_GENERATOR = struct
  type maze = M.maze
  type cell = M.cell
  let generate = M.generate
end

(* Recursive Backtracking Maze Generator *)
module RecursiveBacktrackerGenerator : MAZE_GENERATOR = struct
  type maze = Maze.maze 
  type cell = Maze.cell

  let generate maze =
    let width = Maze.get_width maze in
    let height = Maze.get_height maze in
    let visited = Array.make_matrix ~dimx:width ~dimy:height false in

    let rec carve_passages_from maze x y =
      visited.(x).(y) <- true;
      let directions = Utils.shuffle [Maze.North; Maze.South; Maze.East; Maze.West] in
      List.fold directions ~init:maze ~f:(fun maze direction ->
        let nx, ny = match direction with
          | North -> (x, y - 1)
          | South -> (x, y + 1)
          | East  -> (x + 1, y)
          | West  -> (x - 1, y)
        in
        if Maze.in_bounds maze nx ny && not visited.(nx).(ny) then
          let cell1 = Maze.get_cell maze x y in
          let cell2 = Maze.get_cell maze nx ny in
          let maze = Maze.remove_wall maze cell1 cell2 in
          carve_passages_from maze nx ny
        else
          maze
      )
    in
    let _ = carve_passages_from maze 0 0 in
    ()
end

(* Prim's Algorithm Maze Generator *)
module PrimGenerator : MAZE_GENERATOR = struct
  type maze = Maze.maze 
  type cell = Maze.cell

  let generate maze =
    let width = Maze.get_width maze in
    let height = Maze.get_height maze in
    let visited = Array.make_matrix ~dimx:width ~dimy:height false in
    let frontier = ref [] in

    let add_frontier x y =
      visited.(x).(y) <- true;
      frontier := Utils.add_neighbors_to_frontier maze visited x y !frontier
    in

    let rec process_frontier () =
      match !frontier with
      | [] -> ()
      | _ ->
        let idx = Random.int (List.length !frontier) in
        let (x1, y1, x2, y2), updated_frontier = Utils.split_nth !frontier idx in
        frontier := updated_frontier;
        let visited1 = visited.(x1).(y1) in
        let visited2 = visited.(x2).(y2) in
        if Bool.(visited1 <> visited2) then
          begin
            (*Maze.remove_wall maze x1 y1 x2 y2;*)
            let cell1 = Maze.get_cell maze x1 y1 in
            let cell2 = Maze.get_cell maze x2 y2 in
            let _ = Maze.remove_wall maze cell1 cell2 in

            if not visited.(x2).(y2) then add_frontier x2 y2
          end;
        process_frontier ()
    in

    let x0 = Random.int width in
    let y0 = Random.int height in
    add_frontier x0 y0;
    process_frontier ()
end

(* Kruskal's Algorithm Maze Generator *)
module KruskalGenerator : MAZE_GENERATOR = struct
  type cell = Maze.cell
  type maze = Maze.maze

  (** Represents the state of connected components using component identifiers. *)
  type component_state = {
    components : (cell, int) Map.M(Maze.Cell).t;  (* Maps each cell to its component ID *)
    next_id : int;                                 (* The next available component ID *)
  }

  (** Initializes the component state with each cell in its own component. *)
  let initialize_components maze =
    let width = get_width maze in
    let height = get_height maze in
    let cells =
      Array.to_list (get_grid maze)
      |> List.concat
    in
    let initial_map =
      List.foldi cells ~init:(Map.empty (module Maze.Cell), 0) ~f:(fun i (map, next_id) cell ->
        (Map.set map ~key:cell ~data:i, next_id + 1)
      )
    in
    {
      components = fst initial_map;
      next_id = snd initial_map;
    }

  (** Finds the component ID of a given cell. *)
  let find_component state cell =
    match Map.find state.components cell with
    | Some id -> id
    | None -> failwith "KruskalGenerator.find_component: Cell not found in component state."

  (** Merges two components by updating component IDs. *)
  let merge_components state id1 id2 =
    if id1 = id2 then
      state  (* Already in the same component *)
    else
      let updated_components =
        Map.map state.components ~f:(fun id ->
          if id = id2 then id1 else id
        )
      in
      { components = updated_components; next_id = state.next_id }

  (** Shuffles a list using the Fisher-Yates algorithm. *)
  let shuffle_list lst =
    let array = Array.of_list lst in
    let len = Array.length array in
    for i = len - 1 downto 1 do
      let j = Random.int (i + 1) in
      Array.swap array i j
    done;
    Array.to_list array

  (** Processes each wall, removing it if it connects two different components. *)
  let rec process_walls maze walls state =
    match walls with
    | [] -> maze
    | (cell1, cell2) :: rest ->
      let id1 = find_component state cell1 in
      let id2 = find_component state cell2 in
      if id1 <> id2 then
        let maze' = remove_wall maze cell1 cell2 in
        let state' = merge_components state id1 id2 in
        process_walls maze' rest state'
      else
        process_walls maze rest state

  (** [generate maze] generates a new maze using Kruskal's algorithm without Union-Find. *)
  let generate maze =
    let state = initialize_components maze in
    let walls = ref [] in

    (* Collect all possible walls between East and South neighbors to avoid duplicates *)
    let rec collect_walls x y =
      if x < get_width maze then
        if y < get_height maze then
          let cell = get_cell maze x y in
          let neighbors = get_neighbors maze cell in
          let valid_neighbors =
            List.filter neighbors ~f:(fun (dir, neighbor) ->
              match dir with
              | East when neighbor.x > x -> true
              | South when neighbor.y > y -> true
              | _ -> false
            )
          in
          let new_walls = List.map valid_neighbors ~f:(fun (_, neighbor) -> (cell, neighbor)) in
          walls := List.append new_walls !walls;
          collect_walls x (y + 1)
        else
          collect_walls (x + 1) 0
      else
        ()
    in

    collect_walls 0 0;

    (* Shuffle the walls to ensure randomness *)
    let shuffled_walls = shuffle_list !walls in

    (* Process each wall and build the maze *)
    process_walls maze shuffled_walls state
end




  (*type maze = Maze.maze 
  type cell = Maze.cell


  let generate (maze: Maze.maze) =
    let width = maze.width in
    let height = maze.height in
    let uf = Union_find.create () in  (* Initialize Union-Find structure *)
    let edges = ref [] in

    (* Initialize Union-Find with all cells *)
    let rec initialize_cells x y =
      if x < width then
        if y < height then
          let cell = Maze.get_cell maze x y in
          Union_find.union uf cell; 
          initialize_cells x (y + 1)
        else
          initialize_cells (x + 1) 0
      else
        ()
    in

    (* Create all possible edges between adjacent cells *)
    let rec create_edges x y =
      if x < width then
        if y < height then
          let cell = Maze.get_cell maze x y in
          let neighbors = Maze.get_neighbors maze cell in
          (* Filter edges to prevent duplicates by ensuring cell.x < neighbor.x or cell.y < neighbor.y *)
          let valid_edges = List.filter_map neighbors ~f:(fun neighbor ->
            if (cell.x < neighbor.x) || (cell.x = neighbor.x && cell.y < neighbor.y) then
              Some (cell, neighbor)
            else
              None
          ) in
          edges := List.append valid_edges !edges;
          create_edges x (y + 1)
        else
          create_edges (x + 1) 0
      else
        ()
    in

    (* Process edges in randomized order to build the maze *)
    let rec process_edges remaining_edges =
      match remaining_edges with
      | [] -> ()
      | (cell1, cell2) :: rest ->
        let set1 = Union_find.find_set uf cell1 in
        let set2 = Union_find.find_set uf cell2 in
        if set1 <> set2 then begin
          Maze.remove_wall maze cell1 cell2;  (* Remove wall between cell1 and cell2 *)
          Union_find.union uf cell1 cell2;   (* Merge the sets *)
        end;
        process_edges rest
    in

    (* Initialize cells and edges *)
    initialize_cells 0 0;
    create_edges 0 0;
    edges := Utils.shuffle !edges;  (* Shuffle edges for randomness *)
    process_edges !edges
end
    (*let sets = Union_find.create () in
    let edges = ref [] in

    let rec initialize_cells x y =
      if x < width then
        if y < height then
          let cell = Maze.get_cell maze x y in
          Union_find. sets cell;
          initialize_cells x (y + 1)
        else
          initialize_cells (x + 1) 0
    in

    let rec create_edges x y =
      if x < width then
        if y < height then
          let cell = Maze.get_cell maze x y in
          let neighbors = Maze.get_neighbors maze cell in
          let valid_edges = List.filter (fun (_, neighbor) ->
            cell.x <= neighbor.x && cell.y <= neighbor.y) neighbors in
          edges := List.append valid_edges !edges;
          create_edges x (y + 1)
        else
          create_edges (x + 1) 0
    in

    let rec process_edges remaining_edges =
      match remaining_edges with
      | [] -> ()
      | (cell1, cell2) :: rest ->
        let set1 = Union_find.find sets cell1 in
        let set2 = Union_find.find sets cell2 in
        if set1 <> set2 then
          begin
            Maze.remove_wall maze cell1.x cell1.y cell2.x cell2.y;
            Union_find.union sets set1 set2
          end;
        process_edges rest
    in

    initialize_cells 0 0;
    create_edges 0 0;
    edges := Utils.shuffle !edges;
    process_edges !edges
end*)

