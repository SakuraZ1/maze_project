open Maze
open Utils
open Core

module type MAZE_GENERATOR = sig
  val generate : Maze.maze -> unit
end

module MakeGenerator (M : MAZE_GENERATOR) : MAZE_GENERATOR = struct
  let generate = M.generate
end

(* Recursive Backtracking Maze Generator *)
module RecursiveBacktrackerGenerator : MAZE_GENERATOR = struct
  let generate maze =
    let width = maze.width in
    let height = maze.height in
    let visited = Array.make_matrix width height false in

    let rec carve_passages_from x y =
      visited.(x).(y) <- true;
      let directions = Utils.shuffle [North; South; East; West] in
      List.iter (fun direction ->
        let nx, ny = match direction with
          | North -> (x, y - 1)
          | South -> (x, y + 1)
          | East  -> (x + 1, y)
          | West  -> (x - 1, y)
        in
        if Maze.in_bounds maze nx ny && not visited.(nx).(ny) then
          begin
            Maze.remove_wall maze x y nx ny;
            carve_passages_from nx ny
          end
      ) directions
    in
    carve_passages_from 0 0
end

(* Prim's Algorithm Maze Generator *)
module PrimGenerator : MAZE_GENERATOR = struct
  let generate maze =
    let width = maze.width in
    let height = maze.height in
    let visited = Array.make_matrix width height false in
    let frontier = ref [] in

    let rec add_frontier x y =
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
        if visited1 <> visited2 then
          begin
            Maze.remove_wall maze x1 y1 x2 y2;
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
  let generate maze =
    let width = maze.width in
    let height = maze.height in
    let sets = UnionFind.create () in
    let edges = ref [] in

    let rec initialize_cells x y =
      if x < width then
        if y < height then
          let cell = Maze.get_cell maze x y in
          UnionFind.make_set sets cell;
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
        let set1 = UnionFind.find sets cell1 in
        let set2 = UnionFind.find sets cell2 in
        if set1 <> set2 then
          begin
            Maze.remove_wall maze cell1.x cell1.y cell2.x cell2.y;
            UnionFind.union sets set1 set2
          end;
        process_edges rest
    in

    initialize_cells 0 0;
    create_edges 0 0;
    edges := Utils.shuffle !edges;
    process_edges !edges
end

