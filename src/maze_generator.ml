
open Maze  
open Utils

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
      (* Randomly order the directions *)
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
            let cell = Maze.get_cell maze x y in
            let neighbor = Maze.get_cell maze nx ny in
            (* Remove the wall between the current cell and the neighbor *)
            Maze.remove_wall maze cell neighbor;
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

    let add_frontier x y =
      visited.(x).(y) <- true;
      let cell = Maze.get_cell maze x y in
      let neighbors = Maze.get_neighbors maze cell in
      List.iter (fun (_, neighbor) ->
        if not visited.(neighbor.x).(neighbor.y) then
          frontier := (x, y, neighbor.x, neighbor.y) :: !frontier
      ) neighbors
    in

    (* Start from a random cell *)
    let x0 = Random.int width in
    let y0 = Random.int height in
    add_frontier x0 y0;

    while !frontier <> [] do
      (* Randomly select an edge from the frontier *)
      let idx = Random.int (List.length !frontier) in
      let (x1, y1, x2, y2) = List.nth !frontier idx in
      (* Remove the edge from the frontier *)
      frontier := Utils.list_remove !frontier idx;
      let visited1 = visited.(x1).(y1) in
      let visited2 = visited.(x2).(y2) in
      if visited1 <> visited2 then
        begin
          let cell1 = Maze.get_cell maze x1 y1 in
          let cell2 = Maze.get_cell maze x2 y2 in
          Maze.remove_wall maze cell1 cell2;
          if not visited.(x2).(y2) then
            add_frontier x2 y2
        end
    done
end

(* Kruskal's Algorithm Maze Generator *)
module KruskalGenerator : MAZE_GENERATOR = struct
  let generate maze =
    let width = maze.width in
    let height = maze.height in
    let sets = UnionFind.create () in
    let edges = ref [] in

    (* Initialize sets for each cell *)
    for x = 0 to width - 1 do
      for y = 0 to height - 1 do
        let cell = Maze.get_cell maze x y in
        UnionFind.make_set sets cell
      done
    done;

    (* Create all possible walls (edges between cells) *)
    for x = 0 to width - 1 do
      for y = 0 to height - 1 do
        let cell = Maze.get_cell maze x y in
        let neighbors = Maze.get_neighbors maze cell in
        List.iter (fun (_, neighbor) ->
          if cell.x <= neighbor.x && cell.y <= neighbor.y then
            edges := (cell, neighbor) :: !edges
        ) neighbors
      done
    done;

    (* Shuffle the edges *)
    edges := Utils.shuffle !edges;

    (* Process each edge *)
    List.iter (fun (cell1, cell2) ->
      let set1 = UnionFind.find sets cell1 in
      let set2 = UnionFind.find sets cell2 in
      if set1 <> set2 then
        begin
          Maze.remove_wall maze cell1 cell2;
          UnionFind.union sets set1 set2
        end
    ) !edges
end

