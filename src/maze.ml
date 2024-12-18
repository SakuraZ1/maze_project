open Core
open Cell


module type MAZE = sig
  type direction =  Cell.direction

  type cell = Cell.t
  

  type maze = {
    width : int;  
    height : int;  
    grid : cell list list; 
  }

  val get_width : maze -> int
  val get_height : maze -> int
  val get_grid : maze -> cell list list
  val with_grid : maze -> cell list list -> maze

  val create : int -> int -> maze
  val display : maze -> unit
  val display_with_solution : maze -> (int * int) list -> unit
  val get_cell : maze -> int -> int -> cell
  val set_cell : maze -> cell -> maze
  val in_bounds : maze -> int -> int -> bool
  val get_neighbors : maze -> cell -> (direction * cell) list
  val remove_wall : maze -> cell -> cell -> maze
  val get_passable_neighbors : maze -> cell -> cell list
  val initialize_cells : maze -> maze
  val test_find_wall : (direction * bool) list -> direction -> bool
end


(** The module that provides the MAZE interface. *)
module Maze : MAZE = struct
  (* Provide the required types *)
  type direction = Cell.direction

  type cell = Cell.t

  type maze = {
    width : int;
    height : int;
    grid : cell list list;
  }


let get_width maze = maze.width
let get_height maze = maze.height
let get_grid maze = maze.grid
let with_grid maze new_grid = { maze with grid = new_grid }

(** [create width height] initializes a new maze with the given dimensions and walls on all sides. *)
let create width height =
  let grid =
    List.init height ~f:(fun y ->
      List.init width ~f:(fun x ->
        {
          x = x;
          y = y;
          walls = [ (North, true); (South, true); (East, true); (West, true) ];
        }
      )
    )
  in
  { width; height; grid }

let find_wall walls direction =
  match List.Assoc.find walls direction ~equal:Poly.equal with
  | Some value -> value
  | None -> false


(** [display maze] prints the maze to the console in ASCII art without using for loops. *)
let display maze =
  let horizontal_walls =
    List.map maze.grid ~f:(fun row ->
      List.map row ~f:(fun cell ->
        if List.Assoc.find_exn cell.walls South ~equal:Poly.equal then "+---" else "+   "
      )
    )
  in
  let vertical_walls =
    List.map maze.grid ~f:(fun row ->
      List.map row ~f:(fun cell -> 
        if List.Assoc.find_exn cell.walls West ~equal:Poly.equal then "|   " else "    "
      )
    )
  in
  let top_boundary =
    String.concat ~sep:"" (List.init maze.width ~f:(fun _ -> "+---")) ^ "+\n"
  in
  let maze_body =
    List.fold2_exn horizontal_walls vertical_walls ~init:top_boundary ~f:(fun acc horiz vert ->
      let horizontal_line = String.concat ~sep:"" horiz ^ "+\n" in
      let vertical_line = String.concat ~sep:"" vert ^ "|\n" in
      acc ^ vertical_line ^ horizontal_line
    )
  in
  print_string maze_body





    let display_with_solution maze solution =
      let is_on_solution x y =
        List.exists solution ~f:(fun (sx, sy) -> sx = x && sy = y)
      in
    
      (* Top boundary of the maze *)
      let top_boundary =
        String.concat ~sep:"" (List.init maze.width ~f:(fun _ -> "+---")) ^ "+\n"
      in
    
      (* Generate the maze rows *)
      let maze_string =
        List.fold maze.grid ~init:top_boundary ~f:(fun acc row ->
          let horizontal_line =
            List.fold row ~init:"" ~f:(fun line cell ->
              let north_wall =
                if List.Assoc.find_exn cell.walls North ~equal:Poly.equal then "+---"
                else "+   "
              in
              line ^ north_wall
            )
          in
          let horizontal_line = horizontal_line ^ "+\n" in
    
          let vertical_line =
            List.fold row ~init:"" ~f:(fun line cell ->
              let content =
                if is_on_solution cell.x cell.y then " * " else "   "
              in
              let west_wall =
                if List.Assoc.find_exn cell.walls West ~equal:Poly.equal then "|" else " "
              in
              line ^ west_wall ^ content
            )
          in
          let vertical_line = vertical_line ^ "|\n" in
    
          acc ^ vertical_line ^ horizontal_line
        )
      in
    
      (* Print the maze *)
      print_string maze_string
    
  

(** [get_cell maze x y] retrieves the cell at position (x, y) in [maze]. *)
let get_cell maze x y =
  if x >= 0 && x < maze.width && y >= 0 && y < maze.height then
    List.nth_exn (List.nth_exn maze.grid y) x
  else
    invalid_arg "get_cell: coordinates out of bounds"

(** [set_cell maze cell] returns a new maze with [cell] updated in the grid. *)
let set_cell maze cell =
  let new_grid =
    List.mapi maze.grid ~f:(fun j row ->
      if j = cell.y then
        List.mapi row ~f:(fun i c -> if i = cell.x then cell else c)
      else
        row
    )
  in
  { maze with grid = new_grid }

(** [in_bounds maze x y] checks if (x, y) is within the bounds of [maze]. *)
let in_bounds maze x y =
  x >= 0 && x < maze.width && y >= 0 && y < maze.height

(** [get_neighbors maze cell] returns a list of neighboring cells adjacent to [cell]. *)
let get_neighbors maze cell =
  let directions = [
    (North, (cell.x, cell.y - 1));
    (South, (cell.x, cell.y + 1));
    (East,  (cell.x + 1, cell.y));
    (West,  (cell.x - 1, cell.y));
  ] in
  List.fold_left directions ~init:[] ~f:(fun acc (dir, (nx, ny)) ->
    if in_bounds maze nx ny then
      (dir, get_cell maze nx ny) :: acc
    else
      acc
  )



(** [remove_wall maze cell1 cell2] returns a new maze with the wall between [cell1] and [cell2] removed. *)(* 打印单元格的墙壁状态 *)
(*
let string_of_direction dir =
  match dir with
  | Cell.North -> "North"
  | Cell.South -> "South"
  | Cell.East -> "East"
  | Cell.West -> "West"
let print_wall_status cell =
  let directions = [Cell.North; Cell.South; Cell.East; Cell.West] in
  Printf.printf "Wall status for cell (%d, %d) before removal:\n" cell.x cell.y;
  List.iter directions ~f:(fun dir ->
    (* 检查当前方向的墙是否存在 *)
    let wall_exists = List.exists cell.walls ~f:(fun (d, exists) -> Poly.equal d dir && exists) in
    Printf.printf "Direction %s: %b\n" 
      (string_of_direction dir) wall_exists
  )
*)
let remove_wall maze cell1 cell2 =
  let dx = cell2.x - cell1.x in
  let dy = cell2.y - cell1.y in

  (* 检查单元格是否相邻 *)
  if abs dx > 1 || abs dy > 1 then
    invalid_arg "remove_wall: cells are not adjacent";

  let dir_to_neighbor, dir_to_cell =
    if dx = 1 && dy = 0 then (Cell.East, Cell.West)
    else if dx = -1 && dy = 0 then (Cell.West, Cell.East)
    else if dx = 0 && dy = 1 then (Cell.South, Cell.North)
    else if dx = 0 && dy = -1 then (Cell.North, Cell.South)
    else
      invalid_arg "remove_wall: cells are not adjacent"
  in

  let update_walls walls target_dir =
    List.map walls ~f:(fun (dir, exists) -> 
      if Poly.equal dir target_dir then (dir, false) else (dir, exists)
    )
  in

  let cell1' = { cell1 with walls = update_walls cell1.walls dir_to_neighbor } in
  let cell2' = { cell2 with walls = update_walls cell2.walls dir_to_cell } in

  let maze = set_cell maze cell1' in
  let maze = set_cell maze cell2' in

  maze



(** [get_passable_neighbors maze cell] returns a list of neighboring cells that are accessible from [cell] (i.e., no wall between them). *)
let get_passable_neighbors maze cell =
  List.fold_left cell.walls ~init:[] ~f:(fun acc (dir, exists) ->
    if not exists then
      let nx, ny = match dir with
        | North -> (cell.x, cell.y - 1)
        | South -> (cell.x, cell.y + 1)
        | East  -> (cell.x + 1, cell.y)
        | West  -> (cell.x - 1, cell.y)
      in
      if in_bounds maze nx ny then
        (get_cell maze nx ny) :: acc
      else
        acc
    else
      acc
  )

(** [initialize_cells maze] returns a new maze with all cells reinitialized with walls on all sides. *)
let initialize_cells maze =
  let new_grid =
    List.init maze.height ~f:(fun y ->
      List.init maze.width ~f:(fun x ->
        {
          x = x;
          y = y;
          walls = [ (North, true); (South, true); (East, true); (West, true) ];
        }
      )
    )
  in
  { maze with grid = new_grid }

  let test_find_wall walls direction = find_wall walls direction
end

