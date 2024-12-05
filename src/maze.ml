open Core
open Cell


 (* Module Type Definition *)
module type MAZE = sig
  type direction =  North | South | East | West

  type cell = Cell.t(*{
    x : int;  
    y : int;  
    walls : (direction * bool) list;  
  }*)
  

  type maze = {
    width : int;  
    height : int;  
    grid : cell array array; 
  }

  val get_width : maze -> int
  val get_height : maze -> int
  val get_grid : maze -> cell array array
  val with_grid : maze -> cell array array -> maze

  val create : int -> int -> maze
  val display : maze -> unit
  val get_cell : maze -> int -> int -> cell
  val set_cell : maze -> cell -> maze
  val in_bounds : maze -> int -> int -> bool
  val get_neighbors : maze -> cell -> (direction * cell) list
  val remove_wall : maze -> cell -> cell -> maze
  val get_passable_neighbors : maze -> cell -> cell list
  val initialize_cells : maze -> maze
end


(** The module that provides the MAZE interface. *)
module Maze : MAZE = struct
  (* Provide the required types *)
  type direction = North | South | East | West

  type cell = {
    x : int;
    y : int;
    walls : (direction * bool) list;
  }

  type maze = {
    width : int;
    height : int;
    grid : cell array array;
  }


let get_width maze = maze.width
let get_height maze = maze.height
let get_grid maze = maze.grid
let with_grid maze new_grid = { maze with grid = new_grid }

(** [create width height] initializes a new maze with the given dimensions and walls on all sides. *)
let create width height =
  let grid =
    Array.init width ~f:(fun x ->
      Array.init height ~f:(fun y ->
        {
          x = x;
          y = y;
          walls = [ (North, true); (South, true); (East, true); (West, true) ];
        }
      )
    )
  in
  { width; height; grid }


(** [display maze] prints the maze to the console in ASCII art without using for loops. *)
let display maze =
  let horizontal_walls =
    Array.init maze.height ~f:(fun y ->
      Array.init maze.width ~f:(fun x ->
        let cell = maze.grid.(x).(y) in
        match List.Assoc.find cell.walls North ~equal:Poly.equal with
        | Some true -> "+---"
        | _ -> "+   "
      )
    )
  in
  let vertical_walls =
    Array.init maze.height ~f:(fun y ->
      Array.init maze.width ~f:(fun x ->
        let cell = maze.grid.(x).(y) in
        match List.Assoc.find cell.walls West ~equal:Poly.equal with
        | Some true -> "|   "
        | _ -> "    "
      )
    )
  in
  (* Build the top boundary *)
  let top_boundary =
    Array.fold ~init:"" ~f:(fun acc _ -> acc ^ "+---") (Array.create ~len:maze.width ()) ^ "+\n"
  in
  (* Build the maze string *)
  let maze_string =
    Array.foldi horizontal_walls ~init:top_boundary ~f:(fun y acc _ ->
      let horizontal_line = Array.fold horizontal_walls.(y) ~init:"" ~f:(^) ^ "+\n" in
      let vertical_line = Array.fold vertical_walls.(y) ~init:"" ~f:(^) ^ "|\n" in
      acc ^ vertical_line ^ horizontal_line
    )
  in
  print_string maze_string



(** [get_cell maze x y] retrieves the cell at position (x, y) in [maze]. *)
let get_cell maze x y =
  if x >= 0 && x < maze.width && y >= 0 && y < maze.height then
    maze.grid.(x).(y)
  else
    invalid_arg "get_cell: coordinates out of bounds"

(** [set_cell maze cell] returns a new maze with [cell] updated in the grid. *)
let set_cell maze cell =
  if cell.x >= 0 && cell.x < maze.width && cell.y >= 0 && cell.y < maze.height then
    let new_row = Array.copy maze.grid.(cell.x) in
    new_row.(cell.y) <- cell;
    let new_grid = Array.copy maze.grid in
    new_grid.(cell.x) <- new_row;
    { maze with grid = new_grid }
  else
    invalid_arg "set_cell: cell coordinates out of bounds"

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



(** [remove_wall maze cell1 cell2] returns a new maze with the wall between [cell1] and [cell2] removed. *)
let remove_wall maze cell1 cell2 =
  let dx = cell2.x - cell1.x in
  let dy = cell2.y - cell1.y in
  let dir_to_neighbor, dir_to_cell =
    if dx = 1 && dy = 0 then (East, West)
    else if dx = -1 && dy = 0 then (West, East)
    else if dx = 0 && dy = 1 then (South, North)
    else if dx = 0 && dy = -1 then (North, South)
    else
      invalid_arg "remove_wall: cells are not adjacent"
  in
  (* Update walls in cell1 *)
  let new_walls1 = List.map cell1.walls ~f:(fun (dir, exists) ->
    if Poly.equal dir dir_to_neighbor then (dir, false) else (dir, exists)
  ) in
  let cell1' = { cell1 with walls = new_walls1 } in
  (* Update walls in cell2 *)
  let new_walls2 = List.map cell2.walls ~f:(fun (dir, exists) ->
    if Poly.equal dir dir_to_cell then (dir, false) else (dir, exists)
  ) in
  let cell2' = { cell2 with walls = new_walls2 } in
  (* Update the maze with the new cells *)
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
    Array.init maze.width ~f:(fun x ->
      Array.init maze.height ~f:(fun y ->
        {
          x = x;
          y = y;
          walls = [ (North, true); (South, true); (East, true); (West, true) ];
        }
      )
    )
  in
  { maze with grid = new_grid }


end

