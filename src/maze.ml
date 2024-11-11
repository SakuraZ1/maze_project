


(* Define the direction type representing the four cardinal directions. *)
type direction = North | South | East | West

(* Define the cell type, representing each cell in the maze. *)
type cell = {
  x : int;  
  y : int;  
  mutable walls : (direction * bool) list; 
}

(* Define the maze type, representing the entire maze. *)
type maze = {
  width : int; 
  height : int; 
  grid : cell array array; 
}

(* initializes a new maze with the given dimensions and walls on all sides. *)
let create width height =
  let grid = Array.make_matrix width height {
    x = 0;
    y = 0;
    walls = [];
  } in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      grid.(x).(y) <- {
        x = x;
        y = y;
        walls = [ (North, true); (South, true); (East, true); (West, true) ];
      }
    done
  done;
  { width; height; grid }

(* retrieves the cell at position (x, y) in [maze]. *)
let get_cell maze x y =
  if x >= 0 && x < maze.width && y >= 0 && y < maze.height then
    maze.grid.(x).(y)
  else
    invalid_arg "get_cell: coordinates out of bounds"

(* updates the cell at the cell's coordinates in [maze] with the provided [cell]. *)
let set_cell maze cell =
  if cell.x >= 0 && cell.x < maze.width && cell.y >= 0 && cell.y < maze.height then
    maze.grid.(cell.x).(cell.y) <- cell
  else
    invalid_arg "set_cell: cell coordinates out of bounds"

(*checks if (x, y) is within the bounds of [maze]. *)
let in_bounds maze x y =
  x >= 0 && x < maze.width && y >= 0 && y < maze.height

(* returns a list of neighboring cells adjacent to [cell]. *)
let get_neighbors maze cell =
  let directions = [
    (North, (cell.x, cell.y - 1));
    (South, (cell.x, cell.y + 1));
    (East,  (cell.x + 1, cell.y));
    (West,  (cell.x - 1, cell.y));
  ] in
  List.fold_left (fun acc (dir, (nx, ny)) ->
    if in_bounds maze nx ny then
      (dir, get_cell maze nx ny) :: acc
    else
      acc
  ) [] directions

(* removes the wall between [cell1] and [cell2] in [maze]. *)
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
  (* Remove wall from cell1 to cell2 *)
  cell1.walls <- List.map (fun (dir, exists) ->
    if dir = dir_to_neighbor then (dir, false) else (dir, exists)
  ) cell1.walls;
  (* Remove wall from cell2 to cell1 *)
  cell2.walls <- List.map (fun (dir, exists) ->
    if dir = dir_to_cell then (dir, false) else (dir, exists)
  ) cell2.walls


(* returns a list of neighboring cells that are accessible from [cell] (i.e., no wall between them). *)
let get_passable_neighbors maze cell =
  List.fold_left (fun acc (dir, exists) ->
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
  ) [] cell.walls

(* prints the maze to the console in ASCII art. *)
let display maze =
  let horizontal_wall x y =
    let cell = maze.grid.(x).(y) in
    if List.assoc North cell.walls then "+---" else "+   "
  in
  let vertical_wall x y =
    let cell = maze.grid.(x).(y) in
    if List.assoc West cell.walls then "|   " else "    "
  in
  (* Print the top boundary *)
  for x = 0 to maze.width - 1 do
    print_string "+---"
  done;
  print_string "+\n";
  for y = 0 to maze.height - 1 do
    (* Print vertical walls and cells *)
    for x = 0 to maze.width - 1 do
      print_string (vertical_wall x y)
    done;
    print_string "|\n";
    (* Print horizontal walls *)
    for x = 0 to maze.width - 1 do
      print_string (horizontal_wall x y)
    done;
    print_string "+\n"
  done

(* initializes all cells in [maze] with walls on all sides. *)
let initialize_cells maze =
  for x = 0 to maze.width - 1 do
    for y = 0 to maze.height - 1 do
      maze.grid.(x).(y) <- {
        x = x;
        y = y;
        walls = [ (North, true); (South, true); (East, true); (West, true) ];
      }
    done
  done

(* the module that provides the MAZE interface. *)
module Maze : MAZE = struct
  type direction = direction
  type cell = cell
  type maze = maze

  let create = create
  let initialize_cells = initialize_cells
  let display = display
  let get_cell = get_cell
  let set_cell = set_cell
  let in_bounds = in_bounds
  let get_neighbors = get_neighbors
  let remove_wall = remove_wall
  let get_passable_neighbors = get_passable_neighbors
end
