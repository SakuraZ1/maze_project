
module type MAZE = sig
  (* Directions a cell may have walls in. *)
  type direction = Cell.direction
  (* Represents a cell in the maze grid. 
     - x, y: coordinates in the maze.
     - walls: list of directions with a boolean for each indicating if a wall exists in that direction. *)
  type cell = Cell.t
  (* Represents the overall maze structure.
     - width, height: dimensions of the maze.
     - grid: 2D array of cells representing the maze grid. *)
  type maze = {
    width : int;
    height : int;
    grid : cell list list;
  }

   
  val get_width : maze -> int
  val get_height : maze -> int
  val get_grid : maze -> cell list list
  val with_grid : maze -> cell list list -> maze

  (* Creates a maze with given width and height, initializing cells with walls. *)
  val create : int -> int -> maze

  (* Displays the maze structure, showing walls and paths. Useful for debugging or visual output. *)
  val display : maze -> unit

  (* Retrieves the cell at specific coordinates (x, y) in the maze. *)
  val get_cell : maze -> int -> int -> cell

  (* Sets or updates a cell in the maze grid, returning the updated maze. *)
  val set_cell : maze -> cell -> maze

  (* Checks if given coordinates (x, y) are within the bounds of the maze. *)
  val in_bounds : maze -> int -> int -> bool

  (* Gets the neighbors of a cell along with the direction to each neighbor.
     Useful in generation and solving for navigating adjacent cells. *)
  val get_neighbors : maze -> cell -> (direction * cell) list

  (* Removes the wall between two adjacent cells, creating a passage between them. *)
  val remove_wall : maze -> cell -> cell -> maze

  (* Retrieves neighboring cells that are accessible (i.e., without walls blocking them),
     useful in maze-solving algorithms. *)
  val get_passable_neighbors : maze -> cell -> cell list

 (* Initializes cells in the maze, preparing them for use in generation or solving. *)
  val initialize_cells : maze -> maze
end


(* Expose the Maze module *)
module Maze : MAZE