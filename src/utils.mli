
open Maze

(** Utility functions for handling matrices, lists, and additional maze operations 
    in an immutable and functional way. *)

(** [shuffle lst] returns a new list with the elements of [lst] in random order.
    - Parameters:
      - [lst]: The input list to shuffle.
    - Returns: A new list containing the same elements as [lst] but in random order.
    - Example:
      [shuffle [1; 2; 3]] might return [2; 1; 3].
*)
val shuffle : 'a list -> 'a list

(** [make_matrix_as_list rows cols init_value] creates a 2D list matrix initialized
    with the given value.
    - Parameters:
      - [rows]: Number of rows in the matrix.
      - [cols]: Number of columns in the matrix.
      - [init_value]: The value to initialize every cell of the matrix.
    - Returns: A 2D list (list of lists) representing the matrix.
    - Example:
      [make_matrix_as_list 2 3 0] returns [[0; 0; 0]; [0; 0; 0]].
*)
val make_matrix_as_list : int -> int -> 'a -> 'a list list

(** [set_matrix matrix x y value] returns a new matrix with the value at position (x, y) updated
    to the specified [value].
    - Parameters:
      - [matrix]: The input 2D list matrix.
      - [x]: The column index (0-based).
      - [y]: The row index (0-based).
      - [value]: The new value to set at position (x, y).
    - Returns: A new matrix with the updated value at (x, y).
    - Example:
      [set_matrix [[1; 2]; [3; 4]] 1 0 9] returns [[1; 9]; [3; 4]].
*)
val set_matrix : 'a list list -> int -> int -> 'a -> 'a list list

(** [get_matrix matrix x y] retrieves the value at position (x, y) in the given matrix.
    - Parameters:
      - [matrix]: The input 2D list matrix.
      - [x]: The column index (0-based).
      - [y]: The row index (0-based).
    - Returns: The value at position (x, y) in the matrix.
    - Raises: 
      - [Invalid_argument] if the indices are out of bounds.
    - Example:
      [get_matrix [[1; 2]; [3; 4]] 1 0] returns [2].
*)
val get_matrix : 'a list list -> int -> int -> 'a

(** [add_neighbors_to_frontier maze visited x y frontier] updates the frontier by adding
    unvisited neighboring cells of the cell at position (x, y) in the maze.
    - Parameters:
      - [maze]: The maze being generated.
      - [visited]: A 2D boolean matrix marking visited positions.
      - [x]: The column index of the current cell.
      - [y]: The row index of the current cell.
      - [frontier]: The current list of frontier edges (potential neighbors).
    - Returns: An updated frontier list with neighboring cells added.
    - Example:
      Adds neighbors like [(x1, y1, x2, y2)] where (x1, y1) is the current cell and 
      (x2, y2) is the neighboring cell.
*)
val add_neighbors_to_frontier :
  Maze.maze -> bool list list -> int -> int -> (int * int * int * int) list -> (int * int * int * int) list

(** [split_nth lst n] removes and returns the nth element of the list [lst].
    - Parameters:
      - [lst]: The input list.
      - [n]: The index (0-based) of the element to remove.
    - Returns: A tuple [(element, updated_list)] where:
      - [element]: The removed element.
      - [updated_list]: The list after removing the nth element.
    - Raises:
      - [Failure] if [n] is out of bounds.
    - Example:
      [split_nth [1; 2; 3; 4] 2] returns (3, [1; 2; 4]).
*)
val split_nth : 'a list -> int -> 'a * 'a list

(** [overlay_solution maze solution] overlays the solution path onto the maze for visualization.
    It marks the cells in the solution path to indicate the traversal.
    - Parameters:
      - [maze]: The input maze to overlay the solution on.
      - [solution]: A list of (x, y) coordinates representing the solution path.
    - Returns: A new maze with the solution path visually marked.
    - Example:
      A maze with a solution path might display special symbols or markings 
      where the solution passes.
*)
val overlay_solution : Maze.maze -> (int * int) list -> Maze.maze

(** [path_exists maze (start_x, start_y) (end_x, end_y)] checks if a valid path exists between 
    the start and end positions in the maze.
    - Parameters:
      - [maze]: The maze in which to check for a path.
      - [(start_x, start_y)]: The starting cell coordinates.
      - [(end_x, end_y)]: The target cell coordinates.
    - Returns: [true] if a path exists, [false] otherwise.
    - Example:
      Verifies if a solution can be found from entry to exit in the maze.
*)
val path_exists : Maze.maze -> (int * int) -> (int * int) -> bool

(** [mark_entrance maze] modifies the maze to create a visible entrance at (0, 0).
    It removes the necessary walls to indicate the maze entry.
    - Parameters:
      - [maze]: The maze to mark the entrance in.
    - Returns: A new maze with the entrance modified.
    - Example:
      The west wall of the top-left cell will be removed to indicate an open entrance.
*)
val mark_entrance : Maze.maze -> Maze.maze
