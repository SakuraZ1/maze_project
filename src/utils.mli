open Maze
(* Utility functions for handling matrices and lists in an immutable way *)

(* Shuffles a list in random order *)
val shuffle : 'a list -> 'a list

(* Immutable set operation for a matrix at (x, y) with a new value *)
val set_matrix : 'a array array -> int -> int -> 'a -> 'a array array

(* Retrieves a value from a matrix at (x, y) *)
val get_matrix : 'a array array -> int -> int -> 'a

(* Adds neighboring cells to the frontier in Prim's algorithm for maze generation *)
val add_neighbors_to_frontier :
  Maze.maze -> bool array array -> int -> int -> (int * int * int * int) list -> (int * int * int * int) list

(* Removes the nth element from a list, returning the removed element and the updated list *)
val split_nth : 'a list -> int -> 'a * 'a list

(** [overlay_solution maze solution] returns a new maze with the solution path [solution] overlaid
    onto it, for visualization purposes. *)
val overlay_solution : Maze.maze -> (int * int) list -> Maze.maze
