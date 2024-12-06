
open Maze

(* Utility functions for handling matrices and lists in an immutable way *)


(* Shuffles a list in random order *)
val shuffle : 'a list -> 'a list

val make_matrix_as_list : int -> int -> 'a -> 'a list list

(* Immutable set operation for a matrix at (x, y) with a new value *)
val set_matrix : 'a list list -> int -> int -> 'a -> 'a list list

(* Retrieves a value from a matrix at (x, y) *)
val get_matrix : 'a list list -> int -> int -> 'a

(* Adds neighboring cells to the frontier in Prim's algorithm for maze generation *)
val add_neighbors_to_frontier :
  Maze.maze -> bool list list -> int -> int -> (int * int * int * int) list -> (int * int * int * int) list

(* Removes the nth element from a list, returning the removed element and the updated list *)
val split_nth : 'a list -> int -> 'a * 'a list

(** [overlay_solution maze solution] returns a new maze with the solution path [solution] overlaid
    onto it, for visualization purposes. *)
val overlay_solution : Maze.maze -> (int * int) list -> Maze.maze
