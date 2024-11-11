
module type MAZE_GENERATOR = sig
  (* [generate maze] generates a maze using the implemented algorithm, modifying [maze] in place. *)
  val generate : Maze.maze -> unit
end

(* Functor to create a maze generator. *)
module MakeGenerator : functor (M : MAZE_GENERATOR) -> MAZE_GENERATOR

(* Recursive Backtracking Maze Generator *)
module RecursiveBacktrackerGenerator : MAZE_GENERATOR

(* Prim's Algorithm Maze Generator *)
module PrimGenerator : MAZE_GENERATOR

(* Kruskal's Algorithm Maze Generator *)
module KruskalGenerator : MAZE_GENERATOR

