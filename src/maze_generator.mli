
open Maze

module type MAZE_GENERATOR = sig
  (* Generates a maze by modifying the given Maze.maze in-place. *)
  val generate : Maze.maze -> unit
end

module MakeGenerator : functor (M : MAZE_GENERATOR) -> MAZE_GENERATOR

(* Recursive Backtracking Maze Generator *)
module RecursiveBacktrackerGenerator : MAZE_GENERATOR

(* Prim's Algorithm Maze Generator *)
module PrimGenerator : MAZE_GENERATOR

(* Kruskal's Algorithm Maze Generator *)
module KruskalGenerator : MAZE_GENERATOR


