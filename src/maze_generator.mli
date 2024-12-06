
open Maze

module type MAZE_GENERATOR = sig

  type cell 
  type maze 
  
  (* Generates a maze by modifying the given Maze.maze in-place. *)
  val generate : Maze.maze -> Maze.maze
end

module MakeGenerator : functor (_ : MAZE_GENERATOR) -> MAZE_GENERATOR

(* Recursive Backtracking Maze Generator *)
module RecursiveBacktrackerGenerator : MAZE_GENERATOR

(* Prim's Algorithm Maze Generator *)
module PrimGenerator : MAZE_GENERATOR

(* Kruskal's Algorithm Maze Generator *)
module KruskalGenerator : MAZE_GENERATOR


