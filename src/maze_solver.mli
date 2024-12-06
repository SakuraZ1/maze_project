open Maze

module type MAZE_SOLVER = sig
  type direction = Cell.direction
  type cell = Cell.t
  type maze = Maze.maze
  (* Solves the maze and returns a list of (x, y) positions representing the path. *)
  val solve : Maze.maze -> (int * int) list
end

module MakeSolver : functor (_ : MAZE_SOLVER) -> MAZE_SOLVER

(* BFS Maze Solver *)
module BFSSolver : MAZE_SOLVER

(* A* Search Maze Solver *)
module AStarSolver : MAZE_SOLVER
