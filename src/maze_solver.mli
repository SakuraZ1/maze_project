
module type MAZE_SOLVER = sig
  (* [solve maze] finds a path from the start to the end of [maze],
      returning a list of coordinates representing the solution path. *)
  val solve : Maze.maze -> (int * int) list
end

(* Functor to create a maze solver. *)
module MakeSolver : functor (M : MAZE_SOLVER) -> MAZE_SOLVER

(* BFS Maze Solver *)
module BFSSolver : MAZE_SOLVER

(* A\* Search Algorithm Maze Solver *)
module AStarSolver : MAZE_SOLVER

