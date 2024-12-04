open Maze

module type MAZE_SOLVER = sig

  type direction =  North | South | East | West

  type cell = {
    x : int;  
    y : int;  
    walls : (direction * bool) list;  
  }
  
  type maze = {
    width : int;
    height : int;
    grid : cell array array;
  }
  (* Solves the maze and returns a list of (x, y) positions representing the path. *)
  val solve : Maze.maze -> (int * int) list
end

module MakeSolver : (*functor (M : MAZE_SOLVER) ->*) MAZE_SOLVER

(* BFS Maze Solver *)
module BFSSolver : MAZE_SOLVER

(* A* Search Maze Solver *)
module AStarSolver : MAZE_SOLVER
