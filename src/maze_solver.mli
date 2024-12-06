open Maze

(* Interface for Maze Solvers *)
module type MAZE_SOLVER = sig
  type direction = Cell.direction  (* Type alias for direction (North, South, East, West) from Cell module *)
  type cell = Cell.t               (* Type alias for a cell from Cell module *)
  type maze = Maze.maze            (* Type alias for the overall maze from Maze module *)

  (* [solve maze] solves the given [maze] and returns a list of (x, y) positions representing the path
     from the start cell to the goal cell.
     - Parameter maze: A maze structure that needs to be solved.
     - Returns: A list of (x, y) tuples that represent the solution path from the start to the end of the maze.
       If no solution is found, the list may be empty.
  *)
  val solve : Maze.maze -> (int * int) list
end

(* Functor for Maze Solver *)
module MakeSolver : functor (_ : MAZE_SOLVER) -> MAZE_SOLVER

(* The [MakeSolver] functor takes a module that satisfies the [MAZE_SOLVER] signature
   and returns a module that adheres to the same [MAZE_SOLVER] interface. This allows for
   dynamic creation of maze solvers based on the input implementation. *)

(* Breadth-First Search (BFS) Maze Solver *)
module BFSSolver : MAZE_SOLVER

(* The [BFSSolver] module uses the Breadth-First Search algorithm to solve the maze.
   BFS is a complete algorithm that explores the maze level by level, ensuring that the shortest path
   from the start cell to the goal cell is found. The resulting path is guaranteed to be the shortest. *)

(* A* Search Maze Solver *)
module AStarSolver : MAZE_SOLVER

(* The [AStarSolver] module uses the A* search algorithm to solve the maze.
   A* is a heuristic-based algorithm that uses both the distance from the start cell and an estimated
   distance to the goal cell to efficiently find a path. It is often faster than BFS for mazes that
   can benefit from heuristic guidance, and it provides an optimal path if the heuristic is admissible. *)
