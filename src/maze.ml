module type MAZE = sig
  type cell = Wall | Path   
  type maze = {
    width : int;
    height : int;
    grid : cell array array;
    }

  val create : int -> int -> maze
  val display : maze -> unit
end


module Maze : MAZE = struct
  type cell = Wall | Path

  type maze = {
    width : int;
    height : int;
    grid : cell array array;
  }

  let create width height =
    let grid = Array.make_matrix width height Wall 
    in
    { width; height; grid }


  let display maze =
    for y = 0 to maze.height - 1 do
      for x = 0 to maze.width - 1 do
        match maze.grid.(x).(y) with
        | Wall -> print_string "#"
        | Path -> print_string " "
      done;
      print_newline ()
    done


