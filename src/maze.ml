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

let get_cell maze x y =
    if x >= 0 && x < maze.width && y >= 0 && y < maze.height then
      maze.grid.(x).(y)
    else
      invalid_arg "get_cell: coordinates out of bounds"

  (* this sets the cell at position (x, y) to [cell] *)
  let set_cell maze x y cell =
    if x >= 0 && x < maze.width && y >= 0 && y < maze.height then
      maze.grid.(x).(y) <- cell
    else
      invalid_arg "set_cell: coordinates out of bounds"

  (* checks if (x, y) is within the maze bounds *)
  let in_bounds maze x y =
    x >= 0 && x < maze.width && y >= 0 && y < maze.height

  (* returns a list of neighboring coordinates *)
  let get_neighbors maze x y =
    let directions = [ (0, -1); (0, 1); (-1, 0); (1, 0) ] in
    List.filter (fun (dx, dy) ->
      in_bounds maze (x + dx) (y + dy)
    ) directions
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))
end


