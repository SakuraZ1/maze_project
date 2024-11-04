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


module type MAZE = sig
  type direction = North | South | East | West

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

  val create : int -> int -> maze
  val initialize_cells : maze -> unit
  val display : maze -> unit
  val get_cell : maze -> int -> int -> cell
  val set_cell : maze -> cell -> unit
  val in_bounds : maze -> int -> int -> bool
  val get_neighbors : maze -> cell -> cell list
  val remove_wall : maze -> int -> int -> int -> int -> unit
  val get_passable_neighbors : maze -> cell -> cell list
end
