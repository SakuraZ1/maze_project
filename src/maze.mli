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


