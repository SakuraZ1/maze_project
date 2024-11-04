module type MAZE = sig
  type cell = Wall | Path   
  type maze = cell list list

  val create : int -> int -> maze
  val display : maze -> unit
end


