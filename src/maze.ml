module type MAZE = sig
  type cell = 0 | 1  
  type maze = cell list list

  val create : int -> int -> maze
  val display : maze -> unit
end
