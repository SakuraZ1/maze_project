module type MAZE = sig
  type cell = 0 | 1    (* 0 stands for Wall and 1 stands for Path *)
  type maze = cell list list

  val create : int -> int -> maze
  val display : maze -> unit
end
