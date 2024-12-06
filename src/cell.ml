open Core

type direction = North | South | East | West [@@deriving compare, sexp]

type t = {
  x : int;
  y : int;
  walls : (direction * bool) list;
} [@@deriving compare, sexp]

let compare c1 c2 =
  match Int.compare c1.x c2.x with
  | 0 -> Int.compare c1.y c2.y
  | cmp -> cmp

let equal c1 c2 = compare c1 c2 = 0

let hash c = Hashtbl.hash (c.x, c.y)
