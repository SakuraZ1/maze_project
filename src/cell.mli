(** Represents a direction in the maze. *)
type direction =
  | North
  | South
  | East
  | West

(** Represents a cell within the maze grid. *)
type t = {
  x : int;
  y : int;
  walls : (direction * bool) list;
} [@@deriving compare, sexp]

(** [compare c1 c2] compares two cells first by their x-coordinate, then by y-coordinate.
    Returns a negative integer if [c1] is less than [c2], zero if equal, and a positive integer otherwise. *)
val compare : t -> t -> int

(** [equal c1 c2] returns [true] if [c1] and [c2] have the same coordinates, [false] otherwise. *)
val equal : t -> t -> bool

(** [hash c] computes a hash value for cell [c], based on its coordinates. *)
val hash : t -> int
