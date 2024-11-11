
(* [shuffle lst] returns a new list with the elements of [lst] randomly shuffled. *)
val shuffle : 'a list -> 'a list

(* [list_remove lst idx] removes the element at index [idx] from [lst] and returns the resulting list.
    Raises [Invalid_argument] if [idx] is out of bounds. *)
val list_remove : 'a list -> int -> 'a list

(* Priority Queue Module *)
module PriorityQueue : sig
  type 'a t  (** The type of priority queues containing elements of type ['a]. *)

  (* [empty] returns an empty priority queue. *)
  val empty : 'a t

  (* [insert pq item priority] inserts [item] with the given [priority] into [pq]. *)
  val insert : 'a t -> 'a -> int -> 'a t

  (* [extract_min pq] removes and returns the item with the lowest priority from [pq],
      along with the updated priority queue.
      Returns [None] if the queue is empty. *)
  val extract_min : 'a t -> ('a * 'a t) option

  (* [is_empty pq] returns [true] if the priority queue [pq] is empty, [false] otherwise. *)
  val is_empty : 'a t -> bool
end

(* Union-Find Data Structure *)
module UnionFind : sig
  type 'a t  (** The type of Union-Find structures holding elements of type ['a]. *)

  (* [create ()] creates a new Union-Find data structure. *)
  val create : unit -> 'a t

  (* [make_set uf x] adds a new set containing [x] to [uf]. *)
  val make_set : 'a t -> 'a -> unit

  (* [find uf x] returns the representative element of the set containing [x] in [uf]. *)
  val find : 'a t -> 'a

  (* [union uf x y] merges the sets containing [x] and [y] in [uf]. *)
  val union : 'a t -> 'a -> 'a -> unit
end
