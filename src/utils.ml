
(* [shuffle lst] returns a new list with the elements of [lst] randomly shuffled. *)
let shuffle lst =
  let arr = Array.of_list lst in
  for i = Array.length arr - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr



(* [list_remove lst idx] removes the element at index [idx] from [lst]. *)
let list_remove lst idx =
  if idx < 0 || idx >= List.length lst then
    invalid_arg "list_remove: index out of bounds"
  else
    let rec aux i acc = function
      | [] -> List.rev acc
      | h :: t ->
        if i = idx then List.rev_append acc t
        else aux (i + 1) (h :: acc) t
    in
    aux 0 [] lst



(* Priority Queue Module *)
module PriorityQueue = struct
  type 'a item = {
    value : 'a;
    priority : int;
  }

  type 'a t = 'a item list

  let empty = []

  let insert pq item priority =
    let rec aux acc = function
      | [] -> List.rev ({ value = item; priority } :: acc)
      | h :: t as l ->
        if priority <= h.priority then
          List.rev_append acc ({ value = item; priority } :: l)
        else
          aux (h :: acc) t
    in
    aux [] pq

  let extract_min pq =
    match pq with
    | [] -> None
    | { value; _ } :: t -> Some (value, t)

  let is_empty pq = (pq = [])
end



(* Union-Find Data Structure *)
module UnionFind = struct
  type 'a node = {
    value : 'a;
    mutable parent : 'a node;
    mutable rank : int;
  }

  type 'a t = ('a, 'a node) Hashtbl.t

  let create () = Hashtbl.create 100

  let make_set uf x =
    let rec node = { value = x; parent = node; rank = 0 } in
    Hashtbl.add uf x node

  let rec find_node node =
    if node != node.parent then
      node.parent <- find_node node.parent;
    node.parent

  let find uf x =
    let node = Hashtbl.find uf x in
    (find_node node).value

  let union uf x y =
    let x_root = Hashtbl.find uf (find uf x) in
    let y_root = Hashtbl.find uf (find uf y) in
    if x_root == y_root then
      ()
    else if x_root.rank < y_root.rank then
      x_root.parent <- y_root
    else if x_root.rank > y_root.rank then
      y_root.parent <- x_root
    else
      begin
        y_root.parent <- x_root;
        x_root.rank <- x_root.rank + 1
      end
end
