
(* Shuffles a list randomly using an auxiliary list of random integers for sorting *)
let shuffle list =
  list |> List.map (fun x -> (Random.bits (), x)) |> List.sort compare |> List.map snd

(* Returns a copy of a matrix with a specific element set at position (x, y) *)
let set_matrix matrix x y value =
  let updated_row = Array.copy matrix.(y) in
  updated_row.(x) <- value;
  let updated_matrix = Array.copy matrix in
  updated_matrix.(y) <- updated_row;
  updated_matrix

(* Retrieves an element from a matrix at (x, y) *)
let get_matrix matrix x y = matrix.(y).(x)

(* Adds neighboring cells to the frontier list in Prim's algorithm *)
let add_neighbors_to_frontier maze visited x y frontier =
  let cell = Maze.get_cell maze x y in
  let neighbors = Maze.get_neighbors maze cell in
  List.fold_left (fun acc (_, neighbor) ->
    if not (get_matrix visited neighbor.x neighbor.y) then
      (x, y, neighbor.x, neighbor.y) :: acc
    else
      acc
  ) frontier neighbors

(* Removes the nth element from a list, returning the removed element and the updated list *)
let split_nth lst n =
  let rec aux i acc = function
    | [] -> raise Not_found
    | h :: t -> if i = n then (h, List.rev acc @ t) else aux (i + 1) (h :: acc) t
  in aux 0 [] lst
