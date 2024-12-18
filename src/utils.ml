open Core
open Maze

(* Shuffles a list randomly using an auxiliary list of random integers for sorting *)
let shuffle list =
  list
  |> List.map ~f:(fun x -> (Random.bits (), x))
  |> List.sort ~compare:(fun (r1, _) (r2, _) -> Int.compare r1 r2)
  |> List.map ~f:snd (*|> List.map (fun x -> (Random.bits (), x)) |> List.sort compare |> List.map snd*)

(* make_matrix as List avoid mutable *)
let make_matrix_as_list dimx dimy initial_value =
    List.init dimy ~f:(fun _ -> List.init dimx ~f:(fun _ -> initial_value))
  

(* Returns a copy of a matrix with a specific element set at position (x, y) *)
let set_matrix (matrix: 'a list list) x y value =
  List.mapi matrix ~f:(fun j row ->
    if j = y then
      List.mapi row ~f:(fun i v -> if i = x then value else v)
    else
      row
  )

(* Retrieves an element from a matrix at (x, y) *)
let get_matrix (matrix: 'a list list) x y =
  List.nth_exn (List.nth_exn matrix y) x

(* Adds neighboring cells to the frontier list in Prim's algorithm *)
let add_neighbors_to_frontier maze (visited: 'a list list) x y frontier =
  let cell = Maze.get_cell maze x y in
  let neighbors = Maze.get_neighbors maze cell in
  List.fold neighbors ~init:frontier ~f:(fun acc (_, neighbor) ->
    if not (get_matrix visited neighbor.x neighbor.y) then
      (x, y, neighbor.x, neighbor.y) :: acc
    else
      acc
  )


(* Removes the nth element from a list, returning the removed element and the updated list *)
let split_nth lst n =
  let rec aux i acc = function
    | [] -> failwith "empty list"
    | h :: t ->
      if i = n then h, List.rev acc @ t
      else aux (i + 1) (h :: acc) t
  in
  aux 0 [] lst


(* Overlay the solution path onto the maze *)
let overlay_solution maze solution =
  let grid = Maze.get_grid maze in
  let new_grid =
    List.mapi grid ~f:(fun j row ->
      List.mapi row ~f:(fun i cell ->
        if List.exists solution ~f:(fun (x, y) -> i = x && j = y) then
          { cell with walls = List.map cell.walls ~f:(fun (dir, _) -> (dir, false)) }
        else
          cell
      )
    )
  in
  Maze.with_grid maze new_grid


let path_exists maze start (end_: (int * int)) =
    let rec dfs visited (x, y) =
      (* If we've reached the end point, return true *)
      let (u, v) = end_ in
      if x = u && y = v then true
      else if List.exists visited ~f:(fun (vx, vy) -> vx = x && vy = y) then false
      else
        let cell = Maze.get_cell maze x y in
        (* Add the current cell to visited list *)
        let visited' = (x, y) :: visited in
        (* Get all passable neighbors of the current cell *)
        let neighbors = Maze.get_passable_neighbors maze cell in
        List.exists neighbors ~f:(fun neighbor -> dfs visited' (neighbor.x, neighbor.y))
    in
    dfs [] start


let mark_entrance maze =
  let entrance = Maze.get_cell maze 0 0 in
  let neighbors = Maze.get_neighbors maze entrance in

  List.iter neighbors ~f:(fun (_, neighbor) ->
    let cell1 = Maze.get_cell maze 0 0 in
    let cell2 = neighbor in
    let maze = Maze.remove_wall maze cell1 cell2 in
    ignore maze
  );

  maze


  