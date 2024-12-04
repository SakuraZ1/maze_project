open Core
open Maze

(* Shuffles a list randomly using an auxiliary list of random integers for sorting *)
let shuffle list =
  list
  |> List.map ~f:(fun x -> (Random.bits (), x))
  |> List.sort ~compare:(fun (r1, _) (r2, _) -> Int.compare r1 r2)
  |> List.map ~f:snd (*|> List.map (fun x -> (Random.bits (), x)) |> List.sort compare |> List.map snd*)

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
  List.fold neighbors ~init:frontier ~f:(fun acc (_, neighbor) ->
    if not (get_matrix visited neighbor.x neighbor.y) then
      (x, y, neighbor.x, neighbor.y) :: acc
    else
      acc
  )


(* Removes the nth element from a list, returning the removed element and the updated list *)
let split_nth lst n =
  let rec aux i acc = function
    | [] -> raise (Not_found_s (Sexp.Atom "split_nth: List is empty"))
    | h :: t -> if i = n then (h, List.rev acc @ t) else aux (i + 1) (h :: acc) t
  in aux 0 [] lst


(* Overlay the solution path onto the maze *)
let overlay_solution maze solution =
  let grid = Maze.get_grid maze in
  let new_grid = Array.map grid ~f:(fun row ->
    Array.map row ~f:(fun cell ->
      if List.exists solution ~f:(fun (x, y) -> cell.x = x && cell.y = y) then
        { cell with walls = List.map cell.walls ~f:(fun (dir, _) -> (dir, false)) }
      else
        cell
    )
  ) in
  Maze.with_grid maze new_grid

  (* Converts the maze to HTML for display *)
let maze_to_html maze solution =
  let solution_set = List.fold solution ~init:[] ~f:(fun acc (x, y) -> (x, y) :: acc) in
  let cell_to_html (cell: Maze.cell) =
    let is_in_solution =let is_in_solution =
      List.mem solution_set (cell.x, cell.y) ~equal:(fun (x1, y1) (x2, y2) ->
        x1 = x2 && y1 = y2
      )
     in
    let cell_class =
      if is_in_solution then "cell path"
      else "cell empty"
    in
    (* Create walls based on the cell's walls *)
    let walls = cell.walls in
    let top_wall = if List.assoc North walls then "border-top: 2px solid black;" else "" in
    let right_wall = if List.assoc East walls then "border-right: 2px solid black;" else "" in
    let bottom_wall = if List.assoc South walls then "border-bottom: 2px solid black;" else "" in
    let left_wall = if List.assoc West walls then "border-left: 2px solid black;" else "" in
    let style = top_wall ^ right_wall ^ bottom_wall ^ left_wall in
    Printf.sprintf "<div class='%s' style='%s'></div>" cell_class style
  in
  let rows = Array.map (fun row ->
    Array.fold_left (fun acc cell -> acc ^ (cell_to_html cell)) "" row
  ) maze.grid in
  Array.fold_left (fun acc row_html -> acc ^ "<div class='maze-row'>" ^ row_html ^ "</div>") "" rows