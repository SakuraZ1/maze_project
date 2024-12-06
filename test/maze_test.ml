(* test/maze_test.ml *)

(*open OUnit2
open Maze

let test_create_maze _ =
  let maze = Maze.create 3 3 in
  assert_equal (Maze.get_width maze) 3;
  assert_equal (Maze.get_height maze) 3;
  assert_bool "Grid is initialized correctly" (List.length (Maze.get_grid maze) = 3);
  assert_bool "Each row has correct number of cells" (
    List.for_all (fun row -> List.length row = 3) (Maze.get_grid maze)
  )

let test_get_cell _ =
  let maze = Maze.create 3 3 in
  let cell = Maze.get_cell maze 1 1 in
  assert_equal cell.x 1;
  assert_equal cell.y 1

let suite =
  "Maze Test Suite" >::: [
    "test_create_maze" >:: test_create_maze;
    "test_get_cell" >:: test_get_cell;
  ]

let () =
  run_test_tt_main suite
*)


 open OUnit2
  open Maze
  open Cell
  open Core
  
  let test_create_maze _ =
    let maze = Maze.create 3 3 in
    assert_equal (Maze.get_width maze) 3;
    assert_equal (Maze.get_height maze) 3;
  
    let grid = Maze.get_grid maze in
    assert_equal (List.length grid) 3;
    
    (* Iterate over each row and assert that each row has 3 elements *)
    List.iter ~f:(fun row ->
      assert_equal (List.length row) 3
    ) grid
  
  let test_get_cell _ =
    let maze = Maze.create 3 3 in
    let cell = Maze.get_cell maze 1 1 in
    assert_equal cell.x 1;
    assert_equal cell.y 1
  
  let test_set_cell _ =
    let maze = Maze.create 3 3 in
    let cell = Maze.get_cell maze 1 1 in
    let updated_cell = { cell with walls = [(North, false)] } in
    let updated_maze = Maze.set_cell maze updated_cell in
    let new_cell = Maze.get_cell updated_maze 1 1 in
    assert_equal new_cell.walls [(North, false)]
  
  let test_in_bounds _ =
    let maze = Maze.create 3 3 in
    assert_bool "Cell is in bounds" (Maze.in_bounds maze 1 1);
    assert_bool "Cell is out of bounds" (not (Maze.in_bounds maze 3 3))
  
  let test_neighbors _ =
    let maze = Maze.create 3 3 in
    let cell = Maze.get_cell maze 1 1 in
    let neighbors = Maze.get_neighbors maze cell in
    assert_equal (List.length neighbors) 4
  
    let test_remove_wall _ =
      let maze = Maze.create 3 3 in
      let cell1 = Maze.get_cell maze 1 1 in
      let cell2 = Maze.get_cell maze 1 2 in
      let updated_maze = Maze.remove_wall maze cell1 cell2 in
      let updated_cell1 = Maze.get_cell updated_maze 1 1 in
      let updated_cell2 = Maze.get_cell updated_maze 1 2 in
    
      (* Verifying that the wall between cell1 and cell2 is removed *)
      let wall_between_cell1_and_cell2 =
        List.exists updated_cell1.walls ~f:(fun (dir, exists) ->
          match dir with
          | South -> not exists
          | _ -> false
        )
      in
      let wall_between_cell2_and_cell1 =
        List.exists updated_cell2.walls ~f:(fun (dir, exists) ->
          match dir with
          | North -> not exists
          | _ -> false
        )
      in
    
      assert_bool "Wall removed from cell1 to cell2" wall_between_cell1_and_cell2;
      assert_bool "Wall removed from cell2 to cell1" wall_between_cell2_and_cell1

      let test_get_passable_neighbors _ =
        (* Initialize a small maze for testing *)
        let maze = Maze.create 3 3 in
        (* Get two adjacent cells and remove the wall between them *)
        let cell1 = Maze.get_cell maze 0 0 in
        let cell2 = Maze.get_cell maze 1 0 in
        let updated_maze = Maze.remove_wall maze cell1 cell2 in
        (* Fetch the updated cell1 *)
        let cell1_updated = Maze.get_cell updated_maze 0 0 in
        (* Get passable neighbors for cell1 *)
        let passable_neighbors = Maze.get_passable_neighbors updated_maze cell1_updated in
        (* Test that cell2 is a passable neighbor of cell1 *)
        let passable_neighbor_exists =
          List.exists passable_neighbors ~f:(fun neighbor -> neighbor.x = cell2.x && neighbor.y = cell2.y)
        in
        assert_bool "Cell2 should be a passable neighbor of Cell1" passable_neighbor_exists;
        (* Test that the number of passable neighbors is correct *)
        assert_equal ~msg:"Number of passable neighbors" 1 (List.length passable_neighbors)
            
      let test_initialize_cells _ =
        let maze = Maze.create 3 3 in
        let modified_maze = Maze.set_cell maze { x = 0; y = 0; walls = [(North, false); (South, false); (East, false); (West, false)] } in
        let reinitialized_maze = Maze.initialize_cells modified_maze in
        let reinitialized_cell = Maze.get_cell reinitialized_maze 0 0 in
        (* Ensure that the cell has all walls intact after initialization *)
        List.iter reinitialized_cell.walls ~f:(fun (_, exists) -> assert_equal exists true)
    
  let suite =
    "Maze Tests" >::: [
      "test_create_maze" >:: test_create_maze;
      "test_get_cell" >:: test_get_cell;
      "test_set_cell" >:: test_set_cell;
      "test_in_bounds" >:: test_in_bounds;
      "test_neighbors" >:: test_neighbors;
      "test_remove_wall" >:: test_remove_wall;
      "test_get_passable_neighbors" >:: test_get_passable_neighbors;
      "test_initialize_cells" >:: test_initialize_cells;
    ]
  
  let () =
    run_test_tt_main suite
  