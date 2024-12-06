  open OUnit2
  open Core
  open Utils
  open Maze
  
  let test_shuffle _ =
    let list = [1; 2; 3; 4; 5] in
    let shuffled_list = shuffle list in
    (* Check that the shuffled list contains all the same elements as the original list *)
    assert_equal
      ~cmp:(List.equal Int.equal)
      ~printer:(fun lst -> String.concat ~sep:", " (List.map lst ~f:Int.to_string))
      (List.sort ~compare:Int.compare list)
      (List.sort ~compare:Int.compare shuffled_list)
  
  let test_make_matrix_as_list _ =
    let matrix = make_matrix_as_list 3 2 0 in
    assert_equal
      ~printer:(fun lst -> String.concat ~sep:"\n" (List.map lst ~f:(fun row -> String.concat ~sep:", " (List.map row ~f:Int.to_string))))
      [[0; 0; 0]; [0; 0; 0]]
      matrix
  
  let test_set_matrix _ =
    let matrix = make_matrix_as_list 3 3 0 in
    let updated_matrix = set_matrix matrix 1 1 5 in
    assert_equal 5 (get_matrix updated_matrix 1 1);
    assert_equal 0 (get_matrix updated_matrix 0 0);
    assert_equal 0 (get_matrix updated_matrix 2 2)
  
  let test_get_matrix _ =
    let matrix = make_matrix_as_list 3 3 9 in
    assert_equal 9 (get_matrix matrix 1 1);
    assert_equal 9 (get_matrix matrix 2 0)
  
    let test_add_neighbors_to_frontier _ =
      let width, height = 3, 3 in
      let maze = Maze.create width height in
      let visited = make_matrix_as_list width height false in
      let frontier = add_neighbors_to_frontier maze visited 1 1 [] in
      (* Check that the correct number of neighbors were added *)
      assert_equal 4 (List.length frontier);
      (* Ensure that each neighbor is correctly added *)
      List.iter frontier ~f:(fun (_, _, nx, ny) ->
        assert_bool "Neighbor coordinates are valid" (Maze.in_bounds maze nx ny)
      )
    
  let test_split_nth _ =
    let lst = [0; 1; 2; 3; 4; 5] in
    let value, updated_list = split_nth lst 3 in
    assert_equal 3 value;
    assert_equal [0; 1; 2; 4; 5] updated_list
  
  let test_overlay_solution _ =
    let width, height = 3, 3 in
    let maze = Maze.create width height in
    let solution = [(0, 0); (1, 0); (2, 0)] in
    let maze_with_solution = overlay_solution maze solution in
    (* Check if the solution path has no walls *)
    List.iter solution ~f:(fun (x, y) ->
      let cell = Maze.get_cell maze_with_solution x y in
      assert_bool
        "Walls of the cell on the solution path should be removed"
        (List.for_all cell.walls ~f:(fun (_, exists) -> not exists))
    )
  
  let suite =
    "Utils Tests" >:::
    [
      "test_shuffle" >:: test_shuffle;
      "test_make_matrix_as_list" >:: test_make_matrix_as_list;
      "test_set_matrix" >:: test_set_matrix;
      "test_get_matrix" >:: test_get_matrix;
      "test_add_neighbors_to_frontier" >:: test_add_neighbors_to_frontier;
      "test_split_nth" >:: test_split_nth;
      "test_overlay_solution" >:: test_overlay_solution;
    ]
  
  let _ =
    run_test_tt_main suite
  
