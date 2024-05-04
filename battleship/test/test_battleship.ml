open OUnit2
open Battleship.Grid

let test_string_of_cell _ =
  assert_equal "wo" (string_of_cell Water);
  assert_equal "wx" (string_of_cell Miss);
  assert_equal "so" (string_of_cell (Ship { id = 0; length = 3 }));
  assert_equal "sx" (string_of_cell (Hit { id = 0; length = 3 }));
  assert_equal "sxx" (string_of_cell Destroyed);
  assert_equal "." (string_of_cell Hidden)

let test_coordinates _ =
  assert_equal (0, 0) (coordinates "A1");
  assert_equal (1, 0) (coordinates "A2");
  assert_equal (1, 1) (coordinates "B2");
  assert_equal (2, 2) (coordinates "C3");
  assert_equal (4, 0) (coordinates "A5");
  assert_equal (4, 0) (coordinates "A5");
  assert_equal (16, 14) (coordinates "O17");
  assert_equal (25, 25) (coordinates "Z26")

let test_create_board _ =
  assert_equal (Array.make_matrix 5 5 Water) (create_board 5);
  assert_equal (Array.make_matrix 17 17 Water) (create_board 17);
  assert_equal (Array.make_matrix 26 26 Water) (create_board 26)

let test_get_ships _ =
  assert_equal [ 4; 3; 3; 2 ] (get_ships 5);
  assert_equal [ 4; 3; 3; 2 ] (get_ships 7);
  assert_equal [ 4; 3; 3; 2 ] (get_ships 9);
  assert_equal [ 4; 4; 3; 3; 2; 2 ] (get_ships 10);
  assert_equal [ 4; 4; 3; 3; 2; 2 ] (get_ships 11);
  assert_equal [ 4; 4; 3; 3; 2; 2 ] (get_ships 14);
  assert_equal [ 5; 5; 4; 3; 3; 3; 2; 2 ] (get_ships 15);
  assert_equal [ 5; 5; 4; 3; 3; 3; 2; 2 ] (get_ships 18);
  assert_equal [ 7; 5; 4; 4; 3; 3; 2 ] (get_ships 19);
  assert_equal [ 7; 5; 4; 4; 3; 3; 2 ] (get_ships 21);
  assert_equal [ 9; 8; 6; 5; 5; 4; 3; 3; 3; 2; 2 ] (get_ships 22);
  assert_equal [ 9; 8; 6; 5; 5; 4; 3; 3; 3; 2; 2 ] (get_ships 24);
  assert_equal [ 10; 9; 9; 8; 8; 6; 4; 4; 4; 3; 3; 3; 2 ] (get_ships 25);
  assert_equal [ 10; 9; 9; 8; 8; 6; 4; 4; 4; 3; 3; 3; 2 ] (get_ships 26);
  assert_equal [ 2; 3; 4; 5 ] (get_ships 4);
  assert_equal [ 2; 3; 4; 5 ] (get_ships 28)

(* Completed functions to test next: print_grid, print_their_board, hit_ship,
   is_sunk *)
(* Incomplete functions or not ready test next: num_ships_sunk, validate_ship,
   change_to_ship, change_state, set_ships *)

let test_grid =
  "tests functionality of grid module"
  >::: [
         "Tests functionality of string_of_cell function."
         >:: test_string_of_cell;
         "Tests functionality of coordinates function." >:: test_coordinates;
         "Tests functionality of create_board function." >:: test_create_board;
         "Tests functionality of get_ships function." >:: test_get_ships;
       ]

let _ = run_test_tt_main test_grid
