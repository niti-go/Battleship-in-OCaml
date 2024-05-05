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

(* let test_print_grid _ = let test_print_their_board _ = *)

(* UNFINSHED: cannot test hit_ship without set_ships let example_grid =
   set_ships [4; 3; 3; 2] (create_board 5)

   let test_hit_ship _ = assert_equal [ 2; 3; 4; 5 ] (hit_ship "A4" 1 );
   assert_equal [ 2; 3; 4; 5 ] (hit_ship 4); assert_equal [ 2; 3; 4; 5 ]
   (hit_ship 4);

   (* UNFINSHED: cannot test is_sunk without set_ships *) let ex_board_2 =
   set_ships [4; 3; 3; 2] (create_board 7) in let hit_ship_4 = hit_ship "A1" 4
   ex_hit_ships in

   let hit_ship_4_2 = hit_ship "A2" 4 hit_ship_4 in

   let hit_ship_3 = hit_ship "C2" 3 hit_ship_4_2 in

   let hit_ship_3_2 = hit_ship "C3" 3 hit_ship_3 in

   let hit_ship_2 = hit_ship "E5" 2 hit_ship_3_2 in

   hit_ship "G5" 2 hit_ship_2

   1 SX wo wo wo wo wo wo 2 SX wo SX|SO SO SO SO 3 wo wo SX wo wo wo wo 4 wo wo
   SO wo wo wo wo 5 wo wo wo wo SX SO SX 6 wo wo wo wo wo wo wo 7 wo wo wo wo wo
   wo wo A B C D E F G

   let test_is_sunk _ = assert_equal true (is_sunk "A1" 1 example_grid); (*
   entire ship is hit*) assert_equal false (is_sunk "B3" 2 example_grid); (*
   some of the ship is hit*) assert_equal false (is_sunk "C1" 3 example_grid);
   (* every other part of the ship is hit*) assert_equal false (is_sunk "C1" 3
   example_grid) none of the ship is hit (and it is side by side another
   ship) *)

(* Completed functions to test next: print_grid, print_their_board, hit_ship, *)
(* Incomplete functions or not ready test next: num_ships_sunk, validate_ship,
   change_to_ship, change_state, set_ships, is_sunk *)

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
