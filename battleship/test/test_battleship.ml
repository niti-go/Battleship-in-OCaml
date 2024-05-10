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
  assert_equal (2, 0) (coordinates "A3");
  assert_equal (1, 1) (coordinates "B2");
  assert_equal (2, 2) (coordinates "C3");
  assert_equal (4, 0) (coordinates "A5");
  assert_equal (4, 3) (coordinates "D5");
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

let test_change_state _ =
  let change = create_board 5 in
  let () = change_state change "A1" in
  assert_equal "wx" (string_of_cell change.(0).(0));
  let () = change_state change "B1" in
  assert_equal "wx" (string_of_cell change.(0).(1));
  let () = change_state change "C1" in
  assert_equal "wx" (string_of_cell change.(0).(2));
  let () = change_state change "D1" in
  assert_equal "wx" (string_of_cell change.(0).(3));
  (*Check that other cells did not change*)
  assert_equal "wo" (string_of_cell change.(1).(1));
  assert_equal "wo" (string_of_cell change.(2).(2))

let test_change_to_ship _ =
  let test_ships = create_board 5 in
  let () = change_to_ship test_ships 1 4 (0, 0) in
  assert_equal "so" (string_of_cell test_ships.(0).(0));
  let () = change_to_ship test_ships 2 3 (3, 0) in
  assert_equal "so" (string_of_cell test_ships.(3).(0))

(* change_to_ship must be further implemented. *)

(* let test_print_grid _ = let test_print_their_board _ = *)

(* UNFINSHED: cannot test hit_ship without set_ships *)
(* let example_grid = set_ships [ 4; 3; 3; 2 ] (create_board 5) *)

(* let () = print_endline "Printing..."; let example_grid = create_board 5 in
   let () = print_grid example_grid in let () = change_to_ship example_grid 1 4
   (0, 0) in let () = print_grid example_grid in

   let () = change_to_ship example_grid 1 4 (1, 0) in let () = print_grid
   example_grid in

   let () = change_to_ship example_grid 1 4 (2, 0) in let () = print_grid
   example_grid in

   let () = change_to_ship example_grid 1 4 (3, 0) in let () = print_grid
   example_grid in

   print_endline "DONE Printing...";

   () *)

let example_grid = create_board 5
let () = change_to_ship example_grid 1 4 (0, 0)
let () = change_to_ship example_grid 1 4 (1, 0)
let () = change_to_ship example_grid 1 4 (2, 0)
let () = change_to_ship example_grid 1 4 (3, 0)
let () = change_to_ship example_grid 2 3 (2, 1)
let () = change_to_ship example_grid 2 3 (2, 2)

let () =
  change_to_ship example_grid 2 3 (2, 3);

  print_endline "PRINTING...";
  print_grid example_grid

let rec print_tuples = function
  | [] -> ()
  | (x, y) :: t ->
      Printf.printf "%i, %i; " x y;
      print_tuples t

(* WORKING ON *)
(* doesn't rturn ships in order and adds the coordinates twice???. *)
let test_hit_ship _ =
  let () = change_state example_grid "A1" in
  (* print_tuples (hit_ship "A1" 1 example_grid); *)
  assert_equal [ (0, 0) ] (hit_ship "A1" 1 example_grid);
  let () = change_state example_grid "A2" in
  (* print_tuples (hit_ship "A2" 1 example_grid); *)
  assert_equal [ (0, 0); (1, 0) ] (hit_ship "A2" 1 example_grid);
  let () = change_state example_grid "A3" in
  print_grid example_grid;
  print_tuples (hit_ship "A3" 1 example_grid);
  assert_equal [ (1, 0); (0, 0); (2, 0) ] (hit_ship "A3" 1 example_grid);
  (* print_tuples (hit_ship "A4" 1 example_grid); assert_equal [ (1, 0); (0, 0);
     (2, 0); (2, 0); (1, 0); (0, 0) ] (hit_ship "A4" 1 example_grid); *)
  assert_equal [] (hit_ship "B2" 2 example_grid)

(* WORKING ON *)
let test_is_sunk _ =
  let () = change_state example_grid "B3" in
  print_tuples (hit_ship "B3" 1 example_grid);
  assert_equal false (is_sunk "B3" 2 example_grid)
(* some of the ship is hit*)

(* let () = change_state example_grid "A4" in assert_equal true (is_sunk "A3" 1
   example_grid) *)
(* entire ship is hit; *)

(* assert_equal false (is_sunk "C1" 3 example_grid); (* every other part of the
   ship is hit*) assert_equal false (is_sunk "C1" 3 example_grid) none of the
   ship is hit (and it is side by side another ship) *)

(* Completed functions to test next: print_grid, print_their_board, hit_ship, *)
(* Incomplete functions or not ready test next: num_ships_sunk, validate_ship,
   change_to_ship, change_state, set_ships, is_sunk *)

let test_grid =
  "tests functionality of grid module"
  >::: [
         "Tests\n   functionality of string_of_cell function."
         >:: test_string_of_cell;
         "Tests\n   functionality of coordinates function." >:: test_coordinates;
         "Tests\n   functionality of create_board function."
         >:: test_create_board;
         "Tests\n   functionality of get_ships function." >:: test_get_ships;
         "Tests\n   functionality of change_state function."
         >:: test_change_state;
         "Tests\n   functionality of change_to_ship function."
         >:: test_change_to_ship;
         "Tests functionality of hit_ship function." >:: test_hit_ship;
         "Tests functionality of is_sunk function." >:: test_is_sunk;
       ]

let _ = run_test_tt_main test_grid
