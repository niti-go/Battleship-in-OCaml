open OUnit2
open Battleship.Grid

let test_string_of_cell _ =
  assert_equal "wo" (string_of_cell Water);
  assert_equal "wx" (string_of_cell Miss);
  assert_equal "so" (string_of_cell (Ship { id = 0; length = 3 }));
  assert_equal "sx" (string_of_cell (Hit { id = 0; length = 3 }));
  assert_equal "ss" (string_of_cell (Destroyed { id = 0; length = 3 }));
  assert_equal ". " (string_of_cell Hidden)

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

let example_grid = create_board 5
let () = change_to_ship example_grid 1 4 (0, 0)
let () = change_to_ship example_grid 1 4 (1, 0)
let () = change_to_ship example_grid 1 4 (2, 0)
let () = change_to_ship example_grid 1 4 (3, 0)
let () = change_to_ship example_grid 2 3 (0, 1)
let () = change_to_ship example_grid 2 3 (1, 1)

let () =
  change_to_ship example_grid 2 3 (2, 1);

  print_endline "PRINTING...";
  print_grid example_grid

let rec print_tuples = function
  | [] -> ()
  | (x, y) :: t ->
      Printf.printf "%i, %i; " x y;
      print_tuples t

let test_hit_ships _ =
  let () = change_state example_grid "A1" in
  (* print_tuples (hit_ships "A1" 1 example_grid); *)
  assert_equal [ (0, 0) ] (hit_ships "A1" 1 example_grid);
  let () = change_state example_grid "A2" in
  (* print_tuples (hit_ships "A2" 1 example_grid); *)
  assert_equal [ (0, 0); (1, 0) ] (hit_ships "A2" 1 example_grid);
  let () = change_state example_grid "A3" in
  (* print_grid example_grid; *)
  (* print_tuples (hit_ships "A3" 1 example_grid); *)
  assert_equal [ (0, 0); (1, 0); (2, 0) ] (hit_ships "A3" 1 example_grid);
  (* print_tuples (hit_ships "A4" 1 example_grid); *)
  assert_equal [ (0, 0); (1, 0); (2, 0) ] (hit_ships "A4" 1 example_grid);
  assert_equal [] (hit_ships "B2" 2 example_grid)

let test_is_sunk _ =
  let () = change_state example_grid "B3" in
  print_tuples (hit_ships "B3" 1 example_grid);
  assert_equal false (is_sunk "B3" 2 example_grid) (* some of the ship is hit*);

  let () = change_state example_grid "A1" in
  let () = change_state example_grid "A2" in
  let () = change_state example_grid "A3" in
  let () = change_state example_grid "A4" in
  (* print_grid example_grid; print_tuples (hit_ships "A4" 1 example_grid); *)
  assert_equal true (is_sunk "A3" 1 example_grid) (* entire ship is hit *);

  let () = change_to_ship example_grid 4 2 (3, 2) in
  let () = change_to_ship example_grid 4 2 (4, 2) in
  let () = change_state example_grid "C4" in
  assert_equal false (is_sunk "C4" 4 example_grid)
  (* every other part of the ship is hit*);

  let () = change_to_ship example_grid 3 3 (2, 2) in
  let () = change_to_ship example_grid 3 3 (2, 3) in
  let () = change_to_ship example_grid 3 3 (2, 4) in
  assert_equal false (is_sunk "C3" 3 example_grid)
  (* none of the ship is hit (and ship is up against other ship) *);

  assert_equal false (is_sunk "E4" 7 example_grid)
  (* not Ship but instead Water *);

  let () = change_state example_grid "E3" in
  assert_equal false (is_sunk "E4" 7 example_grid)
(* not Ship but instead Miss *)

(* Test next: print_grid, print_their_board, num_ships_sunk, validate_ship,
   set_ships*)
let test_validate_ship _ =
  assert_equal (true, 2) (validate_ship "A1" "B1" (create_board 7))
  (* Valid horizontal ship *);
  assert_equal (true, 4) (validate_ship "D1" "D4" (create_board 7))
  (* Valid vertical ship *);
  assert_equal (false, 0) (validate_ship "A1" "A8" (create_board 5))
  (* Invalid ship (too long) *);
  assert_equal (false, 0) (validate_ship "A1" "A1" (create_board 7))
  (* Invalid ship (too short) *);
  assert_equal (false, 0) (validate_ship "A1" "B2" (create_board 7))
  (* Invalid ship (diagonal) *);
  let valid_ship_4 = create_board 5 in
  let () = change_to_ship valid_ship_4 1 4 (0, 0) in
  let () = change_to_ship valid_ship_4 1 4 (1, 0) in
  let () = change_to_ship valid_ship_4 1 4 (2, 0) in
  let () = change_to_ship valid_ship_4 1 4 (3, 0) in
  assert_equal (false, 4) (validate_ship "A1" "A4" valid_ship_4)
  (* Invalid ship (cooridnates overlap another ship) *);
  assert_equal (false, 0) (validate_ship "A1" "A9" (create_board 7))

(* Invalid ship (coordinates are off the grid) *)
(* let tuple = validate_ship "A1" "A4" valid_ship_4 in Printf.printf "%b %i"
   (fst tuple) (snd tuple) *)
let test_set_ships_invalid_lengths _ =
  let grid = create_board 5 in
  let expected_grid = create_board 5 in
  (* Compare the expected grid with the actual grid *)
  for i = 0 to 4 do
    for j = 0 to 4 do
      assert_equal
        (string_of_cell expected_grid.(i).(j))
        (string_of_cell grid.(i).(j))
    done
  done

let test_invalid_board_size _ =
  try
    let _ = create_board 4 in
    assert_failure "Expected Invalid_argument exception"
  with Invalid_argument _ -> ()

let test_grid =
  "tests functionality of grid module"
  >::: [
         "Tests\n functionality of string_of_cell function."
         >:: test_string_of_cell;
         "Tests\n   functionality of coordinates function." >:: test_coordinates;
         "Tests\n   functionality of create_board function."
         >:: test_create_board;
         "Tests\n   functionality of get_ships function." >:: test_get_ships;
         "Tests\n   functionality of change_state function."
         >:: test_change_state;
         "Tests\n   functionality of change_to_ship function."
         >:: test_change_to_ship;
         "Tests functionality of hit_ship function." >:: test_hit_ships;
         "Tests functionality of is_sunk function." >:: test_is_sunk;
         "Tests functionality of validate_ship function." >:: test_validate_ship;
         "Test set_ships with invalid ship lengths"
         >:: test_set_ships_invalid_lengths;
         "Test creating a board with invalid size" >:: test_invalid_board_size;
       ]

let _ = run_test_tt_main test_grid
