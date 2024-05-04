open OUnit2
open Battleship.Grid

let test_coordinates _ =
  assert_equal (0, 0) (coordinates "A1");
  assert_equal (1, 0) (coordinates "A2");
  assert_equal (1, 1) (coordinates "B2");
  assert_equal (2, 2) (coordinates "C3");
  assert_equal (4, 0) (coordinates "A5");
  assert_equal (4, 0) (coordinates "A5");
  assert_equal (16, 14) (coordinates "O17");
  assert_equal (25, 25) (coordinates "Z26")

let test_grid =
  "tests functionality of grid module"
  >::: [ "Tests functionality of coordinates function." >:: test_coordinates ]

let _ = run_test_tt_main test_grid
