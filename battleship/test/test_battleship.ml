open OUnit2
open Battleship.Grid

let test_coordinates _ = assert_equal (0, 6) (Battleship.Grid.coordinates "A5")

let test_grid =
  "tests functionality of grid module"
  >::: [ "Tests functionality of coordinates function." >:: test_coordinates ]

let _ = run_test_tt_main test_grid
