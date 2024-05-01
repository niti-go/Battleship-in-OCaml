open OUnit2
open Battleship.Grid

(* need to fix functionaility of coordinates & review test cases and make sample
   grid. currently recieveing error. *)
let test_coordinates _ = assert_equal (0, 6) (coordinates "A5")

let test_grid =
  "tests functionality of grid module"
  >::: [ "Tests functionality of coordinates function." >:: test_coordinates ]

let _ = run_test_tt_main test_grid
