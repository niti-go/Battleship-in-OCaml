open Battleship.Grid
(**@authors Niti Goyal, Ginger McCoy, Naakai McDonald, Nidhi Soma *)

let () =
  print_endline "Welcome to Battleship!";
  print_endline "Please enter the grid size (5 to 26): ";
  let size = read_int () in
  if size < 5 || size > 26 then (
    print_endline "Invalid size. The size must be between 5 and 26.";
    exit 0)
  else
    let board = create_board size in
    print_grid [] board (* Passing an empty list as the first argument *)
