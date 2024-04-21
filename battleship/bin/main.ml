open Battleship.Grid
(**@authors Niti Goyal, Ginger McCoy, Naakai McDonald, Nidhi Soma *)

let rec main_loop board =
  print_endline
    "Enter 'otherboard' to view the opponent's board, 'exit' to quit, or \
     'show' to see your board:";
  match read_line () with
  | "otherboard" ->
      print_their_board board;
      (* Print the opponent's view of the board *)
      main_loop board (* Continue the loop *)
  | "show" ->
      print_grid [] board;
      (* Print the player's board *)
      main_loop board
  | "exit" ->
      print_endline "Exiting game.";
      exit 0
  | _ ->
      print_endline "Invalid command.";
      main_loop board

let () =
  print_endline "Welcome to Battleship!";
  print_endline "Please enter the grid size (5 to 26): ";
  let size = read_int () in
  if size < 5 || size > 26 then (
    print_endline "Invalid size. The size must be between 5 and 26.";
    exit 0)
  else
    let board = create_board size in
    main_loop board (* Start the interactive loop with the initial board *)
