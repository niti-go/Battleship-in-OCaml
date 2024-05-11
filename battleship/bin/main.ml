open Battleship.Grid

(**@authors Niti Goyal, Ginger McCoy, Naakai McDonald, Nidhi Soma *)

type player = {
  name : string;
  board : t;
  mutable is_ships_set : bool;
}

type game_state = {
  mutable current_player : player;
  mutable opponent : player;
}

let create_player name size =
  { name; board = create_board size; is_ships_set = false }

let switch_player state =
  let temp = state.current_player in
  state.current_player <- state.opponent;
  state.opponent <- temp;
  print_endline ("It's now " ^ state.current_player.name ^ "'s turn.")

(*need to verify user input further - what if they put CC or 55. C0 case is
  taken care of in validate ships*)
(* let rec ask_for_coords (grid : t) (current_player : player) : string * string
   = print_endline "\n\ Rules - Ship must be only vertical or horizontal and
   cannot be placed \ outside of grid."; print_endline "\nEnter the top/left
   coordinate of a new ship. (e.g A5): "; let left_coord = read_line () in
   print_endline "Enter the bottom/right coordinate. (e.g C5): "; let
   right_coord = read_line () in if String.length left_coord <> 2 ||
   String.length right_coord <> 2 then let () = print_endline "Your input is not
   valid. Try again. " in ask_for_coords grid current_player else if fst
   (validate_ship left_coord right_coord grid) = true then let () =
   current_player.is_ships_set <- true in (left_coord, right_coord) else let ()
   = print_endline "Your coordinates are not valid. Try again. " in
   ask_for_coords grid current_player *)

let rec main_loop state =
  print_endline
    "Enter 'otherboard' to view the opponent's board, 'nextplayer' to switch \
     player, 'show' to see your board, 'play' to start your turn, or 'exit' to \
     quit:";
  match read_line () with
  | "otherboard" ->
      print_their_board state.opponent.board;
      main_loop state
  | "nextplayer" ->
      switch_player state;
      main_loop state
  | "show" ->
      print_grid state.current_player.board;
      main_loop state
      (*need to modify so each player gets one turn. currently player 1 can play
        multiple turns before giving it to other. play should loop until
        someones wins or quit, this should actually be hit*)
  | "play" -> begin
      let () =
        if state.current_player.is_ships_set = true then
          print_endline "player gonna play"
        else
          set_ships
            (get_ships (Array.length state.current_player.board))
            state.current_player.board;
        state.current_player.is_ships_set <- true
      in
      main_loop state
    end
  | "exit" ->
      print_endline "Exiting game.";
      exit 0
  | _ ->
      print_endline "Invalid command.";
      main_loop state

let enter_player_name num =
  print_endline ("Player " ^ string_of_int num ^ ", please enter your name:");
  read_line ()

let init_game size =
  let name1 = enter_player_name 1 in
  let name2 = enter_player_name 2 in
  let player1 = create_player name1 size in
  let player2 = create_player name2 size in
  { current_player = player1; opponent = player2 }

let rec enter_size () =
  print_endline "Please enter the grid size (5 to 26): ";
  let user_in = read_line () in
  try
    let size = int_of_string user_in in
    if size < 5 || size > 26 then (
      print_endline "Invalid size. The size must be between 5 and 26.";
      enter_size ())
    else init_game size
  with
  | Failure _ ->
      print_endline "Invalid input. Please enter a number.";
      enter_size ()
  | _ ->
      print_endline "Something went wrong. Shutting down...";
      exit 1

let () =
  let game_state = enter_size () in
  print_endline ("It's now " ^ game_state.current_player.name ^ "'s turn.");
  main_loop game_state
