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

let rec main_loop state =
  print_endline
    "Enter 'other' to view the opponent's board, 'next' to switch player, \
     'show' to see your board, 'play' to start your turn, or 'exit' to quit:";
  match read_line () with
  | "other" ->
      print_their_board state.opponent.board;
      main_loop state
  | "next" ->
      if state.current_player.is_ships_set then (
        switch_player state;
        main_loop state)
      else (
        print_endline "You must set your ships before switching players.";
        main_loop state)
  | "show" ->
      print_grid state.current_player.board;
      main_loop state
  | "play" -> begin
      if state.current_player.is_ships_set = false then (
        set_ships
          (get_ships (Array.length state.current_player.board))
          state.current_player.board;
        state.current_player.is_ships_set <- true;
        main_loop state)
      else if state.opponent.is_ships_set = false then (
        print_endline "Waiting for the opponent to set their ships.";
        main_loop state)
      else
        let rec play_turn () =
          print_endline "Enter the coordinates to attack (e.g., A5):";
          let coord = read_line () in
          try
            let row, col = coordinates coord in
            if
              row < 0
              || row >= Array.length state.opponent.board
              || col < 0
              || col >= Array.length state.opponent.board.(0)
            then (
              print_endline "Invalid coordinates. Please try again.";
              play_turn ())
            else
              let cell = state.opponent.board.(row).(col) in
              match cell with
              | Water ->
                  change_state state.opponent.board coord;
                  print_endline "MISS!";
                  print_their_board state.opponent.board;
                  switch_player state;
                  main_loop state
              | Ship { id; _ } ->
                  change_state state.opponent.board coord;
                  print_endline "HIT!";
                  if is_sunk coord id state.opponent.board then (
                    print_endline "You sunk a ship!";
                    incr num_ships_sunk;
                    if
                      !num_ships_sunk
                      = List.length
                          (get_ships (Array.length state.opponent.board))
                    then (
                      print_endline "Congratulations! You won the game!";
                      exit 0))
                  else ();
                  print_their_board state.opponent.board;
                  switch_player state;
                  main_loop state
              | _ ->
                  print_endline "You already attacked this position.";
                  play_turn ()
          with Failure _ ->
            print_endline "Invalid coordinates. Please try again.";
            play_turn ()
        in
        play_turn ()
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
