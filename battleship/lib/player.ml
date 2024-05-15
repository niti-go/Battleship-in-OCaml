type t = {
  name : string;
  board : Grid.t;
  mutable is_ships_set : bool;
  mutable num_ships_sunk : int;
  mutable missed_turns : int;
}

let create_player n b =
  {
    name = n;
    board = b;
    is_ships_set = false;
    num_ships_sunk = 0;
    missed_turns = 0;
  }

(* if time, make it so counts misses in a row to be more fair*)
let allowed_turn player =
  let misses =
    List.fold_left ( + ) 0 (Grid.get_ships (Array.length player.board))
  in
  if player.missed_turns >= misses then
    let () =
      print_endline
        ("Penalty for " ^ string_of_int misses ^ " misses: skip 1 turn")
    in
    let () = player.missed_turns <- 0 in
    false
  else true
