type t = {
  name : string;
  board : Grid.t;
  mutable is_ships_set : bool;
  mutable num_ships_sunk : int;
}

let create_player n b =
  { name = n; board = b; is_ships_set = false; num_ships_sunk = 0 }
