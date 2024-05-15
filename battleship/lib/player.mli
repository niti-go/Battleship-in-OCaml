type t = {
  name : string;
  board : Grid.t;
  mutable is_ships_set : bool;
  mutable num_ships_sunk : int;
}
(**[t] is the type of a player. Stores information like their name, their
   current board, whether they set ships, and how many ships they have sunken.*)

val create_player : string -> Grid.t -> t
(**[create_player name board] creates a new player with ships placed set to
   false and number of ships sunk to 0.*)
