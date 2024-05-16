type t = {
  name : string;
  board : Grid.t;
  minigame : bool;
  mutable is_ships_set : bool;
  mutable num_ships_sunk : int;
  mutable missed_turns : int;
}
(**[t] is the type of a player. Stores information like their name, their
   current board, whether they set ships, and how many ships they have sunken.*)

val create_player : string -> Grid.t -> bool -> t
(**[create_player name board] creates a new player with ships placed set to
   false and number of ships sunk to 0, and minigame set to true or false.*)

val allowed_turn : t -> bool
(**[allowed_turn player] decides if [player] is allowed to have a turn based off
   their previous miss count. *)

val play_mini_game : t -> bool
(**[play_mini_game player] plays a minigame, if the [player]'s minigame option
   is set to true, and returns whether or not [player] wins the minigame.*)

val valid_mult_answer : int -> int -> int -> bool
(** [valid_mult_answer input num1 num2] checks if the input matches the
    multiplication result of [num1] and [num2]. *)

val multiplication_game : int -> int -> bool
(** [multiplication_game] simulates a minigame for the player to solve a
    multiplication problem involving user interaction, and returns true if the
    player succeeds/wins. *)
