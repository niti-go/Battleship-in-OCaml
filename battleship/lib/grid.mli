(* lib/grid.mli: Interface for the Battleship grid module *)

(** Type representing the different possible states of a cell in the game grid *)
type cell =
  | Water
  | Miss
  | Ship
  | Hit
  | Destroyed

val create_board : int -> string list list
(** Create a battleship grid of a given size with all cells initialized to Water *)

val print_grid : 'a list -> string list list -> unit
(** Print a grid to the console with color coding for different cell states *)
