(* lib/grid.mli: Interface for the Battleship grid module *)

(* ToDo 1: create type for grid *)
(*type t = cell array array*)

type cell
(** Type representing the different possible states of a cell in the game grid *)

val size : int

(* alb2 *)
val coordinates : string -> int * int

(* add 1 when is_sunk is true*)
val num_ships_sunk : int ref

(* more time potential fxns: low and high fxn, low outputs a and high outputs
   1. *)

val create_board : int -> string list list
(** Create a battleship grid of a given size with all cells initialized to Water *)

val print_grid : string list list -> unit
(** Print a grid to the console with color coding for different cell states *)

val print_their_board : string list list -> unit

val get_ships : int -> int list
(** [get_ships size] takes in the size of the board and determines a list of
    ships that the user must place and what lengths they should be. *)

(* ToDo 3: take in the two coordinates, the length of the ship, and the grid and
   return true if all three hold and false if otherwise: 1) is not diagonal
   (either x value has to be the same on both or y) 2) satisfy the given length
   (use math) 3) does nto overlap a pre existing ship 4) doesn't go out of
   bounds. Length must be GREATER/GREATER THAN OR EQUAL TO???? 0. *)
val validate_ship : int -> string -> string -> string list list -> bool

(* ToDo 4: Only call after we know the user hit the ship. Check every cell
   (row/col of coordinate). Addes to string list if 1) has the same ship id 2)
   is hit*)
val hit_ship : string -> string list list -> string list

(* ToDo 5: Check if length of list is length of string list, true if so. false
   if otherwise. If hit_ship adds the beginning ship to the list then this
   holds, if otherwise then length of list - 1 = length of ship. ASK GC IF
   CONFUSED!*)
val is_sunk : string -> string list list -> bool

(* ToDo 6: Change the state of the cell. Example ship -> hit, water -> miss.*)
val change_state : string -> int -> unit

(* ToDo 7: Change the state of the cell to ship. Also add ship id to Ship type
   when initializing ship. Each ship must have different Id.*)
val change_to_ship : string -> int -> unit

(* ToDo 8: Asks user to place the ships (using get_ships) and change the cells
   on the grid depending on where the user tries to place their ship. (Use
   validate_ships to validate ship.) Example: You have 4 ships to place of
   length 4, 3, 3, and 2. Please give two coordinates in the form a1 to place
   your ship of length 4. DO EACH LENGTH INDIVIDUALLY! after they place each
   ship print the grid. call change_to_ship to give each ship a unique ship id.
   this fxn will be called individually on each ship the user sets.*)
val set_ships : int list -> string list list -> unit
