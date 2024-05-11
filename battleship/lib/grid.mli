(* lib/grid.mli: Interface for the Battleship grid module *)

type cell =
  | Water
  | Miss
  | Ship of {
      id : int;
      length : int;
    }
  | Hit of {
      id : int;
      length : int;
    }
  | Destroyed (* destroyed = sunk *)
  | Hidden (*for printing the opponent's board, the cell type is "hidden"*)

(** Type representing the different possible states of a cell in the game grid *)

(* COMPLETED ToDo 1: create type for grid. *)
type t = cell array array

val string_of_cell : cell -> string

val coordinates : string -> int * int
(** [coordinates str] is the integer tuple Cartesian coordinates of a string
    indicating a position on the board. Requires that [str] is in the form where
    first character is "A-Z" and second character is a number. *)

(* add 1 when is_sunk is true*)
val num_ships_sunk : int ref

(* more time potential fxns: low and high fxn, low outputs a and high outputs
   1. *)

val create_board : int -> t
(** Create a battleship grid of a given size with all cells initialized to Water *)

val print_grid : t -> unit
(** Print a grid to the console with color coding for different cell states *)

val print_their_board : t -> unit

(*Completed todo 2*)
val get_ships : int -> int list
(** [get_ships size] takes in the size of the board and determines a list of
    ships that the user must place and what lengths they should be. *)

(* ToDo 3: take in the two coordinates and the grid and return true if all three
   hold and false if otherwise: 1) is not diagonal (either x value has to be the
   same on both or y) 2) does not overlap a pre existing ship 3) doesn't go out
   of bounds. *)

val validate_ship : string -> string -> t -> bool * int

(* COMPLETED ToDo 4: We call this each time the user hits a ship. This checks
   every cell in the row and column of that coordinate for other ships of that
   ID. (row/col of coordinate). A cell will be added to a "hit list" (string
   list of cells) if 1) it has the same ship id 2) is hit. This returns a list
   of other ship cells of that same ID that have been hit. TAKES IN COORDINATE,
   GRID, AND SHIP ID*)
val hit_ship : string -> int -> t -> (int * int) list

(* COMPLETED ToDo 5: Returns true if length of "hit list" is length of ship ID
   list (number of ships of that ID), false if otherwise. This means that the
   ship that was just hit has sunk the entire ship. ASK GC IF CONFUSED!*)
val is_sunk : string -> int -> t -> bool

(*ToDo 6: Change the state of the cell to ship. Used when placing initial ships.
  Also add ship id to Ship type when initializing ship. Each ship must have
  different Id.*)
val change_to_ship : t -> int -> int -> int * int -> unit

(* COMPLETED ToDo 7: Change the state of the cell. Example ship -> hit, water ->
   miss.*)
val change_state : t -> string -> unit

(* ToDo 8: Asks user to place the ships (using get_ships) and change the
   necessary cells on the grid to "ship". (Call validate_ships to validate
   ship.) Example: You have 4 ships to place of length 4, 3, 3, and 2. Please
   give two coordinates in the form a1 to place your ship of length 4. DO EACH
   LENGTH INDIVIDUALLY! after they place each ship print the grid. call
   change_to_ship to give each ship a unique ship id. this fxn will be called
   individually on each ship the user sets.*)
val set_ships : int list -> t -> unit
