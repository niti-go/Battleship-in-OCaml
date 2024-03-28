val spacing : string
(** [spacing] is the standard spacing between all strings when printing. *)

val begin_game : unit -> int
(**[begin_game] asks the user for a valid size of the grid to be made. Returns
   this size, which is the length of rows/columns in the table. *)

val size : int
(** [size] is the length of rows and columns of the grid, which must be square. *)

val create_board : int -> string list list
(** Create an initial board of [size] by [size] dimension without any ships. *)

val print_grid : string list list -> int -> unit
(** Print the grid [lst]. Requires 5 <= len(lst) <= 26. [rowNum] keeps track of
    what row we are printing on, and it starts at 1. *)

val print_board : string list list -> unit
(** Print the grid [lst] with headers A-Z and 1-26. Requires 5 <= len(lst) <= 26*)

val print_my_board : string list list -> unit
(** [print_my_board] prints the current player's grid [lst] with headers A-Z and
    1-26. Requires 5 <= len(lst) <= 26 Display all information (ships, water,
    hit attempts, sunken ships) *)

val print_their_board : string list list -> unit
(** [print_their_board] prints the opponent's grid [lst] with headers A-Z and
    1-26. Requires 5 <= len(lst) <= 26 Display only miss attempts (wx), ship
    hits (sx), fully sunken ships (sxx)*)
