(* lib/grid.ml: Module for Battleship grid representation and utilities *)

(** Abstract Function (AF): The battleship grid is represented as a string list
    list, where each string can be "wo", "wx", "so", "sx", "sxx" corresponding
    to different states of a cell in the game grid. This representation helps
    visualize the game state with colors. *)

(** Representation Invariant (RI): The grid is always a square with dimensions
    between 5 and 26. *)
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

type t = cell array array

let string_of_cell = function
  | Water -> "wo"
  | Miss -> "wx"
  | Ship _ -> "so" (* Ignoring id and length *)
  | Hit _ -> "sx" (* Ignoring id and length *)
  | Destroyed -> "sxx"
  | Hidden -> "."

let create_board size =
  if size < 5 || size > 26 then
    invalid_arg "Board size must be between 5 and 26";
  Array.make size (Array.make size Water)
(* List.init size (fun _ -> List.init size (fun _ -> string_of_cell Water)) *)

(* Function to print the entire grid with row and column labels *)
let print_grid (grid : t) =
  let print_header size =
    ANSITerminal.print_string [ ANSITerminal.on_default ] "    ";
    (* Padding for row numbers alignment *)
    for i = 0 to size - 1 do
      let letter = Char.chr (65 + i) |> String.make 1 in
      (* Convert integer to A, B, C, etc. *)
      ANSITerminal.print_string [ ANSITerminal.on_default ] (letter ^ "   ")
      (* Append two spaces for spacing *)
    done;
    ANSITerminal.print_string [ ANSITerminal.on_default ] "\n"
    (* Move to the next line after headers *)
  in
  Array.iteri
    (fun i row ->
      ANSITerminal.print_string
        [ ANSITerminal.on_default ]
        (string_of_int (i + 1) ^ "  ");
      (* Print row number with padding *)
      Array.iter
        (fun cell ->
          let color =
            match string_of_cell cell with
            | "wo" -> [ ANSITerminal.blue ]
            | "wx" -> [ ANSITerminal.cyan ]
            | "sx" -> [ ANSITerminal.magenta ]
            | "sxx" -> [ ANSITerminal.red ]
            | _ -> [ ANSITerminal.on_default ]
          in
          ANSITerminal.print_string color (string_of_cell cell ^ "  "))
        row;
      (* Print each cell with color and spacing *)
      ANSITerminal.print_string [ ANSITerminal.on_default ] "\n")
    grid;
  (* New line for each row *)
  print_header (Array.length grid)
(* Call to print the column headers at the bottom *)

(* Additional function to print the opponent's view of the board *)
let print_their_board (board : t) =
  let masked_board =
    Array.map
      (Array.map (fun cell ->
           match cell with
           | Hit _ -> cell
           | Miss -> cell
           | Destroyed -> cell (* Misses and hits remain visible *)
           | _ -> Hidden (* Hide water and ships not hit *)))
      board
  in
  print_grid masked_board (* Reuse print_grid function *)

let coordinates str =
  let col = String.get str 0 in
  (* This gets the letter *)
  let row = String.sub str 1 (String.length str - 1) in

  (* This gets the number *)

  (* Convert the column letter to a zero-based index: Subtract the ASCII value
     of 'A' from the ASCII value of the letter to get 0 for 'A', 1 for 'B',
     etc. *)
  let x = int_of_char col - int_of_char 'A' in

  (* Convert the row string to an integer and adjust it to be zero-based:
     Subtract 1 from the integer value of the row string ('2' becomes 1, '1'
     becomes 0, etc.). *)
  let y = int_of_string row - 1 in

  (y, x)
(* Return the coordinates as (y, x) to match the grid's row and column
   indexing *)

(*Function takes a coordinate string like 'B2' and returns (1, 1), where 'B' is
  the second column and '2' is the second row, typical zero-based indexing in
  programming. *)

let num_ships_sunk = ref 0 (* Tracks the number of sunk ships *)

let get_ships size =
  (* Example: For a 5x5 board, return a ship of size 4, two ships of size 3, and
     a ship of size 2 *)
  if size >= 5 && size <= 9 then [ 4; 3; 3; 2 ]
  else if size >= 10 && size <= 14 then [ 4; 4; 3; 3; 2; 2 ]
  else if size >= 15 && size <= 18 then [ 5; 5; 4; 3; 3; 3; 2; 2 ]
  else if size >= 19 && size <= 21 then [ 7; 5; 4; 4; 3; 3; 2 ]
  else if size >= 22 && size <= 24 then [ 9; 8; 6; 5; 5; 4; 3; 3; 3; 2; 2 ]
  else if size >= 25 && size <= 26 then
    [ 10; 9; 9; 8; 8; 6; 4; 4; 4; 3; 3; 3; 2 ]
  else [ 2; 3; 4; 5 ]
(* Default case for larger boards *)

(*Possible edge case, what if coord1 and coord2 are the same? is ship still
  valid?*)
(* Should always be in the form where coord1 is leftmost/topmost coordinate and
   coord2 is rightmost/lowest coordinate*)
let validate_ship length coord1 coord2 (grid : t) =
  let c1x, c1y = coordinates coord1 in
  let c2x, c2y = coordinates coord2 in
  let tmp = Array.make length Water in
  (*checks if ship is out of bounds, then diagonal*)
  if
    c1x < 0
    || c1x >= Array.length grid.(0)
    || c2x < 0
    || c2x >= Array.length grid.(0)
    || c1y < 0
    || c1y >= Array.length grid
    || c2y < 0
    || c2y >= Array.length grid
  then false
  else if c1x = c2x then
    (*checks if coordinate ship length matches given length *)
    abs (c2y - c1y) + 1 = length
    &&
    (*gets the specific location we are looking at*)
    (*COULD BE PLACE FOR EXCEPTION IF OUT OF BOUNDS*)
    let () =
      for x = 0 to length - 1 do
        tmp.(x) <- grid.(c1x).(c1y + x)
      done
    in
    Array.for_all
      (fun x ->
        match x with
        | Ship _ -> false
        | Destroyed -> false
        | _ -> true)
      tmp
  else if c1y = c2y then
    abs (c2x - c1x) + 1 = length
    &&
    (*COULD BE PLACE FOR EXCEPTION IF OUT OF BOUNDS*)
    let () =
      for x = 0 to length - 1 do
        tmp.(x) <- grid.(c1x + x).(c1y)
      done
    in
    Array.for_all
      (fun x ->
        match x with
        | Ship _ -> false
        | Destroyed -> false
        | _ -> true)
      tmp
  else false

let hit_ship (coord : string) (grid : t) = [ "dummy" ]
(* Placeholder returning a dummy list. Implement actual logic later. *)

let is_sunk ship_id (grid : t) = false
(* Placeholder that assumes ships are not sunk. Implement actual logic later. *)

let change_state state index = ()
(* Placeholder function that does nothing. Implement actual state changing logic
   later. *)

let change_to_ship ship_id index = ()
(* Placeholder function that does nothing. Implement ship placement logic
   later. *)

let set_ships ship_lengths (grid : t) = ()
(* Placeholder function that does nothing. Implement ship setting logic
   later. *)
