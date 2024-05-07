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

(** [coordinates "A5" returns a tuple containing the pair "4,0" (x = 0, y = 4)]*)
let coordinates (str : string) =
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
(*TODO INSTEAD OF TAKING IN LENGTH, RETURN A TUPLE (bool, length) WHERE IF THE
  SHIP IS VALID, RETURN THE LENGTH OF THE SHIP*)
let validate_ship coord1 coord2 (grid : t) =
  let c1x, c1y = coordinates coord1 in
  let c2x, c2y = coordinates coord2 in
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
    (*using dummy length*)
  then (false, 0)
  else if c1x = c2x then
    let length = abs (c2y - c1y) + 1 in
    let tmp = Array.make length Water in
    (*gets the specific location we are looking at*)
    (*COULD BE PLACE FOR EXCEPTION IF OUT OF BOUNDS*)
    let () =
      for x = 0 to length - 1 do
        tmp.(x) <- grid.(c1x).(c1y + x)
      done
    in
    (* returns whether ship valid and calculated length *)
    ( Array.for_all
        (fun x ->
          match x with
          | Ship _ -> false
          | Destroyed -> false
          | _ -> true)
        tmp,
      length )
  else if c1y = c2y then
    let length = abs (c2x - c1x) + 1 in
    let tmp = Array.make length Water in
    (*COULD BE PLACE FOR EXCEPTION IF OUT OF BOUNDS*)
    let () =
      for x = 0 to length - 1 do
        tmp.(x) <- grid.(c1x + x).(c1y)
      done
    in
    ( Array.for_all
        (fun x ->
          match x with
          | Ship _ -> false
          | Destroyed -> false
          | _ -> true)
        tmp,
      abs (c2x - c1x) + 1 )
  else (false, 0)

(*TODO 4 COMPLETED *)
let hit_ship coord ship_id grid =
  let row_index, col_index = coordinates coord in
  let hit_coords = ref [] in
  (* Function that loops through the row of the given coordinate *)
  let check_row grid =
    let row = Array.get grid row_index in
    (* Access the cell in the specified column *)
    Array.iteri
      (fun col_index cell ->
        match cell with
        | Hit { id = curr_id; length = _ } when curr_id = ship_id ->
            if not (List.mem (row_index, col_index) !hit_coords) then
              hit_coords := (row_index, col_index) :: !hit_coords
        | _ -> ())
      row
  in

  (* Function that loops through the column of the given coordinate *)
  let check_col grid =
    Array.iteri
      (fun row_index row ->
        let cell = row.(col_index) in
        match cell with
        | Hit { id = curr_id; length = _ } when curr_id = ship_id ->
            if not (List.mem (row_index, col_index) !hit_coords) then
              hit_coords := (row_index, col_index) :: !hit_coords
        | _ -> ())
      grid
  in

  check_row grid;
  check_col grid;

  !hit_coords

(*TODO 5 COMPLETED *)
let is_sunk coord ship_id grid =
  let hit_coords = hit_ship coord ship_id grid in
  Array.exists
    (fun row ->
      Array.exists
        (fun cell ->
          match cell with
          | Hit { id = curr_id; length = len_ship }
            when len_ship = List.length hit_coords && curr_id = ship_id -> true
          | _ -> false)
        row)
    grid

(* TODO 7 COMPLETED *)
(*Changes the state of a cell, either upon a player hitting the cell, or upon
  positioning initial ships at the beginning of the game.*)
let change_state grid state coordinate =
  let row, col = coordinates coordinate in
  Array.mapi
    (fun x row_cell ->
      if x = row then
        Array.mapi
          (fun y col_cell -> if y = col then state else col_cell)
          row_cell
      else row_cell)
    grid

(*Changes the state of a cell SPECIFICALLY TO A SHIP of ship_id [ship_id]. It
  was previously water,for positioning initial ships at the beginning of the
  game.*)
let change_to_ship (grid : t) (ship_id : int) (ship_length : int)
    (index : int * int) =
  let row, col = index in
  grid.(row).(col) <- Ship { id = ship_id; length = ship_length }

(*TODO GET RID OF LENGTH PARAMETER FROM VALIDATE_SHIP*)
let rec ask_for_coords (grid : t) : string * string =
  print_endline "Enter the top left coordinate of a new ship. (e.g A5): ";
  let left_coord = read_line () in
  print_endline "Enter the bottom right coordinate. (e.g C5): ";
  let right_coord = read_line () in
  if String.length left_coord <> 2 || String.length right_coord <> 2 then
    let () = print_endline "That is not valid. Try again. " in
    ask_for_coords grid
  else if fst (validate_ship left_coord right_coord grid) = true then
    (left_coord, right_coord)
  else
    let () = print_endline "That is not valid. Try again. " in
    ask_for_coords grid

let set_ships (ship_lengths : int list) (grid : t) =
  let () =
    print_endline
      ("You have "
      ^ string_of_int (List.length ship_lengths)
      ^ "ships left to place.")
  in
  let c1, c2 = ask_for_coords grid in
  let is_valid, id = validate_ship c1 c2 grid in
  if is_valid = true then
    let c1x, c1y = coordinates c1 in
    let c2x, c2y = coordinates c2 in
    if c1x < c2x then
      for y = c1x to c2x + 1 do
        let () = change_to_ship grid id (abs (c1x - c2x)) (y, c1y) in
        ()
      done
    else if c1x > c2x then
      for y = c2x to c1x + 1 do
        let () = change_to_ship grid id (abs (c1x - c2x)) (y, c1y) in
        ()
      done
    else if c1y < c2y then
      for y = c1y to c2y + 1 do
        let () = change_to_ship grid id (abs (c1y - c2y)) (y, c1x) in
        ()
      done
    else if c1y > c2y then
      for y = c2y to c1y + 1 do
        let () = change_to_ship grid id (abs (c1y - c2y)) (y, c1x) in
        ()
      done
    else
      let () = print_string "Something went wrong in set_ships." in
      ()
