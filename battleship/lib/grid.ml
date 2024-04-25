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
  | Destroyed

let string_of_cell = function
  | Water -> "wo"
  | Miss -> "wx"
  | Ship _ -> "so" (* Ignoring id and length *)
  | Hit _ -> "sx" (* Ignoring id and length *)
  | Destroyed -> "sxx"

let create_board size =
  if size < 5 || size > 26 then
    invalid_arg "Board size must be between 5 and 26";
  List.init size (fun _ -> List.init size (fun _ -> string_of_cell Water))

(* Function to print the entire grid with row and column labels *)
let print_grid grid =
  let print_header size =
    ANSITerminal.print_string [ ANSITerminal.on_default ] "    ";
    (* Padding for row numbers alignment *)
    for i = 0 to size - 1 do
      let letter = Char.chr (65 + i) |> String.make 1 in
      (* Convert integer to A, B, C, etc. *)
      ANSITerminal.print_string [ ANSITerminal.on_default ] (letter ^ "  ")
      (* Append two spaces for spacing *)
    done;
    ANSITerminal.print_string [ ANSITerminal.on_default ] "\n"
    (* Move to the next line after headers *)
  in
  print_header (List.length grid);
  (* Call to print the column headers *)
  List.iteri (fun i row ->
      ANSITerminal.print_string
        [ ANSITerminal.on_default ]
        (string_of_int (i + 1) ^ "  ");
      (* Print row number with padding *)
      List.iter
        (fun cell ->
          let color =
            match cell with
            | "wo" -> [ ANSITerminal.blue ]
            | "wx" -> [ ANSITerminal.cyan ]
            | "sx" -> [ ANSITerminal.magenta ]
            | "sxx" -> [ ANSITerminal.red ]
            | _ -> [ ANSITerminal.on_default ]
          in
          ANSITerminal.print_string color (cell ^ "  "))
        row;
      (* Print each cell with color and spacing *)
      ANSITerminal.print_string [ ANSITerminal.on_default ] "\n")
(* New line for each row *)

(* Additional function to print the opponent's view of the board *)
let print_their_board board =
  let masked_board =
    List.map
      (List.map (fun cell ->
           match cell with
           | "wx" | "sx" | "sxx" -> cell (* Misses and hits remain visible *)
           | _ -> "." (* Hide water and ships not hit *)))
      board
  in
  print_grid [] masked_board (* Reuse print_grid function *)

let size = 10 (* Example default size; adjust as needed *)

let coordinates str =
  ( int_of_char (String.get str 0) - int_of_char 'a',
    int_of_string (String.sub str 1 (String.length str - 1)) - 1 )
(* Converts a board coordinate like 'a1' into a tuple (0,0) *)

let num_ships_sunk = ref 0 (* Tracks the number of sunk ships *)

let get_ships size =
  match size with
  | 5 ->
      [ 2; 3 ] (* Example: For a 5x5 board, return two ships of sizes 2 and 3 *)
  | _ -> [ 2; 3; 4; 5 ]
(* Default case for larger boards *)

let validate_ship length coord grid = true
(* Placeholder that always validates ship placement. Implement actual logic
   later. *)

let hit_ship coord grid = [ "dummy" ]
(* Placeholder returning a dummy list. Implement actual logic later. *)

let is_sunk ship_id grid = false
(* Placeholder that assumes ships are not sunk. Implement actual logic later. *)

let change_state state index = ()
(* Placeholder function that does nothing. Implement actual state changing logic
   later. *)

let change_to_ship ship_id index = ()
(* Placeholder function that does nothing. Implement ship placement logic
   later. *)

let set_ships ship_lengths grid = ()
(* Placeholder function that does nothing. Implement ship setting logic
   later. *)
