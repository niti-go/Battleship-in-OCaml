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
  | Ship
  | Hit
  | Destroyed

let string_of_cell = function
  | Water -> "wo"
  | Miss -> "wx"
  | Ship -> "so"
  | Hit -> "sx"
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
