(*Each box in the grid has 5 states: "wo": water, no attempt "wx": water, miss
  "so": ship, no attempt "sx": ship, hit "sxx": ship, complete ship destroyed
  Basically, o means the opponent didn't try to hit it yet, x means they did *)

(* Visual Representation of board - my board: I see everything - their board: I
   only see x's, o's, and double xx's *)

(*Prompt user to input a size for the game board, and return this size*)

(*Instructions to get emoji library Run opam update Run opam upgrade run opam
  install emoji.1.1.0 *)

(**AF: The battleship grid is represented as a string list list. So row 1 of the
   grid is the first list in the list, and so on. The coordinates on the grid
   will be the row and column in the string list list. *)

(**RI: The grid size should not change, and it should always be square. *)

let spacing = "  "

let rec begin_game () =
  let () =
    print_string
      "Welcome to Battleship!\nEnter the grid size (between 5 and 26): "
  in
  let size = int_of_string (read_line ()) in
  if not (5 <= size && size <= 26) then begin
    let () = print_string "\nThis is not a valid size. " in
    begin_game ()
  end
  else size

let size = begin_game ()

let create_board size : string list list =
  (* Helper function *)
  let create_list size : string list = List.init size (fun x -> "wo") in
  List.init size (fun x -> create_list size)

(** Helper function*)
let rec print_list lst : unit =
  match lst with
  | [] -> print_string ""
  | h :: t ->
      let () = print_string h in
      let () = print_string " " in
      print_list t

let rec print_colored_list lst =
  match lst with
  | [] -> print_string ""
  | h :: t ->
      if h = "wo" then
        let () =
          ANSITerminal.print_string
            [ ANSITerminal.blue; ANSITerminal.on_default ]
            (h ^ " ")
        in
        print_colored_list t
      else if h = "wx" then
        let () =
          ANSITerminal.print_string
            [ ANSITerminal.cyan; ANSITerminal.on_default ]
            (h ^ " ")
        in
        print_colored_list t
      else if h = "sx" then
        let () =
          ANSITerminal.print_string
            [ ANSITerminal.magenta; ANSITerminal.on_default ]
            (h ^ " ")
        in
        print_colored_list t
      else if h = "ssx" then
        let () =
          ANSITerminal.print_string
            [ ANSITerminal.red; ANSITerminal.on_default ]
            (h ^ " ")
        in
        print_colored_list t
      else
        let () = print_string h in
        let () = print_string " " in
        print_colored_list t

let rec print_grid lst (rowNum : int) : unit =
  match lst with
  | [] -> print_string ""
  | h :: t ->
      let () = print_colored_list h in
      let () = print_string (string_of_int rowNum ^ "\n") in
      let () = print_grid t (rowNum + 1) in
      ()

let print_board lst =
  List.iter print_string
    (List.map
       (fun x -> String.make 1 x ^ spacing)
       (List.init (List.length lst) (fun x -> Char.chr (x + 65))));
  print_endline "";
  print_grid lst 1

let print_my_board lst = print_board lst

let print_their_board lst =
  let tmp_lst =
    List.map
      (List.map (fun x -> if x = "wx" || x = "sx" || x = "sxx" then x else "."))
      lst
  in
  print_board tmp_lst

(* ANSITerminal.print_string [ ANSITerminal.green; ANSITerminal.on_default ]
   (String.make 1 guess_word.[index]) *)
