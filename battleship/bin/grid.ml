(*Each box in the grid has 5 states: "wo": water, no attempt "wx": water, miss
  "so": ship, no attempt "sx": ship, hit "sxx": ship, complete ship destroyed
  Basically, o means the opponent didn't try to hit it yet, x means they did *)

(* Visual Representation of board - my board: I see everything - their board: I
   only see x's, o's, and double xx's *)

(*Prompt user to input a size for the game board, and return this size*)
let rec begin_game () =
  let () =
    print_string
      "Welcome to Battleship!\nEnter the grid size (between 5 and 20): "
  in
  let size = int_of_string (read_line ()) in
  if not (5 < size && size < 26) then begin
    let () = print_string "\nThis is not a valid size." in
    begin_game ()
  end
  else size

let size = begin_game ()

(** Helper function *)
let create_list size : string list = List.init size (fun x -> "wo")

(** Create an initial board of [size] by [size] dimension without any ships*)
let create_board size : string list list =
  List.init size (fun x -> create_list size)

(** Helper function*)
let rec print_list lst : unit =
  match lst with
  | [] -> print_string ""
  | h :: t ->
      let () = print_string h in
      let () = print_string " " in
      print_list t

(** Print the grid [lst]. Requires 5 <= len(lst) <= 26*)
let rec print_grid lst : unit =
  match lst with
  | [] -> print_string ""
  | h :: t ->
      let () = print_list h in
      let () = print_string "\n" in
      let () = print_grid t in
      ()

(** Print the grid [lst] with headers A-Z and 1-26. Requires 5 <= len(lst) <= 26*)
let print_board lst = failwith

exception NeedToImplement

(** Print the current player's grid [lst] with headers A-Z and 1-26. Requires 5
    <= len(lst) <= 26 Display all information (ships, water, hit attempts,
    sunken ships)*)
let print_my_board lst = failwith

exception NeedToImplement

(** Print the opponent's grid [lst] with headers A-Z and 1-26. Requires 5 <=
    len(lst) <= 26 Display only miss attempts, ship hits, fully sunken ships*)
let print_their_board lst = failwith

exception NeedToImplement
