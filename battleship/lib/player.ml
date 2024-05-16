let () = Random.self_init ()

(* AF: Each player is represented as a record of Player.t {name, board,
   ships_set, ships_sunk, missed turns}, with information specific to each
   player over the course of the game. *)
(*RI: is_ships_set must start as false when a player is initialized and
  num_ships_sunk and missed_turns must be nonnegative. *)

type t = {
  name : string;
  board : Grid.t;
  mutable is_ships_set : bool;
  mutable num_ships_sunk : int;
  mutable missed_turns : int;
}

let create_player n b =
  {
    name = n;
    board = b;
    is_ships_set = false;
    num_ships_sunk = 0;
    missed_turns = 0;
  }

(* if time, make it so counts misses in a row to be more fair*)
let allowed_turn player =
  let misses =
    List.fold_left ( + ) 0 (Grid.get_ships (Array.length player.board))
  in
  if player.missed_turns >= misses then
    let () =
      print_endline
        ("Penalty for " ^ string_of_int misses ^ " misses: skip 1 turn")
    in
    let () = player.missed_turns <- 0 in
    false
  else true

(* return whether or not [player] has a mini-game associated with their turns.*)
let has_mini_game (player : t) : bool = true
(*do random.int from 1 to n depending on n game ideas I have 1. guess the number
  from 1 to 5 2. coin flip heads or tails 3. trivia about camels 4. addition and
  multiplication problems 5. qs about ocaml if we're really down bad *)

(*simulates a minigame for the player, involving user interaction, and returns
  true if the player succeeds/wins*)
let multiplication_game () : bool =
  let num1 = Random.int 11 in
  let num2 = Random.int 11 in
  let answer = num1 * num2 in
  let () =
    print_endline
      ("What is " ^ string_of_int num1 ^ " * " ^ string_of_int num2 ^ ": ")
  in
  let input = read_line () in
  let users_num = int_of_string_opt input in
  match users_num with
  | None -> false
  | Some input -> if input = answer then true else false
(* let anagram_game () : bool = let words = BatList.of_enum (BatFile.lines_of
   "lib/dictionary.txt") in let word_choice = List.nth words (Random.int
   (List.length words)) in *)
