open State

(** [format_midtop c] returns the string representation of the middle top row
    of card [c]. *)
let format_midtop c = 
  match get_rank c with
  | "Ace" -> "│A    │"
  | "Jack" -> "│J    │"
  | "Queen" -> "│Q    │"
  | "King" -> "│K    │"
  | "10" -> "│10   │"
  | "Bribe" -> "│\\~~~/│"
  | "Steal" -> "│     │"
  | "Nope" -> "│ \\ / │"
  | "Foresee" -> "│  _✧ │"
  | "Shuffle" -> "│  ?  │"
  | "Switch" -> "│ ▲ ╻ │"
  | "Force" -> "│  ║  │"
  | "Clarkson" -> "│     │"
  | x -> "│" ^ x ^ "    │"

(** [format_midmid c] returns the string representation of the middle row
    of card [c]. *)
let format_midmid c = 
  match get_suit c with
  | "Spades" -> "│  ♠  │"
  | "Diamonds" -> "│  ♦  │"
  | "Clubs" -> "│  ♣  │"
  | "Hearts" -> "│  ♥  │"
  | "Wine" -> "│ \\_/ │"
  | "Kenneth" -> "│ ◘=◘ │"
  | "Kyrylo" -> "│  ╳  │"
  | "Future" -> "│ (_) │"
  | "Mystery" -> "│ ? ? │"
  | "Hands" -> "│ ╏ ╏ │"
  | "Hit" -> "│  ▼  │"
  | "Gates461" -> "│ ,-* │"
  | _ -> "unknown"

(** [format_midbot c] returns the string representation of the middle bottom 
    row of card [c]. *)
let format_midbot c = 
  match get_rank c with
  | "Ace" -> "│    A│"
  | "Jack" -> "│    J│"
  | "Queen" -> "│    Q│"
  | "King" -> "│    K│"
  | "10" -> "│   10│"
  | "Bribe" -> "│ _|_ │"
  | "Steal" -> "│  ▔  │"
  | "Nope" -> "│ / \\ │"
  | "Foresee" -> "│ /_\\ │"
  | "Shuffle" -> "│  ?  │"
  | "Switch" -> "│ ╹ ▼ │"
  | "Force" -> "│  ▔  │"
  | "Clarkson" -> "│(⍟_⍟)│"
  | x -> "│    " ^ x ^ "│"

(** [top nc] returns the string representation of the top of [nc] cards. *)
let rec top nc = 
  if nc = 1 then "┌─────┐" else "┌─────┐ " ^ top (nc-1)

(** [middle cl b i] returns the string representation of the specified middle 
    row of the cards in [cl] where all cards are shown if [b] is true. 
    If [i] = 0, return the top middle row, if [i] = 1, return the middle row,
    else return the bottom middle row. *)
let middle cl b i =
  let rec middle_helper i = function
    | [] -> ""
    | h::t when i = 0 -> format_midtop h ^ " " ^ middle_helper i t
    | h::t when i = 1 -> format_midmid h ^ " " ^ middle_helper i t
    | h::t -> format_midbot h ^ " " ^ middle_helper i t
  in if b then middle_helper i cl 
  else "│░░░░░│ " ^ middle_helper i (match cl with | [] -> [] | _::t -> t)

(** [print_bottom nc] returns the string representation that contains the 
    bottom of [nc] cards. *)
let rec bottom nc = 
  if nc = 1 then "└─────┘" else "└─────┘ " ^ bottom (nc-1)

let format_cards (cl:card list) b = 
  if cl = [] then "" else
    let nc = List.length cl in
    top nc ^ "\n" ^ middle cl b 0 ^ "\n" ^ middle cl b 1 ^ "\n" ^ 
    middle cl b 2 ^ "\n" ^ bottom nc

let format_cards_text (cl:card list) b = 
  let rec print_all = function
    | [] -> ""
    | h::[] -> h.rank ^ " of "^ h.suit 
    | h::t -> h.rank ^ " of "^ h.suit ^ ", " ^ (print_all t)
  in if b then print_all cl 
  else ((Char.escaped (Char.chr 95))^(Char.escaped (Char.chr 95)) 
        ^ ", " ^ print_all (match cl with |[] -> [] | h::t -> t))

let assign_name i nl b = 
  let s = List.assoc i nl in 
  if b then s ^ ": " else s

(** [print_state st] returns the formatted string representation of the 
    current hands of each player in the game given a state [st] where for the
    current user, all cards are shown while the first card of every other
    player is hidden. *)
let format_state st = 
  let rec print_help lst i l1 l2 l3 = 
    match lst with
    | [] -> ((l1,l3),l2)
    | (k,v)::t -> 
      let s = "\n" ^ (assign_name k (get_names st) true) ^ "\n" ^ 
              (format_cards v (k = i)) in
      if k < i then print_help t i (s ^ l1) l2 l3 
      else if k = i then print_help t i l1 (s ^ l2) l3 
      else print_help t i l1 l2 (s ^ l3)
  in print_help (List.rev (get_hands st)) (get_turn st) "" "" ""

(** [print_ai_state st] returns the formatted string representation of the 
    current hands of each player in the game given a state [st] where all
    cards are shown. *)
let format_ai_state st = 
  let rec print_ai lst acc = 
    match lst with
    | [] -> ("","")
    | (k,v)::[] -> (acc , ((assign_name k (get_names st) true) ^
                           "\n" ^ (format_cards v true) ^ "\n"))
    | (k,v)::t -> print_ai t (acc ^ (assign_name k (get_names st) true) ^
                              "\n" ^ (format_cards v true) ^ "\n") in
  print_ai (get_hands st) ""

let print_hands st = 
  let t = get_turn st in
  if not (check_dealer st)
  then (
    let s = format_state st in
    if get_diff st = Normal then (
      print_string "\nThe current hands are:\n";
      print_string (fst (fst s));
      ANSITerminal.(print_string [red] (snd s)); 
      print_string (snd (fst s)))
    else (
      print_string "\nThe current table hands are:\n";
      print_string (fst (fst s));
      ANSITerminal.(print_string [red] (snd s)); 
      print_string (snd (fst s) ^ "\n");
      ANSITerminal.(print_string [red] (
          ( "\n" ^ assign_name t (get_names st) false) 
          ^ "'s special cards are: " ^ "\n" ^ "[ " ^
          (format_cards_text (List.assoc t (get_special_hands st)) true)
          ^ " ]" ^ "\n" ^
          (format_cards (List.assoc t (get_special_hands st)) true) 
          ^ "\n"));))
  else let s = format_ai_state st in
    print_string ("\nThe current hands are:\n" ^ (fst s));
    ANSITerminal.(print_string [black] (snd s))

let print_hand st = 
  let id = get_turn st in
  let cards = List.assoc id (get_hands st) in
  if get_diff st = Special then 
    let special = st |> get_special_hands |> List.assoc id in
    let num_special = special |> List.length |> string_of_int in
    print_string (format_cards cards false);
    if id = (List.length (get_hands st) - 1) then
      print_string ("\nThe dealer has " ^ num_special ^ " special card" ^ 
                    if num_special = "1" then "." else "s.")
    else 
      (print_string ("\n[ " ^ format_cards_text special true ^" ]");
       print_string ("\n" ^ format_cards special true))
  else print_string (format_cards cards true)

let print_score st = 
  let rec print_score_helper l =
    match l with 
    | [] -> ""
    | (k,v)::[] -> assign_name k (get_names st) true ^ string_of_int v
    | (k,v)::t -> assign_name k (get_names st) true ^ string_of_int v ^ ", " ^ 
                  print_score_helper t
  in print_score_helper (get_score st)

let print_sum st = 
  let rec print_sum_h ns lst i = 
    match lst with
    | [] -> ""
    | (k,v)::t -> if k = i then (string_of_int v) ^ "\n" 
      else print_sum_h ns t i in 
  print_sum_h (get_names st) (get_sum st) (get_turn st)

let fancy () =   
  print_string ("\n" ^ "
  ♦ ♣ ♥ ♠ —————————————————————————————————————————————————————— ♠ ♥ ♣ ♦");
  ANSITerminal.(print_string [red;Bold;Blink] 
                  ("
               { 💥 💥 💥 EXPLODING BLACKJACK 💥 💥 💥 }"));
  print_string ("
  ♦ ♣ ♥ ♠ —————————————————————————————————————————————————————— ♠ ♥ ♣ ♦\n")

let divider () = 
  print_string "\n—————————————————————————————————————————————————————————\n"

let help op = 
  let s = String.concat " " op in 
  match s with
  | "steal" | "steal of kenneth" | "mask" -> 
    print_string ("\nSteal allows you to steal a card from another player." ^
                  "\nYou can only steal from their faceup cards on the table.")
  | "foresee" | "foresee of future" | "crystal ball" -> 
    print_string ("Foresee allows you to see the next three cards in the deck." 
                  ^ "\nIf there are fewer than three cards left, " ^ 
                  "then you will see all of the remaining cards.")
  | "nope" | "nope of kyrylo" | "x" ->
    print_string ("\nNope allows you to counter an action card that another" ^ 
                  "player uses against you.")
  | "shuffle" | "shuffle of mystery" | "question mark" | "?" -> 
    print_string ("\nShuffle allows you to reshuffle the deck.")
  | "switch" | "switch of hands" -> 
    print_string ("\nSwitch lets you to swap table hands with another player.")
  | "force" | "force of hit" -> 
    print_string ("\nForce allows you to force the next player to hit.")
  | "bribe" | "bribe of wine" | "wine" -> 
    print_string ("\nBribe allows you to safely defuse a bomb.")
  | "bomb" | "bomb of clarkson" ->
    print_string ("\nIf hit, you will lose 6 pts unless you use a defuse card.")
  | _ -> 
    print_string ("\nUnknown card.")