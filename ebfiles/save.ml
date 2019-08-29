open State
open Command
open Unix
open Filename
open Yojson.Basic
open Yojson.Basic.Util

(** [to_difficulty s] parses a string s into a difficulty level. If [s] is not
    a valid difficulty level, a failure is raised. *)
let to_difficulty: string -> difficulty_level = function
  |"Normal" -> Normal
  |"Special" -> Special
  |"Quit" -> Quit
  |"Rules" ->Rules
  |_-> failwith "not a valid difficulty1"

(** [nameTuple_from_json (x,y)] parses json to type name.
    Requires: [(x,y)] is a valid JSON representation of a name tuple. *)
let nameTuple_from_json (x,y) =
  (int_of_string x, to_string y)

(** [nameTuple_from_json (x,y)] parses json to type int * int to be used for 
    both score, sum, and bomb_loc.
    Requires: [(x,y)] is a valid JSON representation of an int tuple. *)
let intTuple_from_json (x,y) =
  (int_of_string x, (to_int y))

(** [card_of_json json] parses json to type card.
    Requires: [json] is a valid JSON representation of a card. *)
let card_of_json json = {
  rank = json |> member "rank" |> to_string;
  suit = json |> member "suit" |> to_string;
  special = json |> member "special" |> to_bool
}

(** [card_of_json json] parses json to type hand.
    Requires: [json] is a valid JSON representation of a hand. *)
let hand_of_json json = 
  let identity = int_of_string (fst json) in
  let card_list = json |> snd |> to_list |> List.map card_of_json in 
  (identity,card_list)

let from_json json = 
  {
    turn = json |> member "turn" |> to_int;
    names = json |> member "names" |> to_assoc |> List.map nameTuple_from_json; 
    deck = json |> member "deck" |> to_list |> List.map card_of_json;
    player_cards = json |> member "player_cards" |> to_assoc 
                   |> List.map hand_of_json;
    difficulty= json |> member "difficulty" |> to_string |> to_difficulty;
    score = json |> member "score" |> to_assoc |> List.map intTuple_from_json;
    sum = json |> member "sum" |> to_assoc |> List.map intTuple_from_json;
    special_deck = json |> member "special_deck" |> to_list 
                   |> List.map card_of_json; 
    special_hands = json |> member "special_hands" |> to_assoc 
                    |> List.map hand_of_json; 
    bomb_loc = json |> member "bomb_loc" |> to_assoc 
               |> List.map intTuple_from_json; 
  }

(** [from_name (id,n)] returns a json representation of (id,n). *)
let from_name (id,name) =
  (string_of_int id,`String name)

(** [from_card c] returns a json representation of card [c]. *)
let from_card card = 
  `Assoc [("rank", `String (card.rank));
          ("suit", `String (card.suit));
          ("special", `Bool (card.special))]

(** [from_hand h] returns a json representation of hand [h]. *)
let from_hand (hand:hand) = 
  ((hand |> fst |> string_of_int), `List (hand |> snd |> List.map from_card))

(** [from_diff d] returns a json representation of difficulty_level [d]. *)
let from_diff d =
  match d with  
  | Normal -> "Normal"
  | Special -> "Special"
  | Quit -> "Quit"
  | Rules -> "Rules"

(** [from_intTuple (x,y)] returns a json representation of (x,y) where x is an 
    int and y is an int. *)
let from_intTuple (x,y)=
  (string_of_int x, `Int y) 

let to_json (st:state) :json= 
  `Assoc[
    ("turn", `Int (st.turn));
    ("names", `Assoc (st.names |> List.map from_name));
    ("deck", `List (List.map from_card (st.deck)) );
    ("player_cards", `Assoc (st.player_cards |> List.map from_hand));
    ("difficulty", `String (st.difficulty|>from_diff));
    ("score", `Assoc(st.score |> List.map from_intTuple));
    ("sum", `Assoc(st.sum |> List.map from_intTuple));
    ("special_deck", `List (List.map from_card (st.special_deck)));
    ("special_hands", `Assoc (st.special_hands |> List.map from_hand));
    ("bomb_loc",`Assoc(st.bomb_loc |> List.map from_intTuple))
  ]

let rec get_files dir acc =
  match readdir dir with
  | exception End_of_file -> acc
  | x -> if check_suffix x ".json" then 
      let cut_json = String.sub x 0 (String.length x - 5) in 
      get_files dir ((String.sub cut_json 16 (String.length cut_json - 16), 
                      String.sub cut_json 0 16)::acc)
    else get_files dir acc

let curr_date () = 
  let curr_stats = time() |> localtime in 
  let form_num num = 
    if num < 10 
    then num|> string_of_int |> (^) "0" 
    else num |> string_of_int in 
  let mm = curr_stats.tm_mon + 1 |> form_num in 
  let dd = curr_stats.tm_mday |> form_num in 
  let yy = (curr_stats.tm_year + 1900) mod 100 |> string_of_int in 
  let hh = curr_stats.tm_hour  |> form_num in 
  let minmin = curr_stats.tm_min |> form_num in 
  mm ^ "-" ^ dd ^ "-" ^ yy ^ " " ^ hh ^ "h" ^ minmin ^ "m "

let get_keys assoc_list =
  let get_key = function | (x,y) -> x
  in List.map get_key assoc_list

let get_fullnames (assoc_list: (string * string) list) = 
  let append = function |(x,y) -> y ^ x
  in List.sort compare (List.map append assoc_list)
