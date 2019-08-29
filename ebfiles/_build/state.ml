open QCheck
open Command

type card = {rank:string; suit:string; special:bool}
type id = int
type name = id * string
type hand = (id * (card list))
type deck = card list

exception WrongBribe
exception WrongNope
exception OutOfCards

(** The type of value representing the current state of the game.
    Record includes {a list}
    - [turn: id] - the id of the player's whose turn it is.
    - [names: name list] - the inputted names of the players.
    - [deck: deck] - the deck of cards remaining to be dealt out consisting of
    only normal playing cards.
    - [player_cards: hand list] - the hands of each player consisting only of
    normal playing cards that are "on the table".
    - [difficulty: difficulty_level] - difficulty level of the current game.
    - [score: (id * int) list] - score of each player over the rounds.
    - [sum: (id * int) list] - current sum of each player's hand. 
    - [special_deck: deck] - the deck of cards that players hit from containing
    special cards as well as normal playing cards.
    - [special_hands: hand list] - the hands of each player consisting only of
    special cards that are "in the hand".
    - [bomb_loc: (id * int) list] - a list of the locations in the deck of the 
    bombs as well as the id of the player (if possible) who placed it there.
*)
type state = 
  { turn: id;
    names: name list;
    deck: deck;
    player_cards: hand list;
    difficulty: difficulty_level;
    score: (id * int) list;
    sum: (id * int) list; 
    special_deck: deck;
    special_hands: hand list; 
    bomb_loc: (id * int) list;
  }

(** [sorted_deck] is the list representation of a single new deck of cards. *)
let sorted_deck : deck = [
  {rank="Ace";suit="Spades";special=false};
  {rank="2";suit="Spades";special=false};
  {rank="3";suit="Spades";special=false};
  {rank="4";suit="Spades";special=false};
  {rank="5";suit="Spades";special=false};
  {rank="6";suit="Spades";special=false};
  {rank="7";suit="Spades";special=false};
  {rank="8";suit="Spades";special=false};
  {rank="9";suit="Spades";special=false};
  {rank="10";suit="Spades";special=false};
  {rank="Jack";suit="Spades";special=false};
  {rank="Queen";suit="Spades";special=false};
  {rank="King";suit="Spades";special=false};

  {rank="Ace";suit="Diamonds";special=false};
  {rank="2";suit="Diamonds";special=false};
  {rank="3";suit="Diamonds";special=false};
  {rank="4";suit="Diamonds";special=false};
  {rank="5";suit="Diamonds";special=false};
  {rank="6";suit="Diamonds";special=false};
  {rank="7";suit="Diamonds";special=false};
  {rank="8";suit="Diamonds";special=false};
  {rank="9";suit="Diamonds";special=false};
  {rank="10";suit="Diamonds";special=false};
  {rank="Jack";suit="Diamonds";special=false};
  {rank="Queen";suit="Diamonds";special=false};
  {rank="King";suit="Diamonds";special=false};

  {rank="Ace";suit="Clubs";special=false};
  {rank="2";suit="Clubs";special=false};
  {rank="3";suit="Clubs";special=false};
  {rank="4";suit="Clubs";special=false};
  {rank="5";suit="Clubs";special=false};
  {rank="6";suit="Clubs";special=false};
  {rank="7";suit="Clubs";special=false};
  {rank="8";suit="Clubs";special=false};
  {rank="9";suit="Clubs";special=false};
  {rank="10";suit="Clubs";special=false};
  {rank="Jack";suit="Clubs";special=false};
  {rank="Queen";suit="Clubs";special=false};
  {rank="King";suit="Clubs";special=false};

  {rank="Ace";suit="Hearts";special=false};
  {rank="2";suit="Hearts";special=false};
  {rank="3";suit="Hearts";special=false};
  {rank="4";suit="Hearts";special=false};
  {rank="5";suit="Hearts";special=false};
  {rank="6";suit="Hearts";special=false};
  {rank="7";suit="Hearts";special=false};
  {rank="8";suit="Hearts";special=false};
  {rank="9";suit="Hearts";special=false};
  {rank="10";suit="Hearts";special=false};
  {rank="Jack";suit="Hearts";special=false};
  {rank="Queen";suit="Hearts";special=false};
  {rank="King";suit="Hearts";special=false};
]

let defuse =    {rank = "Bribe"; suit = "Wine";special=true}
let steal =     {rank = "Steal"; suit = "Kenneth";special=true}
let nope =      {rank = "Nope"; suit = "Kyrylo";special=true}
let foresee =   {rank = "Foresee"; suit = "Future";special=true}
let shuffle_c = {rank = "Shuffle"; suit = "Mystery";special=true}
let switch =    {rank = "Switch"; suit = "Hands";special=true}
let force_hit = {rank = "Force"; suit = "Hit";special=true}
let bomb = {rank = "Clarkson"; suit = "Gates461";special=true}

let get_special card = card.special
let get_suit card = card.suit
let get_rank card = card.rank
let get_id st = st.turn  
let get_deck st = st.deck
let get_hands st = List.sort compare st.player_cards
let get_score st = List.sort compare st.score
let get_diff st = st.difficulty
let get_sum st = st.sum
let get_player_cards st = st.player_cards
let get_turn st = st.turn
let get_names st = st.names
let get_special_deck st = st.special_deck
let get_special_hands st = st.special_hands
let get_bomb_loc st = st.bomb_loc

let shuffle d : deck = 
  QCheck.Gen.(generate1 (shuffle_l d))

(** [dup_dec] returns a deck containing deck [d] duplicated [n] times. *)
let rec dup_dec d n = 
  if n = 1 then d else d @ (dup_dec d (n-1))

let rec put_cards num c acc = 
  if num = 0 then acc else put_cards (num-1) c (c :: acc)

let gen_spec np = 
  (if np <= 2 then sorted_deck else dup_dec sorted_deck 2) |> 
  put_cards (6-np) defuse |> put_cards (np*3) steal |> put_cards (np*2) nope
  |> put_cards (np*3) foresee |> put_cards (np+2) shuffle_c |> 
  put_cards (np*2) switch |> put_cards (np*2) force_hit

let deal n s : (deck * hand list) = 
  (* m = #players, d = rest of deck, acc = dealt out cards *)
  let rec deal_helper m (d:deck) acc = 
    if m = 0 then (d,acc) else
      match d with
      | [] -> raise OutOfCards
      | a::b::t -> deal_helper (m-1) t (((m-1),[a;b])::acc)
      | _ -> raise OutOfCards
  in deal_helper n s []

let deal_one (d:deck) = 
  match d with
  | h::t -> (h,t)
  | _ -> raise OutOfCards

let get_value c b : int = 
  match c.rank with
  | "Ace" -> if b then 11 else 1
  | "King"
  | "Queen"
  | "Jack" -> 10
  | x -> int_of_string x

(* sums up single hand only *)
let sum_cards (lst:hand)=
  let rec sum_cards_helper l acc counter = 
    match l with 
    | [] -> acc
    | h::t when h.rank <> "Ace" -> 
      if acc + (get_value h true) > 21 && counter = 1
      then sum_cards_helper t ((get_value h true) + acc-10) 0
      else sum_cards_helper t ((get_value h true) + acc) counter
    | h::t -> 
      if acc + (get_value h true) > 21 
      then if counter = 1 && (acc + (get_value h false) > 21) 
        then sum_cards_helper t ((get_value h false) + acc - 10) 0 
        else sum_cards_helper t ((get_value h false) + acc) counter
      else sum_cards_helper t ((get_value h true) + acc) 1 
  in (fst lst), (sum_cards_helper ((snd lst)) 0 0)

let check_bust st = 
  if List.assoc (get_turn st) (get_sum st) > 21 then true else false 

(** [init_score n x] returns a list with all values equal to [x] of length n. 
    If n = 0, an empty list is returned regardless of the value of [x].
    Requires: n >= 0. 
    Example: init_score 2 false returns [false;false]. *)
let init_idlist n x = 
  let rec init_score_helper m acc accn x = 
    if m = 0 then acc else init_score_helper (m-1) ((accn,x)::acc) (accn+1) x
  in init_score_helper n [] 0 x

(** [gen_bomb np nc] returns a list of integers containing the randomly 
    generated locations of [np]-1 bombs in a deck with [nc] cards. The index 
    for the location of the bomb must be >= 0 and < [nc].
    Requires: [np] >= 2. *)
let gen_bomb np nc = 
  Random.self_init ();
  let rec helper np' nc' acc = 
    if np' = 0 then acc else 
      let x = Random.int nc' in 
      if List.mem x acc then 
        helper np' nc' acc else helper (np'-1) nc' (x::acc) 
  in helper np nc []

let gen_bombid np nc = 
  let rec helper lst n acc =
    match lst with 
    | [] -> acc
    | h::t -> helper t n ((-1,h)::acc) in 
  helper (gen_bomb np nc) np []

let init_state nl : state = 
  let n = List.length nl in
  let d = deal n (shuffle (dup_dec sorted_deck n)) in
  { turn = 0;
    names = nl;
    deck = fst d;
    player_cards = snd d;
    difficulty = Normal;
    score = init_idlist n 0;
    sum = List.map sum_cards (snd d);
    special_deck = [];
    special_hands = []; 
    bomb_loc = [];
  }

let init_special nl : state = 
  let n = List.length nl in
  let d = deal n (shuffle (dup_dec sorted_deck (n-1))) in
  let s = shuffle (gen_spec n) in 
  { turn = 0;
    names = nl;
    deck = fst d;
    player_cards = snd d;
    difficulty = Special;
    score = init_idlist n 0;
    sum = List.map sum_cards (snd d);
    special_deck = s;
    special_hands = init_idlist n [defuse]; 
    bomb_loc = gen_bombid n (List.length s);
  }

let check_win st = 
  let rec check_win_helper lst = 
    match lst with
    | [] -> (0,false)
    | (a,b)::t -> if b = 8 then (a,true) else check_win_helper t 
  in check_win_helper (get_score st)

let parse_use str = 
  match str with 
  | "steal" -> steal
  | "steal of kenneth" -> steal
  | "foresee" -> foresee
  | "foresee of future" -> foresee
  | "nope" -> raise WrongNope
  | "nope of kyrylo" -> raise WrongNope
  | "shuffle" -> shuffle_c
  | "shuffle of mystery" -> shuffle_c
  | "switch" -> switch
  | "switch of hands" -> switch
  | "force" -> force_hit
  | "force of hit" -> force_hit
  | "bribe" -> raise WrongBribe
  | "bribe of wine" -> raise WrongBribe
  | _ -> raise Invalid

let get_id_from_name st name = 
  let rec gifn_helper nl name = 
    match nl with
    | [] -> failwith "no names"
    | (k,v)::t -> if v = name then k else gifn_helper t name
  in gifn_helper (get_names st) name

let dealer_in st = 
  let nl = List.map (fun x -> snd x) (get_names st) in List.mem "dealer" nl

let check_dealer st = 
  let t = get_turn st in 
  dealer_in st && t = List.length (get_names st) - 1

let last_id st = 
  List.length (get_names st) - 1