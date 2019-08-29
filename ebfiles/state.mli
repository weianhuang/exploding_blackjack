(** Representation of the dynamic game state. *)
open Yojson.Basic
open Command

(** The type of value representing a single card. *)
type card = {rank:string; suit:string; special:bool}

(** The type of value representing the unique id of a player.

    Requires: [id] >= 0 *)
type id = int

(** The type of value representing the inputted name of a player.

    Requires: [id] be a valid id. *)
type name = id * string

(** The type of value representing the cards, or the hand, each player has. *)
type hand = id * (card list)

(** The type of value representing a deck of cards waiting to be dealt out. *)
type deck = card list

(** Raised if the deck runs out of cards waiting to be dealt out. *)
exception OutOfCards

(** Raised if a user tries to use a defuse card in an invalid situation. *)
exception WrongBribe

(** Raised if a user tries to use a nope card in an invalid situation. *)
exception WrongNope

(** [steal] represents the special steal card. *)
val steal: card

(** [shuffle_c] represents the special shuffle card. *)
val shuffle_c: card

(** [nope] represents the special nope card. *)
val nope: card

(** [foresee] represents the special foresee card. *)
val foresee: card

(** [switch] represents the special switch card. *)
val switch: card

(** [force_hit] represents the special force hit card. *)
val force_hit: card

(** [bomb] represents the special bomb card. *)
val bomb: card

(** [defuse] represents the special defuse card. *)
val defuse: card

(** The type of value representing the current state of the game. *)
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

(** [get_suit card] returns the suit of the given card. *)
val get_suit: card -> string

(** [get_rank card] returns the rank of the given card. *)
val get_rank: card -> string

(** [get_special card] returns true if the given card is a speal card. *)
val get_special: card -> bool

(** [get_turn st] returns the id of the player who has the current turn. *)
val get_turn: state -> id

(** [get_deck st] returns the deck of undealt cards for state [st]. *)
val get_deck: state -> deck

(** [get_deck st] returns the cards of each player for state [st]. *)
val get_hands: state -> hand list

(** [get_score st] returns the scores of the players for state [st]. *)
val get_score: state -> (id * int) list

(** [get_diff st] returns the difficulty level of state [st]. *)
val get_diff: state -> Command.difficulty_level

(** [get_sum st] returns the sum of each player's hand for state [st]. *)
val get_sum: state -> (id * int) list

(** [get_player_cards st] returns an association list in which the keys are 
    player id's and the values are lists repreenting the hands of each 
    corresponding player.*)
val get_player_cards : state -> (id * (card list)) list

(** [get_names st] returns the list of names of the players in state [st]. *)
val get_names: state -> name list 

(** [get_special_deck st] returns the deck containing the special cards. *)
val get_special_deck: state -> deck

(** [get_special_hands st] returns the list of hands for all players that 
    contain special cards. *)
val get_special_hands: state ->  hand list

(** [get_special_hands st] returns the list of the bomb locations along with
    the player id if that player knows where it is. *)
val get_bomb_loc: state -> (id * int) list

(** [shuffle d] returns a randomly shuffled version of deck [d]. *)
val shuffle: card list -> deck

(** [deal n d] returns a tuple representing the result of dealing deck [d]. 
    With [n] players and deck (a;b;c;d;...), the first player receives cards 
    a,b and the second player receives cards c,d, and so on. 

    Requires: [n] >= 2. 

    @raise OutOfCards if [d] does not have enough cards for each player*)
val deal: int -> deck -> (deck * hand list)

(** [dealone d] returns a tuple representing the result of dealing the top
    card from deck [d] as well as the remaining deck.

    @raise OutOfCards if [d] does not contain any cards *)
val deal_one: deck -> card * deck

(** [sum_cards lst] sums the values of the cards in [lst]. An Ace can be valued
    at either 1 or 11, but will be valued at 11 unless the sum of the values is
    greater than 21. Face cards are 10 and any other card is its rank value. 

    Example: 
    - [sum_cards (1, [Ace, Ace, Ace, Ace])] is [(1, 14)],
    - [sum_cards (2, [8,Ace,10])] is [(2, 19)]. *)
val sum_cards: hand -> id * int

(** [check_bust state] returns true if the current player whose turn it is has
    a hand whose value exceeds 21 and false otherwise. *)
val check_bust: state -> bool

(** [gen_bombid np nc] returns a list of int * int tuples of length [np] where 
    for each bomb, it has been randomly shuffled into a spot in the deck with 
    [nc] number of cards and can store the id of the player who knows of its 
    location is stored. *)
val gen_bombid: int -> int -> (int*int) list

(** [init_state nl] is the initial state at a new normal game of blackjack
    with players whose names are listed in [nl] with a shuffled deck dealt out
    according to the number of players in the game.

    Requires: List.length [nl] >= 2. *)
val init_state: name list -> state

(** [init_special nl] is the initial state at a new special game of blackjack 
    with players whose names are listed in [nl] with both a normal deck and a
    special deck that have been shuffled and dealt out according to the number 
    of players in the game. 

    Requires: List.length [nl] >= 2. *)
val init_special: name list -> state

(** [check_win st] returns the tuple representing the winner of the overall
    game. If a player has reached a score of 8pts, [check_win] will return
    (id,true). Otherwise, it will return (_,false) where no player has yet
    to reach 8pts. *)
val check_win: state -> id * bool

(** [parse_use s] returns the card that matches the string name [s]. 

    Example:
    - [parse_use steal] is [steal].
    - [parse_use foresee of future] is [foresee].

    @raise WrongBribe if [s] contains "bribe".
    @raise WrongNope if [s] contains "nope".
    @raise Invalid if [s] does not match any valid cards. *)
val parse_use: string -> card

(** [get_id_from_name st s] returns the id of the player whose name is [s] 
    in state [st]. 

    Requires: [s] is the name of an existing player. *)
val get_id_from_name: state -> string -> id

(** [dealer_in st] returns true if the dealer is still in the game and false
    otherwise. *)
val dealer_in: state -> bool

(** [check_dealer st] returns true if the dealer is still in the game and it is
    currently the dealer's turn. *)
val check_dealer: state -> bool

(** [last_id st] returns the id of the last player in the game. *)
val last_id: state -> int

(** [get_value c b] returns the value of card [c]. If [b] is true, then an Ace
    will be valued at 11, else 1. Face cards are 10, and any other card is 
    its rank value. *)
val get_value: card -> bool -> int