(** Formats game state to print in game. *)

open State

(** [format_cards cl b] returns the string representation of a player's hand 
    whose cards are [cl]. If [b] is true, then all cards will be shown, 
    otherwise, the first card will be hidden by an underscore. *)
val format_cards: card list -> bool -> string

(** [format_cards_text cl b] returns the text string representation of a 
    player's hand whose cards are [cl]. If [b] is true, then all cards will be 
    shown, otherwise, the first card will be hidden by an underscore. *)
val format_cards_text: card list -> bool -> string

(** [assign_name id nl bool] returns the name of the player with id [id] and
    a colon added if [bool] is true. *)
val assign_name: 'a -> ('a * string) list -> bool -> string

(** [print_hands st] prints out the formatted string representation of the 
    current hands of each player in the game given a state [st] where the hand
    of the player whose turn it is is colored red. If it is the dealer's turn,
    all cards are shown for all hands. *)
val print_hands: state -> unit

(** [print_hand st] prints out the formatted string representation of the 
    single hand of the current player in the game given a state [st]. If it is
    a special game, the number of special cards a player has is printed. *)
val print_hand: state -> unit

(** [print_score st] returns the formatted string representation of the current
    scores of each player. *)
val print_score: state -> string

(** [print_sum st] returns a string representation of the sum of the table hand
    of the current player whose turn it is in state [st]. *)
val print_sum: State.state -> string

(** [fancy ()] prints a fancy opening screen. *)
val fancy: unit -> unit

(** [divider ()] prints a divider used to differentiate between turns. *)
val divider: unit -> unit

(** [help lst] prints out a helpful message for the card specified in [lst]. 
    If the card specified is not a valid card, an invalid card message is 
    printed out.

    Example:
    - [help ["steal";"of";"kenneth"]] prints information about the steal card.
    - [help ["notacard"]] prints "Unknown card". *)
val help: string list -> unit