(** Representation of player actions. *)
open State

(** Raised when a bomb card is encountered/drawn. *)
exception Bomb

(** [hit st] returns the state in which the current player whose turn it is has 
    drawn another card. If it is a normal card, add it to the table hand, else
    add it to their special hand.

    Requires: [id] to be a valid id.

    @raise Bomb if card drawn is a special bomb card.
    @raise OutOfCards if there are no more cards left in the deck. *)
val hit: State.state -> State.state

(** [discard_special c st] returns the state where card [c] has been removed
    from the current player's special hand in state [st].

    Requires: card [c] is in the player's special hand. *)
val discard_special: State.card -> State.state -> State.state

(** [minus_six st] returns the state where the current player's score has been
    decreased by 6 pts. *)
val minus_six: State.state -> State.state

(** [check_out state] returns true if the current player is out of the game,
    that is, their score is less than or equal to -10 pts. *)
val check_out: State.state -> bool

(** [update_out st] returns the state where if the current player is out of the
    game (their score is <= -10 pts), they are removed from state [st] and 
    their name, cards, score, and sum are no longer stored. *)
val update_out: State.state -> State.state

(** [get_win_out st] returns the string of the name for the only player who
    is still remaining in the game where all other players are out.

    Requires: [st] has 1 player left in the game. *)
val get_win_out: State.state -> string

(** [remove_bomb st] returns the state with the bomb that was previously at the
    top of the special deck removed. The bomb was at the top of the deck, drawn,
    and used so it is no longer in play and must be removed. *)
val remove_bomb: State.state -> State.state

(** [insert_bomb n st] returns the state where the top bomb has been relocated 
    to index [n] of the special deck in state [st]. 

    Requires: [n] is an integer between 0 and the length of the special deck. *)
val insert_bomb: int -> State.state -> State.state

(** [update_score st] returns a tuple containing the string representation of
    the updated scores of state [st] and the list of scores. *)
val update_score: State.state -> string * ((State.id * int) list )

(** [create_name_list lst] returns a name list where each string in [lst] has 
    been assigned a corresponding player id, creating a value of type name to
    put into a list..

    Requires: [lst] is a list of all strings. *)
val create_name_list: string list -> name list

(** [use_steal id st pos] returns the state where the card with position/index
    [pos] has been taken from the table hand of the player with id [id] and 
    added into the table hand of the current player in state [st]. 

    Requires: 
    - [pos] is a valid index for the table hand of player [id]. 
    - [id] is a valid player id that is not the current player's id. *)
val use_steal: State.id -> State.state -> State.id -> State.state

(** [use_switch id st] returns the state where the table hands of the player 
    with id [id] and the current player in state [st] have been swapped with
    one another.

    Requires: [id] is a valid player id that is not the current player's id. *)
val use_switch: State.id -> State.state -> State.state

(** [noninteractive c st] returns the state where card [c] has been applied
    to state [st]. Noninteractive cards include foresee and shuffle. 

    Requires: [c] be special card shuffle or foresee. *)
val noninteractive: State.card -> State.state -> State.state

(** [dealer_foresee st] returns the state where the dealer has used special
    card foresee and may have gained insight on where any bombs are located.
    The special card foresee is then discarded. *)
val dealer_foresee: State.state -> State.state

(** [dealer_want st] returns the id of the player that the dealer would like to
    switch with or steal from. The dealer chooses this player by picking the
    player with the most hand cards and at least 1 card. Return -1 if all 
    players have fewer than 2 cards. The dealer cannot choose themselves. *)
val dealer_want: State.state -> int