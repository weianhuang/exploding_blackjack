(** Game interface. *)
open State

(** [play] initializes and starts the game of blackjack. *)
val play: unit -> unit

(** [main] prompts for the game to begin, then starts it. *)
val main: unit -> unit
