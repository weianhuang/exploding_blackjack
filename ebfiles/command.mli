(** Representation of player commands. *)

(** Raised when empty command is entered and parsed. *)
exception Empty

(** Raised when invalid command is entered and parsed. *)
exception Invalid

(** type [object_phrase] represents the object phrase that can be part of
    commands use and help. Each element of the list represents a word without
    any whitespace and is in the same order that the user entered them in.

    Requires: an [object_phrase] be an non-empty list. 

    Example:
    - if a player enters ["use shuffle of mystery"], the corresponding 
    object_phrase is [["shuffle";"of";"mystery"]]. *)
type object_phrase = string list

(** type [command] represents the various actions that a player can make. *)
type command = 
  | Start
  | Load
  | Save
  | Delete
  | Restart
  | Quit
  | Rules
  | Hit
  | Stand
  | Score
  | Next
  | Use of object_phrase
  | Help of object_phrase

(** type [difficulty_level] represents the different versions of the game that
    users can choose as well as actions that are always possible. *)
type difficulty_level = 
  | Normal
  | Special
  | Quit
  | Rules

(** type [num_players] represents the number of players that the user inputted 
    to play the game as well as actions that are always possible. *)
type num_players = 
  | Int of int
  | Quit
  | Rules

(** type [player_names] represents the names of thes players that the user 
    inputted as well as actions that are always possible and restarting. *)
type player_names = 
  | StrList of (string list)
  | Quit
  | Rules
  | Restart

(** type [yes_no] represents the result of a player selecting yes or no. *)
type yes_no = 
  | Yes
  | No

(** [parse s] returns parses the player's input into type command.
    Case does not matter.

    Examples:
    - [parse "   start "] is [Start]
    - [parse "   NeXT "] is [Next]
    - [parse "h  "] is [Hit]
    - [parse "asda"] raises exception [Invalid]

    @raise Empty if [s] is the empty string, or only spaces.
    @raise Invalid if [s] does not contain a valid player command. *)
val parse: string -> command

(** [parse_yes s] returns parses the player's input into type yes_no.
    Case does not matter.

    Examples:
    - [parse "   yes "] is [Yes]
    - [parse "   NO "] is [No]

    @raise Empty if [s] is the empty string, or only spaces.
    @raise Invalid if [s] does not contain a "yes" or "no". *)
val parse_yes: string -> yes_no

(** [parse_level s] returns parses the player's input into a difficulty level.
    Case does not matter.

    Examples:
    - [parse "  normal "] is [Normal]
    - [parse "   SPecIAl "] is [Special]

    @raise Empty if [s] is the empty string, or only spaces.
    case does not matter.
    @raise Invalid if [s] does not contain a valid difficulty level. *)
val parse_level: string -> difficulty_level

(** [parse_name s] returns parses the player's input into type num_players.
    Case does not matter.

    Examples:
    - [parse " 2 "] is [Int 2]
    - [parse "one"] is [Int 1]
    - [parse " a "] raises [Invalid]

    @raise Empty if [s] is the empty string, or only spaces.
    @raise Invalid if [s] does not contain a valid integer between 1 and 4. *)
val parse_num: string -> num_players

(** [parse_num s] returns parses the player's input into string list of names.
    Case does not matter.

    Examples:
    - [parse " a   b c "] is \["a";"b";"c"\]
    - [parse "ab "] is \["ab"\]

    @raise Empty if [s] is the empty string, or only spaces. *)
val parse_names: string -> player_names

(** [get_rules] prints out the string representations of the rules. *)
val get_rules: unit -> unit