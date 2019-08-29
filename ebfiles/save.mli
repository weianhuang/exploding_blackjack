(** Ability to save and load game states into json. *)

open State
open Filename
open Yojson.Basic

(** [from_json json] converts [json] to a type [state].

    Requires: [json] is a valid JSON state representation. *)
val from_json: Yojson.Basic.json -> state

(** [to_json st] is the json representation of [st] *)
val to_json: state -> json

(** [get_files dir] returns a list of valid files in directory [dir]. *)
val get_files : Unix.dir_handle -> (string*string) list -> (string*string) list

(** [curr_date] returns the string representation of the current date and time
    of the local system in the format mm-dd-yy xxhyym.

    Example:
    - 11/07/18 10h42m represents Nov. 7th, 2018 at 10:42am. 
    - 08/19/16 20h00m represents Aug. 19th, 2016 at 8:00pm. *)
val curr_date: unit -> string

(** [get_keys lst] returns the list of keys given an association list [lst].
    If [lst] is empty, return itself.

    Requires: [lst] be a valid association list.

    Example:
    - [get_keys [(0,0);(1,0);(2,0)]] is [0;1;2]. *)
val get_keys: ('a * 'b) list -> 'a list

(** [get_fullnames lst] returns a string list where each string * string tuple
    in [lst] has been appended together. Given tuple (x,y), append x to y in
    the form of y ^ x. If [lst] is empty, return itself.

    Requires: [lst] is an association list of string * string tuples. 

    Example:
    - [get_fullnames [("fst","snd")]] is ["sndfst"]*)
val get_fullnames: (string * string) list -> string list