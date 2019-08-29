open State
open Command
open Print

exception Bomb 

(** [add_card id handlist c] adds card [c] to the hand in the association list
    [handlist] belonging to the player with id [id]. 
    The card is appended to the end of the hand.

    Requires: [id] is a valid player id. *)
let rec add_card id (hl:hand list) c = 
  match hl with
  | [] -> []
  | (k,v)::t -> if k = id then (k,v@[c])::(add_card id t c)
    else (k,v)::(add_card id t c)

(** [decrement l] decreases each value in the association list [l] by 1.

    Requires: [l] be a valid association list with type ('a * int) list. 

    Example: 
    - [decrement [(1,1);(2,2);(3,3)]] is [[(1,0);(2,1);(3,2)]]*)
let decrement l = 
  List.map (fun (k,v) -> (k,v-1)) l

let hit st =
  let id = get_turn st in if get_diff st = Normal then
    (match deal_one (get_deck st) with
     | exception OutOfCards -> raise OutOfCards
     | d -> 
       let pc = add_card id (get_hands st) (fst d) in
       { st with 
         deck = snd d;
         player_cards = List.sort compare pc;
         sum = List.map sum_cards pc; }) 
  else (
    let bs = st |> get_bomb_loc |> List.map snd in 
    if (List.mem 0 bs) then raise Bomb
    else (match deal_one (get_special_deck st) with
        | exception OutOfCards -> raise OutOfCards
        | d -> 
          if get_special (fst d) = true then 
            let pc = add_card id (get_special_hands st) (fst d) in
            { st with 
              special_deck = snd d;
              bomb_loc = decrement (get_bomb_loc st);
              special_hands = List.sort compare pc; }
          else 
            let pc = add_card id (get_hands st) (fst d)
            in { st with 
                 special_deck = snd d;
                 bomb_loc = decrement (get_bomb_loc st);
                 player_cards = List.sort compare pc;
                 sum = List.map sum_cards pc; }))

(** [get_spec_hand st] returns the special hand of the player whose turn it
    currently is given state [st]. *)
let get_spec_hand st = 
  List.assoc (get_turn st) (get_special_hands st)

(** [delete elt lst] returns a list where at element [elt] has been deleted
    from list [lst]. If there are multiple occurences of [elt], delete at most
    one. If [elt] does not appear, then return [lst]. 

    Example:
    - [delete 1 [1;2;3]] is [[2;3]]. 
    - [delete 0 [1;2;3]] is [[1;2;3]]. 
    - [delete 1 [1;1;3]] is [[1;3]]. *)
let delete e l = 
  let rec help e l acc f= 
    match l with 
    | [] -> acc
    | h::t -> if (h = e && f) then help e t acc false else 
        help e t (h::acc) f in 
  List.rev (help e l [] true)

let discard_special c st = 
  let s = get_spec_hand st in 
  let i = get_turn st in 
  if List.mem c s then 
    let h = (i, delete c s) in 
    let ns = List.sort compare (h::(List.remove_assoc i (get_special_hands st))) 
    in {st with special_hands = ns;}
  else failwith "can't happen"

(** [winners st] returns a list containing the [ids] of the players who have won
    the overall game of blackjack. If there is a tie, award 1 pt to all tied
    winners. If all busted, dealer wins. *)
let winners st = 
  let rec max_key lst acc = 
    match lst with
    | [] -> acc
    | (k,v)::t -> 
      let head = snd (List.hd acc) in 
      if v >  head && v < 22 then max_key t [(k,v)] else if v = head && v < 22 
      then max_key t ((k,v)::acc) else max_key t acc  
  in List.map (fun x -> fst x) (max_key (get_sum st) [(last_id st ,0)])

(** [plus_score ids lst acc] returns a list of scores where the score for the
    players with ids [ids] has been incremented by 1 pt. *)
let rec plus_score ids lst acc = 
  match lst with
  | [] -> acc
  | (k,v)::t -> 
    if List.mem k ids then plus_score ids t ((k,v+1)::acc) 
    else plus_score ids t ((k,v)::acc)

let minus_six st = 
  let rec help id l acc = 
    match l with 
    | [] -> acc 
    | (k,v)::t -> if k = id then help id t ((k,v-6)::acc) 
      else help id t ((k,v)::acc) in
  let ns = List.sort compare (help (get_turn st) (get_score st) []) in 
  {st with score = ns;}

(** [delete_name id nl] returns the name list without the player with id [id] 
    and all subsequent ids after this player have been decreased by 1. *)
let delete_name id nl = 
  let rec del_name id' nl' acc = 
    match nl' with
    | [] -> acc
    | (i,n)::t -> if id' = i then del_name id' t acc else if id' < i then
        del_name id' t ((i-1,n)::acc) else del_name id' t ((i,n)::acc)
  in List.sort compare (del_name id nl [])

let check_out st = (List.assoc (get_turn st) (get_score st)) <= -10

let update_out st = 
  if check_out st then 
    {st with names = delete_name (get_turn st) (get_names st);
             player_cards = delete_name (get_turn st) (get_hands st);
             score = delete_name (get_turn st) (get_score st);
             sum = delete_name (get_turn st) (get_sum st);
             special_hands = delete_name (get_turn st) (get_special_hands st);
    } else st 

let get_win_out st = 
  match get_names st with 
  | (k,v)::t -> v 
  | _ -> failwith "can't happen"

(** [remove_val lst elt] returns the list where all occurrences of element 
    [elt] have been removed from list [lst]. If [elt] does not appear in [lst],
    return [lst] as is.

    Examples:
    - [remove_val [(1,1);(1,1);(1,2);(1,3)] 1] is [(1,3);(1,2)]. *)
let remove_val l e = 
  let rec help l' e' acc = 
    match l' with
    | [] -> acc
    | (k,v)::t -> if v = e' then help t e' acc else help t e' ((k,v)::acc) 
  in help l e []

let remove_bomb st = 
  {st with bomb_loc = remove_val (get_bomb_loc st) 0;}

let insert_bomb n st = 
  let rec help i acc = function 
    | [] -> acc
    | (k,v)::t -> 
      if v >= i then help i ((k,v+1)::acc) t else help i ((k,v)::acc) t in 
  let ns = remove_bomb st in 
  let id = get_turn st in 
  let nbl = (id,n)::(help n [] (get_bomb_loc ns)) in 
  {ns with bomb_loc = List.sort compare nbl;}

let update_score st = 
  let rec arr_to_string = function
    | [] -> ""
    | h::[] -> assign_name h (get_names st) false
    | h'::t' -> assign_name h' (get_names st) false ^ ", " ^ arr_to_string t' 
  in let wins = winners st 
  in let s = (if List.length wins > 1 
              then "There is a tie: " ^ arr_to_string (List.sort compare wins) 
              else arr_to_string wins) 
  in (s, (List.sort compare (plus_score wins (get_score st) [])))

let create_name_list name_list = 
  let rec create_list_helper lst n acc =
    match lst with 
    | [] -> acc
    | h::t -> create_list_helper t n ((n - List.length t - 1,h)::acc) in 
  List.sort compare (create_list_helper name_list (List.length name_list) 
                       [(List.length name_list,"dealer")])

(** [use_foresee st] prints out the next three cards in the hit deck and 
    returns the state where the special card foresee has been discarded from
    the current player's special hand. If there are fewer than three cards
    remaining, show as many as possible. *)
let use_foresee st = 
  print_endline "The next cards are:";
  let check_bomb = List.map snd (get_bomb_loc st) 
                   |> List.filter (fun x -> x < 3) |> List.sort compare in
  let nb = List.length check_bomb in
  if nb > 0 then
    (print_string 
       (match get_special_deck st with
        | [] -> if nb = 3 then format_cards [bomb;bomb;bomb] true
          else (if nb = 2 then format_cards [bomb;bomb] true 
                else format_cards [bomb] true)
        | h::[] -> if nb = 3 && check_bomb = [0;1;2] then 
            format_cards [bomb;bomb;bomb] true
          else if nb = 2 && check_bomb = [0;1] then 
            format_cards [bomb;bomb;h] true
          else if nb = 2 && check_bomb = [0;2] then
            format_cards [bomb;h;bomb] true
          else if nb = 2 && check_bomb = [1;2] then
            format_cards [h;bomb;bomb] true
          else if nb = 1 && check_bomb = [0] then
            format_cards [h;bomb] true
          else if nb = 1 && check_bomb = [1] then
            format_cards [bomb;h] true
          else failwith "foresee uncaught bomb case"
        | h::a::_ -> 
          if nb = 3 && check_bomb = [0;1;2] then 
            format_cards [bomb;bomb;bomb] true
          else if nb = 2 && check_bomb = [0;1] then 
            format_cards [bomb;bomb;h] true
          else if nb = 2 && check_bomb = [0;2] then
            format_cards [bomb;h;bomb] true
          else if nb = 2 && check_bomb = [1;2] then
            format_cards [h;bomb;bomb] true
          else if nb = 1 && check_bomb = [0] then
            format_cards [bomb;h;a] true
          else if nb = 1 && check_bomb = [1] then
            format_cards [h;bomb;a] true
          else if nb = 1 && check_bomb = [2] then
            format_cards [h;a;bomb] true
          else failwith "foresee uncaught bomb case"); 
     discard_special foresee st)
  else (print_string 
          (match get_special_deck st with
           | [] -> "No cards left in the deck."
           | h::[] -> format_cards [h] true
           | h::a::[] -> format_cards [h;a] true
           | h::a::b::_ -> format_cards [h;a;b] true);
        discard_special foresee st)

(** [use_shuffle st] returns the state where the hit deck has been shuffled.
    All bomb locations are regenerated and the special card shuffle has been
    discarded from the current player's special hand. *)
let use_shuffle st = 
  let np = List.length (get_names st) in 
  let nc = List.length (get_special_deck st) in 
  {(st |> discard_special shuffle_c) with 
   special_deck = shuffle (get_special_deck st);
   bomb_loc = (gen_bombid np nc);}

(** [remove_card lst pos] removes the card with position [pos] from the list
    of cards [lst] and returns the tuple of the removed card and the remaining
    list. If [pos] is not a valid index/position, no card will be removed. 
    The order of the list is maintained. *)
let remove_card l ci = 
  let rec rc_helper lst cpos pos acc = 
    match lst with
    | [] -> acc
    | h::t -> if cpos = pos then rc_helper t cpos (pos+1) ([h], snd acc)
      else rc_helper t cpos (pos+1) (fst acc, h::(snd acc)) in 
  let m = rc_helper l ci 0 ([],[]) in 
  ((fst m),(List.rev (snd m)))

let use_steal id st cid = 
  let turn = get_turn st in
  let hands = get_hands st in
  (* cid is the position of card they want to steal from player with id = id*)
  let cl = List.assoc id hands in 
  let removed = remove_card cl cid in 
  (* remove card, store stolen card, add stolen card to current hand *)
  let added = (List.assoc turn hands) @ (fst removed)
  in let rec update_hands lst acc p1 p2 add re = 
       match lst with
       | [] -> acc
       | (k,v)::t -> if k = p1 then update_hands t ((k,add)::acc) p1 p2 add re
         else if k = p2 then update_hands t ((k, re)::acc) p1 p2 add re
         else update_hands t ((k,v)::acc) p1 p2 add re
  in let updated = List.sort compare 
         (update_hands hands [] turn id added (snd removed))
  in {st with player_cards = updated; sum = List.map sum_cards updated;}

(* id = player who want to switch with *)
let use_switch id st = 
  let t = get_turn st in 
  let h = get_hands st in 
  let rec switch_hands lst id' j acc = 
    match lst with
    | [] -> acc
    | (i,c)::t -> 
      if i = id' then switch_hands t id' j ((j,c)::acc)
      else if i =  j then switch_hands t id' j ((id',c)::acc)
      else switch_hands t id' j ((i,c)::acc) in 
  let updated = List.sort compare (switch_hands h id t []) in 
  {st with player_cards = updated; sum = List.map sum_cards updated;}

let noninteractive c st = 
  match c with 
  | x when x = shuffle_c -> use_shuffle st
  | x when x = foresee -> use_foresee st 
  | _ -> failwith "shouldn't happen, use"

(* updates bomb_loc with 0 as first position if first three cards *)
let dealer_foresee st = 
  let rec df_helper i l acc = 
    match l with
    | [] -> acc 
    | (k,v)::t -> if v <=2 then df_helper i t ((i,v)::acc) 
      else df_helper i t ((k,v)::acc) in 
  {(st |> discard_special foresee) with 
   bomb_loc = df_helper (get_turn st) (get_bomb_loc st) [] }

(** [compare_hands x y] returns 0 if the length of [x] is equal to the length
    of [y], -1 if the length of [x] < length of [y], else 1.

    Requires: [x],[y] are association lists of ('a * 'b list) tuples. *)
let compare_hands x y = 
  if List.length (snd x) < List.length (snd y) then 1
  else if List.length (snd x) = List.length (snd y) 
  then 0 else -1

let dealer_want st = 
  let l = List.filter (fun x -> List.length (snd x) > 1 ) (get_hands st) in 
  let cl = List.sort compare_hands l in 
  let rec helper lst i = 
    match lst with 
    | [] -> -1
    | (k,v)::t -> if k = i then helper t i else k in 
  helper cl (get_turn st)