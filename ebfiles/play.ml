open State
open Command
open Moves
open Unix
open Print
open Save

(** [ai st] automatically iterates over the dealer's moves with a short pause
    in between moves. If all previous players busted, the dealer will not hit. 
    Otherwise it will use a simple rule to determine whether or not to hit.
    In a special game, the ai will use a certain logic to determine whether or
    not to use a special card and if so, the applicable player and/or card. *)
let rec ai st =
  print_hand st; print_string "\n"; 
  sleepf 0.5;
  let t = get_turn st in 
  let all_bust = get_sum st |> List.filter (fun x -> fst x <> (get_turn st))
                 |> List.map snd |> List.map (fun x -> x > 21) 
                 |> List.fold_left (fun x y -> x && y) true in
  if all_bust then endround st else if check_bust st then  
    (ANSITerminal.(print_string [yellow;Bold] ("\ndealer busted.\n")); 
     flush Pervasives.stdout; sleepf 0.8; 
     print_hands st; endround st)
  else if get_diff st = Normal then 
    (if List.assoc t (get_sum st) < 18 then ai_hit st
     else ai_stand st)
  else (
    (* modified ai for special gameplay *)
    let bs = get_bomb_loc st in 
    let sp = List.assoc t (get_special_hands st) in 
    (* if ai knows next card is bomb, then use shuffle *)
    if List.mem_assoc t bs && List.assoc t bs = 0 then (
      if List.mem shuffle_c sp then 
        (ANSITerminal.(print_string [yellow; Bold] 
                         ("\nThe dealer used shuffle.\n")); 
         flush Pervasives.stdout; sleepf 0.5; 
         ai (noninteractive shuffle_c st))
      else if List.assoc t (get_sum st) < 18 
      then ai_inter st else ai_stand st)
    (* if sum of cards > 18 then ai_stand st. *)
    else if List.assoc t (get_sum st) > 18 then ai_stand st
    (* if has foresee, use foresee, update if known bomb, don't hit if next draw 
       puts over 21. *)
    else if List.mem foresee sp then (
      ANSITerminal.(print_string [yellow; Bold] "\nThe dealer used foresee.\n"); 
      flush Pervasives.stdout; sleepf 0.5; 
      let ns = dealer_foresee st in 
      let fc =  List.hd (get_special_deck st) in 
      if (not (List.mem 0 (List.map (fun x -> snd x) (get_bomb_loc st)))) && 
         (get_special fc = true || 
          if not(get_special fc) 
          then List.assoc t (get_sum st) + (get_value fc false) <22 else false)
      then ai_hit ns else ai_stand ns)
    else ai_hit st)

(** [ai_inter st] is called when the ai does not want to hit, but the sum of
    their cards is < 18. The ai will try to use switch and steal if possible. 
    This can occur when the ai knows that the next card hit is a bomb. 
    If there are no players to steal or switch from, the ai will stand. *)
and ai_inter st = 
  let t = get_turn st in 
  let sp = List.assoc t (get_special_hands st) in 
  let want = dealer_want st in 
  (* if all player has less than 1 table card. *)
  if want >= 0 then (
    let p2 = assign_name want (get_names st) false in 
    if List.mem switch sp then (
      ANSITerminal.(print_string [Bold] 
                      ("\nThe dealer used switch on " ^ p2)); 
      flush Pervasives.stdout; sleepf 0.5; clear st t; 
      nope_handle (-1) {st with turn = want} t switch)
    else if List.mem steal sp then (
      ANSITerminal.(print_string [Bold] 
                      ("\nThe dealer wants to steal card 1 from " ^ p2)); 
      flush Pervasives.stdout; sleepf 0.5; clear st t; 
      nope_handle 1 {st with turn = want} t steal))
  else ai_stand st

(** [ai_stand st] ends the dealer's turn when they have chosen to stand, 
    telling the other players, printing the end hands, and ending the round. *)
and ai_stand st = 
  ANSITerminal.(print_string [Bold] "\nThe dealer has chosen to stand.\n"); 
  flush Pervasives.stdout; sleepf 0.8;
  print_hands st; endround st

(** [ai_hit st] handles the various scenarios that can occur when the dealer
    chooses to hit, such as handling the bomb. If the dealer has a defuse or 
    shuffle card, it will try to use it to avoid the bomb. *)
and ai_hit st = 
  sleepf 0.5;
  let t = get_turn st in 
  match hit st with
  | exception OutOfCards -> 
    ANSITerminal.(print_string [Bold] "\nThe deck has run out of cards.");
    print_endline "\nYou will now be returned to the main menu.\n"; 
    flush Pervasives.stdout; sleepf 0.8; main ()
  | exception Bomb -> 
    let spec_hands = List.assoc t (get_special_hands st) in 
    let cl = List.length (get_special_deck st) -1 in 
    Random.self_init ();
    let rc = Random.int cl in
    if List.mem defuse spec_hands then (
      let ns = st |> discard_special defuse in
      let sh = List.assoc t (ns |> get_special_hands) in 
      Random.self_init ();
      let random_player = Random.int (last_id st) in
      let ns' = (if (List.mem defuse sh || List.mem shuffle_c sh) 
                 then insert_bomb random_player ns 
                 else insert_bomb rc ns) in 
      ANSITerminal.(print_string [Bold] 
                      ("\nThe dealer hits the bomb, but used a bribe " ^ 
                       "and then put the bomb back in.\n")); 
      flush Pervasives.stdout; ai_stand ns')
    else (
      ANSITerminal.(print_string [Bold] 
                      "\nThe dealer hits the bomb and lost six points.\n");
      flush Pervasives.stdout;
      let ns = st |> minus_six in 
      if check_out ns then out ns else endround (ns |> insert_bomb rc))
  | h -> 
    ANSITerminal.(print_string [Bold] "\nThe dealer has chosen to hit.\n"); 
    flush Pervasives.stdout; ai h

(** [ai_nope st id c] passes on the state [st], card [c] and id [id] to the
    correct nope handler depending on whether or not the dealer chooses to use
    a nope card. It will always use a nope card if possble. *)
and ai_nope stealpos st id c = 
  let sp = List.assoc (get_turn st) (get_special_hands st) in 
  if List.mem nope sp then 
    let ns = discard_special nope st in 
    yes_nope {ns with turn = id;} c
  else (no_nope stealpos (get_turn st) {st with turn = id} c)

(** [print_st] prints out the current hands of all players as well as the
    current player's sum. *)
and print st = 
  ignore (Sys.command "clear");
  print_hands st;
  print_string ("\nYour current sum is: " ^ (print_sum st));
  loop st

(** [loop st] is a REPL (Read Eval Print Loop) that allows the user to make 
    their own moves. They may [hit], [stand], or [use card] where [hit] draws 
    another card from the deck and puts it into their hand, [stand] passes the 
    turn on to the next player, and [use card] uses the special card that the
    player names. If they bust, their turn automatically ends.
    If the deck runs out of cards, the game automatically ends and the user is 
    returned to the main menu.
    The user may [quit], see their [score], get [help], or access the [rules] 
    at any time.
    If they enter an empty or invalid command, the state does not change, and
    the user is prompted for another command. 
    The user is also allowed to save the current game state or load up a 
    previous game state in order to play from. 
    If this is a special game, special cards will be placed into a new special
    hand and the player will be given the option to use those cards if a 
    situation arises, such as drawing a bomb and using a defuse card to 
    neutralize it. *)
and loop st =
  divider (); if check_bust st && List.length (get_names st) = get_turn st + 1 
  then endround st else if check_bust st then transition st
  else (
    print_string "\nWhat would you like to do? \n> ";
    let invalid_msg = ("You have already started the game, please type restart "
                       ^ "to go back to the main menu.") in
    match parse (read_line ()) with
    | exception Empty -> 
      print_endline "\nYou didn't type anything. Please try again."; 
      loop st
    | exception Invalid -> 
      ANSITerminal.(print_string [Bold] 
                      ("\nCommand must be of form [start], [quit], [rules]," ^
                       " [hit], [stand], [use card], [help card], " ^ 
                       "[restart], or [score].\n")); loop st
    | Quit -> exit 0
    | Start | Load | Delete -> 
      ANSITerminal.(print_string [Bold] invalid_msg); loop st
    | Save -> save st                   
    | Restart ->
      ANSITerminal.(print_string [Bold] 
                      "You will now be returned to the main menu.\n"); main ()
    | Hit -> (match hit st with
        | exception OutOfCards -> 
          ANSITerminal.(print_string [Bold] "The deck has run out of cards.");
          print_endline "You will now be returned to the main menu.\n"; main ()
        | exception Bomb -> bomb_handle st
        | _ -> print (hit st );)
    | Stand -> 
      let t = (get_turn st + 1) mod List.length (get_names st) in 
      let next_player = assign_name t (get_names st) false in 
      let msg = ("\nIt is now " ^ next_player ^ "'s turn.\n") in
      let next = { st with turn = t; } in
      if check_dealer next 
      then (ANSITerminal.(print_string [Bold] msg); ai next)
      else (clear st t; ANSITerminal.(print_string [Bold] msg)); print next
    | Next ->
      ANSITerminal.(print_string [Bold] "\nPlease finish your turn."); loop st
    | Use a -> use a st
    | Help a -> help a; loop st
    | Score -> print_endline ("\n" ^ (print_score st)); loop st
    | Rules -> get_rules (); loop st)

(** [use a st] allows the player to use a special card [a] in state [st]. 
    If they do not enter a valid card, or try to use a card incorrectly, the
    user will be brought back to loop. Else, the user is led through the steps
    needed to use card [a], such as naming a player to use it on. *)
and use a st = 
  match parse_use (String.concat " " a) with 
  | exception Invalid -> 
    ANSITerminal.(print_string [Bold] "\nPlease enter a valid card name.\n"); 
    loop st
  | exception WrongBribe -> 
    ANSITerminal.(print_string [Bold] "\nYou cannot use bribe right now.\n"); 
    loop st 
  | exception WrongNope ->  
    ANSITerminal.(print_string [Bold] "\nYou cannot use nope right now.\n"); 
    loop st 
  | c -> 
    let card = format_cards_text [c] true in let d_id = last_id st in 
    let t = get_turn st in let force_victim = t + 1 in 
    if List.mem c (List.assoc t (get_special_hands st)) then (
      ANSITerminal.(print_string [Bold] ("\nYou have used " ^ card ^ "\n"));
      if c = shuffle_c || c = foresee then loop (noninteractive c st) 
      else if c = force_hit 
      then (let ns = {st with turn = force_victim} in
            if d_id = force_victim && dealer_in ns then ai_nope 0 ns t c 
            else clear st force_victim; nope_handle (-1) ns t c)
      else (
        ANSITerminal.(print_string [Bold] 
                        ("\nWho do you want to use " ^ card ^ " on?\n> ")); 
        match read_line () with 
        | p2 when List.mem (String.trim p2) (List.map snd (get_names st)) = true 
          -> let victim = get_id_from_name st (String.trim p2) in 
          if victim = get_turn st then (
            ANSITerminal.(print_string [Bold] 
                            ("\nYou cannot use this card on yourself.")); 
            use a st)
          else if c = switch && victim = d_id && dealer_in st then 
            ai_nope 0 {st with turn = victim} (get_turn st) c
          else if c = switch then 
            (clear st victim; 
             nope_handle 0 {st with turn = victim;} (get_turn st) c)
          else steal_loop st victim p2 c a
        | _ -> ANSITerminal.(print_string [Bold] 
                               "\nPlease enter a valid name.\n"); use a st))
    else (ANSITerminal.(print_string [Bold] "\nYou do not have this card.\n"); 
          loop st)

(** [steal_loop] performs the action of stealing a card from another player 
    where the position of card to steal and a victim have been chosen already.
    If these are not valid, the user will be prompted for another input. 
    Otherwise, a steal will be performed. *)
and steal_loop st victim p2 c a =
  (* nc = #cards in victim's hand *)
  let nc = List.length (List.assoc victim (get_hands st)) -1 
           |> string_of_int in 
  if int_of_string nc = 0
  then (ANSITerminal.(print_string [Bold] 
                        ("\nThis player has no faceup cards. " ^ 
                         "Please choose another player.")); use a st)
  else (ANSITerminal.(
      print_string [Bold] 
        ("\nPlease enter the position of the card you want to steal from " ^
         p2 ^ ". \n1 is the leftmost faceup card and " ^ nc 
         ^ " is the rightmost card."));
     let rec steal_card () = 
       print_string "\n> ";
       match parse_num (read_line ()) with
       | exception Empty -> 
         print_endline ("Nothing entered, please enter a number between 1 and " 
                        ^ nc ^ "."); 
         steal_card ()
       | exception Invalid -> 
         print_endline ("Not a number, please enter a number between 1 and " 
                        ^ nc ^ "."); 
         steal_card ()
       | Int x when x >= 1 && x <= int_of_string nc -> 
         clear st victim; 
         if victim = last_id st && dealer_in st then 
           ai_nope x {st with turn = victim;} (get_turn st) c 
         else nope_handle x {st with turn = victim;} (get_turn st) c
       | Int _ -> 
         print_endline 
           ("Please enter a number between 1 and " ^ nc ^ "."); 
         steal_card ()
       | Quit -> exit 0
       | Rules -> get_rules (); steal_card ()
     in steal_card ())

(** [nope_handle pos st id c] performs the necessary actions for the user on
    which the special card is being used on. They are told what is happening, 
    and then asked if they would like to use a nope card. If they do not have
    a nope card, they are still asked to hide the fact that they do not have
    a nope card. *)
and nope_handle stealpos st id c = 
  let p1 = List.assoc id (get_names st) in 
  let card = format_cards_text [c] true  in  
  print_hands st;
  print_endline ("\n" ^ p1 ^ " used " ^ card ^ " on you."); 
  print_string (if c = steal then 
                  ("They are trying to steal card " ^ string_of_int stealpos ^
                   " from you where 0 is your leftmost card.\n") else "");
  if List.mem nope (List.assoc (get_turn st) (get_special_hands st)) then 
    (print_endline "\nDo you want to use nope? (yes, no)";
     print_string  "> ";
     match parse_yes (read_line ()) with
     | exception Empty -> 
       print_endline "\nYou didn't type anything. Please try again."; 
       nope_handle stealpos st id c
     | exception Invalid -> 
       ANSITerminal.(print_string [Bold] 
                       ("\nCommand must be of form yes or no.\n"));
       nope_handle stealpos st id c
     | Yes -> print_endline "\nYou successfully noped."; 
       let ns = discard_special nope st in 
       let new_st = {ns with turn = id;} in 
       clear st id; yes_nope new_st c 
     | No -> print_endline "\nYou did not nope."; 
       if dealer_in st && id = last_id st then 
         ai_no_nope stealpos (get_turn st) {st with turn = id}  c
       else no_nope stealpos (get_turn st) {st with turn = id} c )
  else (
    print_endline "\nYou do not have a nope, please type anything to continue."; 
    print_string  "> ";
    match (read_line ()) with 
    | _ -> clear st id; 
      if dealer_in st && id = last_id st then 
        ai_no_nope stealpos (get_turn st) {st with turn = id}  c
      else no_nope stealpos (get_turn st) {st with turn = id} c )

(** [yes_nope st c] occurs when a player tries to use card [c] on another player
    but a nope is used to stop it and the original player loses their card. *)
and yes_nope st c = 
  let card = format_cards_text [c] true in 
  let ns = discard_special c st in 
  ANSITerminal.(print_string [red;Bold] 
                  ("\nA nope was used, " ^ card ^ " was wasted.\n"));
  if check_dealer st then ai ns else print ns

(** [ai_no_nope pos id st c] performs the action when the no nope has been used
    and the ai is free to use a steal or switch on the user with id [id]. *)
and ai_no_nope stealpos id st c = 
  let card = format_cards_text [c] true in 
  let ns = discard_special c st in 
  ANSITerminal.(
    print_string [red;Bold] 
      ("\nNo nope was used. The dealer successfully used a " ^ card ^ "\n"));
  flush Pervasives.stdout; sleepf 0.8; 
  if c = switch then ai (use_switch id ns)
  (* steal *)
  else ai (use_steal id st stealpos)

(** [no_nope id st c] occurs when a player uses card [c] on player [id] who
    does not choose to use a nope. The action is then played ou and the user 
    loses card [c]. *)
and no_nope stealpos id st c = 
  let p1 = List.assoc (get_turn st) (get_names st) in
  let card = format_cards_text [c] true in  
  let new_s = discard_special c st in 
  if c = steal then (ANSITerminal.(
      print_string [red;Bold] 
        ("\nNo nope was used. You successfully used a " ^ card ^ "\n"));
     print (use_steal id new_s stealpos))
  else if c = force_hit then 
    let ns = {new_s with turn = id} in 
    if dealer_in st && id = last_id st then ai_hit ns 
    else (
      (match hit ns with
       | exception OutOfCards -> 
         ANSITerminal.(print_string [Bold] "The deck has run out of cards.");
         print_endline "You will now be returned to the main menu.\n"; main ()
       | exception Bomb -> bomb_handle ns
       | _ -> 
         print_string ("\n" ^ p1 ^ " used a Force of Hit on you!\n");
         print (hit ns);))
  else 
    (ANSITerminal.(print_string [Bold] 
                     ("\nNo nope was used. You successfully used a " ^ card));
     print (use_switch id new_s);)

(** [bomb_handle st] is a REPL (Read Eval Print Loop) that allows the user to 
    make their own decision about what to do when a bomb is encountered. They
    can choose to use a defuse card if they have one in their special hand and
    can type yes/no to do so. If they do not have a defuse card, then they will 
    not be able to make any actions at the time. *)
and bomb_handle st = 
  let n = assign_name (get_turn st) (get_names st) false in 
  let t = get_turn st in
  ANSITerminal.(print_string [red;Bold] 
                  ("\n" ^ n ^ " hit Clarkson the bomb." ));
  if List.mem defuse (List.assoc t (get_special_hands st)) 
  then (    
    ANSITerminal.(print_string [Bold] 
                    ("\n\nIf you do not defuse Clarkson, you will lose 6 pts."
                     ^ "\nYou will lose if your score reaches -10 pts."));
    print_endline ("\nThe current score is: " ^ print_score st);
    print_endline "\nDo you want to use bribe? (yes, no)";
    print_string "> ";
    match parse_yes (read_line ()) with
    | exception Empty -> 
      print_endline "\nYou didn't type anything. Please try again."; 
      bomb_handle st
    | exception Invalid -> 
      ANSITerminal.(print_string [Bold] 
                      ("\nCommand must be of form yes or no.\n"));
      bomb_handle st
    | Yes -> print_endline "\nYou successfully bribed Clarkson."; 
      resume (discard_special defuse st)
    | No -> explode st )
  else explode st

(** [explode st] occurs when a player encounters a bomb and does not choose to
    or is unable to defuse it. *)
and explode st = 
  ANSITerminal.(print_string [Bold] 
                  "\nYou exploded under Clarkson's intense stare.");
  let ns = st |> minus_six in if check_out ns then out ns else resume ns

(** [out st] removes a player from the game entirely if their score falls
    below a certain threshold. It also checks to see if there is only a single
    player left in the game, who wins by default. *)
and out st = 
  let names = get_names st in
  let loser = assign_name (get_turn st) names false in 
  ANSITerminal.(print_string [red;Bold] 
                  ("\n" ^ loser ^ " is out. We will go to the next player.\n"));
  let ns = st |> remove_bomb |> update_out in 
  if List.length names = 1 then (
    let winner = get_win_out ns in 
    ANSITerminal.(print_string [Bold] 
                    ("\n" ^ winner ^ " is the only player left, they win.\n"));
    print_endline ("\nThe final score is: \n" ^ print_score ns);)
  else if check_dealer ns then 
    (if last_id st = (get_turn ns) then ai ns else print ns) else endround ns 

(** [resume st] returns the state where a user has chosen to defuse the bomb
    and is now given the option to place it back into the deck at a location
    which only they know of. If the user inputs an invaild location (index of
    of bounds), then they will be prompted for another input. *)
and resume st = 
  let n = (List.length (get_special_deck st)-1) in 
  let nc = string_of_int n in 
  let t = get_turn st in 
  ANSITerminal.(print_string [Bold] 
                  ( "\nWhere in the deck would you like to put the bomb back?" 
                    ^ "\nPlease enter a number between 0 and " ^ nc ^
                    " (inclusive) with 0 being the top card of the deck.\n"));
  print_string  "> ";
  match int_of_string (read_line ()) with
  | exception Failure _ -> 
    print_endline ("Please enter a number between 0 and " ^ nc);
    resume st 
  | x when (x >= 0) && (x <= n) -> 
    let ns = {(insert_bomb x st) with turn = t + 1} in 
    if List.length (get_names ns) - 1 = get_turn ns then 
      (if check_dealer ns then ai ns else endround ns) else print ns
  | _ -> 
    print_endline ("Please enter a number between 0 and " ^ nc);
    resume st

(** [transition st] returns the state that goes to the next player's turn when
    the current player has busted. The user may [quit], check the [rules], or
    [restart]. *)
and transition st = 
  let name = assign_name (get_turn st) (get_names st) false in 
  let t = (get_turn st + 1) mod List.length (get_names st) in 
  let ns = {st with turn = t} in 
  let dealer_msg = "\nIt is now the dealer's turn.\n" in
  ANSITerminal.(print_string [red;Bold] 
                  ("\n" ^ name ^ " busted. \nType anything to continue.\n> "));
  match parse (read_line ()) with
  | exception Empty -> 
    if check_dealer ns then 
      (ANSITerminal.(print_string [Bold] dealer_msg); ai ns)
    else (clear st t; print ns )
  | exception Invalid -> 
    if check_dealer ns then 
      (ANSITerminal.(print_string [Bold] dealer_msg); ai ns)
    else (clear st t; print ns )
  | Quit -> exit 0
  | Rules -> get_rules ()
  | Restart -> 
    ANSITerminal.(print_string [Bold] 
                    ("You will be brought back to the main menu.\n")); 
    main ()
  | _ -> if check_dealer ns then
      (divider (); ANSITerminal.(print_string [Bold] dealer_msg); ai ns)
    else (clear st t; print ns)

(** [endround st] is a REPL (Read Eval Print Loop) that allows the user to make 
    their own moves. At this point, the current round has ended. The game checks
    to see which player has won and awards points accordingly. The user is 
    prompted for a command of the form [next], which advances the game to the
    next round. If there are not enough cards left in the deck, the game ends 
    and the user is brought back to the main menu.
    The user may [quit] or [save], but any other command will end the game 
    and bring the user back to the main menu. *)
and endround st = 
  let name = update_score st in 
  let ns = {st with score = snd name} in 
  ANSITerminal.(print_string [red;Bold] 
                  ("\n" ^ (fst name) ^ " won this round."));
  print_string ("\nThe current score is: " ^ (print_score ns));
  ANSITerminal.(print_string [Bold] 
                  ("\n\nHit enter or type [next] to go to the next round. " ^ 
                   "\nOtherwise you will be returned to the main menu."));
  print_string "\n> ";
  match parse (read_line ()) with
  | exception Empty -> next_round st
  | exception Invalid -> main ()
  | Next -> next_round st
  | Quit -> exit 0
  | Save -> save st
  | _ -> main ()

(** [next_round st] returns the state where the user has chosen to move on
    to the next round after ending their turn. The game checks for a winner, 
    and if there is one, ends the game and checks that the deck has not 
    run out of cards. *)
and next_round st =
  let names = get_names st in
  let ns = {st with score = snd (update_score st)} in 
  let win = check_win ns in 
  if snd win then (
    let name = assign_name (fst win) names false in 
    ANSITerminal.(print_string [Bold] ("\n" ^ name ^ " has reached 8 points " ^
                                       "and won the game.\n"));
    print_endline "Type anything to return to the main menu.\n"; 
    match read_line () with
    | _ -> main ())
  else (match deal (List.length names) (get_deck st) with
      | exception OutOfCards -> 
        ANSITerminal.(print_string [Bold] "The deck has run out of cards.");
        print_endline "\nYou will now be returned to the main menu.\n";
        main ()
      | new_deal -> 
        let new_st = { ns with turn = 0;
                               deck = fst new_deal;
                               player_cards = snd new_deal; 
                               sum = List.map sum_cards (snd new_deal);} 
        in print new_st)

and play () = 
  let rec num () = 
    ANSITerminal.(print_string [Bold] "\nPlease enter the number of players:");
    print_string "\n> ";
    match parse_num (read_line ()) with
    | exception Empty -> 
      print_endline "Nothing entered, please enter a number between 1 and 4."; 
      num ()
    | exception Invalid -> 
      print_endline "Invalid number, please enter a number between 1 and 4."; 
      num ()
    | Int x -> names x
    | Quit -> exit 0
    | Rules -> get_rules (); num ()

  (** [names n] checks to see that the user has inputted the correct number of
      names for the number of players [n] and creates the association list
      linking ids and names. *)
  and names n = 
    ANSITerminal.(print_string [Bold] 
                    "\nPlease enter player names separated by a space:");
    print_string "\n> ";
    match parse_names (read_line ()) with
    | exception Empty -> print_endline "You did not type anything."; names n
    | StrList l when (List.length l <> n) ->
      print_endline ("You need to enter " ^ (string_of_int n) ^ 
                     " names. Please try again."); names n
    | StrList l -> choose (create_name_list l)
    | Quit -> exit 0
    | Rules -> get_rules (); names n
    | Restart ->
      ANSITerminal.(print_string [Bold] "You will now restart a new game.\n"); 
      main ()

  (** [choose nl] initializes a game state given a list of names [nl] for the
      players. The user chooses a game mode such as [normal] or [special]. *)
  and choose nl = 
    ANSITerminal.(print_string [Bold] 
                    "\nChoose a game mode (Normal, Special):");
    print_string (" \nType [rules] for more information about each.");
    print_string "\n> ";
    match parse_level (read_line ()) with
    | exception Empty ->
      print_endline "You did not type anything. Try again."; choose nl
    | exception Invalid ->
      print_endline "Not a valid game mode. Try again."; choose nl
    | Normal -> print (State.init_state nl)
    | Special -> print (State.init_special nl)
    | Quit -> exit 0
    | Rules -> get_rules (); choose nl
  in num ()

(** [delete_confirm fn] asks the user if they would like to delete savefile [fn]
    and if so, deletes it and returns them to the game. *)
and delete_confirm name file_names =
  ANSITerminal.(print_string [red;Bold] 
                  ("\nAre you sure you would like to delete " ^ name ^ "?\n"));
  print_string "\n> ";
  match parse_yes (read_line ()) with
  | Yes -> 
    (Sys.remove ((List.assoc name file_names) ^ name ^ ".json"));
    ANSITerminal.(print_string [yellow;Bold] 
                    ("\nYou have successfully deleted " ^ name ^ "!\n"));
    load ();
  | No -> 
    ANSITerminal.(print_string [yellow;Bold] 
                    ("\nYou did not delete " ^ name ^ "\n"));
    load ()
  | exception Empty -> 
    print_string ("\nPlease enter [yes] or [no]\n"); 
    delete_confirm name file_names
  | exception Invalid -> 
    print_string ("\nPlease enter [yes] or [no]\n"); 
    delete_confirm name file_names

(** [delete] is called when the user would like to delete a save file and 
    deletes the named file if possible. *)
and delete () =
  ANSITerminal.(print_string [red;Bold] 
                  "\nWhich file would you like to delete?\n");
  print_string "\n> ";
  let file_names = Save.get_files (opendir ".") [] in 
  let entry = read_line() in
  let error_message = "\nCould not find game '" ^ entry ^ "'.\n" in 
  match parse entry with
  | Quit -> exit 0
  | Rules -> get_rules (); load ()
  | exception Invalid -> 
    if List.mem_assoc entry file_names then delete_confirm entry file_names
    else ANSITerminal.(print_string [yellow;Bold] (error_message)); delete ()
  | exception Empty -> 
    ANSITerminal.(print_string [yellow] ("Please enter a name.")); delete ()
  |_ -> ANSITerminal.(print_string [yellow] (error_message)); delete ()


(** [load] provides the user with a list of all saved game files and allows them
    to choose one to load up and play from. If the user does not input a valid
    game file, they will be prompted for another input. *)
and load () =
  ANSITerminal.(print_string [red;Bold;Underlined] ("\nSaved Games\n\n"));
  let file_names = Save.get_files (opendir ".") [] in 
  (* let keys = get_keys file_names in  *)
  let rec print_files = function
    |[] -> print_string ("Could not find any saved games.\n"^
                         "Type [start] to start a new game.\n\n")
    | h::[] -> (print_endline (" ♠ " ^ h ^ "\n"))
    | h::t -> print_string(" ♠ " ); print_string ( h ^ "\n"); print_files t
  in print_files (get_fullnames file_names); 
  print_string "> ";
  let entry = read_line() in
  let error_message = "\nCould not find game '" ^ entry ^ 
                      "'. Type [start] to start a new game\n" in 
  match parse entry with
  | Quit -> exit 0
  | Start -> play ()
  | Rules -> get_rules (); load ()
  | Delete -> delete ()
  | exception Invalid -> 
    if List.mem_assoc entry file_names 
    then let game = ((List.assoc entry file_names) ^ entry ^ ".json") 
                    |> Yojson.Basic.from_file 
                    |> from_json in print game else
      ANSITerminal.(print_string [yellow;Bold] (error_message)); load ()
  | exception Empty -> 
    ANSITerminal.(print_string [yellow] ("Please enter a name.")); load ()
  |_ -> ANSITerminal.(print_string [yellow;Bold] (error_message)); load ()

(** [save_confirm st file_name] ask the user for confirmation of saving file
    to the json file named [file_name] given game state [st]. *)
and save_confirm st name file_names =
  match parse_yes (read_line ()) with
  | Yes -> 
    Sys.remove (List.assoc name file_names ^ name ^ ".json");
    ignore (open_out ((curr_date ()) ^ name ^ ".json"));
    ANSITerminal.(print_string [yellow;Bold] 
                    "\nYou have successfully saved!\n");
    Yojson.Basic.to_file (curr_date () ^ name ^ ".json") (to_json st);
    loop st
  | No -> 
    ANSITerminal.(print_string [yellow;Bold] ("\nYou did not save!\n"));
    loop st
  | exception Empty -> 
    print_string ("\n Please enter [yes] or [no]\n"); 
    save_confirm st name file_names
  | exception Invalid -> 
    print_string ("\n Please enter [yes] or [no]\n"); 
    save_confirm st name file_names

(** [save st] prompts the user to save a game state [st] under a name for the 
    json file. If the file name is already the name of a saved game, it asks
    if the user wants to override. *)
and save st =
  ANSITerminal.(print_string [yellow;Bold] 
                  "\nWhat name would you like to save under?\n\n");
  print_string "> ";
  let file_names = Save.get_files (opendir ".") [] in 
  let entry = read_line () |> String.trim |> String.lowercase_ascii in 
  match entry with
  | "" ->
    ANSITerminal.(print_string [red;Bold] "\nPlease enter a name.\n"); 
    save st 
  | "quit" | "start" | "rules" | "delete" -> 
    ANSITerminal.(print_string [red;Bold] 
                    ("\nYou cannot save a file under this name. " ^ 
                     "Please try another name.\n")); save st
  | _ -> if List.mem entry (get_keys file_names) then
      (ANSITerminal.(print_string [red;Bold] 
                       ("\n" ^ entry ^ " is already a saved game. " ^ 
                        "Would you like to overwrite it?")); 
       print_string "\n> ";
       save_confirm st entry file_names)
    else (ignore (open_out (curr_date() ^entry ^ ".json")); 
          ANSITerminal.(print_string [yellow;Bold] 
                          ("\nYou have successfully saved!\n"));
          Yojson.Basic.to_file (curr_date() ^ entry ^ ".json") (to_json st);
          loop st)

(** [clear st t] prints out the transition between players that hides the 
    previous player's cards from the next player as well as the next player's 
    cards from the previous player, creating a transition screen in between 
    where the next player can then start their turn. *)
and clear st turn = 
  let name = List.assoc turn (get_names st) in
  ANSITerminal.(print_string [red;Bold] 
                  ("\nType anything to clear the screen and pass to " ^ name));
  print_string "\n> ";
  ignore (read_line ()); ignore (Sys.command "clear");
  print_string ("\n\n\n\n\n\n\n\n\n\n\n\n\n" ^ 
                "Type anything to see " ^ name ^ "'s turn.");
  print_string "\n> ";
  ignore (read_line ())

and main () = 
  ignore (Sys.command "clear");
  fancy ();
  let rec start () = 
    ANSITerminal.(print_string [yellow;Bold]
                    ("\nType:" ^
                     "\n[start] to begin a new game              "^
                     "[rules] to read the rules" ^
                     "\n\n[load] to continue a previous game       "^
                     "[quit] to exit.\n\n"));
    print_string  "> ";
    match parse (read_line ()) with
    | exception Empty -> 
      print_endline "You did not type anything. Try again."; start ()
    | exception Invalid -> 
      print_endline ("\nInvalid command. " ^ 
                     "Type [start], [load], [rules], or [quit].");
      start ()
    | Quit -> exit 0
    | Start -> play ()
    | Load -> load ()
    | Rules -> get_rules (); start ()
    | _ -> print_endline "\nYou have not started a game yet."; start ()
  in start ()

let () = main ()