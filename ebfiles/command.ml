exception Empty
exception Invalid

type object_phrase = string list

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

type difficulty_level = 
  | Normal
  | Special
  | Quit
  | Rules

type num_players = 
  | Int of int
  | Quit
  | Rules

type player_names = 
  | StrList of (string list)
  | Quit
  | Rules
  | Restart

type yes_no = 
  | Yes
  | No

type num =
  | Int of int

let parse str = 
  let l1 = String.split_on_char ' ' (String.lowercase_ascii str) in
  let l = List.filter (fun a -> a <> "") l1 in
  match l with
  | [] -> raise Empty
  | h::[] when h = "start" -> Start
  | h::[] when h = "load" -> Load
  | h::[] when h = "save" -> Save
  | h::[] when h = "delete" -> Delete
  | h::[] when h = "restart" -> Restart
  | h::[] when h = "quit" -> Quit
  | h::[] when h = "rules" -> Rules
  | h::[] when h = "hit" -> Hit
  | h::[] when h = "h" -> Hit
  | h::[] when h = "stand" -> Stand
  | h::[] when h = "s" -> Stand
  | h::m::t when h = "use" -> Use (m::t)
  | h::m::t when h = "help" -> Help (m::t)
  | h::[] when h = "score" -> Score
  | h::[] when h = "next" -> Next
  | h::[] when h = "n" -> Next
  | _ -> raise Invalid

let parse_yes str = 
  let l1 = String.split_on_char ' ' (String.lowercase_ascii str) in
  let l = List.filter (fun a -> a <> "") l1 in
  match l with 
  | [] -> raise Empty
  | h::[] when h = "yes" -> Yes
  | h::[] when h = "no" -> No 
  | _ -> raise Invalid

let parse_level str = 
  let l1 = String.split_on_char ' ' (String.lowercase_ascii str) in
  let l = List.filter (fun a -> a <> "") l1 in
  match l with
  | [] -> raise Empty
  | h::[] when h = "normal" -> Normal
  | h::[] when h = "special" -> Special
  | h::[] when h = "quit" -> Quit
  | h::[] when h = "rules" -> Rules
  | _ -> raise Invalid

let parse_num str = 
  let l1 = String.split_on_char ' ' (String.lowercase_ascii str) in
  let l = List.filter (fun a -> a <> "") l1 in
  match l with
  | [] -> raise Empty
  | h::[] when h = "1" -> (Int 1: num_players)
  | h::[] when h = "one" -> (Int 1)
  | h::[] when h = "2" -> (Int 2)
  | h::[] when h = "two" -> Int 2
  | h::[] when h = "3" -> (Int 3)
  | h::[] when h = "three" -> (Int 3)
  | h::[] when h = "4" -> (Int 4)
  | h::[] when h = "four" -> (Int 4)
  | h::[] when h = "quit" -> Quit
  | h::[] when h = "rules" -> Rules
  | _ -> raise Invalid

let parse_names str = 
  let l1 = String.split_on_char ' ' str in
  let l = List.filter (fun a -> a <> "") l1 in
  match l with
  | [] -> raise Empty
  | h::[] when h = "quit" -> Quit
  | h::[] when h = "rules" -> Rules
  | l -> StrList l

let get_rules () = 
  ANSITerminal.(print_string [red;Bold] ("\n{ LEVEL INFORMATION }"));
  print_endline "\n{ NORMAL }";
  print_endline ("
    At the start of the game, each player is dealt two cards, one face up, 
    and one face down. You may only view your own facedown card.
    The objective of the game is to get as close to 21 while beating the dealer.
    Face cards are valued at 10, Aces can be valued at either 1 or 11, 
    while all other cards are valued at their pip value. 
    You bust if your hand goes over 21 during any round, and lose if the dealer
    gets closer to 21 than you do. Players will go in the order that their names
    are entered in with the dealer going last. 
    During their turn, players can either [hit] or [stand]. If you [hit], you
    will receive another card. If you [stand], your turn will end for the round.
    At the end of each round, the player with the highest score will receive 
    1 pt and if there is a tie, all players in the tie will receive 1 pt. 
    The game ends if either the deck runs out, or one player reaches 8 pts." );
  print_endline "\n{ SPECIAL }";
  print_endline ("
    There is now a new deck containing special cards that has been shuffled 
    in with a normal set of playing cards.

    The new cards include:
    - bomb cards called \"Clarkson of Gates461\", which will explode when hit.
      The player will lose 6 pts if the bomb explodes. 
    - defuse cards called \"Bribe of Wine\" that allow the player to safely
      counteract the bomb. Each player starts the game with one defuse card.
    - foresee cards called \"Foresee of Future\", which allows the player to 
      view the next three cards in the hit deck.
    - shuffle cards called \"Shuffle of Mystery\", which allows the player to
      shuffle the hit deck.
    - steal cards called \"Steal of Kenneth\", which allows the player to steal
      a faceup card from the table hand of another player.
    - switch cards called \"Switch of Hands\", which allows the player to switch
      table hands with another player.
    - force hit called \"Force of Hit\", which lets the player force the next
      player to hit from the deck.
    - nope cards called \"Nope of Kyrylo\", which allows the player to 
      counteract any special card being used against them.

    At the beginning of the game, each player will be dealt 2 hands, one on the 
    table like in blackjack and a special hand containing only special cards
    that contains 1 defuse card to begin with. The special hand will be kept
    throughout the rounds while the table hand will be re-dealt each round. 
    When a player hits, number cards will be placed in the table hand face-up 
    while special cards will be placed directly in the special hand. 
    Players can perform any actions, such as hitting or using a special card 
    until the player busts, chooses to stand, or otherwise end their turn by
    using a special card. If a bomb is hit, it may be defused with a defuse card
    and the player will have to opportunity to place it back into the deck into 
    a location that only they know. If the user doesn't have a defuse card, 
    they will lose 6 points and their turn will end. If any player's points 
    reach -10, they will automatically lose and cannot play in future rounds.
    The game ends if any player reaches 8 pts, if there is only a single
    player left in the game, or the deck runs out of cards. In the latter case,
    the player with the highest points currently is declared the winner." );
  print_endline "\n{ TIPS/SHORTCUTS }";
  print_endline ("
  try typing
    - [n] instead of [next], 
    - [h] instead of [hit], 
    - [s] instead of [stand].");