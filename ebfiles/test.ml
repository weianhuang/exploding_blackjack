open OUnit2
open State
open Command
open Moves
open Print
open Save

let defuse = {rank = "Bribe"; suit = "Wine";special=true}
let sa = {rank="Ace";suit="Spades";special=false}
let s2 = {rank="2";suit="Spades";special=false}
let s3 = {rank="3";suit="Spades";special=false}
let s4 = {rank="4";suit="Spades";special=false}
let s5 = {rank="5";suit="Spades";special=false}
let s6 = {rank="6";suit="Spades";special=false}
let s7 = {rank="7";suit="Spades";special=false}
let s8 = {rank="8";suit="Spades";special=false}
let s9 = {rank="9";suit="Spades";special=false}
let s10 = {rank="10";suit="Spades";special=false}
let sk = {rank="King";suit="Spades";special=false}
let sa = {rank="Ace";suit="Spades";special=false}
let ca = {rank="Ace";suit="Clubs";special=false}
let ha = {rank="Ace";suit="Hearts";special=false}
let da = {rank="Ace";suit="Diamonds";special=false}
let dk = {rank="King";suit="Diamonds";special=false}
let d9 = {rank="9";suit="Diamonds";special=false}
let d10 = {rank="10";suit="Diamonds";special=false}
let dj = {rank="Jack";suit="Diamonds";special=false}
let dq = {rank="Queen";suit="Diamonds";special=false}
let h3 = {rank="3";suit="Hearts";special=false}
let c6 = {rank="6";suit="Clubs";special=false}

let hand1 = (1,[sa;s2;s3;])
let hand2 = (1,[sk;sa;ca;ha;da]) 
let hand3 = (1,[dk;sk;ca]) 
let hand4 = (1,[dk;sk;ca;ca]) 
let hand5 = (1,[dk;sa]) 
let hand6 = (1,[da;sa;d9;da;d10;])

let deck1 = [sa;s2;s3;s4;s5;s6;sk]
let deck1' = [s2;s3;s4;s5;s6;sk]
let deck2 = [sa;ca;ha;da]
let deck2' = [sa;ca;ha;da;sa]
let deck3 = [sa;sk]
let deck3' = [sa;sk;sa]
let deck3'' = [sa;sk;d9]
let deck4 = [d9;d10;dj;dq]
let deck4' = [d10;dj;dq]

let pc1 = [(0,deck3);(1,deck2)]
let pc1' = [(0,deck3');(1,deck2)]
let pc1'' = [(0,deck3);(1,deck2')]
let pc2 = [(0,deck3'');(1,deck2)]
let pc3 = [(0,[dk;sa]);(1,[da;sa;d9;da;d10]);(2,[sa;s2;s3])]

let nl = [(0,"a");(1,"b");(2,"dealer")]

let st1 =   { turn = 0;
              deck = deck1;
              player_cards = pc1;
              difficulty = Normal;
              score = [(0,0);(1,0)];
              sum = [(0,21); (1,14)]; 
              names = [(0,"a");(1,"dealer")];
              special_hands = [];
              special_deck = [];
              bomb_loc = [];}

let sum1 = get_sum st1

let steal1 = {st1 with player_cards = pc3;}
let steal2 = {steal1 with turn = 1;}

let hit1 = {st1 with turn = 1;}

let st1' =   { st1 with deck = deck1';
                        player_cards = pc1';
                        sum = [(0,12); (1,14)]; }
let st1'' = { hit1 with deck = deck1'; 
                        player_cards = pc1''; 
                        sum = [(0,21);(1,15)]; }
let st2 = {st1 with deck = deck4;}
let st2' = {st2 with deck = deck4'; 
                     player_cards = pc2; 
                     sum = [(0,20); (1,14)]; }
let ste =   { turn = 0;
              deck = [];
              player_cards = pc1;
              difficulty = Normal;
              score = [(0,0);(1,0)];
              sum = [(0,21); (1,14)]; 
              names = [(0,"a");(1,"dealer")];
              special_hands = [];
              special_deck = [];
              bomb_loc = [];}
let spec1 = {st1 with difficulty = Special; special_deck = deck1; 
                      bomb_loc = [(-1,0)]; special_hands = [(0,[]);(1,[])];}
let spec2 = {spec1 with bomb_loc = [(-1,8)]}
let spece = {spec2 with special_deck = [];}
let spec2' = {spec2 with special_deck = deck1'; player_cards = pc1';
                         sum = [(0,12); (1,14)]; bomb_loc = [(-1,7)]}
let spec3 = {spec2 with special_deck = [defuse;defuse]} 
let spec3' = {spec3 with special_deck = [defuse]; bomb_loc = [(-1,7)];
                         special_hands = [(0,[defuse]);(1,[])];}
let specd = {spec3' with turn = 0; 
                         special_hands = [(0,[defuse;defuse]);(1,[])];}
let specd2 = {spec3' with turn = 1; 
                          special_hands = [(0,[]);(1,[defuse;defuse])];}

let fore1 = {specd2 with special_hands = [(0,[]);(1,[defuse;foresee;foresee])];
                         bomb_loc = [(-1,0);(0,2)]}
let fore2 = {fore1 with special_hands = [(0,[]);(1,[defuse;foresee])];
                        bomb_loc = [(-1,3);(0,2)]}
let wa1 = {spec1 with turn = 1}
let wa2 = {wa1 with player_cards = [(0,[sa]);(1,deck2)];}
let wa3 = { turn = 2;
            deck = deck1;
            player_cards = [(0,[sa]);(1,deck2);(2,deck2)];
            difficulty = Special;
            score = [(0,0);(1,0);(2,0)];
            sum = [(0,21); (1,14);(2,14)]; 
            names = nl;
            special_hands = [(0,[]);(1,[]);(1,[])];
            special_deck = deck1;
            bomb_loc = [(-1,0)];}
let nod = { turn = 1;
            deck = deck1;
            player_cards = [(0,[sa]);(1,deck2)];
            difficulty = Special;
            score = [(0,0);(1,0)];
            sum = [(0,21); (1,14)]; 
            names = [(0,"a");(1,"b")];
            special_hands = [(0,[]);(1,[])];
            special_deck = deck1;
            bomb_loc = [(-1,0)]}

let stt = {ste with player_cards = []; sum = [(0,21); (1,21)]}
let b1 = {stt with sum = [(0,22); (1,21)]}
let b2 = {stt with sum = [(0,22); (1,21)]; turn = 1;}
let st11 = {stt with score = [(0,10);(1,4)]}
let st12 = {stt with score = [(0,1);(1,10)]}
let st13 = {stt with score = [(0,10);(1,10)]}
let stee = {stt with player_cards = []}
let st3 = {st1 with turn = 3;}
let w1 = {ste with score = [(0,8);(1,0)];}
let w2 = {ste with score = [(0,0);(1,8)];}
let o1 = {st1 with score = [(0,-11);(1,4)]}
let o2 = {st1 with score = [(0,1);(1,-10)]}
let o2' = {st1 with score = [(0,1);(1,-10)]; turn = 1;}
let bo1 = {spec1 with turn = 1; bomb_loc = [(-1,8);(-1,0)];}

let deal_test
    (name: string) 
    (num: int)
    (deck: deck)
    (output: (deck * hand list)): test = 
  name >:: (fun _ -> assert_equal output (deal num deck))

let deal_test_error
    (name: string) 
    (num: int)
    (deck: deck)
    (output: exn): test = 
  name >:: (fun _ -> assert_raises output (fun () -> deal num deck))

let deck = [ sa;s2;s3;sk;sa;ca;ha;da;dk;d9; ]
let deal_5 = ([], [(0, [dk;d9]);(1, [ha;da;]);(2, [sa;ca;]);
                   (3, [s3;sk;]);(4, [sa;s2;]); ])
let deal_0 = ([sa;ca;ha;da;dk;d9], [(0, [s3;sk;]);(1, [sa;s2;]);])

let deal_tests = 
  [
    deal_test "deck5 deal5" 5 deck deal_5;
    deal_test "deck5 deal2" 2 deck deal_0;
    deal_test_error "deck0 deal2" 2 [] OutOfCards;
    deal_test_error "deck5 deal6" 2 [] OutOfCards;
  ]

let deal_one_test
    (name: string) 
    (deck: deck)
    (output: (card * deck)): test = 
  name >:: (fun _ -> assert_equal output (deal_one deck))

let deal_one_test_error
    (name: string) 
    (deck: deck)
    (output: exn): test = 
  name >:: (fun _ -> assert_raises output (fun () -> deal_one deck))

let deal_one_tests=
  [
    deal_one_test "deal5" deck (sa, [s2;s3;sk;sa;ca;ha;da;dk;d9]);
    deal_one_test "deal1" [s2] (s2, []);
    deal_one_test_error "deal0" [] OutOfCards;
  ]

let check_bust_test
    (name: string) 
    (st: state) 
    (output: bool): test = 
  name >:: (fun _ -> assert_equal output (check_bust st))

let check_bust_tests =
  [
    check_bust_test "bust " b1 true;
    check_bust_test "not bust" b2 false;
  ]

let sum_cards_test
    (name: string) 
    (input: hand)
    (output: (id * int)): test = 
  name >:: (fun _ -> assert_equal output (sum_cards input))

let sum_cards_tests = 
  [ 
    sum_cards_test "hand1" hand1 (1,16);
    sum_cards_test "hand2" hand2 (1,14);
    sum_cards_test "hand3" hand3 (1,21);
    sum_cards_test "hand4" hand4 (1,22);
    sum_cards_test "hand5" hand5 (1,21);
    sum_cards_test "hand6" hand6 (1,22); 
  ]

let init_state_test
    (name: string) 
    (nl: name list)
    (output: int): test = 
  name >:: (fun _ -> 
      assert_equal output (List.length (init_state nl).player_cards ))

let init_special_test
    (name: string) 
    (nl: name list)
    (output: int * int * int): test = 
  name >:: (fun _ -> 
      assert_equal output ((List.length (init_special nl).player_cards),
                           (List.length (init_special nl).special_hands),
                           (List.length (init_special nl).bomb_loc)))

let init_tests=
  [
    init_state_test "1 player" [(0,"a");(1,"dealer")] 2;
    init_state_test "2 players" [(0,"a");(1,"b");(2,"dealer")] 3;
    init_special_test "1 player special" [(0,"a");(1,"dealer")] (2,2,2);
    init_special_test "2 players special" [(0,"a");(1,"b");(2,"dealer")] 
      (3,3,3);
  ]

let get_deck_test
    (name: string) 
    (state:state)
    (output: deck): test = 
  name >:: (fun _ -> assert_equal output (get_deck state))

let get_hands_test
    (name: string) 
    (state:state)
    (output: hand list): test = 
  name >:: (fun _ -> assert_equal output (get_hands state))

let get_score_test
    (name: string) 
    (state:state)
    (output: (id * int) list): test = 
  name >:: (fun _ -> assert_equal output (get_score state))

let get_sum_test
    (name: string) 
    (state:state)
    (output: (id * int) list): test = 
  name >:: (fun _ -> assert_equal output (get_sum state))

let get_diff_test
    (name: string) 
    (state:state)
    (expected_output: difficulty_level): test = 
  name >:: (fun _ -> assert_equal expected_output (get_diff state))

let get_turn_test     
    (name: string)
    (st: state)
    (output: id) = 
  name >:: (fun _ -> assert_equal output (get_turn st) 
               ~printer: string_of_int)

let get_names_test 
    (name: string)
    (st: state)
    (output: name list) = 
  name >:: (fun _ -> assert_equal output (get_names st))

let get_special_deck_test
    (name: string)
    (st: state)
    (output: int) = 
  name >:: (fun _ -> assert_equal output (List.length (get_special_deck st))
               ~printer: string_of_int)

let get_special_hands_test
    (name: string)
    (st: state)
    (output: int) = 
  name >:: (fun _ -> assert_equal output (List.length (get_special_hands st)))

let get_bomb_loc_test
    (name: string)
    (st: state)
    (output: int) = 
  name >:: (fun _ -> assert_equal output (List.length (get_bomb_loc st)))

let get_tests = 
  [
    get_deck_test "get deck st1" st1 deck1;
    get_deck_test "get deck st1" ste [];
    get_hands_test "get hands s1" st1 pc1;
    get_score_test "get score st1" st1 [(0,0);(1,0)];
    get_sum_test "get sum st1" st1 [(0,21); (1,14)];
    get_diff_test "get diff st1" st1 Normal;
    get_turn_test "turn st1" st1 0;
    get_turn_test "turn hit1" hit1 1;
    get_turn_test "turn st3" st3 3;
    get_names_test "names 1" (init_state [(0,"a");(1,"dealer")])
      [(0,"a");(1,"dealer")];
    get_special_deck_test "special init" (init_special [(0,"a");(1,"dealer")]) 
      82;
    get_special_hands_test "special hands"(init_special [(0,"a");(1,"dealer")]) 
      2;
    get_bomb_loc_test "special bomb"(init_special [(0,"a");(1,"dealer")]) 2;
  ]

let print_score_test
    (name: string)
    (st: state)
    (output: string) = 
  name >:: (fun _ -> assert_equal output (print_score st) 
               ~printer: String.escaped) 

let check_win_test 
    (name: string)
    (st: state )
    (output: id * bool) = 
  name >:: (fun _ -> assert_equal output (check_win st ))

let check_win_tests = 
  [
    check_win_test "none wins" stt (0,false);
    check_win_test "user wins" w1 (0,true);
    check_win_test "dealer wins" w2 (1,true);
  ]

let hit_test 
    (name: string)
    (st: state )
    (output: state) = 
  name >:: (fun _ -> assert_equal output (hit st))

let hit_error_test 
    (name: string)
    (st: state )
    (output: exn) = 
  name >:: (fun _ -> assert_raises output (fun () -> hit st))

let hit_tests = 
  [
    hit_test "hit st1" st1 st1';
    hit_test "hit st1 1" hit1 st1'';
    hit_test "hit st1 2" st2 st2';
    (* hit special *)
    hit_test "hit special table cards" spec2 spec2';
    hit_test "hit special special cards" spec3 spec3';
    (* exception handling tests *)
    hit_error_test "hit empty normal" ste OutOfCards;
    hit_error_test "hit empty special" spece OutOfCards;
    hit_error_test "hit bomb" spec1 Bomb;
  ]

let print_sum_test 
    (name: string) 
    (st: state) 
    (output: string): test = 
  name >:: (fun _ -> assert_equal output (print_sum st) 
               ~printer: String.escaped)

let format_cards_text_test 
    (name: string) 
    (cl: card list) 
    (b: bool)
    (output: string): test = 
  name >:: (fun _ -> assert_equal output (format_cards_text cl b) 
               ~printer: String.escaped)

let assign_name_test
    (name: string) 
    (i: id) 
    (nl: name list)
    (b: bool)
    (output: string): test = 
  name >:: (fun _ -> assert_equal output (assign_name i nl b) 
               ~printer: String.escaped)

let print_tests = [
  print_score_test "print init score" (init_state [(0,"a");(1,"dealer")]) 
    "a: 0, dealer: 0";
  print_score_test "print init score" (init_special [(0,"a");(1,"dealer")]) 
    "a: 0, dealer: 0";
  print_score_test "print st1 score" st1 "a: 0, dealer: 0";
  print_sum_test "print st1 sum" st1 "21\n";
  print_sum_test "print st1' sum" st1' "12\n";
  format_cards_text_test "format text deck2 true" deck2 true 
    "Ace of Spades, Ace of Clubs, Ace of Hearts, Ace of Diamonds";
  format_cards_text_test "format text deck2 false" deck2 false 
    "__, Ace of Clubs, Ace of Hearts, Ace of Diamonds";
  format_cards_text_test "format text deck3 true" deck3 true 
    "Ace of Spades, King of Spades";
  format_cards_text_test "format text deck3 false" deck3 false 
    "__, King of Spades";
  assign_name_test "assign dealer true" 2 nl true "dealer: ";
  assign_name_test "assign dealer false" 2 nl false "dealer";
  assign_name_test "assign player true" 1 nl true "b: ";
  assign_name_test "assign player false" 0 nl false "a";
]

let update_score_test 
    (name: string) 
    (st: state)
    (output: string * (State.id * int) list): test = 
  name >:: (fun _ -> assert_equal output (update_score st))

let update_score_tests = 
  [
    update_score_test "update a win" st1 ("a",[(0,1);(1,0)]);
    update_score_test "update dealer win" st1' ("dealer", [(0,0);(1,1)]);
    update_score_test "update dealer, a tie" stt 
      ("There is a tie: a, dealer",[(0,1);(1,1)]);
  ]

let create_name_list_test 
    (name: string) 
    (nl: string list)
    (output: name list): test = 
  name >:: (fun _ -> assert_equal output (create_name_list nl))

let create_name_list_tests =
  [
    create_name_list_test "name list a b c" ["a";"b";"c"] 
      [(0,"a");(1,"b");(2,"c");(3,"dealer")];
    create_name_list_test "name list a" ["a"] [(0,"a");(1,"dealer")];
  ]

let discard_special_test 
    (name: string) 
    (c: card)
    (st: state)
    (output: hand list): test = 
  name >:: (fun _ -> assert_equal output (discard_special c st).special_hands)

let discard_special_tests = [
  discard_special_test "discard defuse 0" defuse spec3' [(0,[]);(1,[])];
  discard_special_test "discard defuse2 0" defuse specd [(0,[defuse]);(1,[])];
  discard_special_test "discard defuse2 0" defuse specd2 [(0,[]);(1,[defuse])];
]

let minus_six_test 
    (name: string) 
    (st: state)
    (output: (id * int) list): test = 
  name >:: (fun _ -> assert_equal output (minus_six st).score)

let minus_six_tests = [
  minus_six_test "a minus six" ste [(0,-6);(1,0)];
  minus_six_test "dealer minus six" hit1 [(0,0);(1,-6)];
]

let update_out_test 
    (name: string) 
    (st: state)
    (output: name list): test = 
  name >:: (fun _ -> assert_equal output (update_out st).names)

let check_out_test     
    (name: string) 
    (st: state)
    (output: bool): test = 
  name >:: (fun _ -> assert_equal output (check_out st))

let get_win_out_test 
    (name: string) 
    (st: state)
    (output: string): test = 
  name >:: (fun _ -> assert_equal output (get_win_out st)
               ~printer: String.escaped)

let out_tests = [
  update_out_test "update out none" st1  [(0,"a");(1,"dealer")];
  update_out_test "update out 0" o1  [(0,"dealer")];
  update_out_test "update out 1 wrong turn" o2  [(0,"a");(1,"dealer")];
  update_out_test "update out 1" o2'  [(0,"a")];
  check_out_test "None" st1 false;
  check_out_test "out 0" o1 true;
  check_out_test "out 1" o2 false;
  check_out_test "out 0 wrong turn" o2' true;
  get_win_out_test "dealer win bc out" (update_out o1) "dealer";
  get_win_out_test "a win bc out" (update_out o2') "a";
]

let remove_bomb_test
    (name: string) 
    (st: state)
    (output: ((id * int) list)): test = 
  name >:: (fun _ -> assert_equal output (remove_bomb st).bomb_loc)

let insert_bomb_test 
    (name: string) 
    (i: int)
    (st: state)
    (output: ((id * int) list)): test = 
  name >:: (fun _ -> assert_equal output (insert_bomb i st).bomb_loc)

let bomb_tests = [
  remove_bomb_test "remove single bomb" spec1 [];
  remove_bomb_test "remove 1/2 bomb" bo1 [(-1,8)];
  insert_bomb_test "insert bomb 0" 0 spec1 [(0,0)];
  insert_bomb_test "insert bomb 4" 4 bo1 [(-1,9);(1,4)];
]

let use_steal_test 
    (name: string) 
    (id: id)
    (st: state)
    (cid: int)
    (output: hand list): test = 
  name >:: (fun _ -> assert_equal output ((use_steal id st cid) |> get_hands))

let use_switch_test 
    (name: string) 
    (id: id)
    (st: state)
    (output: hand list): test = 
  name >:: (fun _ -> assert_equal output ((use_switch id st ) 
                                          |> get_hands))

let use_tests = [
  use_steal_test "steal card 1 from player index 2" 
    2 steal1 1 [(0,[dk;sa;s2]);(1,[da;sa;d9;da;d10]);(2,[sa;s3])];
  use_steal_test "steal card 2 from player index 1" 
    1 steal1 2 [(0,[dk;sa;d9]);(1,[da;sa;da;d10]);(2,[sa;s2;s3])];
  use_steal_test "steal card 4 from player index 1" 
    1 steal1 4 [(0,[dk;sa;d10]);(1,[da;sa;d9;da]);(2,[sa;s2;s3])];
  use_steal_test "player 1 steal card 1 from player index 0" 
    0 steal2 1 [(0,[dk]);(1,[da;sa;d9;da;d10;sa]);(2,[sa;s2;s3])] ;
  use_switch_test "player 1 switch with player 2" 
    2 steal2 [(0,[dk;sa]);(1,[sa;s2;s3]);(2,[da;sa;d9;da;d10])];
  use_switch_test "player 0 switch with player 1" 
    1 steal1 [(0,[da;sa;d9;da;d10]);(1,[dk;sa]);(2,[sa;s2;s3])];
  use_switch_test "player 0 switch with player 2" 
    2 steal1 [(0,[sa;s2;s3]);(1,[da;sa;d9;da;d10]);(2,[dk;sa])];
]

let dealer_foresee_test1
    (name: string) 
    (st: state)
    (output: ((id * int) list)): test = 
  name >:: (fun _ -> assert_equal output (st |> dealer_foresee |> get_bomb_loc))

let dealer_foresee_test2
    (name: string) 
    (st: state)
    (output: card list): test = 
  name >:: (fun _ -> assert_equal output (st |> dealer_foresee |> 
                                          get_special_hands |> 
                                          List.assoc (get_turn st)))

let dealer_want_test
    (name: string) 
    (st: state)
    (output: int): test = 
  name >:: (fun _ -> assert_equal output (dealer_want st) 
               ~printer: string_of_int)

let dealer_in_test 
    (name: string) 
    (st: state)
    (output: bool): test = 
  name >:: (fun _ -> assert_equal output (dealer_in st) 
               ~printer: string_of_bool)

let check_dealer_test
    (name: string) 
    (st: state)
    (output: bool): test = 
  name >:: (fun _ -> assert_equal output (check_dealer st) 
               ~printer: string_of_bool)

let dealer_tests = [
  dealer_foresee_test1 "dealer use foresee 1" fore1 [(1,2);(1,0)];
  dealer_foresee_test2 "dealer use foresee discard" fore1 [defuse;foresee];
  dealer_foresee_test1 "dealer use foresee 2" fore2 [(1,2);(-1,3)];
  dealer_foresee_test2 "dealer use foresee 2 discard" fore2 [defuse];
  dealer_want_test "one choice want" wa1 0;
  dealer_want_test "no choice want" wa2 (-1);
  dealer_want_test "top choice want" wa3 1;
  dealer_in_test "dealer is in 1" wa1 true;
  dealer_in_test "dealer is in 2" wa3 true;
  dealer_in_test "dealer not in" nod false;
  check_dealer_test "is dealer's turn 1" wa3 true; 
  check_dealer_test "is dealer's turn 2" hit1 true; 
  check_dealer_test "not dealer's turn 1" nod false; 
  check_dealer_test "not dealer's turn 2" st1 false; 
]

let get_id_from_name_test
    (name: string) 
    (st: state)
    (n: string)
    (output: id): test = 
  name >:: (fun _ -> assert_equal output (get_id_from_name st n) 
               ~printer: string_of_int)

let last_id_test
    (name: string) 
    (st: state)
    (output: int): test = 
  name >:: (fun _ -> assert_equal output (last_id st) 
               ~printer: string_of_int)

let id_tests = [
  get_id_from_name_test "get dealer id" st1 "dealer" 1;
  get_id_from_name_test "get player1 id" st1 "a" 0;
  last_id_test "2 players" st1 1;
  last_id_test "3 players" wa3 2;
]

let parse_test
    (name: string) 
    (input: string) 
    (output: command): test = 
  name >:: (fun _ -> assert_equal output (parse input))

let parse_test_error
    (name: string) 
    (input: string)
    (output: exn): test = 
  name >:: (fun _ -> assert_raises output (fun () -> parse input))

let parse_yes_test
    (name: string) 
    (input: string)
    (output: yes_no): test = 
  name >:: (fun _ -> assert_equal output (parse_yes input))

let parse_num_test
    (name: string) 
    (input: string)
    (output: num_players): test = 
  name >:: (fun _ -> assert_equal output (parse_num input))

let parse_name_test
    (name: string) 
    (input: string)
    (output: player_names): test = 
  name >:: (fun _ -> assert_equal output (parse_names input))

let parse_level_test
    (name: string) 
    (input: string) 
    (output: difficulty_level): test = 
  name >:: (fun _ -> assert_equal output (parse_level input))

let parse_tests = 
  [
    (* parse *)
    parse_test "parse start" "start" Start;
    parse_test "parse s case" "S" Stand;
    parse_test "parse start case" "Start" Start; 
    parse_test "parse start space" "start " Start;
    parse_test "parse restart" "restart " Restart;
    parse_test "parse quit" "quit " Quit;
    parse_test "parse rules" "rules " Rules;
    parse_test "parse hit" "hit " Hit;
    parse_test "parse h" "h " Hit;
    parse_test "parse stand" "stand " Stand;
    parse_test "parse score" "score " Score;
    parse_test "parse next" "next " Next;
    parse_test "parse n" "n " Next;
    (* parse_yes *)
    parse_yes_test "parse yes" "yes" Yes;
    parse_yes_test "parse no" "no" No;
    (* parse_names *)
    parse_name_test "parse a b c" "a b c" (StrList ["a";"b";"c"]);
    parse_name_test "parse a" "a" (StrList ["a"]);
    parse_name_test "parse A" "A " (StrList ["A"]);
    (* parse_num *)
    parse_num_test "parse quit" "quit" (Quit);
    parse_num_test "parse 1" "1" (Int 1);
    parse_num_test "parse one" "one" (Int 1);
    parse_num_test "parse 2" "2" (Int 2);
    parse_num_test "parse two" "two" (Int 2);
    parse_num_test "parse 3" "3" (Int 3);
    parse_num_test "parse three" "three" (Int 3);
    parse_num_test "parse 4" "4" (Int 4);
    parse_num_test "parse four" "Four" (Int 4);
    (* parse_level *)
    parse_level_test "parse easy" "normal" Normal;
    parse_level_test "parse easy" "special" Special;
    parse_level_test "parse Quit " "Quit " Quit;
    parse_level_test "parse ruleS" "ruleS" Rules;
    (* parse exception handling - ensure correct exceptions are raised *)
    parse_test_error "parse empty" "" Empty;
    parse_test_error "parse space" " " Empty;
    parse_test_error "parse st art" "st art" Invalid;
    parse_test_error "parse startt" "startt" Invalid;
    parse_test_error "parse random" "random" Invalid;
    parse_test_error "parse ." "." Invalid;
  ]

let get_keys_test
    (name: string) 
    (input: ('a * 'b) list) 
    (output: 'a list): test = 
  name >:: (fun _ -> assert_equal output (get_keys input))

let get_fullnames_test
    (name: string) 
    (input: (string * string) list) 
    (output: string list): test = 
  name >:: (fun _ -> assert_equal output (get_fullnames input))

let save_tests = 
  [
    get_keys_test "keys empty" [] [];
    get_keys_test "keys example" [(0,0);(1,0);(2,0)] [0;1;2];
    get_fullnames_test "fullnames empty" [] [];
    get_fullnames_test "fullnames example" [("fst","snd")] ["sndfst"];
    get_fullnames_test "fullnames empty fst" [("","snd")] ["snd"];
    get_fullnames_test "fullnames empty snd" [("fst","")] ["fst"];
    get_fullnames_test "fullnames empty empty" [("","")] [""];
    get_fullnames_test "fullnames 1 2" [("1","2")] ["21"];
  ]

let suite = "blackjack test suite" >::: List.flatten 
              [ 
                sum_cards_tests;
                deal_tests;
                deal_one_tests;
                check_bust_tests;
                init_tests;
                get_tests;
                print_tests;
                check_win_tests;
                parse_tests;
                hit_tests;
                update_score_tests;
                create_name_list_tests;
                discard_special_tests;
                minus_six_tests;
                out_tests;
                bomb_tests;
                use_tests;
                dealer_tests;
                id_tests;
                save_tests;
              ]

let _ = run_test_tt_main suite