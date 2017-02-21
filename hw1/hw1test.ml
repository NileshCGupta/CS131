let my_subset_test0 = subset [5;2;3] [2;2;3;5]

let my_equal_sets_test0 = not (equal_sets [1;3;3;4] [3;1;3])

let my_set_union_test0 = equal_sets (set_union [3;2] [1;2;3]) [1;2;3]

let my_set_intersection_test0 =
  equal_sets (set_intersection [3;4;2] [1;2;3]) [2;3]

let my_set_diff_test0 = equal_sets (set_diff [1;3;5] [1;4;3;1]) [5]

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x *. x) 2. = infinity

let my_computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x - (2 * x)) 2 (-1) = -1

let my_while_away_test0 = while_away ((+) 5) ((>) 20) 0 = [0;5;10;15]

let my_rle_decode_test0 = rle_decode [1, "h"; 1, "e"; 2, "l"; 1, "o"] = ["h"; "e"; "l"; "l"; "o"]

type insect_nonterminals =
  | Conversation | Mate | Sting | Buzz | Shout | Quiet

let insect_grammar =
  Conversation,
  [Buzz, [T"ZZZ"];
   Quiet, [];
   Sting, [T"ahh"];
   Shout, [T"Roar!"];
   Mate, [N Quiet; N Shout];
   Mate, [N Sting];
   Mate, [N Shout];
   Conversation, [N Buzz];
   Conversation, [N Mate; T","; N Conversation]]

let my_filter_blind_alleys_test0 =
  filter_blind_alleys insect_grammar = insect_grammar
