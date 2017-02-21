#use "hw2.ml";;

let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

(* An example grammar for a small subset of Awk, derived from but not
   identical to the grammar in
   <http://web.cs.ucla.edu/classes/winter06/cs132/hw/hw1.html>.
   Note that this grammar is not the same as Homework 1; it is
   instead the same as the grammar under "Theoretical background"
   above.  *)

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

type test_nonterminals =
  | Clause | Noun | Verb | Person | Place | Thing | Article | Adjective | Action | Preposition | Adverb

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let rec accept_only_plus derivation frag = 
	match frag with
	| [] -> None
	| h :: t -> if h = "+"
					then Some(derivation, frag)
				else None

let test_1 = (* this test case makes sure that the program calls the acceptor with each appropriate prefix *)
  ((parse_prefix awkish_grammar accept_only_plus ["9"; "-"; "9"; "+"; "8"; "-"]) = 
  	Some
  		([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "9"]); (Binop, [T "-"]); 
  			(Expr, [N Term]); (Term, [N Num]); (Num, [T "9"])], ["+"; "8"; "-"])) 
  		

let test_grammar =
  (Clause,
   function
     | Clause ->
         [[N Noun; N Verb]]
     | Noun ->
	 [[N Person];
	  [N Place];
	  [N Thing];
	  [N Article; N Noun];
	  [N Adjective; N Noun]]
     | Article ->
	 [[T"The"];
	  [T"the"];
	  [T"a"];
	  [T"an"]]
     | Person ->
	 [[T"man"];
	  [T"woman"]]
	 | Place ->
	 [[T"Pheonix"];
	  [T"mall"];
	  [T"Buffalo"]]
	 | Thing ->
	 [[T"fox"];
	  [T"dog"];
	  [T"Buffalo"]]
	 | Adjective ->
	 [[T"quick"];
	  [T"brown"];
	  [T"lazy"];
	  [T"Buffalo"]]
     | Verb ->
	  [[N Action];
	   [N Action; N Preposition];
	   [N Verb; N Noun];
	   [N Adverb; N Verb]]
	 | Action ->
	  [[T"jumped"];
	   [T"Buffalo"]]
	 | Adverb ->
	  [[T"furiously"];
	   [T"not"]]
	 | Preposition ->
	  [[T"over"]])


let test_2 = ((parse_prefix test_grammar accept_empty_suffix ["The"; "quick"; "brown"; "fox"; "jumped"; "over"; "the"; "lazy"; "dog"]) =
				Some ([(Clause, [N Noun; N Verb]); (Noun, [N Article; N Noun]); (Article, [T "The"]); 
					(Noun, [N Adjective; N Noun]); (Adjective, [T "quick"]); (Noun, [N Adjective; N Noun]); 
					(Adjective, [T "brown"]); (Noun, [N Thing]); (Thing, [T "fox"]); (Verb, [N Verb; N Noun]); 
					(Verb, [N Action; N Preposition]); (Action, [T "jumped"]); (Preposition, [T "over"]); 
					(Noun, [N Article; N Noun]); (Article, [T "the"]); (Noun, [N Adjective; N Noun]); 
					(Adjective, [T "lazy"]); (Noun, [N Thing]); (Thing, [T "dog"])], []))


(* SPEC TEST CASES *)

let test0 =
  ((parse_prefix awkish_grammar accept_all ["ouch"]) = None)

let test1 =
  (parse_prefix awkish_grammar accept_all ["9"])
   = Some ([(Expr, [N Term]); (Term, [N Num]); (Num, [T "9"])], [])

let test2 =
  ((parse_prefix awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"])
   = Some
       ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "9"]);
	 (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Lvalue]);
	 (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Num]);
	 (Num, [T "1"])],
	["+"]))

let test3 =
  ((parse_prefix awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"])
   = None)

(* This one might take a bit longer.... *)
let test4 =
 ((parse_prefix awkish_grammar accept_all
     ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
      "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
      "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
      "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";
      "++"; "+"; "0"])
  = Some
     ([(Expr, [N Term; N Binop; N Expr]); (Term, [T "("; N Expr; T ")"]);
       (Expr, [N Term]); (Term, [N Lvalue]); (Lvalue, [T "$"; N Expr]);
       (Expr, [N Term]); (Term, [N Num]); (Num, [T "8"]); (Binop, [T "-"]);
       (Expr, [N Term; N Binop; N Expr]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "++"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "--"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Num]); (Num, [T "9"]); (Binop, [T "+"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Lvalue]); (Lvalue, [T "$"; N Expr]);
       (Expr, [N Term; N Binop; N Expr]); (Term, [N Incrop; N Lvalue]);
       (Incrop, [T "++"]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "2"]); (Binop, [T "+"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]); (Term, [N Num]);
       (Num, [T "8"]); (Binop, [T "-"]); (Expr, [N Term]); (Term, [N Num]);
       (Num, [T "9"]); (Binop, [T "-"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Incrop; N Lvalue]);
       (Incrop, [T "++"]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "5"]); (Incrop, [T "++"]); (Incrop, [T "++"]);
       (Incrop, [T "--"]); (Binop, [T "-"]); (Expr, [N Term]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "++"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]);
       (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "8"]); (Incrop, [T "++"]); (Incrop, [T "++"]);
       (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "0"])],
      []))

let rec contains_lvalue = function
  | [] -> false
  | (Lvalue,_)::_ -> true
  | _::rules -> contains_lvalue rules

let accept_only_non_lvalues rules frag =
  if contains_lvalue rules
  then None
  else Some (rules, frag)

let test5 =
  ((parse_prefix awkish_grammar accept_only_non_lvalues
      ["3"; "-"; "4"; "+"; "$"; "5"; "-"; "6"])
   = Some
      ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "3"]);
	(Binop, [T "-"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "4"])],
       ["+"; "$"; "5"; "-"; "6"]))