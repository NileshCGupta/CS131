type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

let rec curry_func rules lhs = match rules with
 	| [] -> []
 	| (sym, rhs) :: t -> if sym = lhs
 							then rhs :: (curry_func t sym)
 						else 
 							curry_func t sym
;;

let convert_grammar g = 
	(fst g), (curry_func (snd g))
;;

let rec rule_matcher accept lhs rhs frag d prod_func = 
	match rhs with
	| [] -> accept d frag
	| h :: t -> match h with
		| N x -> start_matcher x (prod_func x) accept frag d prod_func
		| T x -> match frag with 
			| h1 :: t1 -> if h1 = x
							then rule_matcher accept lhs rhs t (d@[(lhs, rhs)]) prod_func
						else None

and start_matcher lhs alt_list accept frag d prod_func = 
	match frag with
	| [] -> accept d frag
	| firstterm :: rest -> match alt_list with 
		| [] -> None
		| h :: t -> match (rule_matcher accept lhs h frag (d @ [(lhs, h)]) prod_func) with
			| None -> start_matcher lhs t accept frag d prod_func
			| Some(d,s) -> match accept d s with
				| Some(de,su) -> Some(de,su)
				| None -> start_matcher lhs t accept frag d prod_func

(* and reg_matcher lhs alt_list frag d prod_func = 
	match alt_list with 
	| [] -> None
	| h :: t -> match (rule_matcher accept lhs h frag (d @ [(lhs, h)]) prod_func) with
		| Some(d,s) -> Some(d,s)
		| None -> reg_matcher lhs t frag d prod_func *)

let parse_prefix gram accept frag =
	start_matcher (fst gram) ((snd gram) (fst gram)) accept frag [] (snd gram)
;;
