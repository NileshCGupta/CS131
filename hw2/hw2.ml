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

let rec matcher_or prod_func alt_list lhs accept d frag =
	match frag with
	| [] -> accept d frag
	| firstterm :: therest -> match alt_list with 
		| [] -> None 
		| h :: t -> match (matcher_and prod_func h accept (d@[(lhs, h)]) frag) with
			| Some(d,s) -> Some(d,s)
			| None -> matcher_or prod_func t lhs accept d frag

and matcher_and prod_func rhs accept d frag = 
	match rhs with
	| [] -> accept d frag
	| h :: t -> match frag with
		| [] -> None
		| firstterm :: therest -> match h with
			| N x -> matcher_or prod_func (prod_func x) x (matcher_and prod_func t accept) d frag
			| T x -> if (firstterm = x)
							then matcher_and prod_func t accept d therest 
						else None

let parse_prefix gram accept frag = 
	matcher_or (snd gram) ((snd gram) (fst gram)) (fst gram) accept [] frag
;;