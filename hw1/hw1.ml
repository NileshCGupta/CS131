type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec inset a b = match b with 
	| [] -> false
	| h :: t -> if h = a
			then true
		else inset a t
;;

let rec subset a b = match a with
	| [] -> true
	| h :: t -> if inset h b
			then subset t b
		else false
;;

let equal_sets a b =
	if subset a b && subset b a
		then true
	else false
;;

let set_union a b = a @ b
;;

let rec set_intersection a b = match a with
	| [] -> []
	| h :: t -> if inset h b
			then h :: set_intersection t b
		else set_intersection t b
;;

let rec set_diff a b = match a with
	| [] -> []
	| h :: t -> if inset h b
			then set_diff t b
		else h :: set_diff t b
;;

let rec computed_fixed_point eq f x = 
	if eq x (f x)
		then x
	else (computed_fixed_point eq f (f x))
;;

let rec period_processor eq f p x = 
	if p = 1
		then (f x)
	else period_processor eq f (p - 1) (f x)
;;

let rec computed_periodic_point eq f p x = 
	if p = 0
		then x
	else if eq (period_processor eq f p x) x
		then x
	else (computed_periodic_point eq f p (f x))
;;

let rec while_away s p x = 
	if p x
		then x :: (while_away s p (s x))
	else []
;;

let sub_by_one x = let (x1, x2) = x in (x1 - 1, x2)
;;

let rec rle_decode lp = match lp with
	| [] -> []
	| h :: t -> if (fst h) > 0
			then (snd h) :: (rle_decode ((sub_by_one h)::t))
		else rle_decode t
;;

(* filter_blind_alleys implementation *)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let check_if_terminal sym terminable_symbols = match sym with
	| T x -> true
	| N x -> inset x terminable_symbols
	| _ -> false
;;

let rec is_rhs_terminable rhs terminable_symbols = match rhs with
	| [] -> true
	| h :: t -> if check_if_terminal h terminable_symbols
				then is_rhs_terminable t terminable_symbols
			else false
;;

let rec make_term_syms_list rules terminable_symbols = match rules with
	| [] -> terminable_symbols
	| (i, j) :: t -> if is_rhs_terminable j terminable_symbols
						then make_term_syms_list t (i :: terminable_symbols)
					else make_term_syms_list t terminable_symbols
;;

let rec make_final_list rules terminable_symbols filtered_rules = match rules with
	| [] -> filtered_rules
	| (i, j) :: t -> if is_rhs_terminable j terminable_symbols
						then make_final_list t terminable_symbols (filtered_rules @ [(i, j)])
					else make_final_list t terminable_symbols filtered_rules
;;

let filter_blind_alleys (start_sym, rules) = 
	start_sym, 
	make_final_list rules (computed_fixed_point (equal_sets) (make_term_syms_list rules) []) []
;;
