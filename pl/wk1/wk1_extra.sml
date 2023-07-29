(* 1. takes a list of numbers and adds them with alternating sign *)

fun alternate (li : int list) =
    if null li
    then 0
    else
	if null (tl li)
	then hd li
	else hd li - hd (tl li) + alternate (tl (tl li))
					    
(* 2. takes a non-empty list of numbers, and returns a pair (min, max) of the minimum and maximum of the numbers in the list *)

fun min_max(li : int list) =
    let
	fun min(li : int list) =
	    let val first = hd li val rest = tl li in
		if null rest
		then first
		else
		    if first < hd rest
		    then min(first::tl rest)
		    else min(rest)
	    end
	fun max(li : int list) =
	    let val first = hd li val rest = tl li in
		if null rest
		then first
		else
		    if first > hd rest
		    then max(first::tl rest)
		    else max(rest)
	    end
    in
	((min li), (max li))
    end

(* 3. takes a list of numbers and returns a list of the partial sums of those numbers ie. [1,4,20] = [1,5,25] *)
	
fun cumsum (li : int list) =						    
    if null li
    then []
    else
	if null (tl li)
	then li
	else hd li::cumsum((hd li + (hd (tl li)))::(tl (tl li)))

(* 4. takes a string option "SOME name" and returns "Hello there, name!". if the option is NONE, then replace name with "you". *)
			  
fun greeting (opt_name : string option) =
    let val name = if isSome opt_name
		   then valOf opt_name
		   else "you"
    in
	"Hello there, " ^ name ^ "!"
    end
	
(* 5. takes a list of ints and another list of non-negative ints, repeats the ints in the first list according to the numbers indicated by the
second list *)

fun repeat (repeaters : int list, counts : int list) =
    if null repeaters
    then []
    else
	if hd counts = 0
	then repeat((tl repeaters), (tl counts))
	else hd repeaters :: repeat(repeaters, ((hd counts) - 1) :: (tl counts))

 (* 6. takes two optional integers, adds them if they are both present (returning an int option), or returns NONE if at least one of
 the two arguments is NONE *)

fun addOpt (n1 : int option, n2 : int option) =
    if isSome n1 andalso isSome n2
    then SOME (valOf n1 + (valOf n2))
    else NONE

(* 7. takes a list of int options, adds those integers that are present. returns NONE if no SOME's are present in the list or the list is empty *)

fun addAllOpt (li0 : int option list) =
    if null li0
    then NONE
    else
	let fun accumOpt (li : int option list) =
		if null li
		then 0
		else
		    if isSome (hd li)
		    then valOf (hd li) + accumOpt (tl li)
		    else accumOpt (tl li)
	    val accum_li0 = accumOpt li0
	in
	    if accum_li0 = 0
	    then NONE
	    else SOME accum_li0
	end;

(* 8. takes a list of booleans. returns true if there is at least one of them that is true, otherwise, returns false. *)

fun any (li : bool list) =
    if null li
    then false
    else
	if hd li
	then true
	else any (tl li);

(* 9. takes a boolean list, returns true if all are true, otherwise, returns false *)

fun all (li : bool list) =
    if null li
    then true
    else
	if hd li
	then all (tl li)
	else false;

(* 10. takes two int lists and creates consecutive pairs. stops when one of the lists is empty. *)

fun zip (li1 : int list, li2 : int list) =
    if null li1 orelse null li2
    then []
    else ((hd li1), (hd li2)) :: zip ((tl li1), (tl li2));

(* 11. same as 10 except when one list is empty it starts recycling from its start until the other list completes. *)

fun intlist_length (li : int list) =
    if null li
    then 0
    else 1 + intlist_length (tl li);

fun zipRecycle (li10 : int list, li20 : int list) =
    let fun zipRepeat (rli0 : int list, rli : int list, long_li : int list) =
	    if null long_li
	    then []
	    else
		if null rli
		then zipRepeat (rli0, rli0, long_li)
		else ((hd rli), (hd long_li)) :: zipRepeat (rli0, (tl rli), (tl long_li));
	val len_li1 = intlist_length li10
	val len_li2 = intlist_length li20
    in
	if len_li1 > len_li2
	then zipRepeat (li20, li20, li10)
	else
	    if len_li1 < len_li2
	    then zipRepeat (li10, li10, li20)
	    else zip (li10, li20)
    end;
	
(* 12. same as zip except returns SOME list when the original lists have the same length, and NONE if they do not *)

fun zipOpt (li1 : int list, li2 : int list) =
    if intlist_length li1 = (intlist_length li2)
    then SOME (zip (li1, li2))
    else NONE;

(* 13. takes a list of pairs (s, i) and also a string s2 to look up. if s2 is a match with an s within the list of pairs, the function returns the
corresponding i. *)

fun lookup (p_list : (string * int) list, str : string) =
    if null p_list
    then NONE
    else
	if #1 (hd p_list) = str
	then SOME (#2 (hd p_list))
	else lookup ((tl p_list), str);

(* 14. takes a list of ints and creates two lists of ints, once containing the non-negative entries, the other containing the negative entries.
Preserve the order of the original list *)

fun splitup (ili : int list) =
    if null ili
    then ([],[])
    else
	let val split_rest = splitup (tl ili)
	in
	    if hd ili >= 0
	    then ((hd ili) :: (#1 split_rest),(#2 split_rest))
	    else (#1 split_rest, (hd ili) :: (#2 split_rest))
	end;

(* 15. similar to splitup except it takes an extra "threshold" parameter and uses that instead of 0 as the separating point for the two resulting
list *)

fun splitat (ili : int list, thresh : int) =
if null ili
    then ([],[])
    else
	let val split_rest = splitat ((tl ili), thresh)
	in
	    if hd ili < thresh
	    then ((hd ili) :: (#1 split_rest),(#2 split_rest))
	    else (#1 split_rest, (hd ili) :: (#2 split_rest))
	end;
    

(* 16. takes a list of integers and determines whether the list is sorted in increasing order *)

fun isSortedInc (ili : int list) =
    if null (tl ili)
    then true
    else
	if (hd ili) < (hd (tl ili)) orelse (hd ili) = (hd (tl ili))
	then isSortedInc (tl ili)
	else false
		 
(* 17. takes a list of integers and determines whether the list is sorted in either increasing or decreasing order *)
fun isAnySorted (ili : int list) =
    if null ili orelse null (tl ili)
    then true
    else
	let fun isSortedDec (ili : int list) =
		if null (tl ili)
		then true
		else
		    if (hd ili) > (hd (tl ili)) orelse (hd ili) = (hd (tl ili))
		    then isSortedDec (tl ili)
		    else false
	    val first = hd ili
	    val next = hd (tl ili)
	    val rest = tl ili
	in
	    if (hd ili) = (hd (tl ili))
	    then isAnySorted (tl ili)
	    else
		if (hd ili) > (hd (tl ili))
		then isSortedDec rest
		else isSortedInc rest
	end;

(* 18. takes two lists of ints that are each sorted from smallest to largest and merges them into one sorted list *)
fun sortedMerge (ili1 : int list, ili2 : int list) =
    if null ili1 andalso null ili2
    then []
    else
	if null ili1
	then hd ili2 :: sortedMerge(ili1, (tl ili2))
	else
	    if null ili2
	    then hd ili1 :: sortedMerge ((tl ili1), ili2)
	    else
		if hd ili1 <= (hd ili2)
		then hd ili1 :: sortedMerge ((tl ili1), ili2)
		else hd ili2 :: sortedMerge (ili1, (tl ili2));

(* 19. takes the first element out of an int list, uses it as the threshold for splitAt. It then recursively sorts the two lists produced by splitAt.
Finally it brings the two lists together *)
fun append (li1 : int list, li2 : int list) =
    if null li1
    then li2
    else hd li1 :: (append ((tl li1), li2))

fun qsort (ili : int list) =
    if isSortedInc ili
    then ili
    else
	let val thresh = hd ili
	    val split_li = splitat ((tl ili), thresh)
	    val full_split_li = if null (#1 split_li)
				then ([thresh], (#2 split_li))
				else
				    if null (#2 split_li)
				    then ((#1 split_li), [thresh])
				    else ((#1 split_li), (thresh :: (#2 split_li)))
	in
	    append ((qsort (#1 full_split_li)), (qsort (#2 full_split_li)))
	end;

(* 20. takes a list of ints and produces two lists by alternating elements between the two lists *)
fun divide (ili : int list) =
    if null ili
    then ([],[])
    else
	if null (tl ili)
	then ([(hd ili)], [])
	else let val div_rest = (divide (tl (tl ili))) in
		 ((hd ili) :: (#1 div_rest), (hd (tl ili)) :: (#2 div_rest))
	     end;

(* 21 takes an initial list of ints, splits it in two lists using divide, then recursively sorts those two lists, then merges them together with
sortedMerge. *)
fun not_so_quick_sort (ili : int list) =
    if isSortedInc ili
    then ili
    else
	let val div_list = divide ili
	in
	    sortedMerge(not_so_quick_sort (#1 div_list), not_so_quick_sort (#2 div_list))
	end;

(* 22. given two numbers, k and n, attempts to evenly divide k into n as many times as possible, and returns a pair (d, n2) where d is the number
of times while n2 is the resulting n after all those divisions *)
fun fullDivide (k : int, n : int) =
    if not (n mod k = 0)
    then (0, n)
    else
	let
	    val div_result = n div k
	    val rest_div = fullDivide(k, div_result)
	in
	    (1 + (#1 rest_div), (#2 rest_div))
	end;
		     
(* 23. takes a number, n, and returns a list of pairs, (d,k) where d is a prime number dividing n and k is the number of times it fits. The pairs
should be in increasing order of prime factor, and the process should stop when the divisor considered surpasses the square root of n  *)
fun factorize(n : int) =
    let fun inc_factor (k : int, n : int) =
	    if n = 1
	    then []
	    else
		let val full_div_factor = fullDivide (k, n)
		in
		    if (#1 full_div_factor) = 0
		    then inc_factor (k + 1, n)
		    else
			(k, (#1 full_div_factor)) :: inc_factor (k + 1, (#2 full_div_factor))
		end
    in
	inc_factor(2, n)
    end;

(* 24. given a factorization of a number n, computes back the number n *)
fun expt (b : int, e : int) =
    if e = 0
    then 1
    else b * (expt (b, (e - 1)));

fun multiply (pli : (int * int) list) =
    if null pli
    then 1
    else expt ((hd pli)) * multiply (tl pli);
				   

(* 25. given a factorization list result from factorize, creates a list of all possible products produced from using some or all those prime factors
no more than the number of times they are available. the recursive process should return the numbers in ascending order, as opposed to sorting them
afterwards *)
	
fun all_products (pli : (int * int) list) =
    let
	fun all_pairs_base (pair: (int * int), mark : int) =
	    if (#2 pair) > mark
	    then []
	    else [pair] :: all_pairs_base(((#1 pair), (#2 pair) + 1), mark);
	fun combine_pairs (p1 : (int * int), plists : ((int * int) list) list) =
	    let
		fun one_combo (anchor : (int * int), mark : int, li : (int * int) list) =
		    if (#2 anchor) = mark
		    then [anchor :: li]
		    else (anchor :: li) :: one_combo (((#1 anchor), (#2 anchor) + 1), mark, li);
	    in
		if null plists
		then []
		else one_combo (((#1 p1), 0), (#2 p1), (hd plists)) @ combine_pairs(p1, (tl plists))
	    end
	fun make_all_combos (pli : (int * int) list) =
	    if null (tl pli)
	    then all_pairs_base (((#1 (hd pli)), 0), (#2 (hd pli)))
	    else combine_pairs ((hd pli), make_all_combos (tl pli))
	val all_combos = make_all_combos pli
	fun multiply_combos (combo_list : (int* int) list list) =
	    if null (tl combo_list)
	    then [multiply (hd combo_list)]
	    else multiply (hd combo_list) :: multiply_combos (tl combo_list)
    in
	multiply_combos all_combos
    end
			    




