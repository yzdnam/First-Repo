(* 1. composes two functions with "optional values. If either function returns NONE, then the result is NONE *)
fun compose_opt f1 f2 a =
    case f2 a of
	NONE => NONE
      | SOME b => f1 b;

(* 2.
   do_until : ('a -> 'a) -> ('a -> bool) -> 'a -> 'a
   do_until f p x will apply f to x and f again to that result and so on until p x is false. *)
fun do_until f p x =
    let val fx = f x in
	if p fx
	then do_until f p fx
	else x
    end;
	     
(* 3. implement factorial using do_until *)
fun fact x =
    let val num = ref x in
	do_until (fn x => (num := (!num - 1); x * !num)) (fn p => p <> 0) x
    end;

(* 4.
   fixed_point: (''a -> ''a) -> ''a -> ''a
   given a function f and an initial value x, applies f to x until f x = x *)
fun fixed_point f x =
    let val fx = f x in
	if fx = x then fx else fixed_point f fx
    end;

(* 5.
   map2 ('a -> 'b) -> 'a * 'a -> 'b * 'b
   given a function that takes 'a values to 'b values and a pair of 'a values, returns the corresponding pair of 'b values *)
fun map2 f (a,b) =
    (f a,f b);

(* 6.
   app_all ('b -> 'c list) -> ('a -> 'b list) -> 'a -> 'c list
   app_all f g x will apply f to every element of the list g x and concatenates the results into a single list *)
fun app_all f g x =
    (foldr (op @) [] (List.map f (g x)));

(* 7.
   implement foldr *)
fun mfoldr f init li =
    case li of
	[] => init
      | first::rest => f(first,(mfoldr f init rest));

(* 8.
   partition : ('a -> bool) -> 'a list -> 'a list * 'a list
   the first part of the result contains the second argument elements for which the first element evaluates to true and the second part of the result
   contains the other second argument elements *)
fun partition p li =
    let
	fun true_part elem (x,y) =
	    (elem::x,y)
	fun false_part elem (x,y) =
	    (x,elem::y)
    in
	case li of
	    [] => ([],[])
	  | first::rest => let val result = partition p rest in
			       if (p first) then true_part first result else false_part first result
			   end
    end;
	
(* 9.
   unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list
   produces a list of 'b values given a "seed" of type 'a and a function that given a seed produces SOME of a pair of a 'b value and a new seed, or
   NONE if it is done seeding. *)
fun unfold f seed =
    case f seed of
	NONE => []
      | SOME(x,y) => x::(unfold f y);

(* 10. use unfold and foldl to implement factorial *)
fun fact2 n =
    foldl (op *) 1 (unfold (fn n =>if n = 1 then NONE else SOME(n,n-1)) n);

(*  11. implement map using List.foldr *)
	   fun mmap f =
	       foldr (fn (e,rest) => (f e)::rest) [];

	   (* 12. implement filter using List.foldr *)
	   fun mfilter f =
	       foldr (fn (e,rest) => if f e then e::rest else rest) [];

	   (* 13. implement foldl using foldr *)
	   fun mfoldl f init li =
	       foldr f init (foldr (fn (x1,rest) => rest @ [x1]) [] li);

	   fun my_foldl f init xs =
	       foldr (fn (x,g) => g o (fn y => f (x,y))) (fn x => x) xs init;

	   (* 14. *)
	   datatype 'a tree = leaf | node of {value : 'a, left : 'a tree, right : 'a tree};
	   fun tree_map f tr =
	       case tr of
		   leaf => leaf
		 | node{value = va, left = left_t, right = right_t} => node{value = f va, left = tree_map f left_t, right = tree_map f right_t};
	   fun tree_fold f init tr =
	       case tr of
		   leaf => init
		 | node{value= va, left = left_t, right = right_t} => f (va, (tree_fold f init left_t), (tree_fold f init right_t))
	   fun tree_filter f tr =
	       case tr of
		   leaf => leaf
		 | node{value=va,left=left_t,right=right_t} => if f va then node{value=va,left=tree_filter f left_t,right=tree_filter f right_t}
										else leaf

						 
