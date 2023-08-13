(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string;

(**** you can put all your code here ****)

(* 1. takes a string list and returns a string list that has only the strings in the argument that start with an uppercase letter. Assume all strings
have at least 1 character. Use List.filter, Char.isUpper, and String.sub. *)
val only_capitals =
    List.filter (fn str => Char.isUpper (String.sub(str,0)));

(* 2. takes a string list and returns the longest string in the list. If the list is empty, return "". In the case of a tie, return the string closest
to the beginning of the list. Use foldl, String.size, and no recursion *)
val longest_string1 =
    foldl (fn (str1, str2) => if String.size str1 > String.size str2 then str1 else str2) "";

(* 3. same as 2. except in case of a tie, returns the string closest to the end of the list *)
val longest_string2 =
    foldl (fn (str1, str2) => if String.size str1 >= String.size str2 then str1 else str2) "";
	  
(* 4. *)
fun longest_string_helper operator =
    foldl (fn (str1, str2) => if (operator ((String.size str1), (String.size str2))) then str1 else str2) "";

val longest_string3 =
    longest_string_helper (fn (x,y) => x > y);

val longest_string4 =
    longest_string_helper (fn (x,y) => x >= y);

(* 5. takes a string_list and returns the longest string in the list that begins with an uppercase letter, or "" if there are no such strings. Assume
all strings have at least 1 character. Use a val binding and the ML library's o operator for composing functions. Resolve ties like in problem 2. *)
val longest_capitalized =
    longest_string1 o only_capitals;

(* 6. takes a string and returns the string that is the same characters in reverse order. Use ML's o operator, the library function rev for reversing
lists, and two library functions in the String module. *)
val rev_string =
    implode o rev o explode;

(* 7. takes a function of type ('a -> 'b option) and an 'a list. the function argument should be applied to elements of the second argument in order
until the first time it returns SOME v for some v and then v is the result of the call. If the first argument returns NONE for all list elements, then
the function should raise the exception NoAnswer. *)
fun first_answer f li =
    case li of
	[] => raise NoAnswer
      | first::r => ( case f first of
			  NONE => first_answer f r
			| SOME v => v ); 

(* 8. takes a function of type ('a -> 'b list option) which is applied to elements of the second argument of type 'a list. If the given function
returns NONE for any element, then the result for all_answers is NONE. Else the calls to the first argument will have produced SOME lst1, SOME lst2,
... SOME lstn and the result of all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together *)
fun all_answers f lst =
    let	fun combine (li,accum) =
	    case li of
		[] => SOME accum
	      | NONE::r => NONE 
	      | (SOME e)::r => combine (r,(accum@e))
    in
	combine ((map f lst),[])
    end;
	
(* 9a. takes a pattern and returns how many Wildcard patterns it contains. *)
val count_wildcards =
    g (fn wc => 1) (fn var => 0);

(* 9b. takes a pattern and returns the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables in the variable
patterns it contains. Use String.size. *)
val count_wild_and_variable_lengths =
    g (fn wc => 1) (fn var => String.size var);

(* 9c. takes a string and a pattern (as a pair) and returns the number of times the string appears as a variable in the pattern while ignoring the
constructor names. *)
fun count_some_var (str,p) =
    g (fn wc => 0) (fn var => if var = str then 1 else 0) p;

(* 10. takes a pattern and returns true if and only if all the variables appearing in the pattern are distinct from each other. *)
fun check_pat p =
    let
	fun extract_strings p =
		case p of
		    Variable x        => [x]
		  | TupleP ps         => List.foldl (fn (p,i) => i @ (extract_strings p)) [] ps
		  | ConstructorP(_,p) => extract_strings p
		  | _                 => []
	fun check_repeats li =
	    case li of
		[] => true
	      | first::rest => (not (List.exists (fn i => List.exists (fn j => i = j) rest) [first]))
				   andalso check_repeats rest
    in
	case extract_strings p of
	    [] => raise NoAnswer
	  | _ => check_repeats (extract_strings p)
    end;

(* 11. takes a valu*pattern and returns a (string * valu) list option, namely NONE if the pattern does not match and SOME lst where lst is the list of
bindings if it does. Note that if the value matches but the pattern has no patterns of the form Variable, s, then the result is SOME []. *)
fun match (v,p) =
    case (v,p) of
	(_,Wildcard) => SOME []
      | (_,Variable s) => SOME [(s,v)]
      | (Unit,UnitP) => SOME []
      | (Const i,ConstP j) => if i = j then SOME [] else NONE
      | (Tuple vs,TupleP ps) => if length vs = length ps
				then all_answers match (ListPair.zip (vs,ps))
				else NONE
      | (Constructor(s1,v'),ConstructorP(s2,p')) => if s1 = s2
						    then match (v',p')
						    else NONE
      | (_,_) => NONE;

(* 12. takes a value and a list of patterns and returns a (string * value) list option, namely NONE if no pattern in the list matches or SOME lst where
lst is the list of bindings for the first pattern in the list that matches. *)
fun first_match v ps =
    SOME (first_answer (fn p => match(v,p)) ps)
    handle NoAnswer => NONE;

(* Challenge *)
fun typecheck_patterns (constructor_type_bindings,ps) =
    let
	(* used to retrieve type for constructor patterns from the given constructor type bindings list *)
	fun retrieve_output_type constructor input_type bindings =
	    case (input_type,bindings) of
		(_,[]) => raise NoAnswer
	      | (SOME t,(cons,outtype,intype)::rest) => if cons = constructor andalso intype=t
							then Datatype(outtype)
							else retrieve_output_type constructor input_type rest
	(* changes the given list of patterns to a list of types *)
	fun accum_types ps =
	    case ps of
		[] => []
	     | Wildcard::rest => Anything::(accum_types rest)
	     | (Variable s)::rest => Anything::(accum_types rest)
	     | UnitP::rest => UnitT::(accum_types rest)
	     | (ConstP i)::rest => IntT::(accum_types rest)
	     | (TupleP nested_ps)::rest => TupleT(accum_types nested_ps)::(accum_types rest)
	     | (ConstructorP(s,p))::rest => (retrieve_output_type s (typecheck_patterns (constructor_type_bindings,[p])) constructor_type_bindings)::
					    (accum_types rest)
	(* returns the most restrictive type or an exception if the given types are not compatible *)
	fun compare_types (t1,t2) =
	    case (t1,t2) of
		(Anything,x) => x
	      | (x,Anything) => x 
	      | (TupleT(ts1),TupleT(ts2)) => TupleT(ListPair.map compare_types (ts1,ts2))
	      | (x,y) => if x = y then x else raise NoAnswer
	(* folds a list of types with compare_types *)
	val return_type =
	    foldl compare_types Anything
    in
	SOME (return_type (accum_types ps))
		    handle NoAnswer => NONE
    end;

