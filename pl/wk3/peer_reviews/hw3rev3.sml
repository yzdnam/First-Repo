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

fun only_capitals xs =
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 xs =
    foldl (fn (x,y) => if String.size x > String.size y then x else y) "" xs

fun longest_string2 xs =
    foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" xs

fun longest_string_helper f xs =
    foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) "" xs

val longest_string3 = longest_string_helper (fn (x,y) => x>y)

val longest_string4 = longest_string_helper (fn (x,y) => x>=y)
								
val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case (f x) of
		      NONE => first_answer f xs'
		    | SOME v => v 

fun all_answers f xs =
    let fun helper f xs acc =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of
			      NONE => NONE
			    | SOME v => helper f xs' (acc @ v)
    in
	helper f xs []
    end

val count_wildcards = g (fn x => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn x => 1) String.size

fun count_some_var(str,p) =
    g (fn x => 0) (fn x => if str = x then 1 else 0) p

fun check_pat p =
    let fun var_list p =
	    case p of
		Wildcard => []
	      | Variable x => [x]
	      | TupleP ps => foldl (fn (p,i) => (var_list p) @ i) [] ps
	      | ConstructorP (s,p) => var_list p
	      | _ => [] 

	fun is_member (s, xs) =
	    case xs of
		[] => false
	      | x::xs' => s=x orelse is_member(s,xs')
					      
	fun is_repeated xs =	    
	    case xs of
		[] => false
	      | x::xs' => is_member(x,xs') orelse is_repeated xs' 
    in			    	    
	not (is_repeated (var_list p))
    end

fun check_pat2 p =
    let fun var_list p =
	    case p of
		Wildcard => []
	      | Variable x => [x]
	      | TupleP ps => foldl (fn (p,i) => (var_list p) @ i) [] ps
	      | ConstructorP (s,p) => var_list p
	      | _ => []

	fun distinct xs =
	    case xs of
		[] => true
	      | x::xs' => not (List.exists (fn y => y=x) xs')
			  andalso distinct xs'

    in distinct (var_list p)
    end

fun match (valu, pattern) =
    case (valu, pattern) of
	(_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s, valu)]
      | (Unit, UnitP) => SOME []
      | (Const i1, ConstP i2) => if i1 = i2 then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if length(vs) = length(ps)
				 then all_answers match (ListPair.zip(vs,ps))
				 else NONE
      | (Constructor (s1,v), ConstructorP (s2,p)) => if s1=s2
						     then match(v,p)
						     else NONE
      | _ => NONE
						      
fun first_match valu ps =
    SOME (first_answer (fn x => match(valu,x)) ps) handle NoAnswer => NONE
	  
(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
