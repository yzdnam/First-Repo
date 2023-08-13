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
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s, 0)));

(* 2 *)
val longest_string1 = foldl (fn (x, last) => if String.size(x) > String.size(last) then x else last) "";

(* 3 *)
val longest_string2 = foldl (fn (x, last) => if String.size(x) >= String.size(last) then x else last) "";

(* 4 *)
fun longest_string_helper operator list =
  foldl (fn (x, last) => if operator(String.size(x), String.size(last)) then x else last) "" list;

val longest_string3 = longest_string_helper (op >);

val longest_string4 = longest_string_helper (op >=);

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals;

(* 6 *)
val rev_string = implode o rev o explode;

(* 7 *)
fun first_answer f list =
  case list of
      [] => raise NoAnswer
    | x :: xs' => case (f x) of
		      NONE => first_answer f xs'
		    | SOME v => v;

(* 8 *)
fun all_answers f list =
  let fun helper (f, list, acc) =
	case list of
	    [] => SOME acc
	  | x :: xs' => case (f x) of
			    NONE => NONE
			  | SOME v => helper(f, xs', acc @ v)
  in
      helper (f, list, [])
  end;

(* 9 a *)
val count_wildcards = g (fn () =>  1) (fn x => 0);

(* 9 b *)
val count_wild_and_variable_lengths = g (fn () =>  1) (fn x => String.size(x));

(* 9 c *)
fun count_some_var (s, p) = g (fn () =>  0) (fn x => if x = s then 1 else 0) p;

(* 10 *)
fun check_pat p =
  let
      fun variables p =
	case p of
	  Variable x => [x]
	 | TupleP ps => List.foldl (fn (p,list) => variables(p) @ list) [] ps
	 | ConstructorP(_,p) => variables(p)
	 | _ => []
      fun has_repetitions list =
	case list of
	    [] => false
	  | x :: xs' => if (List.exists (fn v => x = v) xs') then true else has_repetitions(xs')
  in
      not (has_repetitions (variables p))
  end;

(* 11 *)
fun match (v, p) =
  case (p, v) of
      (Wildcard, _) => SOME []
    | (Variable s, value) => SOME [(s, value)]
    | (UnitP, Unit) => SOME []
    | (ConstP x, Const y) => if x = y then SOME [] else NONE
    | (TupleP ps, Tuple vs) => if List.length(ps) = List.length(vs)
			       then all_answers match (ListPair.zip(vs, ps))
			       else NONE
    | (ConstructorP(s1,p), Constructor(s2,v)) => if s1 <> s2
						 then NONE
						 else match(v, p)
    | _ => NONE;

(* 12 *)
fun first_match v ps =
  SOME (first_answer match (List.map (fn p => (v, p)) ps))
  handle NoAnswer => NONE;
