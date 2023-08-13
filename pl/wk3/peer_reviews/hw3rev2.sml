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
fun only_capitals(list_of_character: string list)=
    List.filter(fn s => (Char.isUpper (String.sub (s,0)))) list_of_character
(* 2 *)
fun longest_string1(my_string_list: string list)=
    if my_string_list=[]
    then ""
    else foldl(
        (fn (x,y) => 
        if String.size x > String.size y
        then x
        else y)) "" my_string_list

(* 3 *)
fun longest_string2(my_string_list: string list)=
    if my_string_list=[]
    then ""
    else foldl(
        (fn (x,y) => 
        if String.size x >= String.size y
        then x
        else y)) "" my_string_list

(* 4 *)

fun longest_string_helper f = 
    fn los => foldl f "" los 

val longest_string3 = 
    longest_string_helper (fn (x, y) => if String.size x > String.size y then x else y)

val longest_string4 = 
    longest_string_helper (fn (x, y) => if String.size x >= String.size y then x else y)

(* 5 *)
val longest_capitalized =
    fn string_list =>  longest_string1  (only_capitals string_list )

(* 6 *)

val rev_string = implode   o rev o  explode

(* 7 *)

fun first_answer f list_of_string = 
    case  list_of_string of
        [] => raise NoAnswer
        | x::xs => case f(x) of
                    SOME y => y
                    | None => first_answer f xs


(* 8 *)

fun all_answers (f: 'a -> 'b list option) (my_list: 'a list): 'b list option  =
    let 
        fun aux (f: 'a -> 'b list option) (my_list: 'a list) (acc: 'b list):'b list option =
            case my_list of
                [] => SOME acc
                | x::xs => case f x of
                            NONE => NONE
                            | SOME y => aux f xs ( acc @ y)
                            
    in
        aux f my_list []
    end

(* 9a *)

val count_wildcards = g(fn wildcounts => 1) (fn var => 0)

(* 9b *)
val count_wild_and_variable_lengths=  g(fn wildcounts => 1) (fn x => String.size x)

(* 9c *)

val count_some_var  = fn (string,pattern) => g (fn x => 0) (fn x => if x=string then 1 else 0) pattern

(* 10 *)

val check_pat = 
    let 
    fun create_list p = 
       case p of 
		    Variable v => [v]
	      | ConstructorP (_, p') => create_list p'
	      | TupleP ps => List.foldl (fn (p, acc) => (create_list p) @ acc)
			   [] ps
	      | _ => []
	fun repeated_list my_list=
	    case my_list of
		    [] => false
	        | s::ss' => (List.exists (fn s' => s = s') ss') orelse repeated_list ss'
    in
	not o repeated_list o create_list
    end
(* 11 *)
fun match(valu, pattern) = 
    case (pattern, valu) of
	(Wildcard, _) => SOME []
      | (Variable s, _) => SOME [(s,valu)]
      | (UnitP, Unit) => SOME []
      | (ConstP cp, Const cv) => if cp = cv then SOME [] else NONE
      | (TupleP ps, Tuple vs) => if List.length ps = List.length vs 
				 then all_answers (fn (vs',ps') => match(vs',ps')) (ListPair.zip(vs,ps))
				 else NONE
      | (ConstructorP(s1,pp), Constructor(s2,pv)) => if s1 = s2 then match(pv,pp) else NONE
      | _ => NONE

(* 12 *)
fun first_match valu pattern = 
    ( SOME(first_answer (fn pattern => match(valu,pattern)) pattern) ) handle NoAnswer => NONE

(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_0 = only_capitals ["A","B","C","c"] = ["A","B","C"]
val test1_1 = only_capitals ["a","b","Z","c"] = ["Z"]
val test1_2 = only_capitals ["a","b","z","c"] = []
val test1_3 = only_capitals ["a","Ba","z","c"] = ["Ba"]
val test1_4 = only_capitals ["a","Bz","z","Ca"] = ["Bz","Ca"]
val test1_5 = only_capitals ["a","Bz","zaB","CaC"] = ["Bz","CaC"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_1 = longest_string1 ["Abca","bced","C"] = "Abca"
val test2_2 = longest_string1 ["A","bc","CCCCCC"] = "CCCCCC"
val test2_3 = longest_string1 ["A","b","C"] = "A"
val test2_4 = longest_string1 [] = ""


val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_1 = longest_string2 ["Abca","bced","C"] = "bced"
val test3_2 = longest_string2 ["A","bc","CCCCCC"] = "CCCCCC"
val test3_3 = longest_string2 ["A","b","C"] = "C"
val test3_4 = longest_string2 [] = ""



val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4a_1 = longest_string3 ["Abca","bced","C"] = "Abca"
val test4a_2 = longest_string3 ["A","bc","CCCCCC"] = "CCCCCC"
val test4a_3 = longest_string3 ["A","b","C"] = "A"
val test4a_4 = longest_string3 [] = ""
 
val test4b = longest_string4 ["A","B","C"] = "C" 
val test4b_1 = longest_string4 ["Abca","bced","C"] = "bced"
val test4b_2 = longest_string4 ["A","bc","CCCCCC"] = "CCCCCC"
val test4b_3 = longest_string4 ["A","b","C"] = "C"
val test4b_4 = longest_string4 [] = ""


val test5 = longest_capitalized ["A","bc","C"] = "A"
val test5_1 = longest_capitalized ["A","BC","C"] = "BC"
val test5_2 = longest_capitalized ["abc","bc","C"] = "C"
val test5_3 = longest_capitalized ["A","abc","CED"] = "CED"





val test6 = rev_string "abc" = "cba"
val test6_1 = rev_string "cba" = "abc"
val test6_2 = rev_string "zyx" = "xyz"
val test6_3 = rev_string "123" = "321"
val test6_4 = rev_string "911" = "119"


val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
(* val test7_1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,1,2]  *)


val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,1,1,1,1,1,1] = SOME [1,1,1,1,1,1,1]

val test9a = count_wildcards Wildcard = 1
val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test10a = check_pat (ConstructorP ("hi",TupleP[Variable "x",Variable "x"])) = false



val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
