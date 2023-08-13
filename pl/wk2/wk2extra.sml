type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail;

(* 1. takes a final_grade and returns pass if the grade field contains SOME i for an i >= 75 (else fail). *)
fun pass_or_fail({ id = sid, grade = s_grade }) =
    case s_grade of
	NONE => fail
      | SOME i => if i >= 75
		  then pass
		  else fail

(* 2. returns true if and only if the grade field contains SOME i for an i >= 75 *)
fun has_passed(input_grade) =
    pass_or_fail(input_grade) = pass;

(* 3. takes a list of type final_grade and returns how many list elements have passing grades *)
fun number_passed(l_grades) =
    case l_grades of
	[] => 0
      | first::rest => if has_passed(first)
		       then 1 + number_passed(rest)
		       else number_passed(rest);

(* 4. indicates how many list elements are mislabeled where mislabeling means a pair (pass,x) where has_passed x is false or (fail,x) where has_passed x
is true *)
fun number_misgraded(l_grades) =
    case l_grades of
	[] => 0
      | (pass,input_grade)::rest => if has_passed(input_grade)
				    then number_misgraded(rest)
				    else 1 + number_misgraded(rest)
      | (fail,input_grade)::rest => if not(has_passed(input_grade))
				    then number_misgraded(rest)
				    else 1 + number_misgraded(rest);

datatype 'a tree = leaf
		 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me;

(* 5. accepts an 'a tree and evaluates to a height of this tree. The height of a tree is the length of the longest path to a leaf. Thus the height of
a leaf is 0 *)
fun tree_height(input_tree) =
    case input_tree of
	leaf => 0
      | node { value = branch_val, left = left_tree, right = right_tree } =>
	let
	    val left_height = tree_height(left_tree)
	    val right_height = tree_height(right_tree)
	in 
	    if left_height > right_height
	    then 1 + left_height
	    else 1 + right_height
	end;

(* 6. takes an int tree and evaluates to the sum of all values in the node *)
fun sum_tree(input_tree) =
    case input_tree of
	leaf => 0
      | node { value = node_val, left = left_tree, right = right_tree } => node_val + sum_tree(left_tree) + sum_tree(right_tree); 
	   
(* 7. takes a flag tree and returns a tree with all nodes of the input containing prune_me (along with all their descendents) replaced with a leaf *)
fun gardener(input_tree) =
    case input_tree of
	leaf => leaf
     | node {value = leave_me_alone, left = l_tree, right = r_tree} => node {value = leave_me_alone, left = gardener(l_tree), right = gardener(r_tree)}
     | node {value = prune_me, left = l_tree, right = r_tree} => leaf;

(* skip 8. *)

datatype nat = ZERO | SUCC of nat;

(* 9. given a natural number, returns whether that number is positive *)
fun is_positive(num) =
    not(num = ZERO);

(* 10. given a natural number returns its predecessor. *)
exception Negative;

fun pred(num) =
    case num of
	ZERO => raise Negative
     |  SUCC n => n;

(* 11. given a natural number, returns the corresponding int. *)
fun nat_to_int(num) =
    case num of
	ZERO => 0
      | SUCC n => 1 + nat_to_int(n);

(* 12. given an integer returns a natural number representation for it, or throws a Negative exception if the integer was negative *)
fun int_to_nat(num) =
    if num = 0
    then ZERO
    else
	if num > 0
	then SUCC (int_to_nat(num - 1))
	else raise Negative;
		   
(* 13. write add for nat *)
fun add(n1, n2) =
    case n1 of
	ZERO => n2
      | SUCC n => SUCC (add (n, n2));

(* 14. write sub for nat *)
fun sub(n1, n2) =
    case n2 of
	ZERO => n1
      | SUCC n => sub((pred n1),(pred n2));

(* 15. write mult for nat *)
fun mult(n1, n2) =
    case n2 of
	ZERO => ZERO
      | SUCC n => add(n1,(mult(n1,(pred n2))));

(* 16. write less_than for nat *)
fun less_than(n1, n2) =
    case (n1, n2) of
	(ZERO, SUCC n) => true
      | (ZERO, ZERO) => false
      | (SUCC n, ZERO) => false
      | (_,_) => less_than((pred n1),(pred n2));

datatype intSet =
	 Elems of int list (* list of ints, possibly with duplicates to be ignored *)
	 | Range of { from : int, to : int } (* integers from one number to another *)
	 | Union of intSet * intSet (* union of the two set *)
	 | Intersection of intSet * intSet (* intersection of the two sets *);

(* 18. returns whether the set contains a certain element or not *)
fun contains(set,i) =
    case set of
	Elems li => (case li of
			 [] => false
		       | f::r => if f = i
				 then true
				 else contains(Elems r, i))
      | Range {from = x, to = y} => i >= x andalso i <= y
      | Union (set1, set2) => contains(set1, i) orelse contains(set2, i)
      | Intersection (set1, set2) => contains(set1, i) andalso contains(set2, i);

(* 19. returns a list with the set's elements, without duplicates *)
fun toList(set) =
    case set of
	Elems li => (case li of
			 [] => []
		       | f::r => if contains(Elems r, f)
				 then toList(Elems r)
				 else f::toList(Elems r))
      | Range {from = x, to = y} => let
	  fun cons_range_list(x,y) =
	      if x = y
	      then [y]
	      else x::(cons_range_list((x + 1),y));
      in
	  cons_range_list(x,y)
      end
      | Union (set1,set2) => let
	  fun combine(set, other_set_list) =
	      case other_set_list of
		  [] => toList(set)
		| f::r => if contains(set,f)
			  then combine(set,r)
			  else f::combine(set,r)
	  val set2_list = toList(set2)
      in
	  combine(set1,set2_list)
      end
      | Intersection (set1,set2) => let
	  fun intersect(set,other_set_list) =
	      case other_set_list of
		  [] => []
		| f::r => if contains(set,f)
			  then f::intersect(set,r)
			  else intersect(set,r)
	  val set2_list = toList(set2)
      in
	  intersect(set1,set2_list)
      end
							    
(* 17. determines if the set is empty or not *)
fun isEmpty(set) =
    case set of
	Elems li => ( case li of
			  [] => true 
			| _ => false )
      | Range { from = x, to = y }  => not( x = y )
      | Union (set1, set2)  => isEmpty(set1) andalso isEmpty(set2)
      | Intersection (set1, set2) => let
	  fun contain_any(set,int_list) =
	      case int_list of
		  [] => false
		| f::r => if contains(set,f)
			  then true
			  else contain_any(set,r)
	  val set2_list = toList(set2)
      in
	  not (contain_any(set1,set2_list))
      end;


	      
		      
		      
