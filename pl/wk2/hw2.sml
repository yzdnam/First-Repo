(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2;

(* 1. *)
(* (a) takes a string and a string list. Return NONE if the string is not in the list, else return SOME lst where lst is
identical to the argument list except the string is not in it. assume the string is in the list at most once. *)
fun all_except_option(s,sl) =
    case sl of
	[] => NONE
      | x::xs => if same_string(s,x)
		 then SOME xs
		 else
		     case all_except_option(s,xs) of
			 NONE => NONE
		       | SOME lst => SOME (x::lst);

(* (b) takes a string list list, substitutions, and a string s and returns a string list. The result has all the strings
that are in some list in substitutions that also has s, but s itself should not be in the result. Assume each list in
the substitutions has no repeats. The result will have repeats if s and another string are both in more than one list in
substitutions *)
fun get_substitutions1(substitutions,s) =
    case substitutions of
	[] => []
      | first::rest => case all_except_option(s,first) of
			   NONE => get_substitutions1(rest,s)
			 | SOME x => x @ get_substitutions1(rest,s);

(* (c) same as get_substitutions1 except it uses a tail-recursive local helper function *)
fun get_substitutions2(substitutions0,s) =
    let
	fun accum_subs(substitutions,accum) =
	    case substitutions of
		[] => accum
	      | first::rest => case all_except_option(s,first) of
				   NONE => accum_subs(rest,accum) 
				 | SOME x => accum_subs(rest,accum @ x);
    in
	accum_subs(substitutions0,[])
    end;

(* (d) takes a string list list of substitutions and a full name of type {first:string,middle:string,last:string} and
returns a list of full names. The result is all the full names you can produce by substituting only the first name using
the substitutions returned by get_substitutions. The answer should begin with the original name and then have 0 or more
other names, including duplicates. *)
fun similar_names(substitutions,{first=fi, middle=mi, last=li}) =
    let
	fun apply_subs(first_name_subs) =
	    case first_name_subs of
		[] => []
	      | new_first::rest => {first=new_first, last=li, middle=mi}::apply_subs(rest)
	val proper_subs = get_substitutions2(substitutions,fi)
    in
	{first=fi, middle=mi, last=li}::apply_subs(proper_subs)
    end;
	
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove;

(* 2. *)
(* (a) takes a card and returns its color *)
fun card_color(c) =
    case c of
	(Clubs,_) => Black
      | (Spades,_) => Black
      | (Diamonds,_) => Red
      | (Hearts,_) => Red;

(* (b) takes a card and returns its value *)
fun card_value(c) =
    case c of
	(_,Jack) => 10
      | (_,Queen) => 10
      | (_,King) => 10
      | (_,Ace) => 11
      | (_,Num i) => i;

(* (c) takes a list of cards cs, a card c, and an exception e. It returns a list that has all the elements of cs except
c. If c is in the list more than once, remove only the first one. If c is not in the list, raise the exception e. *)
fun remove_card(cs,c,e) =
    case cs of
	[] => raise e
      | first::rest => if c = first
		       then rest
		       else first::remove_card(rest,c,e);

(* (d) takes a list of cards and returns true if all the cards in the list are the same color. *)
fun all_same_color(cs) =
    case cs of
	[] => true
      | x::[] => true
      | first::(second::rest) => card_color(first) = card_color(second) andalso all_same_color(second::rest);

(* (e) takes a list of cards and returns the sum of their values. *)
fun sum_cards(cs0) =
    let
	fun accum_cards(cs,accum) =
	    case cs of
		[] => accum
	      | first::rest => accum_cards(rest,(card_value(first)) + accum)
    in
	accum_cards(cs0,0)
    end;

(* (f) takes a card list (the held-cards) and an int (the goal) and computes the score as follows:
let sum be the sum of the values of the held-cards. If sum is greater than goal, the preliminary score is three times
(sum-goal), else the preliminary score is (goal-sum). The score is the preliminary score unless all the held-cards are
the same color, in which case the score is the preliminary score divided by 2. *)
fun score(cs,goal) =
    let
	val sum = sum_cards(cs)
	val prelim_score = if sum > goal
			   then 3 * (sum - goal)
			   else (goal - sum)
    in
	if all_same_color(cs)
	then prelim_score div 2
	else prelim_score
    end;

(* (g) takes a card list (the card-list), a move list (what the player does at each point), and an int (the goal) and
returns the score at the end of the game after processing (some or all of) the moves in the move list in order. *)
fun officiate(cs0,ml0,goal) =
    let
	fun play(cs,held_cs,ml,sum) =
	    case ml of
		[] => score(held_cs,goal)
	      | move::rest_moves => case move of
					Discard c => play(cs,remove_card(held_cs,c,IllegalMove),rest_moves,sum - card_value(c))
				      | Draw => case cs of
						    [] => score(held_cs,goal)
						  | first::rest_cards => let
						      val new_sum = card_value(first) + sum
						      val new_hand = (first::held_cs)
						  in
						      if new_sum > goal
						      then score(new_hand,goal)
						      else play(rest_cards,new_hand,rest_moves,new_sum)
						  end
    in
	play(cs0,[],ml0,0)
    end;

(* 3. *)
(* (a) modify the game so that each ace can have a value of 1 or 11. if (goal - sum) >= 11, then a drawn ace will have a
value of 11 else it will have a value of 1. *)
fun extract_sum({cards_sum=s,full_val_aces=a}) =
    s
				      
fun score_challenge(cs,goal,sum) =
    let
	val prelim_score = if sum > goal
			   then 3 * (sum - goal)
			   else (goal - sum)
    in
	if all_same_color(cs)
	then prelim_score div 2
	else prelim_score
    end;

fun officiate_challenge(cs0,ml0,goal) =
    let
	fun add_card(c,{cards_sum=s,full_val_aces=a}) =
	    case c of
		(_,Ace) => if (goal - s) >= 11
		       then {cards_sum=(s + 11),full_val_aces=(a + 1)}
		       else {cards_sum=(s + 1),full_val_aces=a}
	      | (_,_) => {cards_sum=(s + (card_value(c))),full_val_aces=a}
	fun subtract_card(c,{cards_sum=s,full_val_aces=a}) =
			     case c of
				 (_,Ace) => if a > 0
					    then {cards_sum=(s - 11),full_val_aces=(a - 1)}
					    else {cards_sum=(s - 1),full_val_aces=a}
			       | (_,_) => {cards_sum=(s - (card_value(c))),full_val_aces=a}
	fun play(cs,held_cs,ml,sum_record) =
	    case ml of
		[] => score_challenge(held_cs,goal,(extract_sum(sum_record)))
	      | move::rest_moves => case move of
					Discard c => play(cs,remove_card(held_cs,c,IllegalMove),rest_moves,
							  subtract_card(c,sum_record))
				      | Draw => case cs of
						    [] => score_challenge(held_cs,goal,(extract_sum(sum_record)))
						  | first::rest_cards => let
						      val new_sum = add_card(first,sum_record)
						      val new_hand = (first::held_cs)
						  in
						      if extract_sum(new_sum) > goal
						      then score_challenge(new_hand,goal,extract_sum(new_sum))
						      else play(rest_cards,new_hand,rest_moves,new_sum)
						  end
    in
	play(cs0,[],ml0,{cards_sum=0,full_val_aces=0})
    end;

(* (b) takes a card-list and a goal and returns a move-list such that calling officiate with the card-list, the goal,
and the move-list has the following behavior:
-the value of the held cards never exceeds the goal
-a card is drawn whenever the goal is more than 10 greater than the value of the held cards. attempt to draw even if no
 cards remain in the card-list
-if a score of 0 is reached, there must be no more moves
-if it is possible to reach a score of 0 by discarding a card followed by drawing a card, then this must be done *)
fun careful_player(cs0,goal) =
    let
	fun accum(cs,sum) =
	    let
		val spread = (goal - sum)
	    in
		case cs of
		    [] => if spread > 10
			  then [Draw]
			  else []
		  | first::(second::rest) => if spread = 0
					     then []
					     else
						 if (spread - card_value(second)) = 0 andalso (spread - card_value(first)) > 0
						 then Draw::Discard(first)::Draw::accum(rest,(card_value(second) + sum))
						 else
						     if spread > 10
						     then Draw::accum((second::rest),(card_value(first) + sum))
						     else []
							 
		  | last::empty_deck => if spread > 10
					then Draw::accum(empty_deck,(card_value(last) + sum))
					else []
	    end
    in
	accum(cs0,0)
    end

					 
val test1 = careful_player([(Spades,Num 7),(Hearts,King),(Clubs,Ace),(Diamonds,Num 2)], 18) = [Draw,Draw,Discard(Hearts,King),Draw]

val test2 = careful_player([(Diamonds,Num 2),(Clubs,Ace)],11) = [Draw,Discard(Diamonds,Num 2),Draw]				
