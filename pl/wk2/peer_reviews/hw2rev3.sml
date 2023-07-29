(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (string, list) =
    let 
        fun word_match (string, list) =
            case list of
                [] => "0"
	            | a::[] => if same_string(a, string) then a else "0"
	            | a::b => if same_string(a, string) then a else word_match(string, b)

        fun remove_match (word, list) = 
            case list of
                [] => [] 
                | a::[] => if same_string(word, a) then [] else a::[]
	            | a::b  => if same_string(word, a) then remove_match(word, b) else a::remove_match(word, b) 
    in
        if word_match(string, list) <> "0"
        then
            SOME (remove_match(word_match(string,list), list))
        else
            NONE
    end

fun get_substitutions1 (substitutions, string) =
    case substitutions of
        [] => []
        | a::b => 
            case all_except_option(string, a) of
	            NONE => get_substitutions1(b, string)   
                | SOME(c) => c @ get_substitutions1(b, string)

fun get_substitutions2 (substitutions, string) =
    let fun f(substitutions, string, accum) = 
        case substitutions of
            [] => accum
            | a::b =>
                case all_except_option(string, a) of
	                NONE => f(b, string, accum)
                    | SOME(c) => f(b, string, c @ accum) 
    in
        f(substitutions, string, [])
    end
            
fun similar_names (substitutions, fullname) =
    let val {first = x, middle = y, last = z} = fullname 
        val c = get_substitutions2(substitutions, x)
        fun f (c, x, y, z) = 
            case c of
            [] => []
            | a::[] => [{first = a, middle = y, last = z}]
            | a::b => f(b, x, y, z) @ [{first = a, middle = y, last = z}] 
    in
        [fullname] @ f (c, x, y, z)
    end
    

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

	      
(* put your solutions for problem 2 here *)

fun card_color (card) =
    case card of
        (Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red
      | (Spades, _) => Black

fun card_value (card) =
    case card of
        (_, Jack) => 10
        | (_, Queen) => 10
        | (_, King) => 10
        | (_, Ace) => 11
        | (_, Num a) => a 
    
fun remove_card (cs, c, e) =
    case cs of
    [] => raise e
    | a::[] => if a = c then [] else raise e
    | a::b => if a = c then b else a::remove_card(b, c, e)

fun all_same_color (cards) =
    case cards of
        [] => true
        | a::[] => true
        | a::(b::c) => card_color(a) = card_color(b) andalso (all_same_color(b::c))

fun sum_cards (cards) =
    let fun accumz (cards, accum) = 
        case cards of
            [] => 0
            | a::[] => accum + card_value(a)
            | a::b => accumz(b, accum + card_value(a)) 
    in
        accumz (cards, 0)
    end

fun score (cards, goal) =
    let val x = if all_same_color(cards) then 2 else 1
    in
        if sum_cards(cards) > goal
        then
            3 * (sum_cards(cards) - goal) div x
        else
            (goal - sum_cards(cards)) div x
    end

fun officiate (card_list, moves, goal) = 
    let val held_cards = []
    fun internal_calc (card_list, moves, goal, held_cards) =
        case moves of
            [] => score(held_cards, goal)
            | a::[] =>
                (case a of
                    Discard y => internal_calc(card_list, [], goal, remove_card(held_cards, y, IllegalMove))
                    | Draw =>
                        case card_list of
                            [] => score(held_cards, goal)
                            | c::[] => internal_calc([], [], goal, c::held_cards)
                            | c::d => internal_calc(d, [], goal, c::held_cards))            
            | a::b =>
                case a of
                    Discard z => internal_calc(card_list, b, goal, remove_card(held_cards, z, IllegalMove))
                    | Draw =>
                        if sum_cards(held_cards) > goal
                        then
                            score(held_cards, goal)
                        else
                            case card_list of
                                [] => score(held_cards, goal)
                                | c::[] => internal_calc([], b, goal, c::held_cards)
                                | c::d => internal_calc(d, b, goal, c::held_cards) 
    in
        internal_calc(card_list, moves, goal, held_cards)
    end


