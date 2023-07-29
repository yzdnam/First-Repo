(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(str, str_list) =
    case str_list of
        [] => NONE
      | hd::tl =>
            if same_string(hd, str) then SOME tl
            else
                case all_except_option(str, tl) of
                    NONE => NONE
                  | SOME result => SOME(hd::result)

fun get_substitutions1(substitutions, str) =
    case substitutions of
        [] => []
      | hd::tl =>
            case all_except_option(str, hd) of
                NONE => get_substitutions1(tl, str)
              | SOME result => result @ get_substitutions1(tl, str)

fun get_substitutions2(substitutions, str) =
    let fun aux (substitutions, acc) =
        case substitutions of
            [] => acc
          | hd::tl =>
                case all_except_option(str, hd) of
                    NONE => aux(tl, acc)
                  | SOME result => aux(tl, acc @ result)
    in aux(substitutions, []) end

fun similar_names(substitutions, { first=first, middle=middle, last=last }) =
    let fun get_names subs =
        case subs of
            [] => []
          | hd::tl => { first=hd, middle=middle, last=last } :: get_names tl
    in get_names(first :: get_substitutions1(substitutions, first)) end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color card =
    case card of
        (Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red

fun card_value card =
    case card of
        (_, Jack) => 10
      | (_, Queen) => 10
      | (_, King) => 10
      | (_, Ace) => 11
      | (_, Num n) => n

fun remove_card(cs, c, e) =
    case cs of
        [] => raise e
      | hd::tl =>
            if hd = c then tl
            else hd :: remove_card(tl, c, e)

fun all_same_color cards =
    case cards of
        [] => true
      | _::[] => true
      | head::(neck::rest) => (
            card_color head = card_color neck andalso
            all_same_color(neck::rest)
        )

fun sum_cards cards =
    let fun aux(cards, acc) =
        case cards of
            [] => acc
          | hd::tl => aux(tl, acc + card_value hd)
    in aux(cards, 0) end

fun score(cards, goal) =
    let
        val sum = sum_cards cards
        val preliminary = if sum > goal then 3 * (sum - goal) else goal - sum
    in
        if all_same_color cards then preliminary div 2 else preliminary
    end

fun officiate(cards, moves, goal) =
    let
        fun draw(cards, held) =
            case cards of
                [] => NONE
              | hd::tl => SOME(tl, hd::held)
        fun play(cards, held, moves) =
            case moves of
                [] => score(held, goal)
              | Draw::tl => (
                    case draw(cards, held) of
                        NONE => score(held, goal)
                      | SOME(cards, held) =>
                            if sum_cards held > goal then score(held, goal)
                            else play(cards, held, tl)
                )
              | Discard(card)::tl =>
                    play(cards, remove_card(held, card, IllegalMove), tl)
    in play(cards, [], moves) end

fun count_ace cards =
    case cards of
        [] => 0
      | (_, Ace)::tl => 1 + count_ace tl
      | _::tl => count_ace tl

fun score_challenge(cards, goal) =
    let
        val aces = count_ace cards
        val sum = sum_cards cards
        fun get_prelim(sum) =
            if sum > goal then 3 * (sum - goal) else goal - sum
        fun best_prelim(curr, n) =
            let val curr_prelim = get_prelim(curr)
            in
                if n > 0 andalso get_prelim(curr - 10) < curr_prelim
                then best_prelim(curr - 10, n - 1)
                else curr_prelim
            end
        val preliminary = best_prelim(sum, aces)
    in
        if all_same_color cards then preliminary div 2 else preliminary
    end

fun officiate_challenge(cards, moves, goal) =
    let
        fun draw(cards, held) =
            case cards of
                [] => NONE
              | hd::tl => SOME(tl, hd::held)
        fun play(cards, held, moves) =
            case moves of
                [] => score_challenge(held, goal)
              | Draw::tl => (
                    case draw(cards, held) of
                        NONE => score_challenge(held, goal)
                      | SOME(cards, held) =>
                            if sum_cards(held) - count_ace(held) * 10 > goal
                            then score_challenge(held, goal)
                            else play(cards, held, tl)
                )
              | Discard(card)::tl =>
                    play(cards, remove_card(held, card, IllegalMove), tl)
    in play(cards, [], moves) end

fun reverse items =
    let fun transfer(reversed, original) =
        case original of
            [] => reversed
          | hd::tl => transfer(hd::reversed, tl)
    in transfer([], items) end

fun find_card(cards, n) =
    case cards of
        [] => NONE
      | hd::tl => if card_value hd = n then SOME hd else find_card(tl, n)

fun careful_player(cards, goal) =
    let
        fun next_move(cards, held, moves) =
            case cards of
                [] => moves
              | hd::tl =>
                let
                    val diff = goal - sum_cards(held)
                    val next_draw_value = card_value hd
                in
                    if diff = 0 then moves
                    else if diff >= next_draw_value then next_move(tl, hd::held, Draw::moves)
                    else
                        case find_card(held, next_draw_value - diff) of
                            NONE => (
                                case held of
                                    [] => moves
                                  | hd::tl => next_move(cards, tl, Discard(hd)::moves)
                            )
                          | SOME card => next_move(
                                tl,
                                hd::(remove_card(held, card, IllegalMove)),
                                Draw::(Discard(card)::moves)
                            )
                 end
    in
        reverse(next_move(cards, [], []))
    end

	
