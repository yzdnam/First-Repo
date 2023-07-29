(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* all_except_option = fn : string * string list -> string list option *)
fun all_except_option( s,  l ) =  
    case l of 
         [] => NONE 
       | x::xs' => if same_string(s, x) 
                   then SOME xs'
                   else case all_except_option(s, xs') of 
                             NONE => NONE
                           | SOME ys => SOME(x::ys)

(* get_substitutions1 = fn : string list list * string -> string list *)
fun get_substitutions1( lst, s ) = 
  case lst of 
       [] => []
     | l::ls' => case all_except_option(s, l) of 
                      NONE => get_substitutions1(ls', s)
                    | SOME xs => xs@get_substitutions1(ls', s)

(* get_substitutions2 = fn : string list list * string -> string list *)
fun get_substitutions2( lst, s ) = 
  let fun append( ls, acc ) =  
    case ls of 
         [] => acc
       | l::ls' => case all_except_option(s, l) of 
                        NONE => append(ls', acc)
                      | SOME xs => append(ls', xs@acc)
  in 
    append(lst, [])
  end  

(* similar_names = fn : string list list * {first:string, last:string, * middle:string} -> {first:string, last:string, middle:string} list *)
fun similar_names( lst, {first=f, last=l, middle=m} ) =  
  let 
    fun append( fs ) =  
      case fs of 
           [] => [] 
         | x::xs' => {first=x, last=l, middle=m}::append(xs')
  in
    {first=f, last=l, middle=m}::append(get_substitutions1(lst, f))
  end

(* you may assume that Num is always used with values 2, 3, ..., 10 though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* card_color = fn : card -> color *)
fun card_color( c ) = 
  case c of 
       (Spades,_) => Black
     | (Clubs,_) => Black
     | _ => Red

(* card_value = fn : card -> int *)
fun card_value( c ) =
  case c of
       (_,Ace) => 11 
     | (_,Num i) => i 
     | _ => 10

(* remove_card = fn : card list * card * exn -> card list *)
fun remove_card( cs, c, e ) = 
  let fun if_found ( l ) = 
    case l of 
         [] => NONE 
       | x::xs' => if c = x 
                   then SOME xs'
                   else case if_found(xs') of 
                             NONE => NONE
                           | SOME ys => SOME(x::ys)
  in
    case if_found(cs) of 
         NONE => raise e
       | SOME xs => xs
  end

(* all_same_color = fn : card list -> bool *)
fun all_same_color( cs ) = 
  case cs of 
       [] => true
     | x::[] => true
     | x1::(x2::xs') => ( card_color(x1) = card_color(x2) andalso all_same_color(x2::xs') )

(* sum_cards = fn : card list -> int *)
fun sum_cards( cs ) = 
  let fun aux( xs, acc ) = 
    case xs of 
         [] => acc
       | x::xs' => aux(xs', card_value(x)+acc)
  in 
    aux(cs,0)
  end

(* score = fn : card list * int -> int *)
fun score( cs, goal ) = 
  let 
    val s = sum_cards(cs)
    fun p ( s ) = 
      if s > goal 
      then 3*(s-goal) 
      else (goal-s)
  in 
    if all_same_color(cs) then ( p s  div 2)
    else p s 
  end

(* officiate = fn : card list * move list * int -> int *)
fun officiate( card_list, ms, goal ) = 
  let fun make_move( ms, cs, held ) =
        case ms of 
             [] => (cs, held)
           | (Discard c)::ms' => make_move(ms', cs, remove_card(held, c, IllegalMove))
           | Draw::ms' => case cs of 
                               [] => (cs, held)
                             | x::xs' => if sum_cards(x::held) >= goal 
                                         then (xs', x::held)
                                         else make_move(ms', xs', x::held)
  in
    case make_move( ms, card_list, [] ) of 
         (_, hs) => score(hs, goal)
  end

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
