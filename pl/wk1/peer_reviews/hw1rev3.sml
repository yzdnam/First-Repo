(* 
  Data example 
  (year, month, day) 
  (1962, 8, 15)
*)

(*
fun eval_dates (xs : (int * int) list) = 
  case xs of 
    [] => []
    | first::rest => 
      ((#1 first) < (#2 first))::eval_dates rest;
*) 

(* How to convert Tuple -> List?? *)



(*
fun compare (xs : (int*int) list) = 
  case xs of 
    [] => false 
    | h::t => 
      if (#1 h) = (#2 h) then compare t 
      else (#1 h) < (#2 h);
*)      


(* fun is_older (d1 : int*int*int, d2 : int*int*int) =  *)
fun is_older (d1 : int * int * int, d2 : int * int * int) =
  let 
    fun zipper (d1 : int * int * int, d2 : int * int * int ) = 
      [ (#1 d1, #1 d2), (#2 d1, #2 d2), (#3 d1, #3 d2) ]

    fun compare (xs : (int * int) list) = 
      if null xs
      then false 
      else 
        if #1 (hd xs) = #2 (hd xs) 
        then compare (tl xs)
        else #1 (hd xs) < #2 (hd xs)
  in
    compare (zipper (d1, d2))
  end

(*
)    
val d1 = (1972, 8, 17);
val d2 = (1972, 6, 12);
val dates = [ (1986, 12, 18), (1986, 7, 18), (1986, 12, 5), (1986, 5, 5) ]; 

*)

(*)
fun number_in_month (xs : (int * int * int) list, month : int) = 
  case xs of 
    [] => 0 
    | h::t =>
      if (#2 h) = month 
      then 1 + number_in_month (tl xs, month)
      else number_in_month (tl xs, month);
*)

fun number_in_month (xs : (int * int * int) list, month : int) =
  if null xs
  then 0
  else 
    let 
      val temp = number_in_month (tl xs, month)
    in
      if #2 (hd xs) = month
      then 1+temp
      else temp
    end

fun number_in_months (xs : (int * int * int) list, months : int list) = 
  if null months 
  then 0 
  else number_in_month (xs, hd months) + number_in_months (xs, tl months)

fun dates_in_month (dl : (int * int * int) list, month : int) =
  if null dl then []
  else 
    if month = (#2 (hd dl))
    then (hd dl)::dates_in_month (tl dl, month)
    else dates_in_month (tl dl, month);

fun dates_in_months (dl : (int * int * int) list, months : int list) = 
  if null months then []
  else dates_in_month (dl, hd months) @ dates_in_months (dl, tl months);

(* val words = ["banana", "bread", "tea", "milk"]; *)
fun get_nth (xs : string list, i : int ) = 
  if i=1 
  then hd xs 
  else get_nth (tl xs, i-1);

fun date_to_string (date : int * int * int) = 
  let 
    val months = 
      ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in 
    get_nth (months, #2 date) ^ " " ^ (Int.toString (#3 date)) ^ ", " ^ (Int.toString (#1 date))
  end

fun number_before_reaching_sum (bound : int, nums : int list) = 
  let 
    fun sum (xs : int list) =
      if null xs 
      then 0 
      else (hd xs) + sum (tl xs)

    fun take (n : int, xs : int list) = 
      if n<=0
      then []
      else (hd xs)::take (n-1, tl xs)

    fun sum_upto (n : int) = 
      if sum (take (n+1,nums)) >= bound 
      then n 
      else sum_upto (n+1)
  in
    sum_upto 0
  end

fun what_month (day : int) = 
  let 
    val months_values = [31,28,31,30,31,30,31,31,30,31,30,31]
  in 
    if day <= (hd months_values)
    then 1 
    else 1+number_before_reaching_sum (day, months_values)
  end

(* 
fun remove_dups (xs : int list) = 
  let 
    fun remove_elem (xs : int list, el : int) = 
      let 
        fun remove (xs : int list) = 
          if null xs 
            then []
            else 
              if (hd xs) = el 
              then remove (tl xs)
              else (hd xs)::remove (tl xs)

      in
        remove xs
      end
  in 
    if null xs
    then []
    else (hd xs)::remove_dups (remove_elem (tl xs, hd xs))
  end
*)

fun month_range (d1 : int, d2 : int) =
  let
    fun range (min : int, max : int) = 
      if min >= max 
      then [max]
      else min::range (min+1, max)

    fun convert2month (day : int) =
      what_month day

    val days = range(d1, d2)

    fun helper (xs : int list) =
      if null xs 
      then []
      else convert2month (hd xs)::helper (tl xs) 
  in
    helper (days)
  end
  
(* val dates = [ (1986, 12, 18), (1986, 7, 18), (1986, 12, 5), (1986, 5, 5) ];  *)
fun oldest (xs : (int * int * int) list) =
  let
    fun inner (xs : (int * int * int) list) = 
      if null (tl xs)
      then (hd xs)
      else 
        let
          val tail = inner (tl xs)
        in
          if is_older (hd xs, tail)
          then (hd xs)
          else tail
        end
  in
    if null xs
    then NONE 
    else SOME (inner xs)
  end