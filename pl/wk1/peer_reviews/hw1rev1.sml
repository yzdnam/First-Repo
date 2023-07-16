(* Question 1 *)

fun is_older (date1 : int * int * int, date2 : int * int * int ) =
    if #1 date1 < #1 date2
    then true
    else
	if #1 date1 > #1 date2
	then false
	else
	    if #2 date1 < #2 date2
	    then true
	    else
		if #2 date1 > #2 date2
		then false
		else
		    if #3 date1 < #3 date2
		    then true
		    else false


(* Question 2 *)
fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else
	if #2 (hd dates) = month
	then 1 + number_in_month(tl dates, month)
	else number_in_month(tl dates, month)


(* Question 3 *)
fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)


(* Question 4 *)
fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
	if #2 (hd dates) = month
	then hd dates::dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)
							     
(* Question 5 *)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months)@dates_in_months(dates, tl months)


(* Question 6 *)
fun get_nth (xs : string list, n : int) =
    if n = 1
    then hd xs
    else get_nth(tl xs, n-1)


(* Question 7 *)
fun date_to_string (date : (int * int * int)) =
    let
	val months = ["January", "February", "March", "April", "May", "June", "July",
		      "August", "September", "October", "November", "December"]
	val year = #1 date		 
	val month = #2 date
	val day = #3 date
    in
	get_nth(months, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
    end


(* Question 8 *)
fun number_before_reaching_sum (sum : int, xs : int list) =
    if sum - hd xs <= 0
    then 0
    else 1 + number_before_reaching_sum(sum - hd xs, tl xs)


(* Question 9 *)
fun what_month (day : int) =
    let
	val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	1 + number_before_reaching_sum(day, days)
    end


(* Question 10 *)
fun month_range (day1 : int, day2 : int) =
    if day2 - day1 <  0
    then []
    else what_month(day1)::month_range(day1 + 1, day2)


(* Question 11 *)
fun oldest ( dates : (int * int * int) list ) =
    if null dates
    then NONE
    else
	let val tl_ans = oldest (tl dates)
	in if isSome tl_ans andalso is_older(valOf tl_ans, hd dates)
	   then tl_ans
	   else SOME (hd dates)
	end


(* Question 12 *)

fun contains (x : int, xs : int list) =
    if null xs
    then false
    else
	if hd xs = x
	then true
	else contains(x, tl xs)

	    
	    
fun unique_vals (xs : int list) =
    if null xs
    then []
    else
	let
	    val tail_unique = unique_vals(tl xs)
	in
	    if contains(hd xs, tail_unique)
	    then tail_unique
	    else (hd xs)::tail_unique
	end

fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
    number_in_months(dates, unique_vals(months))


fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
    dates_in_months(dates, unique_vals(months))
		   
(* Question 13 *)
fun reasonable_date (date : (int * int * int)) =
    let
	fun leap_year (year : int ) =
	    (year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 <> 0)


	fun get_nth_int (xs : int list, n : int) =
	    if n = 1
	    then hd xs
	    else get_nth_int(tl xs, n-1)
	    
	val days = if leap_year(#1 date)
		   then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		   else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	val max_days = get_nth_int(days, #2 date)
    in
	(#1 date > 0) andalso (#2 date >= 1 andalso #2 date <= 12) andalso (#3 date <= max_days)
    end
