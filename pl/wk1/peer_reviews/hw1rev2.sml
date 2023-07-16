fun is_older(date1: int*int*int, date2: int*int*int) =
    let
	val y1 = #1 date1
	val m1 = #2 date1
	val d1 = #3 date1
	val y2 = #1 date2
	val m2 = #2 date2
	val d2 = #3 date2		    		    
    in
	(y1 < y2) orelse (y1 = y2 andalso m1 < m2)
	orelse (y1=y2 andalso m1=m2 andalso d1 < d2)
		 
    end
	
		     
fun number_in_month(lst:(int*int*int) list, month: int) =
    if null lst then 0
    else if #2 (hd lst) = month
    then 1 + number_in_month(tl lst, month)
    else
	number_in_month(tl lst, month)

fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null months then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)
							    
fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd months)@dates_in_months(dates, tl months)

fun get_nth(lst: string list, n:int) =
    if n = 1
    then hd lst
    else get_nth(tl lst, n-1)

fun date_to_string(date: int*int*int) =
    let
	val months = ["January", "February", "March", "April",
		      "May", "June", "July", "August",
		      "September", "October", "November", "December"]
    in
	get_nth(months, #2 date)^" "^Int.toString(#3 date)^
	", "^Int.toString(#1 date)
    end
	
fun number_before_reaching_sum(sum: int, ints: int list) =
    if sum <= hd ints
    then 0
    else 1 + number_before_reaching_sum(sum - hd ints, tl ints)

fun what_month(day:int) =
    let
	val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	1 + number_before_reaching_sum(day, months)
    end
	
fun month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1 + 1, day2)

fun oldest(dates: (int*int*int) list) =
    if null dates
    then NONE
    else
	let
	    val ans = oldest(tl dates)
	in
	    if isSome ans andalso is_older(valOf ans, hd dates)
	    then ans
	    else SOME(hd dates)
	end
	    
fun mem(x:int, xs: int list) =
    if null xs
    then false
    else if (x = hd xs) orelse mem(x, tl xs)
    then true
    else false
	  

fun remove_duplicates(months: int list) =
    if null months
    then []
    else
	let
	    val tl_ans = remove_duplicates(tl months)
	in
	    if mem(hd months, tl_ans)
	    then tl_ans
	    else (hd months)::tl_ans
	end
	    

fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
    number_in_months(dates, remove_duplicates(months))

		    
 fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
     dates_in_months(dates, remove_duplicates(months))




