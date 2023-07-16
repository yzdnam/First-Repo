(* a "date" is an SML values of type int*int*int where the first part is the year, the second part is the month, and the third part is the
 day. a "day of year" is a number from 1 to 365 where, for example, 33 represents February 2. *)

fun year(date : int*int*int) =
    #1 date

fun month(date : int*int*int) =
    #2 date

fun day(date : int*int*int) =
    #3 date
       
(* takes two dates and evaluates to true or false. evaluates to true if the first argument is a date that comes before the second argument. 
 if the two dates are the same, the result is false. *)
       
fun is_older(d1 : int*int*int, d2 : int*int*int) =
    let
	val y1 = year d1 val y2 = year d2
	val m1 = month d1 val m2 = month d2
	val day1 = day d1 val day2 = day d2
    in
	if y1 < y2
	then true
	else
	    if y1 = y2
	    then if m1 < m2
		 then true
		 else
		     if m1 = m2
		     then if day1 < day2
			  then true
			  else false
		     else false
	    else false
    end
		
(* takes a list of dates and a month and returns how many dates in the list are in the given month *)
		       
fun number_in_month(lods : (int*int*int) list, mon : int) =
    if null lods
    then 0
    else if month (hd lods) = mon
    then 1 + number_in_month ((tl lods),mon)
    else number_in_month ((tl lods),mon)

(* takes a list of dates and a list of months and returns the number of dates in the list of dates that are in any of the months in the list
of months. *)
			 
fun number_in_months(lods : (int*int*int) list, mons : int list) =
    if null lods orelse null mons
    then 0
    else number_in_month(lods,(hd mons)) + number_in_months(lods,(tl mons))

(* takes a list of dates and a month and returns a list holding the dates from the arglist of dates that are in the month. the returned list
should contain dates in the order they were originally given. *)
							   
fun dates_in_month(lods : (int*int*int) list, mon : int) =
    if null lods
    then []
    else let val first_date = hd lods in
	     if month first_date = mon
	     then first_date :: dates_in_month((tl lods), mon)
	     else dates_in_month((tl lods), mon) end

(* takes a list of dates and a list of months and returns a list holding the dates from the arglist of dates that are in any of the months in the
list of months *)
	     
fun dates_in_months(lods : (int*int*int) list, mons : int list) =
    if null lods orelse null mons
    then []
    else dates_in_month(lods, (hd mons)) @ dates_in_months(lods, (tl mons))

(* takes a list of strings and an int n and returns the nth element of the list where the head of the list is 1st. assumes the length of the list
is greater than n *)
							  
fun get_nth (los : string list, n : int) =
    if n = 1
    then hd los
	     else get_nth(tl los, n - 1)

(* takes a date and returns a string of the form January 20, 2013. *)
			 
fun date_to_string (date : int*int*int) =
    let
	val months = ["January ", "February ", "March ", "April ", "May ", "June ", "July ", "August ", "September ", "October ",
		      "November ", "December "]
	val outyear = get_nth(months, month date) in
	outyear ^ (Int.toString (day date)) ^ ", " ^ (Int.toString (year date))
end

(* takes a positive int, sum, and an int list with all positive numbers and returns an int n. n has two properties:
   1. the first n elements of the given list add up to less than the given int
   2. the first n + 1 elements of the given list add to the given int or more
assumes the entire list sums to more than the passed in value *)
	
fun number_before_reaching_sum(sum : int, intlist : int list) =
    let val first = hd intlist
	val rest = tl intlist
    in
	if first >= sum
	then 0
	else 1 + number_before_reaching_sum(sum, first + hd rest :: tl rest)
    end

(* takes a day of year and returns what month that day is in *)
	
fun what_month(doy : int) =
    let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 1 + number_before_reaching_sum(doy, days_in_months)
    end

(* takes two days of the year, day1 and day2, and returns an int list [m1, m2, ..., mn] where m1 is the month of day1, m2 is the month of day1+1,
..., and mn is the month of day2 *)

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month day1 :: month_range(day1+1, day2)

(* takes a list of dates and evaluates to an (int*int*int) option. Returns NONE if the list has no dates and SOME d if the date d is the oldest
date in the list *)

fun oldest(lod : (int*int*int) list) =
    if null lod
    then NONE
    else let
	fun find_oldest (lod : (int*int*int) list) =
	    if null (tl lod)
	    then hd lod
	    else let val tl_ans = find_oldest(tl lod)
		 in
		     if is_older(hd lod, tl_ans)
		     then hd lod
		     else tl_ans
		 end
    in
	SOME (find_oldest lod)
    end

(* modifies the solutions to 3 and 5 so that they allow duplicates in the second argument *)
	     
fun is_in(item : int, li : int list) =
    if null li
    then false
    else
	if item = hd li
	then true
	else is_in(item, tl li)
		  
fun remove_dupes(li : int list) =
    if null (tl li)
    then li
    else
	if is_in(hd li, tl li)
	then remove_dupes(tl li)
	else hd li :: remove_dupes(tl li)
	    			  
fun number_in_months_challenge(lod : (int*int*int) list, mons : int list) =
    number_in_months(lod, remove_dupes mons)

fun dates_in_months_challenge(lod : (int*int*int) list, mons : int list) =
    dates_in_months(lod, remove_dupes mons)
	
(* takes a date and determines if it describes a real date in the common era. a real date has a positive year, a month between  1 and 12, and
a day appropriate for the month. Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100. *)
fun divis(num : int, denom : int) =
    num mod denom = 0

fun reasonable_date(date : int*int*int) =
    let
	val ydate = year date
	val mdate = month date
	val ddate = day date
	val reg_month_lens = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	val leap_month_lens = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	fun leap_year(y : int) =
	    divis(y, 400) orelse divis(y, 4) andalso not (divis(y,100))
	fun get_nth_numb(n : int, lon : int list) =
	    if n = 1
	    then hd lon
	    else get_nth_numb( n - 1, tl lon)
	fun get_month_days(y : int, m : int) =
	    if leap_year y
	    then get_nth_numb(m, leap_month_lens)
	    else get_nth_numb(m, reg_month_lens)
    in
	if ydate > 0
	then if mdate >= 1 andalso mdate <= 12
	     then let
		 val month_days = get_month_days(ydate, mdate)
	     in
		 if ddate >= 1 andalso ddate <= month_days
		 then true
		 else false
	     end
	     else false
	else false
    end
	
