
(* year / mongth / day *)
(* this is the most wierd function *)
fun is_older(first : int * int * int, second : int * int * int) = 
  if #1 first > #1 second
  then false
  else
      if #1 first = #1 second
      then
          if #2 first > #2 second
          then false
          else 
              if #2 first = #2 second
              then
                  #3 first < #3 second
              else true
      else true
  
  


fun number_in_month(dates : (int * int * int) list, month : int) =
  if null dates
  then 0
  else number_in_month(tl dates, month) 
       + (if #2 (hd dates) = month then 1 else 0)

fun number_in_months(dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int * int * int) list, month : int) =
  if null dates
  then []
  else if #2 (hd dates) = month
  then (hd dates) :: dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings : string list, n : int) =
  let fun iter(strings : string list, i : int) =
        if i = n
        then hd strings 
        else iter(tl strings, i + 1)
  in iter(strings, 1) end

fun date_to_string(date : int * int * int) =
  let val months = ["January", "February", "March", "April", "May", "June", "July",
                    "August", "September", "October", "November", "December"]
  in
      get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date) 
  end
 
fun number_before_reaching_sum(sumToReach : int, data : int list) =
  let fun sum(data : int list, acc : int, i : int) =
        if acc + hd data >= sumToReach
        then i
        else sum(tl data, acc + hd data, i + 1)
  in sum(data, 0, 0) end
  
fun what_month(day : int) =
  let val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      number_before_reaching_sum(day, months) + 1
  end

fun month_range(day1 : int, day2 : int) =
  if day1 > day2 
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates : (int * int * int) list) =
  if null dates
  then NONE
  else let fun safeOldest(dates : (int * int * int) list) =             
             let
                 val tail = tl dates
                 val head = hd dates
             in
                 if null tail
                 then head
                 else
                     let val min = safeOldest(tail) in
                         if is_older(head, min)
                         then head
                         else min
                     end
             end
       in
           SOME (safeOldest(dates))
       end
