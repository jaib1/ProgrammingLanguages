(* HW1 Solution - Jai Bhagat*)

(* Comparing dates, where first number in tuple is an integer of the year, 
second number in tuple is an integer of the month, third number in tuple 
is an integer of the day *)
fun is_older(date1:(int*int*int), date2:(int*int*int)) =
	if #1(date1) <> #1(date2)
		then #1(date1) < #1(date2)
	else if #2(date1) <> #2(date2)
		then #2(date1) < #2(date2) 
	else
		 #3(date1) < #3(date2)

(* Takes a list of dates in above tuple format, and a single month, 
and returns number of listed dates which are in that month *)
fun number_in_month(dates:(int*int*int) list, month:int) = 
	if null(dates)
	then
		0
	else	
		let 
			val cur_month = #2(hd(dates))
		in
			if cur_month = month
			then
				1 + number_in_month(tl(dates), month)
			else
				number_in_month(tl(dates), month)
		end

(* Same as above, but now for a list of months*)
fun number_in_months(dates:(int*int*int) list, months:int list) = 
	if null(dates) orelse null(months)
	then
		0
	else
		let
			val tl_months = number_in_months(dates, tl(months))
		in
			tl_months + number_in_month(dates, hd(months))
		end

(* Same thing as "fun number_in_month", but now return a list of 
all the entire dates if the month of the date matches with "month" *)
fun dates_in_month(dates:(int*int*int) list, month:int) =
	if null(dates)
	then
		[]
	else	
		let 
			val cur_month = #2(hd(dates))
		in
			if cur_month = month
			then
				hd(dates)::dates_in_month(tl(dates), month)
			else
				dates_in_month(tl(dates), month)
		end

(* Same thing as above, but now for a list of months *)
fun dates_in_months(dates:(int*int*int) list, months: int list) =
	if null(dates) orelse null(months)
	then
		[]
	else
		let
			val tl_months = dates_in_months(dates, tl(months))
		in
			dates_in_month(dates, hd(months))@tl_months
		end

(* get "nth" string item in a string list *)
fun get_nth(strings: string list, n:int) = (* add helper function for "ct" *)
	let 
		fun helper(ct: int, strings:string list, n:int) = 
			if ct < n
			then
				helper(ct+1, tl(strings), n)
			else
				hd(strings)
	in
		helper(1, strings, n)
	end

(* convert a date in int tuple format to string format *)
fun date_to_string(date:(int*int*int)) = 
	let 
		val stringsMonths = ["January", "February", "March", "April", "May", "June", 
		"July", "August", "September", "October", "November", "December"]
	in
		get_nth(stringsMonths, #2(date))^" "^Int.toString(#3(date))^", "^Int.toString(#1(date))
	end

(* add up numbers in an int list, compare to a set int, "sum", return the
element number of the last number in the int list that doesn't exceed "sum" *)
fun number_before_reaching_sum(sum:int, numList:int list) = 
	if hd(numList) >= sum
	then
		0
	else
		let 
			fun helper(ct:int, sum:int, numList:int list, ct2:int) =
				let
					val cur_add = hd(tl(numList))
				in
					if ct + cur_add >= sum
					then
						ct2
					else
						helper(ct+cur_add, sum, tl(numList), ct2+1)
				end
			in
				helper(hd(numList), sum, numList, 1)
			end

(* returns the month (in integer form) in which the integer labeled day is in, 
based on a 365 day calendar*)
fun what_month(num:int) =
	let 
		val monthsByNumDays = [31, 28, 31, 30, 31, 30, 31, 
		31, 30, 31, 30, 31]
	in
		1+number_before_reaching_sum(num, monthsByNumDays)
	end

(* returns an int list with all days in between day 1 and day 2 as month-ints *)
fun month_range(day1:int, day2:int) = 
	if day1 > day2
	then
		[]
	else
		what_month(day1)::month_range(day1+1, day2)

(* returns an option "date" value of the oldest date in a list of dates*)
fun oldest(dates:(int*int*int) list) =
	if null(dates)
	then
		NONE
	else
		let
			fun helper(date:(int*int*int), dates:(int*int*int) list) =
				if null(dates)
				then
					SOME(date)				
				else if is_older(date, hd(dates))
				then
					helper(date, tl(dates))
				else
					helper(hd(dates), tl(dates))
		in
			helper(hd(dates), tl(dates))
		end
(* --------------------------Challenge Problems-----------------------*)

(* My attempt *)

(* fun number_in_months_challenge(dates:(int*int*int) list, months:int list) =
	if null(months)
	then
		0
	else
		let
			fun rmvDuplicates(newMonths:int list, months:int list, cur_month:int) =
				if null(months)
				then
					cur_month::newMonths
				else if
					cur_month = hd(months)
				then
					rmvDuplicates(newMonths, tl(months), hd(months))
				else if we're still checking tl(months) for duplicates
					rmvDuplicates(newMonths, tl(months), cur_month)
		in
			number_in_months(dates, rmvDuplicates([], tl(months), hd(months)))
		end
*)

(* Sample Solutions *)

(* quadratic algorithm rather than sorting which is nlog n *)
(* returns a boolean of true when some int "x" is in an int list "xs",
and false otherwise *)
fun inMem(x:int, xs:int list) =
	if null(xs)
	then
		false
	else
    	x = hd(xs) orelse inMem(x, tl(xs))

fun rmvDuplicates(xs:int list) =
    if null(xs)
    then []
    else
        let 
            val tl_ans = rmvDuplicates(tl(xs))
        in
            if inMem(hd(xs), tl_ans)
            then 
            	tl_ans
            else 
            	(hd xs)::tl_ans
        end

fun number_in_months_challenge(dates:(int*int*int) list, months:int list) =
    number_in_months(dates, rmvDuplicates(months))

fun dates_in_months_challenge(dates:(int*int*int) list, months:int list) =
    dates_in_months(dates, rmvDuplicates(months))

fun reasonable_date (date : int * int * int) =
    let    
        fun get_nth (lst : int list, n : int) =
        if n=1
        then hd lst
        else get_nth(tl lst, n-1)
        val year  = #1 date
        val month = #2 date
        val day   = #3 date
        val leap  = year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
        val feb_len = if leap then 29 else 28
        val lengths = [31,feb_len,31,30,31,30,31,31,30,31,30,31]
    in
        year > 0 andalso month >= 1 andalso month <= 12
        andalso day >= 1 andalso day <= get_nth(lengths,month)
    end










