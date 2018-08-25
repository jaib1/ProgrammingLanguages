(* HW1 Solution *)

(* Comparing dates, where first number in tuple is an integer of the year, 
second number in tuple is an integer of the month, third number in tuple 
is an integer of the day *)
fun is_older(x:(int*int*int), y:(int*int*int)) =
	if #1(x) <> #1(y)
		then #1(x) < #1(y)
	else if #2(x) <> #2(y)
		then #2(x) < #2(y) 
	else
		 #3(x) < #3(y)

(* Takes a list of dates in above tuple format, and a single month, 
and returns number of listed dates which are in that month*)
fun number_in_month(dates:(int*int*int) list, month:int) = 
	if null(dates)
	then
		0
	else	
		let 
			val hd_ans = hd(dates)
		in
			if #2(hd_ans) = month
			then
				1 + number_in_month(tl(dates), month)
			else
				number_in_month(tl(dates), month)
		end
	end

(* Same as above, but now for a list of months*)
(*
fun number_in_months(dates:(int*int*int) list, months:int list) = 
	if null(dates) orelse null(months)
	then
		0
			else if null(tl(months))
		then
			number_in_month(dates, hd(months))
	else
		let
			val matches = 0
			val tl_months = number_in_months(dates, tl(months))
		in
			matches + number_in_month(dates, hd(tl_months))
		end
*)


