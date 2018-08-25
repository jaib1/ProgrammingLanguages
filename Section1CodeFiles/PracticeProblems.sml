(* altNum must be = 1 to start function call*)
fun alternate(xs : int list, altNum : int) =
	if null(xs)
	then 
		0
	else
		altNum*hd(xs) + alternate(tl(xs), ~1*altNum)

fun alternate2(x:int list) =
    let
        fun helper(factor:int, x:int list) =
            if null x
            then 
            	0
            else 
            	(factor * hd(x)) + helper(~1*factor, tl(x))
    in
        helper(1, x)
    end


fun divideBy(x:int, y:int) = 
	if x < y
	then
		0
	else
		1 + divideBy(x-y,y)

fun anyDivisibleBy(a:int list, x:int) =
	if null(a)
	then
		false
	else if hd(a) mod x = 0
	then 
		true
	else
		anyDivisibleBy(tl(a),x)

fun addOpt(x:int option, y: int option) = 
	if not(isSome(x)) orelse not(isSome(y))
	then
		NONE
	else
		SOME(valOf(x) + valOf(y))

(* opt must = NONE to start function call *)
fun addOptAll(optList:int option list, opt: int option) =
	(*if x = NONE orelse x = [], return NONE) *)
	if null(optList)
	then
		opt
	else if opt = NONE 
		then 
			addOptAll(tl(optList), hd(optList))
	else if hd(optList) = NONE
		then
			addOptAll(tl(optList), opt)
	else
		addOptAll(tl(optList), SOME(valOf(opt) + valOf(hd(optList))))


fun minMax(numList:int list) =
	let 
		fun helper(numList:int list, min: int, max: int) =
			if null(tl(numList))
			then
				(min, max)
			else if hd(numList) < min 
				then 
					helper(tl(numList), hd(numList), max) 
			else if hd(numList) > max
				then
					helper(tl(numList), min, hd(numList))
			else
				helper(tl(numList), min,max)
	in 
		helper(numList, hd(numList), hd(numList))
	end


fun minMax2(numList:int list) =
	let 
		fun helper(numList:int list, min: int, max: int) =
			if null(tl(numList))
			then
				(hd(numList), hd(numList))
			else
				let 
					val tl_ans = helper(tl(numList), hd(numList), hd(numList))
				in
					if hd(numList) < #1(tl_ans) 
					then 
						(hd(numList), #2(tl_ans))
					else if hd(numList) > #2(tl_ans)
						then 
							(#1(tl_ans), hd(numList))
					else (* hd(numList) is not a min or a max *)
						(#1(tl_ans), #2(tl_ans))
				end
	in
		helper(numList, hd(numList), hd(numList))
	end

fun unzip(pairs:(int*int) list) =
	if null(pairs)
	then
		([],[])
	else
		let 
			val tl_ans = unzip(tl(pairs))
		in
			let 
				val hd_tl_ans = #1(tl_ans)
				val tl_tl_ans = #2(tl_ans)
			in
					(#1(hd(pairs))::hd_tl_ans, #2(hd(pairs))::tl_tl_ans)
			end
		end

fun zip(list1:int list, list2: int list) =
	if null(list1) orelse null(list2)
	then
		[]
	else
		let 
			val tl_ans = zip(tl(list1), tl(list2))
		in
			(hd(list1), hd(list2))::tl_ans
		end

(* Helper function for the following two problem *)
fun list_length (numbers : int list) =
    if null numbers
    then 
    	0
    else 
    	1 + list_length (tl numbers)

fun zip_opt (xs : int list, ys : int list) =
    if list_length xs = list_length ys
    then 
    	SOME (zip (xs, ys))
    else 
    	NONE








