
(* Do not use "hd", "tl", "null", "isSome", "valOf", or "#" *)

(* Add together two int options *)
(* This function is bad b/c uses nested case expressions *)
fun addOpt2Bad(x,y) =
	case x of
		SOME(i) => 	
			(case y of
				SOME(j) => SOME(i+j)
				| _ => NONE)
		| _ => NONE

fun addOpt2(twoIntOpts) =
	case twoIntOpts of
		(SOME(x),SOME(y)) => SOME(x+y)
		| _ => NONE

(* Add together a list of int options *)
fun addOptAll2(x) = (* x will pattern-match: int option list*)
	let 
		fun helper(x,acc) = 
			case x of
				[] => SOME(acc)
				| SOME(hdx)::x' => helper(x', (hdx+acc))
	in
		helper(x, 0)
	end

(* As above, but with "pattern-matching in function binding" *)
fun addOptAll2b(x) =  
	let 
		fun helper ([],acc) = SOME(acc)
		| helper (SOME(hdx)::x', acc) = helper(x', (hdx+acc)) 
	in
		helper(x, 0)
	end

(* Add and subtract values in an int list in alternating fashion *)
(* This only works for lists of size 'n' where 'n' is an 
odd-number; due to the nature of switching between addition and 
subtraction (i.e. not using just a single alogirthmic operation), we can't 
use TCO for all solutions (i.e., 'n' as an even-number, in this example *)
fun alternate2(x) = (* x will pattern-match: int list *)
	let 
		fun helper([], acc) = acc (* [] will pattern-match: int list, acc will pattern-match: int *)
			| helper(hdx::x', acc) = helper(x', ~1*(acc-hdx))
	in
		helper(x,0)
	end

fun minMax2(x) = (* x will pattern-match: int list *)
	let 
		fun helper([], min, max) = (min, max)
			| helper(x, min, max) = helper(x', 
				if hdx < min then hdx else min, 
					if hdx > max then hdx else max)
	in
		let 
			val (hdx::x') = x
		in
			helper(x, hdx, hdx)
		end

	end



		(* case x of
			hdx::x' => helper(x', hdx, hdx) *)









