
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

(* Return the min and max from an int list *)
(* a try using minMax2 without any *explcit* case expressions *)
fun minMax2(x) = (* x will pattern-match: int list *)
	let 
		fun helper([], min, max) = (min, max)
			| helper(hdx::x', min, max) = helper(x', 
				if hdx < min then hdx else min, 
					if hdx > max then hdx else max)
	in
		let 
			val (hdx::x') = x
		in
			helper(x, hdx, hdx)
		end

	end

(* Take a list of pairs and return two lists, with the first list
containing the first element of all pairs, and the second list 
containing the second element of all pairs *)
fun unzip2([]) = ([],[])
	| unzip2((hdhdx,tlhdx)::x') = 
		let 
			val (hdhdx2,tlhdx2) = unzip2(x')
		in
			(hdhdx::hdhdx2, tlhdx::tlhdx2)
		end 

(* Computes the length of any 'a' list *)
fun lengthOfAList(x) = 
	let
		fun helper([], acc) = acc
			| helper(hdx::x', acc) = helper(x', acc+1)
	in
		helper(x, 0)
	end

(* checks list to see if an element is repeated, returns a boolean*)
fun inMem ([], _) = false
	| inMem(hdx::xs', x) = x = hdx orelse inMem(xs', x)

(* removes duplicates in a list *)
fun rmvDuplicates2([]) = []
	| rmvDuplicates2(hdx::xs') = 
	let
		val tlAns = rmvDuplicates2(xs')
	in
		if inMem(hdx, xs')
		then
			xs'
		else
			hdx::tlAns
	end

(* Set types and datatypes for following problems *)
    type student_id = int
    type grade = int (* must be in 0 to 100 range *)
    type final_grade = { id : student_id, grade : grade option }
    datatype pass_fail = pass | fail

(* Takes a type "final_grade" and returns pass if grade >=75, else fail *)
fun passOrFail({grade = SOME grade, id = x}) =
	if grade >= 75 then pass else fail
	| passOrFail(_) = fail

(*Set datatype for following problems*)

datatype 'a tree = leaf | node of 
{value:'a, left:'a tree, right:'a tree}

(* takes as input a tree, and returns length of longest path to a leaf*)
(* can't figure out how to use an accumulator for tree datatype *)
fun treeHeight(leaf) = 0
	| treeHeight(node{value=_, left=ltree, right=rtree}) = 
		let 
			fun helper(node{value=_, left=leaf, right=leaf}, acc) = acc
				| helper() = helper([], acc+1)
				| helper 

















