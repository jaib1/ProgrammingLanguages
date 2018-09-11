(* Dan Grossman, Coursera PL, HW2 Provided Code *)
(* Jai Bhagat, HW2 *)

(* if you use this function to compare two strings (returns true if the 
same string), then you avoid several of the functions in problem 1 having
polymorphic types that may be confusing *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* helper function: checks list to see if an element is repeated, returns
 a boolean*)
fun inMem ([], _) = false
	| inMem(hdx::xs', x) = (x = hdx orelse inMem(xs', x))

(* Takes any type item and a type item list, returns NONE if the item
is not in the list, returns SOME list2 if the item is in the list, where 
list2 is list with the item removed*)
fun all_except_option(x, hdxs::xs') = 
	if not(inMem(hdxs::xs', x))
	then
		NONE
	else if x = hdxs
		then
			SOME(xs')
	else
		all_except_option(x, xs')
	| all_except_option(x, []) = NONE

(* Same as above, but returns the list itself, not an option list*)
fun all_except_option2(x, hdxs::xs') = 
	if not(inMem(hdxs::xs', x))
	then
		[]
	else 
		let val tlAns = all_except_option2(x, xs')
		in
			if x = hdxs
			then
				xs'
			else
				hdxs::tlAns
		end
	| all_except_option2(x, []) = []

(* same as above but with inputting a list of lists, and concatenating
all sublists together which contain an item that matches the input item *)
fun get_substitutions1(hdxs::xs', x) =
	let val curList = all_except_option2(x, hdxs)
	in
		curList@get_substitutions1(xs', x)
	end
	| get_substitutions1([],x) = []

(* Same as above, but using TCO *)
fun get_substitutions2(hdxs::xs', x) = 
	let val acc = []
		fun helper(hdxs::xs', x, acc) =
			let val curAcc = all_except_option2(x, hdxs)
			in
				helper(xs', x, acc@curAcc)
			end
			| helper([],x,acc) = acc
	in
		helper(hdxs::xs', x, acc)
	end
	| get_substitutions2([],x) = []

(* creates a type synonym for following function *)
type fullName = {first:string, middle:string, last:string}

(* Takes a list of list of strings and a fullName, and returns a list 
of fullNames that can be produced by substituting only the first name 
in the single fullName argument using "get_substitutions2"*)
fun similar_names(hdxs::xs', {first = frst, middle = mid, last = fin}) =
	let val frstSubs = get_substitutions2(hdxs::xs', frst)
		fun helper(hdsubs::subs', acc) = 
			helper(subs', acc@[{first = hdsubs, middle = mid, last = fin}])
			| helper([], acc) = acc
	in
		helper(frstSubs, [{first = frst, middle = mid, last = fin}])
	end
	| similar_names([], {first = frst, middle = mid, last = fin}) = [{first = frst, middle = mid, last = fin}]


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)