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
		all_except_option(x, xs'@[hdxs])
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
	let fun helper(hdxs::xs', x, acc) =
			let val curAcc = all_except_option2(x, hdxs)
			in
				helper(xs', x, acc@curAcc)
			end
			| helper([],x,acc) = acc
	in
		helper(hdxs::xs', x, [])
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
	| similar_names([], {first = frst, middle = mid, last = fin}) = 
		[{first = frst, middle = mid, last = fin}]


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* Takes a card, returns the color *)
fun card_color(suit, rank) = 
	case suit of
		Clubs => Black
		| Spades => Black
		| Hearts => Red
		| Diamonds => Red

(* Takes a card, returns the value *)
fun card_value(suit, rank) = 
	case rank of
		Num(n) => n
		| Ace => 11
		| _ => 10

(* Removes a matching card from a deck of cards *)
fun remove_card(hdcs::cs', c, e) = 
	if not(inMem(hdcs::cs', c))
	then
		raise e
	else if c = hdcs
		then
			cs'
	else (* c is somewhere in cs, but not at the head *)
		remove_card(cs'@[hdcs], c, e) (* move c to the back of the list and continue*)
	| remove_card([],c,e) = raise e

(* Takes a deck of cards and checks to see if all same color, returns
a boolean*)
fun all_same_color(hdcs::hdcs2::cs') = 
	if card_color(hdcs) = card_color(hdcs2)
	then
		all_same_color(hdcs2::cs')
	else
		false
	| all_same_color(hdcs::[]) = true
	| all_same_color([]) = true

(* Adds together values of a deck of cards *)
fun sum_cards(hdcs::cs') = 
	let fun helper(hdcs::cs', acc) = helper(cs', acc + card_value(hdcs))
		| helper([], acc) = acc
	in
		helper(hdcs::cs', 0)
	end
	| sum_cards([]) = 0

fun score(cs, goal) = 
	let val sum = sum_cards(cs)
		val bonus = all_same_color(cs)
	in
		if (sum > goal andalso bonus) then 3*(sum-goal) div 2
		else if	(sum > goal) then 3*(sum-goal)
		else if	(sum < goal andalso bonus) then (goal-sum) div 2
		else goal-sum
	end

fun officiate(cs, ms, goal) =
	let fun helper([], ms, goal, hand) = score(hand, goal)
		| helper(cs, [], goal, hand) = score(hand, goal)
		| helper (hdcs::cs', hdms::ms', goal, hand) =
			if sum_cards(hand) > goal 
			then
				score(hand, goal)
			else
				case hdms of 
					Draw => helper(cs', ms', goal, hdcs::hand)
		 			| Discard(c) => helper(hdcs::cs', ms', goal, remove_card(hand, c, IllegalMove))
	in
		helper(cs, ms, goal, [])
	end


