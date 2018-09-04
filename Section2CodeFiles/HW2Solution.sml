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



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)