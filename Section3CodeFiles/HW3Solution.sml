(* Coursera Programming Languages, Homework 3, Provided Code *)

val only_capitals = List.filter(fn x=> Char.isUpper(String.sub(x,0)))

val longest_string1 = List.foldl (fn(hdx, acc) => 
	if String.size(hdx) > String.size(acc) then hdx else acc) ""

val longest_string2 = List.foldl (fn(hdx, acc) => 
	if String.size(acc) > String.size(hdx) then acc else hdx) ""

val longest_string_helper = (fn(x,y) => 
	if x>y then longest_string1 else longest_string2) 

val longest_string3 = longest_string_helper (1,0);

val longest_string4 = longest_string_helper (0,1);



exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)