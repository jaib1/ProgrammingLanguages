(* Coursera Programming Languages, Homework 3, Provided Code *)

val only_capitals = List.filter(fn x=> Char.isUpper(String.sub(x,0)))

val longest_string1 = List.foldl (fn(hdx, acc) => 
	if String.size(hdx) > String.size(acc) then hdx else acc) ""

val longest_string2 = List.foldl (fn(hdx, acc) => 
	if String.size(acc) > String.size(hdx) then acc else hdx) ""

fun longest_string_helper f = 
    foldl (fn (hdx,acc) => if f(String.size hdx, String.size acc) then hdx 
    else acc) ""

val longest_string3 = longest_string_helper (fn (x,y) => x>y);

val longest_string4 = longest_string_helper (fn (x,y) => x>=y);

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o List.rev o explode

exception NoAnswer

fun first_answer f xs = 
	case xs of 
		[] => raise NoAnswer
		| x::xs' => case f x of
		 				NONE => first_answer f xs' 
		 			 	| SOME x2 => x2

fun all_answers f xs =
	let fun helper(xs, acc) =
		case xs of
			[] => SOME acc
			| SOME(hdx)::xs' => helper(xs', acc@hdx)
			| NONE::xs' => NONE
	in
		helper(map f xs, [])
	end

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

val count_wildcards = g (fn() => 1) (fn(_) => 0)

val count_wild_and_variable_lengths = g (fn() => 1) 
	String.size

fun count_some_var (str, p) = g (fn() => 0) 
	(fn(p) => if str = p then 1 else 0) p

fun returnVariableList p =
		case p of
			Variable x => [Variable x]
			| TupleP xs => List.foldl (fn (p2,acc) =>
				acc@returnVariableList(p2)) [] xs 
			| ConstructorP(_,pat) => returnVariableList pat
			| _ => []

fun isDuplicate [] = false
	| isDuplicate(hdx::xs') = List.exists(fn y => hdx = y) xs' orelse isDuplicate(xs')

val check_pat = (not o isDuplicate o returnVariableList)

fun match (v, p) =
	case (v, p) of 
		(_, Wildcard) => SOME []
		| (v, Variable s) => SOME [(s,v)]
		| (Unit, UnitP) => SOME []
		| (Const x1, ConstP x2) => if x1=x2 then SOME [] else NONE
		| (Tuple vs, TupleP ps) => if List.length(vs) = List.length(ps) 
			then all_answers match (ListPair.zip(vs,ps)) else NONE
		| (Constructor (s2,va), ConstructorP (s1,pat)) =>if s1=s2 
			then match(va,pat) else NONE
		| _ => NONE

fun first_match v ps = 
	SOME (first_answer (fn p => match(v,p)) ps)
	handle NoAnswer => NONE

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)