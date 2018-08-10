(* altNum must be = 1 *)
fun alternate(xs : int list, altNum : int) =
	if null(xs)
	then 
		0
	else
		altNum*hd(xs) + alternate(tl(xs), ~1*altNum) 

fun alternate2 (numbers : int list) =
    let
        fun helper (factor : int, numbers : int list) =
            if null numbers
            then 0
            else (factor * hd numbers) + helper (~1 * factor, tl numbers)
    in
        helper (1, numbers)
    end


