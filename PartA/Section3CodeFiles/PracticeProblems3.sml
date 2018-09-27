fun filter (f,hdx::xs') =
    if f(hdx) then hdx::filter(f,xs') else filter(f,xs')
    | filter(f,[]) = [] 

fun map (f,hdx::xs') =
    f(hdx)::map(f,xs')
    | map(f, []) = []

(* My implementation of "fold" *)
fun fold (f, acc, hdx::xs') =
	fold (f,f(hdx),xs')
	| fold (f, acc, []) = acc


fun addOptAll3 (x) = fold (fn(acc,hdx)=> )

fun alternate3

fun minMax3

fun unzip3

fun zip3

fun lengthOfAList2
