(* Programming Languages, Dan Grossman *)
(* Section 3: Lexical Scope and Higher-Order Functions *)

(* first example *)
val x = 1 (* this binding will be irrelevant *)
fun f y = 
    let 
        val x = y+1
    in
        fn z => x + y + z
    end
val x = 3 (* this binding will be irrelevant *)
val g = f 4 
val y = 5 (* this binding will be irrelevant *)
val z = g 6

(* second example *)
fun f2 g = 
    let 
        val x = 3 (* this binding will be irrelevant *)
    in
        g 2
    end
val x2 = 4
fun h y = x + y 
val z2 = f h
