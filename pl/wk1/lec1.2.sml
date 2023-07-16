(* section 1: examples to demo shadowing *)

val a = 10;

(* a : int
a -> 10 *)

val b = a * 2;

(* b -> 20 *)

val a = 5;

(* a -> 5 *)

val c = b;

(* c -> 20 *)

(* functions *)

(* only works if y>=0 *)
fun pow(x : int, y : int) =
    if y=0
    then 1
    else x * pow(x,y-1)

fun cube(x : int) =
    pow(x,3)

val sixtyfour = cube(4)
