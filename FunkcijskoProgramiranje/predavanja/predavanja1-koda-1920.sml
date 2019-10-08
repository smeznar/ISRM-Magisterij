(*

val x = 3;
(* staticno okolje: x -> int *)
(* dinamicno okolje: x -> 3 *)

val y = 5;
(* staticno okolje: y -> int, x -> int *)
(* dinamicno okolje: y -> 5, x -> 3 *)

val z = x + y;
(* staticno okolje: z -> int, y -> int, x -> int *)
(* dinamicno okolje: z -> 8, y -> 5, x -> 3 *)

val uspesno = if z > 5 then true else false;
(* staticno okolje: uspesno -> bool, ... *)
(* dinamicno okolje: uspesno -> true, ... *)
*)



(* PRIMERI SINTAKTIÈNIH, TIPSKIH in EVALVACIJSKIH NAPAK *)

val x = 34

val y = x + 1

val z = if y=1 then 34 else 4

val q = if y > 0 then 0 else 1

val a = ~5

val w = 0

val fun1 = 34

val v = x * w

val fourteen = 7 - 7



(* prikaz senèenja *)

val a = 3
val b = 2*a
val a = 5

val a = a + 1

(* END prikaz senèenja *)




fun obseg (r: real) = 
    2.0 * Math.pi * r

fun potenca (x: int, y: int) =
    if y=0
    then 1
    else x * potenca(x, y-1)

fun faktoriela (n: int) =
    if n=0
    then 1
    else n * faktoriela(n-1)

fun sestej1N (n: int) =
    if n=1
    then 1
    else sestej1N(n-1) + n

fun sestejAB (a: int, b: int) =    (* sestejemo vsa naravna stevila od a do b *)
    if a=b
    then a
    else sestejAB(a, b-1) + b

fun sestej1N_easy (n: int) =
    sestejAB (1, n)


(* SML obvlada tudi sklepanje na podatkovne tipe iz stat. konteksta *)
    
fun obseg2 r = 
    2.0 * Math.pi * r

fun faktoriela2 n =
    if n=0
    then 1
    else n * faktoriela(n-1)

	