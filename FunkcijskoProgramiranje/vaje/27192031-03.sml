(* Naravna števila definiramo tako:
 * - Obstaja ničla (ZERO).
 * - Vsako naravno število ima svojega naslednika (NEXT).
 * - Ničla ni naslednik nobenega naravnega števila.
 * - Če sta dve naravni števili enaki, potem sta enaka tudi njuna naslednika.
 *)
datatype natural = NEXT of natural | ZERO

exception PreviousOfZero

(* Vrne true, če so podatki urejeni naraščajoče. *)
fun sorted [] = true
    | sorted [(x:int)] = true
    | sorted (x1::x2::xs) = x1 <= x2 andalso sorted(x2::xs);

(* Vrne seznam, ki ima na i-tem mestu terko (xs_i, ys_i),
   v kateri je xs_i i-ti element seznama xs, ys_i pa i-ti
   element seznama ys. Če sta dolžini seznamov različni, vrnite
   pare do dolžine krajšega. *)
fun zip ([], ys: 'b list): ('a * 'b) list = []
    | zip (xs: 'a list, []) = []
    | zip (x::xs, y::ys) = (x,y)::zip(xs,ys);

(* Vrne obrnjen seznam. Uporabite repno rekurzijo. *)
fun reverse (xs: 'a list): 'a list = 
    let 
	    fun pomozna(sez,acc) =
        case sez of
            [] => acc
            | x::rep => pomozna(rep, x::acc)
    in
        pomozna(xs,[])
    end;

(* Vrne natural, ki predstavlja število a, ali proži izjemo
   PreviousOfZero, če a < 0. *)
fun toNatural 0: natural = ZERO
    | toNatural a = 
        if a < 0
        then raise PreviousOfZero
        else NEXT (toNatural (a-1));

(* Vrne true, če je a sodo število. *)
fun isEven (ZERO): bool = true
    | isEven (NEXT(ZERO)) = false
    | isEven (NEXT(NEXT(a))) = isEven(a);

(* Vrne true, če je a liho število. *)
fun isOdd (ZERO): bool = false
    | isOdd (NEXT(ZERO)) = true
    | isOdd (NEXT(NEXT(a))) = isOdd(a);

(* Vrne predhodnika naravnega števila a.
   Če je a ZERO, proži izjemo PreviousOfZero. *)
fun previous (ZERO): natural = raise PreviousOfZero
    | previous (NEXT(a)) = a;

(* Vrne naravno število, ki ustreza razliki števil a in b (a - b).
   Če rezultat ni naravno število, proži izjemo PreviousOfZero. *)
fun subtract (ZERO, NEXT(_)): natural = raise PreviousOfZero
    | subtract (a, ZERO) = a
    | subtract (NEXT(a), NEXT(b)) = subtract(a,b);

(* Vrne true, če funkcija f vrne true za kateri koli element seznama.
   Za prazen seznam naj vrne false. *)
fun any (f: 'a -> bool, []): bool = false
    | any (f, (x::xs)) = (f x) orelse (any (f, xs));

(* Vrne true, če funkcija f vrne true za vse elemente seznama.
   Za prazen seznam naj vrne true. *)
fun all (f: 'a -> bool, []): bool = true
    | all (f, (x::xs)) = f(x) andalso (all (f, xs));

(* TESTI *)
(*use "unittest.sml";

test("test-sorted", [
    assert(sorted []),
	assert(sorted [1]),
	assert(sorted [1,2,3,4,5,6]),
	assert_false(sorted [1,2,3,5,4])
]);

test("test-zip",[
    assert_equal(zip, ([], []), []),
    assert_equal(zip, ([1,2,3], ["a","b"]), [(1, "a"), (2, "b")]),
    assert_equal(zip, ([1,2], ["a","b","c"]), [(1, "a"), (2, "b")]),
    assert_equal(zip, (["a", "b"], [1, 2]), [("a", 1),("b", 2)])
]);

test("test-reverse",[
    assert_equal(reverse, [], []),
    assert_equal(reverse, [1,2,3], [3,2,1]),
    assert_equal(reverse, ["a","b","c"], ["c", "b", "a"]),
    assert_equal(reverse, [1], [1])
]);

test("test-toNatural",[
    assert_equal(toNatural, 0, ZERO),
    assert_equal(toNatural, 1, NEXT(ZERO)),
    assert_equal(toNatural, 3, NEXT(NEXT(NEXT(ZERO)))),
    assert_raises(fn () => toNatural (~1), PreviousOfZero),
    assert_raises(fn () => toNatural (~5), PreviousOfZero)
]);

test("test-isEven", [
    assert(isEven ZERO),
	assert(isEven (NEXT(NEXT(ZERO)))),
	assert(isEven (NEXT(NEXT(NEXT(NEXT(ZERO)))))),
	assert_false(isEven (NEXT(ZERO))),
    assert_false(isEven (NEXT(NEXT(NEXT(ZERO))))),
    assert_false(isEven (NEXT(NEXT(NEXT(NEXT(NEXT(ZERO)))))))
]);

test("test-isOdd", [
    assert_false(isOdd ZERO),
	assert_false(isOdd (NEXT(NEXT(ZERO)))),
	assert_false(isOdd (NEXT(NEXT(NEXT(NEXT(ZERO)))))),
	assert(isOdd (NEXT(ZERO))),
    assert(isOdd (NEXT(NEXT(NEXT(ZERO))))),
    assert(isOdd (NEXT(NEXT(NEXT(NEXT(NEXT(ZERO)))))))
]);

test("test-previous",[
    assert_equal(previous, (NEXT(ZERO)), ZERO),
    assert_equal(previous, (NEXT(NEXT(ZERO))), (NEXT(ZERO))),
    assert_raises(fn () => previous ZERO, PreviousOfZero)
]);

test("test-subtract",[
    assert_equal(subtract, (ZERO, ZERO), ZERO),
    assert_equal(subtract, (NEXT(ZERO), ZERO), (NEXT(ZERO))),
    assert_equal(subtract, (NEXT(NEXT(ZERO)), NEXT(ZERO)), (NEXT(ZERO))),
    assert_raises(fn () => subtract (ZERO, NEXT(ZERO)), PreviousOfZero),
    assert_raises(fn () => subtract (ZERO, NEXT(NEXT(ZERO))), PreviousOfZero)
]);

test("test-any", [
    assert_false(any (fn x => x>5, [])),
	assert_false(any (fn x => x>5, [3])),
    assert_false(any (fn x => x>10, [1,2,3,4,5,6,7,8])),
	assert(any (fn x => (x mod 25) = 0, [0])),
    assert(any (fn x => (x mod 25) = 0, [1,2,25,8])),
    assert(any (fn x => (x mod 20) = 5, [1,2,3,4,6,7,25]))
]);

test("test-all", [
    assert(all (fn x => x>5, [])),
	assert_false(all (fn x => x>5, [3])),
    assert_false(all (fn x => x<10, [1,2,3,4,5,6,7,8,11])),
	assert(all (fn x => (x mod 25) = 0, [0])),
    assert(all (fn x => (x mod 25) = 0, [100,25,50,75]))
]);
*)