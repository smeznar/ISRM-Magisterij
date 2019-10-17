exception NaturalNumbersOnlyException;
exception IndexToBigException;

(*  Vrne fakulteto števila n, n >= 0. *)
fun factorial (n: int): int = 
    if n < 0
    then raise NaturalNumbersOnlyException
    else
        if n = 0 
        then 1
        else n * factorial(n-1);

(*  Vrne n-to potenco števila x, n >= 0. *)
fun power (x: int, n: int): int = 
    if n < 0
    then raise NaturalNumbersOnlyException
    else
        if n = 0 
        then 1
        else x * power(x, (n-1));

(*  Vrne največjega skupnega delitelja
    pozitivnih števil a in b, a >= b. *)
fun gcd (a: int, b: int): int = 
    if b = 0
    then (abs a)
    else gcd(b, (a mod b));

(*  Vrne dolžino seznama. *)
fun len (xs: int list): int = 
    if (null xs)
    then 0
    else 1 + len(tl xs) ;

(*  Vrne SOME zadnji element seznama.
    Če je seznam prazen vrne NONE. *)
fun last (xs: int list): int option = 
    if null xs
    then NONE
    else
        let
            val remaining = last(tl xs)
        in
            case remaining of
               NONE => SOME (hd xs)
             | _ => remaining
        end;

(*  Vrne SOME n-ti element seznama.
    Prvi element ima indeks 0.
    Če indeks ni veljaven, vrne NONE. *)
fun nth (xs: int list, n: int): int option = 
    if n < 0
    then NONE
    else
        if null xs
        then NONE
        else
            if n=0
            then SOME (hd xs)
            else nth ((tl xs), (n-1));

(*  Vrne nov seznam, ki je tak kot vhodni,
    le da je na n-to mesto vrinjen element x.
    Prvo mesto v seznamu ima indeks 0.
    Indeks n je veljaven (0 <= n <= length xs). *)
fun insert (xs: int list, n: int, x: int): int list = 
    if n < 0
    then raise NaturalNumbersOnlyException 
    else
        if null xs
        then 
            if n = 0
            then [x]
            else raise IndexToBigException
        else
            if n = 0
            then x::xs
            else (hd xs) :: insert((tl xs), (n-1), x);

(*  Vrne nov seznam, ki je tak kot vhodni,
    le da so vse pojavitve elementa x odstranjene. *)
fun delete (xs: int list, x: int): int list = 
    if null xs
    then []
    else 
        if (hd xs) = x
        then delete((tl xs), x)
        else (hd xs) :: delete((tl xs), x);

(*  Vrne obrnjen seznam. V pomoč si lahko spišete
    še funkcijo append, ki doda na konec seznama. *)
fun reverse (xs: int list): int list =
    case xs of
       nil => []
     | _ => (reverse(tl xs))@([hd xs]);

(*  Vrne true, če je podani seznam palindrom.
    Tudi prazen seznam je palindrom. *)
fun palindrome (xs: int list): bool = 
    let
        val r = reverse(xs)
        fun check (s1: int list) (s2: int list) = 
            if (null s1) orelse (null s2)
            then true
            else
                if (hd s1) = (hd s2)
                then check (tl s1) (tl s2)
                else false
    in
        check xs r
    end;

(* TESTS *)
(*
use "unittest.sml";

test("tests-factorial", [
    assert_eq(factorial(0),1),
	assert_eq(factorial(1),1),
	assert_eq(factorial(5),120),
	assert_raises((fn () => (factorial(~1))), NaturalNumbersOnlyException)
]);

test("tests-power", [
	assert_eq(power(546, 0),1),
    assert_eq(power(2,8),256),
    assert_eq(power(889,1),889),
    assert_eq(power(3,4),81),
	assert_raises((fn() => (power(648,~1))), NaturalNumbersOnlyException)
]);

test("tests-gcd", [
	assert_eq(gcd(1,9283467),1),
	assert_eq(gcd(1024,2048),1024),
	assert_eq(gcd(18,~12),6),
    assert_eq(gcd(930, 124),62)
]);

test("tests-len", [
	assert_eq(len([]),0),
	assert_eq(len([1,2,3]),3),
	assert_eq(len([1]),1)
]);

test("tests-last", [
	assert(not (Option.isSome(last([])))),
	assert_eq(Option.valOf(last([1])), 1),
	assert_eq(Option.valOf(last([1,2,3,4,5])), 5)
]);

test("tests-nth", [
	assert(not (Option.isSome(nth([], 0)))),
    assert(not (Option.isSome(nth([0,1,2,3,4], 5)))),
    assert(not (Option.isSome(nth([0,1,2,3,4], ~1)))),
	assert_eq(Option.valOf(nth([1], 0)), 1),
	assert_eq(Option.valOf(nth([1,2,3,4,5], 3)), 4),
    assert_eq(Option.valOf(nth([1,2,3,4,5], 4)), 5)
]);

test("tests-insert", [
	assert_eq(insert ([], 0, 1),[1]),
	assert_eq(insert ([0,1,2], 3, 3),[0,1,2,3]),
    assert_eq(insert ([0,1,3,4], 2, 2),[0,1,2,3,4]),
    assert_eq(insert ([1,2,3], 0, 0),[0,1,2,3]),
	assert_raises((fn() => (insert([0,1,2], ~1, 8))), NaturalNumbersOnlyException),
    assert_raises((fn() => (insert([0,1,2], 4, 7))), IndexToBigException)
]);

test("tests-delete", [
	assert_eq(delete([],5),[]),
	assert_eq(delete([1,2,3],4),[1,2,3]),
	assert_eq(delete([1,2,3],2),[1,3]),
    assert_eq(delete([1,2,2,3,2],2),[1,3])
]);

test("tests-reverse", [
	assert_eq(reverse([]),[]),
	assert_eq(reverse([0]),[0]),
	assert_eq(reverse([1,2,3,4]),[4,3,2,1])
]);

test("tests-palindrome", [
	assert(palindrome([])),
	assert(not (palindrome([0,1,2,3]))),
	assert(palindrome([0,1,2,3,2,1,0])),
    assert(palindrome([0,1,1,0])),
    assert(palindrome([1]))
]);

val _ = OS.Process.exit(OS.Process.success);*)