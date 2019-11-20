(* Implementirajte sledeči programski vmesnik za delo z
 * racionalnimi števili. Vaša struktura naj se imenuje
 * Rational in naj definira podatkovni tip (datatype) rational,
 * ki je lahko bodisi celo število (Whole) bodisi ulomek (Frac).
 *
 * Ulomki naj sledijo naslednjim pravilom:
 *   1. Imenovalec je vedno strogo večji od 1
 *   2. Vsak ulomek je okrajšan
 *
 * V oddani datoteki implementirajte le strukturo Rational :> RATIONAL!
 * Z drugimi besedami, vaša datoteka naj ne vsebuje podpisa RATIONAL.
 *)
(*
signature RATIONAL =
sig
    (* Definirajte podatkovni tip rational,
       ki podpira preverjanje enakosti. *)
    eqtype rational

    (* Definirajte izjemo, ki se kliče pri
       delu z neveljavnimi ulomki. *)
    exception BadRational

    (* Vrne racionalno število, ki je rezultat
       deljenja dveh podanih celih števil. *)
    val makeRational: int * int -> rational

    (* Vrne nasprotno vrednost podanega števila. *)
    val neg: rational -> rational

    (* Vrne obratno vrednost podanega števila. *)
    val inv: rational -> rational

    (* Funkcije za seštevanje, odštevanje, množenje in deljenje.
       Rezultat vsake operacije naj sledi postavljenim pravilom. *)
    val add: rational * rational -> rational
    val sub: rational * rational -> rational
    val mul: rational * rational -> rational
    val dvd: rational * rational -> rational

    (* Vrne niz, ki ustreza podanemu številu.
       Če je število celo, naj vrne niz oblike "x" oz. "~x".
       Če je število ulomek, naj vrne niz oblike "x/y" oz. "~x/y". *)
    val toString: rational -> string
end
*)
structure Rational :> RATIONAL =
struct
    datatype rational = 
        Whole of int
        | Frac of int*int ;

    exception BadRational ;

    fun gcd a b = 
        if b = 0
        then a
        else gcd b (a mod b);

    fun makeRational (_, 0) = raise BadRational
        | makeRational (a, b) =
            case a mod b of
            0 => Whole (a div b)
            | _ => 
                let
                    val g = gcd a b
                in
                    Frac (a div g, b div g)
                end ;

    fun add (Whole a,Whole b) = Whole (a+b)
        | add (Whole a, Frac (b,c)) = makeRational ((a*c)+b, c)
        | add (Frac (b,c), Whole a) = makeRational ((a*c)+b, c)
        | add (Frac (a1, a2), Frac (b1,b2)) = makeRational ((a1*b2)+(b1*a2), a2*b2);

    fun sub (Whole a,Whole b) = Whole (a-b)
        | sub (Whole a, Frac (b,c)) = makeRational ((a*c)-b, c)
        | sub (Frac (b,c), Whole a) = makeRational (b-(a*c), c)
        | sub (Frac (a1, a2), Frac (b1,b2)) = makeRational ((a1*b2)-(b1*a2), a2*b2);

    fun mul (Whole a,Whole b) = Whole (a*b)
        | mul (Whole a, Frac (b,c)) = makeRational (a*b, c)
        | mul (Frac (b,c), Whole a) = makeRational (a*b, c)
        | mul (Frac (a1, a2), Frac (b1,b2)) = makeRational (a1*b1, a2*b2);

    fun dvd (Whole a,Whole b) = makeRational (a, b)
        | dvd (Whole a, Frac (b,c)) = makeRational (a*c, b)
        | dvd (Frac (b,c), Whole a) = makeRational (b, a*c)
        | dvd (Frac (a1, a2), Frac (b1,b2)) = makeRational (a1*b2, b1*a2);

    fun neg r = sub (Whole 0, r);

    fun inv r = dvd (Whole 1, r);

    fun toString (Whole a) =  Int.toString(a)
        | toString (Frac (a,b)) = (Int.toString(a))^"/"^(Int.toString(b));
    
end ;

(* TESTI *)

(*
val tv1 = Rational.makeRational(2,1);
val tv2 = Rational.makeRational(1,2);
val tv3 = Rational.makeRational(5,2);
val tv4 = Rational.makeRational(2,5);
val tv5 = Rational.makeRational(~2,1);
val tv6 = Rational.makeRational(~1,2);

use "unittest.sml";

test("test-makeRational+toString", [
    assert_equal(Rational.toString, Rational.makeRational(2,1), "2"),
    assert_equal(Rational.toString, Rational.makeRational(1,2), "1/2"),
    assert_equal(Rational.toString, Rational.makeRational(~1,1), "~1"),
    assert_equal(Rational.toString, Rational.makeRational(10,25), "2/5"),
    assert_equal(Rational.toString, Rational.makeRational(~1,~2), "1/2"),
    assert_equal(Rational.toString, Rational.makeRational(1100,10), "110"),
    assert_equal(Rational.toString, Rational.makeRational (1,~5), "~1/5"),
    assert_raises(fn () => Rational.makeRational (1, 0), Rational.BadRational)
]);

test("test-inv", [
    assert_equal(Rational.inv, tv1, tv2),
    assert_equal(Rational.inv, tv2, tv1),
    assert_equal(Rational.inv, tv3, tv4),
    assert_equal(Rational.inv, tv4, tv3),
    assert_equal(Rational.inv, tv5, tv6),
    assert_equal(Rational.inv, tv6, tv5),
    assert_raises(fn () => Rational.inv (Rational.makeRational(0,1)), Rational.BadRational)
]);

test("test-neg", [
    assert_equal(Rational.neg, tv1, tv5),
    assert_equal(Rational.neg, tv5, tv1),
    assert_equal(Rational.neg, tv2, tv6),
    assert_equal(Rational.neg, Rational.makeRational(0,1), Rational.makeRational(0,1))
]);

test("test-add", [
    assert_equal(Rational.add, (tv2, tv2), Rational.makeRational(1,1)),
    assert_equal(Rational.add, (tv1, tv1), Rational.makeRational(4,1)),
    assert_equal(Rational.add, (tv1, tv2), Rational.makeRational(5,2)),
    assert_equal(Rational.add, (tv1, tv5), Rational.makeRational(0,2))
]);

test("test-sub", [
    assert_equal(Rational.sub, (tv2, tv2), Rational.makeRational(0,1)),
    assert_equal(Rational.sub, (tv1, tv1), Rational.makeRational(0,1)),
    assert_equal(Rational.sub, (tv1, tv2), Rational.makeRational(3,2)),
    assert_equal(Rational.sub, (tv2, tv1), Rational.makeRational(~3,2)),
    assert_equal(Rational.sub, (tv1, tv5), Rational.makeRational(4,1))
]);

test("test-mul", [
    assert_equal(Rational.mul, (tv2, tv2), Rational.makeRational(1,4)),
    assert_equal(Rational.mul, (tv1, tv1), Rational.makeRational(4,1)),
    assert_equal(Rational.mul, (tv1, tv2), Rational.makeRational(1,1)),
    assert_equal(Rational.mul, (tv1, tv5), Rational.makeRational(~4,1))
]);

test("test-dvd", [
    assert_equal(Rational.dvd, (tv2, tv2), Rational.makeRational(1,1)),
    assert_equal(Rational.dvd, (tv1, tv1), Rational.makeRational(1,1)),
    assert_equal(Rational.dvd, (tv1, tv2), Rational.makeRational(4,1)),
    assert_equal(Rational.dvd, (tv2, tv1), Rational.makeRational(1,4)),
    assert_equal(Rational.dvd, (tv1, tv5), Rational.makeRational(~1,1)),
    assert_raises(fn () => Rational.dvd (tv1,Rational.makeRational(0,1)), Rational.BadRational)
]);
*)