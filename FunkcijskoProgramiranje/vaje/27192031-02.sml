(* Naravna števila definiramo tako:
 * - Obstaja ničla (ZERO).
 * - Vsako naravno število ima svojega naslednika (NEXT).
 * - Ničla ni naslednik nobenega naravnega števila.
 * - Če sta dve naravni števili enaki, potem sta enaka tudi njuna naslednika.
 *)
datatype natural = NEXT of natural | ZERO;

(* Binarno drevo celih števil definiramo tako:
 * - Obstaja vozlišče (NODE), ki ima poleg vrednosti tudi dve poddrevesi.
 * - Obstaja list (LEAF), ki ima le vrednost.
 *)
datatype tree = NODE of int * tree * tree | LEAF of int;

(* Vrne celoštevilsko vrednost (int) naravnega števila. *)
fun toInt (a: natural): int =
    case a of
        ZERO => 0
        | NEXT n => 1 + toInt(n);

(* Vrne vsoto naravnih števil a in b. Pretvorba v int ni dovoljena! *)
fun add (a: natural, ZERO): natural = a
    | add (a: natural, NEXT n) = NEXT(add (a, n)); 

(* Vrne najmanjši element drevesa, če obstaja. *)
fun min (LEAF n): int option = SOME n
    | min (NODE (n,t1,t2)) : int option = 
    let 
        fun trimin a b c =
        Int.min(a, Int.min(Option.valOf(b),Option.valOf(c)))
    in
        SOME (trimin n (min t1) (min t2))
    end;

(* Vrne največji element drevesa, če obstaja. *)
fun max (LEAF n): int option = SOME n
    | max (NODE (n,t1,t2)) : int option = 
    let 
        fun trimax a b c =
        Int.max(a, Int.max(Option.valOf(b),Option.valOf(c)))
    in
        SOME (trimax n (max t1) (max t2))
    end;

(* Vrne true, če drevo vsebuje element x. *)
fun contains (tree: tree, x: int): bool = 
    case tree of 
    LEAF n => n = x
    | NODE (n,t1,t2) => contains(t1, x) orelse contains(t2, x) orelse n=x;

(* Vrne število listov v drevesu. *)
fun countLeaves (tree: tree): int = 
    case tree of
    LEAF _ => 1
    | NODE (_, t1, t2) => countLeaves(t1)+countLeaves(t2);

(* Vrne število število vej v drevesu. *)
fun countBranches (tree: tree): int = 
    case tree of
    LEAF _ => 0
    | NODE (_, t1, t2) => 2 + countBranches(t1)+countBranches(t2);

(* Vrne višino drevesa. Višina lista je 1. *)
fun height (tree: tree): int = 
    case tree of 
    LEAF _ => 1
    | NODE (_, t1, t2) => 1 + Int.max(height t1, height t2);

(* Pretvori drevo v seznam z vmesnim prehodom (in-order traversal). *)
fun toList (tree: tree): int list = 
    case tree of 
    LEAF n => [n]
    | NODE (n, t1, t2) => toList(t1) @ (n::(toList(t2)));

(* Vrne true, če je drevo uravnoteženo:
 * - Obe poddrevesi sta uravnoteženi.
 * - Višini poddreves se razlikujeta kvečjemu za 1.
 * - Listi so uravnoteženi po definiciji.
 *)
fun isBalanced (tree: tree): bool = 
    let 
        fun isTreeBalanced tree =
            case tree of
            LEAF _ => (true, 1)
            | NODE (_, t1, t2) => 
                let
                    val tt1 = (isTreeBalanced t1)
                    val tt2 = (isTreeBalanced t2)
                in 
                    ((#1 tt1) andalso (#1 tt2) andalso Int.abs((#2 tt1)-(#2 tt2))<2, 1 + Int.max((#2 tt1),(#2 tt2)))
                end
    in
        #1 (isTreeBalanced tree)
    end; 

(* Vrne true, če je drevo binarno iskalno drevo:
 * - Vrednosti levega poddrevesa so strogo manjši od vrednosti vozlišča.
 * - Vrednosti desnega poddrevesa so strogo večji od vrednosti vozlišča.
 * - Obe poddrevesi sta binarni iskalni drevesi.
 * - Listi so binarna iskalna drevesa po definiciji.
 *)
fun isBST (tree: tree): bool = 
    case tree of
    LEAF _ => true
    | NODE (n, t1, t2) => Option.valOf(max(t1))<n andalso Option.valOf(min(t2))>n andalso isBST(t1) andalso isBST(t2);

(* TESTS *)
(*
use "unittest.sml";

test("tests-toInt", [
    assert_eq(toInt(ZERO),0),
	assert_eq(toInt(NEXT(ZERO)),1),
	assert_eq(toInt(NEXT(NEXT(NEXT(NEXT(NEXT(ZERO)))))),5)
]);

test("tests-add", [
	assert_eq(add(ZERO, ZERO),ZERO),
    assert_eq(add(NEXT(ZERO), ZERO),NEXT(ZERO)),
    assert_eq(add(ZERO, NEXT(ZERO)),NEXT(ZERO)),
    assert_eq(add(NEXT(ZERO), NEXT(ZERO)),NEXT(NEXT(ZERO))),
    assert_eq(add(NEXT(NEXT(NEXT(ZERO))), NEXT(NEXT(ZERO))),NEXT(NEXT(NEXT(NEXT(NEXT(ZERO))))))
]);

test("tests-min", [
	assert_eq(Option.valOf(min(LEAF 1)),1),
	assert_eq(Option.valOf(min(NODE(2, LEAF 1, LEAF 2))),1),
    assert_eq(Option.valOf(min(NODE(2, LEAF 2, LEAF 1))),1),
    assert_eq(Option.valOf(min(NODE(1, LEAF 2, LEAF 2))),1),
    assert_eq(Option.valOf(min(NODE(2, NODE (2, LEAF 0, LEAF 1), LEAF 2))),0),
    assert_eq(Option.valOf(min(NODE(2, LEAF 1, NODE(0, LEAF 1, LEAF 1)))),0)
]);

test("tests-max", [
	assert_eq(Option.valOf(max(LEAF 1)),1),
	assert_eq(Option.valOf(max(NODE(2, LEAF 1, LEAF 1))),2),
    assert_eq(Option.valOf(max(NODE(1, LEAF 2, LEAF 1))),2),
    assert_eq(Option.valOf(max(NODE(1, LEAF 1, LEAF 2))),2),
    assert_eq(Option.valOf(max(NODE(2, NODE (2, LEAF 5, LEAF 1), LEAF 2))),5),
    assert_eq(Option.valOf(max(NODE(2, LEAF 1, NODE(5, LEAF 1, LEAF 1)))),5)
]);

test("tests-contains", [
	assert(contains(LEAF 1, 1)),
    assert(not (contains(LEAF 1, 2))),
	assert(contains(NODE(2, LEAF 1, LEAF 1),2)),
    assert(not (contains(NODE(2, LEAF 1, LEAF 1),3))),
    assert(contains(NODE(1, LEAF 2, LEAF 2),2)),
    assert(contains(NODE(1, LEAF 1, LEAF 2),2)),
    assert(contains(NODE(2, NODE (2, LEAF 5, LEAF 1), LEAF 2), 5)),
    assert(not (contains(NODE(2, NODE (2, LEAF 3, LEAF 1), LEAF 2), 5))),
    assert(contains(NODE(2, LEAF 2, NODE (2, LEAF 5, LEAF 1)), 5))
]);

test("tests-countLeaves", [
	assert_eq(countLeaves(LEAF 1),1),
	assert_eq(countLeaves(NODE(2, LEAF 1, LEAF 1)),2),
    assert_eq(countLeaves(NODE(2, NODE (2, LEAF 5, LEAF 1), LEAF 2)),3),
    assert_eq(countLeaves(NODE(2, NODE(1, LEAF 1, LEAF 1), NODE(5, LEAF 1, LEAF 1))),4)
]);

test("tests-countBranches", [
	assert_eq(countBranches(LEAF 1),0),
	assert_eq(countBranches(NODE(2, LEAF 1, LEAF 1)),2),
    assert_eq(countBranches(NODE(2, NODE (2, LEAF 5, LEAF 1), LEAF 2)),4),
    assert_eq(countBranches(NODE(2, NODE(1, LEAF 1, LEAF 1), NODE(5, LEAF 1, LEAF 1))),6),
    assert_eq(countBranches(NODE(2, NODE(1, NODE (1, LEAF 1, LEAF 1), LEAF 1), NODE(5, LEAF 1, LEAF 1))),8)
]);

test("tests-height", [
	assert_eq(height(LEAF 1),1),
	assert_eq(height(NODE(2, LEAF 1, LEAF 1)),2),
    assert_eq(height(NODE(2, NODE (2, LEAF 5, LEAF 1), LEAF 2)),3),
    assert_eq(height(NODE(2, NODE(1, LEAF 1, LEAF 1), NODE(5, LEAF 1, LEAF 1))),3),
    assert_eq(height(NODE(2, NODE(1, NODE (1, LEAF 1, LEAF 1), LEAF 1), NODE(5, LEAF 1, LEAF 1))),4)
]);

test("tests-toList", [
	assert_eq(toList(LEAF 1),[1]),
	assert_eq(toList(NODE(2, LEAF 1, LEAF 3)),[1,2,3]),
    assert_eq(toList(NODE(4, NODE (2, LEAF 1, LEAF 3), LEAF 5)),[1,2,3,4,5]),
    assert_eq(toList(NODE(4, NODE(2, LEAF 1, LEAF 3), NODE(6, LEAF 5, LEAF 7))),[1,2,3,4,5,6,7]),
    assert_eq(toList(NODE(6, NODE(4, NODE (2, LEAF 1, LEAF 3), LEAF 5), NODE(8, LEAF 7, LEAF 9))),[1,2,3,4,5,6,7,8,9]),
    assert_eq(toList(NODE(2, LEAF 1, NODE(4, LEAF 3, NODE(6, LEAF 5, NODE (8, LEAF 7, LEAF 9))))),[1,2,3,4,5,6,7,8,9])
]);

test("tests-isBalanced", [
	assert(isBalanced(LEAF 1)),
	assert(isBalanced(NODE(2, LEAF 1, LEAF 3))),
    assert(isBalanced(NODE(4, NODE (2, LEAF 1, LEAF 3), LEAF 5))),
    assert(isBalanced(NODE(4, NODE(2, LEAF 1, LEAF 3), NODE(6, LEAF 5, LEAF 7)))),
    assert(not (isBalanced(NODE(6, NODE(4, NODE (2, LEAF 1, LEAF 3), LEAF 5), LEAF 3)))),
    assert(not (isBalanced(NODE(6, NODE(4, NODE (2, LEAF 1, NODE (3, LEAF 1, LEAF 1)), LEAF 5), NODE (3, NODE (1, LEAF 2, LEAF 1), LEAF 1))))),
    assert(not (isBalanced(NODE(2, LEAF 1, NODE(4, LEAF 3, NODE(6, LEAF 5, NODE (8, LEAF 7, LEAF 9)))))))
]);

test("tests-isBST", [
	assert(isBST(LEAF 1)),
    assert(isBST(NODE(2, LEAF 1, LEAF 3))),
	assert(not (isBST(NODE(2, LEAF 2, LEAF 3)))),
    assert(isBST(NODE(4, NODE (2, LEAF 1, LEAF 3), LEAF 5))),
    assert(isBST(NODE(4, NODE(2, LEAF 1, LEAF 3), NODE(6, LEAF 5, LEAF 7)))),
    assert(not (isBST(NODE(6, NODE(4, NODE (2, LEAF 1, LEAF 3), LEAF 5), LEAF 3)))),
    assert(not (isBST(NODE(8, NODE(6, NODE (2, LEAF 1, NODE (4, LEAF 3, LEAF 5)), LEAF 7), NODE (11, NODE (10, LEAF 9, LEAF 9), LEAF 12))))),
    assert(not (isBST(NODE(2, LEAF 1, NODE(4, LEAF 3, NODE(6, LEAF 5, NODE (8, LEAF 10, LEAF 9)))))))
]);

val _ = OS.Process.exit(OS.Process.success);
*)