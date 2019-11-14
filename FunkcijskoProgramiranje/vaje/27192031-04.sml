(* Podan seznam xs agregira z začetno vrednostjo z
   in funkcijo f v vrednost f(f(f(z, xs_1),  xs_2), xs_3) ... *)
(* Aggregates xs with an initial value z and function f
   and returns f(f(f(z, xs_1),  xs_2), xs_3) ... *)
fun reduce (f: 'a * 'b -> 'a, z: 'a, []: 'b list): 'a = z
    | reduce (f, z, (x::xs)) = reduce (f, f (z, x), xs);

(* Vrne seznam, ki vsebuje kvadrate števil
   iz vhodnega seznama. Uporabite List.map. *)
(* Returns a list of squares of the numbers.
   Use List.map. *)
fun squares (xs: int list): int list = List.map (fn x => x*x) xs;

(* Vrne seznam, ki vsebuje vsa soda števila
   iz vhodnega seznama. Uporabite List.filter. *)
(* Returns a list that contains only even numbers from xs.
   Use List.filter. *)
fun onlyEven (xs: int list): int list  = List.filter (fn x => (x mod 2) = 0) xs;

(* Vrne najboljši niz glede na funkcijo f. Funkcija f primerja dva niza
   in vrne true, če je prvi niz boljši od drugega. Uporabite List.foldl.
   Najboljši niz v praznem seznamu je prazen niz. *)
(* Returns the best string according to the function f.
   The function f compares two strings and returns true
   if the first string is better than the other. Use List.foldl.
   The best string in an empty list is an empty string. *)
fun bestString (f: string * string -> bool, xs: string list): string = List.foldl (fn (x,y) => if f(x,y) then x else y) "" xs;

(* Vrne leksikografsko največji niz. Uporabite bestString. *)
(* Returns the largest string according to alphabetical ordering.
   Use bestString. *)
fun largestString (xs: string list): string = bestString ((fn (x,y) => case String.compare (x, y) of GREATER => true | _ => false), xs);


(* Vrne najdaljši niz. Uporabite bestString. *)
(* Returns the longest string. Use bestString. *)
fun longestString (xs: string list): string = bestString ((fn (x,y) => (String.size x) > (String.size y)), xs);

(* Seznam uredi naraščajoče z algoritmom quicksort. *)
(* Sorts the list with quicksort. *)
fun quicksort ([]: int list): int list = []
    | quicksort (x::xs) = (quicksort (List.filter (fn z => z<x) xs)) @ (List.filter (fn z => z=x) (x::xs)) @(quicksort (List.filter (fn z => z>x) xs));

(* Vrne skalarni produkt dveh vektorjev.
   Uporabite List.foldl in ListPair.map. *)
(* Returns the scalar product of two vectors.
   Use List.foldl and ListPair.map. *)
fun dot (xs: int list, ys: int list): int = foldr (fn (x,y) => x+y) 0 (ListPair.map (fn (x,y)=> x*y) (xs, ys));

(* Vrne transponirano matriko. Matrika je podana z vrstičnimi vektorji
   od zgoraj navzdol: [[1,2,3],[4,5,6],[7,8,9]] predstavlja matriko
   [ 1 2 3 ]
   [ 4 5 6 ]
   [ 7 8 9 ]
   *)
(* Returns the transpose of m. The matrix m is given with row vectors
   from top to bottom: [[1,2,3],[4,5,6],[7,8,9]] represents the matrix
   [ 1 2 3 ]
   [ 4 5 6 ]
   [ 7 8 9 ]
   *)
fun transpose (m: 'a list list): 'a list list = 
    case (hd m) of
    [] => []
    | _ => (map (fn ys => hd ys) m)::(transpose (map (fn ys => tl ys) m));

(* Zmnoži dve matriki. Uporabite dot in transpose. *)
(* Multiplies two matrices. Use dot and transpose. *)
fun multiply ([]: int list list, b: int list list): int list list = []
    | multiply ((x::xs), ys) = (map (fn z => dot (x,z)) (transpose(ys)))::(multiply(xs,ys));

(* V podanem seznamu prešteje zaporedne enake elemente in vrne seznam
   parov (vrednost, število ponovitev). Podobno deluje UNIX-ovo orodje
   uniq -c. *)
(* Counts successive equal elements and returns a list of pairs (value, count).
   The unix tool uniq -c works similarly. *)
fun group (xs: ''a list): (''a * int) list = foldr (fn (x, zs) => case zs of [] => [(x, 1)] | ((y,num)::ys) => if x = y then (y, (num+1))::ys else (x,1)::(y,num)::ys) [] xs;

(* Elemente iz podanega seznama razvrsti v ekvivalenčne razrede.
   Znotraj razredov naj bodo elementi v istem vrstnem redu kot v
   podanem seznamu. Ekvivalentnost elementov definira funkcija f,
   ki za dva elementa vrne true, če sta ekvivalentna. *)
(* Sorts the elements from a list into equivalence classes.
   The order of elements inside each equivalence class should be the same as in
   the original list. The equivalence relation is given with a function f,
   which returns true, if two elements are equivalent. *)
fun equivalenceClasses (f: ''a * ''a -> bool, []: ''a list): ''a list list = []
    | equivalenceClasses (f, (x::xs)) = (List.filter (fn y => f(y,x)) (x::xs))::(equivalenceClasses(f, List.filter (fn y => not (f(y,x))) (x::xs)));
(* TESTI *)
(*
use "unittest.sml";

test("test-reduce", [
    assert_equal(reduce, (op+,0,[1,2,3,4,5,6,7,8,9,10]), 55),
    assert_equal(reduce, (op-,0,[1,2,3,4,5,6,7,8,9,10]), ~55),
    assert_equal(reduce, ((fn (s,n) => s^Int.toString(n)),"",[1,2,3,4,5,6,7,8,9,10]), "12345678910"),
    assert_equal(reduce, (op+,0,[]), 0),
    assert_equal(reduce, (op+,0,[1]), 1)
]);

test("test-squares", [
    assert_equal(squares,[], []),
    assert_equal(squares,[1], [1]),
    assert_equal(squares,[2], [4]),
    assert_equal(squares,[1,2,3,4,5], [1,4,9,16,25])
]);

test("test-onlyEven", [
    assert_equal(onlyEven,[], []),
    assert_equal(onlyEven,[1], []),
    assert_equal(onlyEven,[2], [2]),
    assert_equal(onlyEven,[1,2,3,4,5], [2,4])
]);

test("test-largestString", [
    assert_equal(largestString,[], ""),
    assert_equal(largestString,["a"], "a"),
    assert_equal(largestString,["aa", "bb"], "bb")
]);

test("test-longestString", [
    assert_equal(longestString,[], ""),
    assert_equal(longestString,["a"], "a"),
    assert_equal(longestString,["aaa","cc", "bbbbb", "dd"], "bbbbb")
]);

test("test-quicksort", [
    assert_equal(quicksort,[], []),
    assert_equal(quicksort,[1], [1]),
    assert_equal(quicksort,[8,7,3,5,4,1,2,9,6], [1,2,3,4,5,6,7,8,9]),
    assert_eq(quicksort [2,3,4,6,9,1,5,8,2,2,5,4], [1,2,2,2,3,4,4,5,5,6,8,9])
]);

test("test-dot", [
    assert_equal(dot,([],[]), 0),
    assert_equal(dot,([1],[1]), 1),
    assert_equal(dot,([1,2,3],[4,5,6]), 32)
]);

test("test-transpose", [
    assert_equal(transpose,[[1]], [[1]]),
    assert_equal(transpose,[[1,2,3],[4,5,6]], [[1,4],[2,5],[3,6]]),
    assert_equal(transpose,[[1,2,3],[4,5,6],[7,8,9]], [[1,4,7],[2,5,8],[3,6,9]])
]);

test("test-multiply", [
    assert_equal(multiply,([[1]],[[1]]), [[1]]),
    assert_equal(multiply,([[1,2,3]],[[4],[5],[6]]),[[32]]),
    assert_equal(multiply,([[0,1],[1,0]],[[4,1],[2,2]]),[[2,2],[4,1]])
]);

test("test-group", [
    assert_equal(group,[], []),
    assert_equal(group,[1],[(1,1)]),
    assert_equal(group,[1,1,2,2,3,4,2,1,1],[(1,2),(2,2),(3,1),(4,1),(2,1),(1,2)]),
    assert_equal(group,["a","a","a","b","c","c","a","a","b"],[("a",3),("b",1),("c",2),("a",2),("b",1)])
]);

test("test-equivalenceClasses", [
    assert_equal(equivalenceClasses,(op=,[]), []),
    assert_equal(equivalenceClasses,(op=,[1,2,3,4,1,2,1,1,2,3]), [[1,1,1,1],[2,2,2],[3,3],[4]]),
    assert_equal(equivalenceClasses,(fn (x,y) => ((x+y)mod 2) = 0,[1,2,3,4,5,6,7]), [[1,3,5,7],[2,4,6]])
]);
*)