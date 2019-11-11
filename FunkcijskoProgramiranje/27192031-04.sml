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
fun bestString (f: string * string -> bool, xs: string list): string = 
    let
        fun strCmp (x,y) =
        if f (x,y) 
        then x
        else y 
    in
        List.foldl strCmp "" xs
    end ;

(* Vrne leksikografsko največji niz. Uporabite bestString. *)
(* Returns the largest string according to alphabetical ordering.
   Use bestString. *)
fun largestString (xs: string list): string = 
let
    fun cmpStr (x,y) = 
        case String.compare (x, y) of
        LESS => false
        | EQUAL => false
        | GREATER => true
in
  bestString (cmpStr, xs)
end ;


(* Vrne najdaljši niz. Uporabite bestString. *)
(* Returns the longest string. Use bestString. *)
fun longestString (xs: string list): string = 
let
    fun cmpStr (x,y) = (String.size x) > (String.size y)
in
  bestString (cmpStr, xs)
end ;

(* Seznam uredi naraščajoče z algoritmom quicksort. *)
(* Sorts the list with quicksort. *)
fun quicksort (xs: int list): int list = [];

(* Vrne skalarni produkt dveh vektorjev.
   Uporabite List.foldl in ListPair.map. *)
(* Returns the scalar product of two vectors.
   Use List.foldl and ListPair.map. *)
fun dot (xs: int list, ys: int list): int = 0;

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
fun transpose (m: 'a list list): 'a list list = [[]];

(* Zmnoži dve matriki. Uporabite dot in transpose. *)
(* Multiplies two matrices. Use dot and transpose. *)
fun multiply (a: int list list, b: int list list): int list list = [[]];

(* V podanem seznamu prešteje zaporedne enake elemente in vrne seznam
   parov (vrednost, število ponovitev). Podobno deluje UNIX-ovo orodje
   uniq -c. *)
(* Counts successive equal elements and returns a list of pairs (value, count).
   The unix tool uniq -c works similarly. *)
fun group (xs: ''a list): (''a * int) list = [];

(* Elemente iz podanega seznama razvrsti v ekvivalenčne razrede.
   Znotraj razredov naj bodo elementi v istem vrstnem redu kot v
   podanem seznamu. Ekvivalentnost elementov definira funkcija f,
   ki za dva elementa vrne true, če sta ekvivalentna. *)
(* Sorts the elements from a list into equivalence classes.
   The order of elements inside each equivalence class should be the same as in
   the original list. The equivalence relation is given with a function f,
   which returns true, if two elements are equivalent. *)
fun equivalenceClasses (f: ''a * ''a -> bool, xs: ''a list): ''a list list = [[]];