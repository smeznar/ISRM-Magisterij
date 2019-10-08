fun sestej_pare (sez: (int*int) list) =
    if null sez
    then []
    else ( #1 (hd sez) + #2 (hd sez)) :: sestej_pare(tl sez)

fun pozitivni_predmeti (sez: (string*int) list) =
    if null sez
    then []
    else if #2 (hd sez) <= 5
        then pozitivni_predmeti (tl sez)
        else (#1 (hd sez)) :: pozitivni_predmeti (tl sez)

val test1 = sestej_pare([(1,2),(2,3),(6,4),(4,5),(9,293)]);

val test2 = pozitivni_predmeti([("mat",10),("and",5),("a",2),("nekej",9),("zadnje",7)]);