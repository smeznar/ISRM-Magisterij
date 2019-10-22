type power = {base: int, exponent: int};

fun potenca (z: power) = 
    if #exponent z = 0
    then 1
    else (#base z) * potenca {base = (#base z), exponent = (#exponent z) - 1};

fun pow {base = x, exponent = 0} = 1
    | pow {base = x, exponent = n} = pow {base = x, exponent = n-1} * x;

(* Kart za remi *)
datatype suit = SPADE | HEART | CLUB | DIAMOND;
datatype rank = ACE | KING | QUEEN | JACK | NUMBER of int;

type card = suit * rank;

datatype color = RED | BLACK;

fun isValid (c: card) = 
    case c of 
        (_, NUMBER n) => n >= 2 andalso n <= 10
        | _ => true;
    
fun cardValue ((_, r): card) =
    case r of
    NUMBER n => n
    | ACE => 11
    | _ => 10;

fun cardColor ((s, _): card) =
    case s of
        (SPADE | CLUB) => BLACK
        | _ => RED;

fun cardColor2 (((SPADE | CLUB), _): card) = BLACK
    | cardColor2 ((_ , _): card) = RED;

fun sumCards cs = 
    if null cs
    then 0
    else cardValue (hd cs) + sumCards (tl cs);

fun sumCards2 nil = 0
    | sumCards2 (c::cs) = cardValue c + sumCards2 cs;

fun increasing xs = 
    case xs of
        h1::h2::tail => h1 <= h2 andalso increasing (h2::tail)
        | _ => true;

fun allSameColor (c1::c2::tail) = (cardColor2 c1) = (cardColor2 c2) andalso allSameColor (c2::tail)
    | allSameColor _ = true;  