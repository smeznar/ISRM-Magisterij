
fun izloci sez = 
    case sez of 
    [] => [] 
    | x::xs => (x,xs):: (List.map (fn (a,b) => (a, x::b)) (izloci xs));

fun f ({g=g,h=h}, [i,j]) k =
    if valOf i 
    then fn x => g (k+x) 
    else fn x => h (k-x) ^ "nil"

fun a x = x+1;

datatype ('a, 'b) node = NIL
                        | Node of ('b, 'a) node * 'a * ('b, 'a) node

val a = Node (NIL, 1, Node (NIL, true, Node (NIL, 2, NIL)))

fun b [] [] [] = []
    | b (x1::xs1) (x2::xs2) (x3::xs3) =
    let 
        fun num x = if x=1 then 1 else 0
        val el = (b xs1 xs2 xs3)
    in
        case ((num x1) + (num x2) + (num x3)) of
        0 => 0::el
        | 3 => 111::el
        | _ => 1::el
    end