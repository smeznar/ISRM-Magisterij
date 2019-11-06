fun f x = (hd x)+1 handle Empty => ~222;

exception Ex of int;

fun head nil = raise Ex ~1
    | head (x::_) = x;

fun sth xs = 
    let
        fun helper xs = head xs handle Ex x => x | _ => ~1000
    in
      helper xs
    end