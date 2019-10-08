fun sqr(x) = x*x;

fun pow(x,p) =
    if p=1 
    then x
    else x* pow(x, p-1);