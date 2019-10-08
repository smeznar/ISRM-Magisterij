let rec power x n =
    if n <= 0 then 1 else x * power x (n-1);;

let a = power 2 6;;

print_int a;;