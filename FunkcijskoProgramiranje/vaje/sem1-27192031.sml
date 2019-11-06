exception InvalidVariable;

datatype 'a expression = Not of 'a expression
                        | Or of 'a expression list
                        | And of 'a expression list
                        | Implies of 'a expression * 'a expression
                        | Equiv of 'a expression * 'a expression
                        | Variable of 'a
                        | True
                        | False;

fun eval (xs:(''a*bool) list) (f: ''a expression) = 
    let
        fun getValueFromList [] item = raise InvalidVariable
            | getValueFromList ((i:''a,b)::xs) (item:''a) = 
                if i = item
                then b
                else getValueFromList xs item
    in
        case f of
        True => true
        | False => false
        | Variable a => getValueFromList xs a
        | Not exp => not (eval xs exp)
        | Implies (a, b) => not (eval xs a) orelse (eval xs b)
        | Equiv (a, b) => (eval xs a) = (eval xs b)
        | Or es => 
            if null es
            then false
            else (eval xs (hd es)) orelse (eval xs (Or (tl es)))
        | And es => 
            if null es
            then true
            else (eval xs (hd es)) andalso (eval xs (And (tl es)))
    end

fun removeEmpty exp =
    case exp of
        And [] => True
        | And (x::[]) => removeEmpty x
        | And (l) => And (map removeEmpty l)
        | Or [] => False
        | Or (x::[]) => removeEmpty x
        | Or l => Or (map removeEmpty l)
        | False => False
        | True => True
        | Variable a => Variable a
        | Not a => Not (removeEmpty a)
        | Implies (a, b) => Implies (removeEmpty a, removeEmpty b)
        | Equiv (a, b) => Equiv (removeEmpty a, removeEmpty b);

fun removeConstants (Not e) = 
    let
        val removed = removeConstants e
    in
        case removed of
        True => False
        | False => True
        | _ => Not removed
    end
    | removeConstants (Equiv (s1, s2)) =
    let
        val sr1 = removeConstants s1
        val sr2 = removeConstants s2
    in
        case (sr1, sr2) of
        ((True, True) | (False, False)) => True
        | ((True, False) | (False, True)) => False
        | ((True, x) | (x, True)) => x
        | ((False, x) | (x, False)) => Not (x)
        | (x, y) => Equiv (x, y)
    end
    | removeConstants (Implies (s1, s2)) = 
    let
        val sr1 = removeConstants s1
    in
        if sr1 = False
        then True
        else
            let
                val sr2 = removeConstants s2
            in
                case (sr1,sr2) of 
                (True, e) => e
                | (e, False) => Not (e)
                | (e, True) => True
                | (e1, e2) => Implies (e1, e2)
            end
    end
    | removeConstants (And l) = 
    let
        fun aux (x, False) = False
        | aux (x, (And(l))) = 
        let
            val removed = removeConstants x
        in
            case removed of
            False => False
            | True => And l
            | _ => And (removed::l)
        end
        | aux (x, _) = x (* Unused statement for type exhaustion *)
    in
        if null l
        then True
        else 
            case (foldr aux (And []) l) of
            And [] => True
            | And (x::[]) => x
            | e => e
    end
    | removeConstants (Or l) =
    let
        fun aux (x, True) = True
        | aux (x, (Or(l))) = 
        let
            val removed = removeConstants x
        in
            case removed of
            True => True
            | False => Or l
            | _ => Or (removed::l)
        end
        | aux (x, _) = x (* Unused statement for type exhaustion *)
    in
        if null l
        then False
        else 
            case (foldr aux (Or []) l) of
            Or [] => False
            | Or (x::[]) => x
            | e => e
    end
    | removeConstants e = e;

fun pushNegations exp = 
    case exp of 
    Not True => False
    | Not False => True
    | Not (Not e) => pushNegations e
    | Not (And xs) => Or (map (fn x => pushNegations (Not x)) xs)
    | Not (Or xs) => And (map (fn x => pushNegations (Not x)) xs)
    | Not (Implies (a, b)) => And [pushNegations a, pushNegations (Not b)]
    | Not (Equiv (a, b)) => Equiv (pushNegations (Not a), pushNegations b)
    | Not e => Not (pushNegations e)
    | Or l => Or (map pushNegations l)
    | And l => And (map pushNegations l)
    | Equiv (a, b) => Equiv (pushNegations a, pushNegations b)
    | Implies (a, b) => Implies (pushNegations a, pushNegations b)
    | e => e;

fun removeVars (Implies (p, q)) = 
    let
        val r1 = removeVars p
        val r2 = removeVars q
    in
        case (r1, r2) of
        ((Variable a, Variable b) | (Not (Variable a), Not(Variable b))) => 
            if a = b 
            then True
            else Implies (r1,r2)
        | ((Variable a, Not (Variable b)) | (Not(Variable a), Variable b)) =>
            if a = b
            then r2
            else Implies (r1, r2)
        | (a, b) => Implies (a, b)
    end
    | removeVars (Equiv (p, q)) = 
    let
        val r1 = removeVars p
        val r2 = removeVars q
    in
        case (r1, r2) of
        ((Variable a, Variable b) | (Not (Variable a), Not(Variable b))) => 
            if a = b 
            then True
            else Equiv (r1,r2)
        | ((Variable a, Not (Variable b)) | (Not(Variable a), Variable b)) =>
            if a = b
            then False
            else Equiv (r1, r2)
        | (a, b) => Equiv (a, b)
    end
    | removeVars (And l) = 
    let
        val mapped = map removeVars l
        fun checkOne x [] = And []
        | checkOne x (y::ys) = 
        let
            val other = checkOne x ys
        in
            case other of
            And l => 
                (case (x,y) of
                ((Variable a, Variable b) | (Not (Variable a), Not (Variable b))) =>
                    if a = b
                    then And l
                    else And (y::l)
                | ((Not (Variable a), Variable b) | (Variable a, Not (Variable b))) => 
                    if a = b
                    then False
                    else And (y::l)
                | (a, b) => And (y::l))
            | e => e
        end
        fun checkAll [] = And []
        | checkAll (x::xs) = 
            case checkAll xs of
            And l => 
                (case checkOne x l of
                And ls => And (x::ls)
                | e => e)
            | e => e (* Unused statement for type exhaustion *)    
    in
      case checkAll mapped of
         And [] => True
       | e => e
    end
    | removeVars (Or l) = 
    let
        val mapped = map removeVars l
        fun checkOne x [] = Or []
        | checkOne x (y::ys) = 
        let
            val other = checkOne x ys
        in
            case other of
            Or l => 
                (case (x,y) of
                ((Variable a, Variable b) | (Not (Variable a), Not (Variable b))) =>
                    if a = b
                    then Or l
                    else Or (y::l)
                | ((Not (Variable a), Variable b) | (Variable a, Not (Variable b))) => 
                    if a = b
                    then True
                    else Or (y::l)
                | (a,b) => Or (y::l))
            | e => e
        end
        fun checkAll [] = Or []
        | checkAll (x::xs) = 
            case checkAll xs of
            Or l => 
                (case checkOne x l of
                Or ls => Or (x::ls)
                | e => e)
            | e => e (* Unused statement for type exhaustion *)        
    in
      case checkAll mapped of
         Or [] => False
       | e => e
    end
    | removeVars (Not p) = Not (removeVars p)
    | removeVars e = e;
    
fun simplify exp = removeEmpty (removeConstants (removeVars (pushNegations (exp))));
(* TESTS *)

use "unittest.sml";

val evaluateWith = eval [("a",true), ("a", false), ("b", false)];

test("test-eval", [
    assert(eval [] True),
	assert_false(evaluateWith False),
    assert(evaluateWith (Variable "a")),
    assert_false(evaluateWith (Variable "b")),
    assert_false(eval [(1, false)] (Variable 1)),
    assert_raises((fn () => (eval [(1,true)] (Variable 2))), InvalidVariable),
    assert(evaluateWith (Not(False))),
	assert_false(evaluateWith (Not (True))),
    assert(evaluateWith (Implies (False, False))),
    assert(evaluateWith (Implies (False, True))),
    assert(evaluateWith (Implies (True, True))),
    assert_false(evaluateWith (Implies (True, False))),
    assert(evaluateWith (Equiv (False, False))),
    assert(evaluateWith (Equiv (True, True))),
    assert_false(evaluateWith (Equiv (True, False))),
    assert_false(evaluateWith (Equiv (False, True))),
    assert_false(evaluateWith (Or [])),
    assert_false(evaluateWith (Or [False])),
    assert(evaluateWith (Or [True])),
    assert_false(evaluateWith (Or [False, False, False])),
    assert(evaluateWith (Or [False, False, False, True])),
    assert(evaluateWith (Or [False, True, False, True])),
    assert(evaluateWith (And [])),
    assert_false(evaluateWith (And [False])),
    assert(evaluateWith (And [True])),
    assert_false(evaluateWith (And [False, False, True])),
    assert(evaluateWith (And [True, True, True, True])),
    assert_false(evaluateWith (And [True, False, False, True]))
]);

test("test-removeEmpty",[
    assert_equal(removeEmpty, True, True),
    assert_equal(removeEmpty, False, False),
    assert_equal(removeEmpty, (Variable 1), (Variable 1)),
    assert_equal(removeEmpty, (Or []), False),
    assert_equal(removeEmpty, (Or [(Variable 1)]), (Variable 1)),
    assert_equal(removeEmpty, (Or [(Variable 1), (Or [])]), (Or [(Variable 1), False])),
    assert_equal(removeEmpty, (And []), True),
    assert_equal(removeEmpty, (And [(Variable 1)]), (Variable 1)),
    assert_equal(removeEmpty, (And [(Variable 1), (Or [])]), (And [(Variable 1), False])),
    assert_equal(removeEmpty, (Not (Or [])), (Not False)),
    assert_equal(removeEmpty, (Implies (Or [], True)), (Implies (False, True))),
    assert_equal(removeEmpty, (Implies (True, Or [])), (Implies (True, False))),
    assert_equal(removeEmpty, (Equiv (Or [], True)), (Equiv (False, True))),
    assert_equal(removeEmpty, (Equiv (True, Or [])), (Equiv (True, False)))
]);

test("test-removeConstants", [
    (*Simple expr*)
    assert_equal(removeConstants, True, True),
    assert_equal(removeConstants, False, False),
    assert_equal(removeConstants, (Variable 1), (Variable 1)),
    (*Implies*)
    assert_equal(removeConstants, (Implies (False, False)), True),
    assert_equal(removeConstants, (Implies (False, True)), True),
    assert_equal(removeConstants, (Implies (True, False)), False),
    assert_equal(removeConstants, (Implies (True, True)), True),
    assert_equal(removeConstants, (Implies (False, (Variable 1))), True),
    assert_equal(removeConstants, (Implies (True, (Variable 1))), (Variable 1)),
    assert_equal(removeConstants, (Implies ((Variable 1), True)), True),
    assert_equal(removeConstants, (Implies ((Variable 1), False)), (Not (Variable 1))),
    assert_equal(removeConstants, (Implies ((Implies (False, False)), (Implies (True, False)))), False),
    (*Equiv*)
    assert_equal(removeConstants, (Equiv (False, False)), True),
    assert_equal(removeConstants, (Equiv (False, True)), False),
    assert_equal(removeConstants, (Equiv (True, False)), False),
    assert_equal(removeConstants, (Equiv (True, True)), True),
    assert_equal(removeConstants, (Equiv (False, Variable 1)), Not (Variable 1)),
    assert_equal(removeConstants, (Equiv (True, Variable 1)), Variable 1),
    assert_equal(removeConstants, (Equiv (Variable 1, True)), Variable 1),
    assert_equal(removeConstants, (Equiv (Variable 1, False)), Not (Variable 1)),
    assert_equal(removeConstants, (Equiv ((Equiv (False, False)), (Equiv (True, False)))), False),
    (* And *)
    assert_equal(removeConstants, (And []), True),
    assert_equal(removeConstants, (And [True]), True),
    assert_equal(removeConstants, (And [False]), False),
    assert_equal(removeConstants, (And [Variable 1]), Variable 1),
    assert_equal(removeConstants, (And [Variable 1, True, Variable 2]), (And [Variable 1, Variable 2])),
    assert_equal(removeConstants, (And [Variable 1, False, Variable 2]), False),
    assert_equal(removeConstants, (And [Variable 1, (Equiv (False, False)), Variable 2]), (And [Variable 1, Variable 2])),
    (* Or *)
    assert_equal(removeConstants, (Or []), False),
    assert_equal(removeConstants, (Or [True]), True),
    assert_equal(removeConstants, (Or [False]), False),
    assert_equal(removeConstants, (Or [Variable 1]), Variable 1),
    assert_equal(removeConstants, (Or [Variable 1, False, Variable 2]), (Or [Variable 1, Variable 2])),
    assert_equal(removeConstants, (Or [Variable 1, True, Variable 2]), True),
    assert_equal(removeConstants, (Or [Variable 1, (Equiv (False, True)), Variable 2]), (Or [Variable 1, Variable 2]))
]);

test("test-pushNegations",[
    assert_equal(pushNegations, True, True),
    assert_equal(pushNegations, False, False),
    assert_equal(pushNegations, (Variable 1), (Variable 1)),
    assert_equal(pushNegations, (Implies (Not(True), True)), (Implies (False, True))),
    assert_equal(pushNegations, (Equiv (Not(True), True)), (Equiv (False, True))),
    assert_equal(pushNegations, (And [Not(True), Not(False), Not(False)]), (And [False, True, True])),
    assert_equal(pushNegations, (Or [Not(True), Not(False), Not(False)]), (Or [False, True, True])),
    assert_equal(pushNegations, (Not False), True),
    assert_equal(pushNegations, (Not True), False),
    assert_equal(pushNegations, Not (Equiv (Variable 1, Variable 2)), Equiv (Not (Variable 1), Variable 2)),
    assert_equal(pushNegations, Not (Equiv (True, Variable 2)), Equiv (False, Variable 2)),
    (* Maybe fix *)
    assert_equal(pushNegations, Not (Implies (Variable 1, Variable 2)), And [Variable 1, Not (Variable 2)]),
    assert_equal(pushNegations, Not (Implies (Variable 1, False)), And [Variable 1, True]),
    (* to here *)
    assert_equal(pushNegations, (Not (Or [True, False])), (And [False, True])),
    assert_equal(pushNegations, (Not (Or [])), (And [])),
    assert_equal(pushNegations, (Not (And [True, False])), (Or [False, True])),
    assert_equal(pushNegations, (Not (And [])), (Or []))
]);

test("test-removeVars", [
    assert_equal (removeVars, True, True),
    assert_equal (removeVars, False, False),
    assert_equal (removeVars, (Variable 1), (Variable 1)),
    assert_equal (removeVars, (Not (True)), (Not(True))),
    assert_equal (removeVars, (Equiv (Variable 1, Variable 1)), True),
    assert_equal (removeVars, (Equiv (Not(Variable 1), Variable 1)), False),
    assert_equal (removeVars, (Equiv (Variable 1, Not(Variable 1))), False),
    assert_equal (removeVars, (Equiv (Not(Variable 1), Not(Variable 1))), True),
    assert_equal (removeVars, (Equiv (Variable 1, Variable 2)), (Equiv (Variable 1, Variable 2))),
    assert_equal (removeVars, (Equiv (Equiv (Variable 1, Variable 1), Variable 2)), (Equiv (True, Variable 2))),
    assert_equal (removeVars, (Implies (Variable 1, Variable 1)), True),
    assert_equal (removeVars, (Implies (Not(Variable 1), Variable 1)), (Variable 1)),
    assert_equal (removeVars, (Implies (Variable 1, Not(Variable 1))), (Not (Variable 1))),
    assert_equal (removeVars, (Implies (Not(Variable 1), Not(Variable 1))), True),
    assert_equal (removeVars, (Implies (Variable 1, Variable 2)), (Implies (Variable 1, Variable 2))),
    assert_equal (removeVars, (Implies (Implies (Variable 1, Variable 1), Variable 2)), (Implies (True, Variable 2))),
    assert_equal (removeVars, (And [Variable 1, Variable 2, Variable 1, True]), (And [Variable 1, Variable 2, True])),
    assert_equal (removeVars, (And [Variable 1, Variable 2, Not (Variable 1), True]), False)
]);

val a = removeVars (Or[Variable 1,Variable 1, True]);


(*val _ = OS.Process.exit(OS.Process.success);*)
