exception InvalidVariable;
exception InsufficientVariables;
Control.Print.printDepth := 100;

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
        | Or [] =>  false
        | Or (y::ys) => (eval xs y) orelse (eval xs (Or ys))
        | And [] => true
        | And (y::ys) => (eval xs y) andalso (eval xs (And ys))
    end ;

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
        | And (x::[]) => x
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
        | Or (x::[]) => x
        | e => e
    end
    | removeVars (Not p) = Not (removeVars p)
    | removeVars e = e;
    
fun simplify exp = (* Maybe improve together with removeVars*)
    let
        fun aux e = removeEmpty (removeConstants (removeVars (pushNegations (e))))
    in
        case exp of
        Implies (a, b) => aux (Implies (simplify a, simplify b))
        | Equiv (a, b) => aux (Equiv (simplify a, simplify b))
        | And l => aux (And (map simplify l))
        | Or l => aux (Or (map simplify l))
        | Not e => aux (Not (simplify e))
        | e => e
    end ;

fun exprToVarList expr =
    case expr of
    And [] => []
    | And (x::xs) => (exprToVarList x) @ (exprToVarList (And(xs)))
    | Or [] => []
    | Or (x::xs) => (exprToVarList x) @ (exprToVarList (Or(xs)))
    | Not x => exprToVarList x
    | Implies (x,y) => (exprToVarList x) @ (exprToVarList y)
    | Equiv (x,y) => (exprToVarList x) @ (exprToVarList y)
    | Variable a => [a]
    | _ => []

fun tseytinTransformation _ True = True
    | tseytinTransformation imena_spremenljivk logicna_formula = 
    let
        fun checkDuplicates [] varList  = []
            | checkDuplicates (x::xs) varList = 
            if (List.exists (fn y => x = y) xs) orelse (List.exists (fn y => x = y) varList)
            then raise InvalidVariable
            else (x::(checkDuplicates xs varList))
        fun toTseytin [] (Variable a) = (Variable a, [], [])
            | toTseytin [] expr = raise InsufficientVariables
            | toTseytin (n::ns) True = (Variable n, ns, [Equiv(Variable n, True)])
            | toTseytin (n::ns) False = (Variable n, ns, [Equiv(Variable n, False)])
            | toTseytin l (Variable a) = (Variable a, l, [])
            | toTseytin l (Not e) =
                let
                    val (v, lr, t) = toTseytin l e
                in
                    case lr of
                    [] => raise InsufficientVariables
                    | (x::xs) => (Variable x, xs, (Equiv (Variable x, Not(v)))::t)
                end
            | toTseytin l (Implies (a,b)) =
                let
                    val (v1, lr1, t1) = toTseytin l a
                    val (v2, lr2, t2) = toTseytin lr1 b
                in
                    case lr2 of
                    [] => raise InsufficientVariables
                    | (x::xs) => (Variable x, xs, (Equiv (Variable x, Implies(v1, v2)))::(t2@t1))
                end
            | toTseytin l (Equiv (a,b)) =
                let
                    val (v1, lr1, t1) = toTseytin l a
                    val (v2, lr2, t2) = toTseytin lr1 b
                in
                    case lr2 of
                    [] => raise InsufficientVariables
                    | (x::xs) => (Variable x, xs, (Equiv (Variable x, Equiv(v1, v2)))::(t2@t1))
                end
            | toTseytin l (And es) =
                let
                    fun transformAnd [] l = ([], l, [])
                        | transformAnd es [] = raise InsufficientVariables
                        | transformAnd (e::es) vs =
                            let
                                val (var, lr1, t) = toTseytin vs e
                                val (vars, lr2, ts) = transformAnd es lr1
                            in
                                (var::vars, lr2, t@ts)
                            end
                in
                    case (transformAnd es l) of
                    (_, [], _) => raise InsufficientVariables
                    | (vs, x::xs, t) => (Variable x, xs, (Equiv(Variable x, And vs))::t)
                end
            | toTseytin l (Or es) =
                let
                    fun transformOr [] l = ([], l, [])
                        | transformOr es [] = raise InsufficientVariables
                        | transformOr (e::es) vs =
                            let
                                val (var, lr1, t) = toTseytin vs e
                                val (vars, lr2, ts) = transformOr es lr1
                            in
                                (var::vars, lr2, t@ts)
                            end
                in
                    case (transformOr es l) of
                    (_, [], _) => raise InsufficientVariables
                    | (vs, x::xs, t) => (Variable x, xs, (Equiv(Variable x, Or vs))::t)
                end
        fun kno (Variable a) = [Variable a]
            | kno (Equiv (Variable a, True)) = [Variable a]
            | kno (Equiv (Variable a, False)) = [Not (Variable a)]
            | kno (Equiv (Variable a, Not (Variable b))) = [Or [Variable a, Variable b],
                                                            Or[Not(Variable a), Not (Variable b)]]
            | kno (Equiv (Variable a, Equiv (Variable b, Variable c))) = [Or [Not (Variable a), Not (Variable b), Variable c],
                                                                          Or [Not (Variable a), Variable b, Not (Variable c)],
                                                                          Or [Variable a, Not (Variable b), Not (Variable c)],
                                                                          Or [Variable a, Variable b, Variable c]]
            | kno (Equiv (Variable a, Implies (Variable b, Variable c))) = [Or [Not (Variable a), Not (Variable b), Variable c],
                                                                            Or [Variable a, Variable b],
                                                                            Or [Variable a, Not (Variable c)]]
            | kno (Equiv (Variable a, Or l)) = 
                let
                    fun other v []= []
                        | other v (x::xs) = (Or [v, Not (x)])::(other v xs)
                in
                    (Or ((Not (Variable a))::l))::(other (Variable a) l)
                end
            | kno (Equiv (Variable a, And l)) = 
                let
                    fun other v []= []
                        | other v (x::xs) = (Or [Not (v), x])::(other v xs)
                in
                    (Or ((Variable a)::(map (fn x => Not(x)) l)))::(other (Variable a) l)
                end
            | kno e = [e]
    in
        let
            val (var, _, tsey) = toTseytin (checkDuplicates imena_spremenljivk (exprToVarList logicna_formula))
                                (simplify logicna_formula)
            fun glue [] = []
                | glue (x::xs) = x@(glue xs)
        in
            simplify (And (glue (map kno (var::tsey))))
        end
    end ;

fun SATsolver expr = 
    let
        fun findFree (And []) = []
            | findFree (And(x::xs)) = 
                (case x of
                Variable a => (a, true)::(findFree (And xs))
                | Or [Variable a] => (a, true)::(findFree (And xs))
                | Or [Not(Variable a)] => (a, false)::(findFree (And xs))
                | _ => findFree (And xs))
            | findFree _ = []

        fun checkFree ([]) = SOME []
            | checkFree ((x,b)::xs) =
                let
                    val same = List.exists (fn (x1,b1) => x = x1 andalso b = b1) xs
                    val dif = List.exists (fn (x1,b1) => x = x1 andalso b = (not b1)) xs
                in
                    case (same, dif) of
                    (_, true) => NONE
                    | (false, false) => 
                        (
                            case checkFree xs of
                            NONE => NONE
                            | SOME e => SOME ((x,b)::e)
                        )
                    | (true, false) => 
                        (
                            case checkFree xs of
                            NONE => NONE
                            | SOME e => SOME (e)
                        )
                end

        fun changeVar [] a = a
            | changeVar ((a,var)::vs) (Variable b) =
                if a = b
                then (
                    if var
                    then True
                    else False)
                else changeVar vs (Variable b) 

        fun simplifyExpr vars expr = 
            case expr of
            And l => simplify (And (map (simplifyExpr vars) l))
            | Or l => simplify (Or (map (simplifyExpr vars) l))
            | Not e => simplify (Not (simplifyExpr vars e))
            | Variable a => changeVar vars (Variable a)
            | e => simplify e

        fun findVar (Variable a) = SOME a
            | findVar (And (x::xs)) =
                (case findVar x of
                NONE => findVar (And xs)
                | e => e)
            | findVar (Or (x::xs)) =
                (case findVar x of
                NONE => findVar (Or xs)
                | e => e)
            | findVar (Not a) = findVar a    
            | findVar e = NONE

        exception NoVariables

        fun stepOne expr = 
        let
            fun stepThree expr = 
            let
                val var = (case findVar expr of
                           NONE => raise NoVariables
                           | SOME a => a)

            in
                case stepOne (simplifyExpr [(var, true)] expr) of
                SOME a => SOME ((var,true)::a)
                | NONE => (
                    case stepOne (simplifyExpr [(var, false)] expr) of
                    NONE => NONE
                    | SOME a => SOME ((var,false)::a)
                )
            end

            fun stepTwo True = (SOME [])
            | stepTwo False = NONE
            | stepTwo (And[]) = (SOME [])
            | stepTwo (Or []) = NONE
            | stepTwo e = stepThree e

            val checked = checkFree (findFree expr)
        in
            case checked of
            NONE => NONE
            | SOME [] => stepTwo expr
            | SOME a => 
                let
                    val res = stepOne (simplifyExpr a expr)
                in
                    if Option.isSome(res)
                    then (SOME (a @ (Option.valOf (res))))
                    else NONE
                end
        end
    in
        stepOne expr
    end ;

(* not(equiv(a,b)) = ((not a) or (not b)) and (a or b)
fun equivalentExpressions e1 e2 =
    let
        fun negate 
    in
      body
    end
    if Option.isSome(SATsolver (And))
    then false
    else true;*)

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

val a = tseytinTransformation ["x4","x3","x2","x1"] (Implies(And [Or [Variable "p", Variable "q"], Variable "r"], Not (Variable "s")));

val b = SATsolver a

(*val _ = OS.Process.exit(OS.Process.success);*)
