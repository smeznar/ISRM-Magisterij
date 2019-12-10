exception InvalidVariable;
exception InsufficientVariables;
exception InvalidExpression;
exception NoVariables;
Control.Print.printDepth := 100;
Control.Print.printLength := 100;

datatype 'a expression = Not of 'a expression
                        | Or of 'a expression list
                        | And of 'a expression list
                        | Implies of 'a expression * 'a expression
                        | Equiv of 'a expression * 'a expression
                        | Variable of 'a
                        | True
                        | False;

fun eval xs e = 
    let
        fun getValueFromList [] item = raise InvalidVariable
            | getValueFromList ((i,b)::xs) item = 
                if i = item
                then b
                else getValueFromList xs item
    in
        case e of
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
        | Not a => Not (removeEmpty a)
        | Implies (a, b) => Implies (removeEmpty a, removeEmpty b)
        | Equiv (a, b) => Equiv (removeEmpty a, removeEmpty b)
        | e => e;

fun removeConstants (Not e) = 
        (case removeConstants e of
            True => False
            | False => True
            | r => Not r)
    | removeConstants (Equiv (s1, s2)) =
        (case (removeConstants s1, removeConstants s2) of
            ((True, True) | (False, False)) => True
            | ((True, False) | (False, True)) => False
            | ((True, x) | (x, True)) => x
            | ((False, x) | (x, False)) => Not (x)
            | (x, y) => Equiv (x, y))
    | removeConstants (Implies (s1, s2)) = 
        (case removeConstants s1 of
            False => True
            | r1 => (
                case (r1, removeConstants s2) of
                    (True, e) => e
                    | (e, False) => Not (e)
                    | (e, True) => True
                    | (e1, e2) => Implies (e1, e2)))
    | removeConstants (And l) = 
        let
            fun aux (x, False) = False
            | aux (x, (And(l))) = 
                (case removeConstants x of
                    False => False
                    | True => And l
                    | r => And (r::l))
        in
            if null l then True
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
                (case removeConstants x of
                    True => True
                    | False => Or l
                    | r => Or (r::l))
        in
            if null l then False
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

fun rmDup [] = []
    | rmDup (x::xs) = 
        if List.exists (fn y => y=x) xs 
        then rmDup xs
        else x::(rmDup xs)

fun removeVars (Implies (p, q)) = 
        let
            val r1 = removeVars p
            val r2 = removeVars q
        in
            case (r1, r2) of
                ((Variable a, Variable b) | (Not (Variable a), Not(Variable b))) => 
                    if a = b then True
                    else Implies (r1,r2)
                | ((Variable a, Not (Variable b)) | (Not(Variable a), Variable b)) =>
                    if a = b then r2
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
                    if a = b then True
                    else Equiv (r1,r2)
                | ((Variable a, Not (Variable b)) | (Not(Variable a), Variable b)) =>
                    if a = b then False
                    else Equiv (r1, r2)
                | (a, b) => Equiv (a, b)
        end
    | removeVars (And l) = 
        let
            val mapped = rmDup (map removeVars l)
            fun checkOne x [] = And []
            | checkOne x (y::ys) = 
                (case checkOne x ys of
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
                    | e => e)
            fun checkAll [] = And []
            | checkAll (x::xs) = 
                case checkAll xs of
                    And l => 
                        (case checkOne x l of
                            And ls => And (x::ls)
                            | e => e)
                    | e => e
        in
            case checkAll mapped of
                And [] => True
                | And (x::[]) => x
                | e => e
        end
    | removeVars (Or l) = 
        let
            val mapped = rmDup (map removeVars l)
            fun checkOne x [] = Or []
            | checkOne x (y::ys) = 
                (case checkOne x ys of
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
                    | e => e)
            fun checkAll [] = Or []
            | checkAll (x::xs) = 
                (case checkAll xs of
                    Or l => 
                        (case checkOne x l of
                            Or ls => Or (x::ls)
                            | e => e)
                    | e => e)
        in
            case checkAll mapped of
                Or [] => False
                | Or (x::[]) => x
                | e => e
        end
    | removeVars (Not p) = Not (removeVars p)
    | removeVars e = e;
    
fun simplify exp = 
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

(* Helper functions *)

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

fun aExpToIntExp expr n = 
    let    
        fun changeVal [] n = ([], n)
            | changeVal (x::xs) n = 
                let
                    val (s, num) = changeVal xs (n+1)
                in
                    ((x,n)::s, num)
                end

        fun transformExpr (vars : (''a *int) list) e = 
            case e of
                Variable a => Variable (#2 (hd (List.filter (fn x => (#1 x)=a) vars)))
                | Not a => Not (transformExpr vars a)
                | Or l => Or (map (transformExpr vars) l)
                | And l => And (map (transformExpr vars) l)
                | Implies (a, b) => Implies (transformExpr vars a, transformExpr vars b)
                | Equiv (a, b) => Equiv (transformExpr vars a, transformExpr vars b)
                | True => True
                | False => False 

        val (l, nn) = changeVal (rmDup (exprToVarList expr)) n
    in
        (transformExpr l expr, nn)
    end;

fun tseytinTransformation imena_spremenljivk logicna_formula = 
        let
            fun checkDuplicates [] varList  = []
                | checkDuplicates (x::xs) varList = 
                    if (List.exists (fn y => x = y) xs) orelse (List.exists (fn y => x = y) varList)
                    then raise InvalidVariable
                    else (x::(checkDuplicates xs varList))

            fun tseytinAndOr [] l = ([], l, [])
                | tseytinAndOr es [] = raise InsufficientVariables
                | tseytinAndOr (e::es) vs =
                    let
                        val (var, lr1, t) = toTseytin vs e
                        val (vars, lr2, ts) = tseytinAndOr es lr1
                    in
                        (var::vars, lr2, t@ts)
                    end

            and toTseytin [] (Variable a) = (Variable a, [], [])
                | toTseytin [] expr = raise InsufficientVariables
                | toTseytin (n::ns) True = raise InvalidExpression
                | toTseytin (n::ns) False = raise InvalidExpression
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
                        | (x::xs) => (Variable x, xs, (Equiv (Variable x, Implies(v1, v2)))::(t1@t2))
                    end
                | toTseytin l (Equiv (a,b)) =
                    let
                        val (v1, lr1, t1) = toTseytin l a
                        val (v2, lr2, t2) = toTseytin lr1 b
                    in
                        case lr2 of
                        [] => raise InsufficientVariables
                        | (x::xs) => (Variable x, xs, (Equiv (Variable x, Equiv(v1, v2)))::(t1@t2))
                    end
                | toTseytin l (And es) =
                    (case tseytinAndOr es l of
                        (_, [], _) => raise InsufficientVariables
                        | (vs, x::xs, t) => (Variable x, xs, (Equiv(Variable x, And vs))::t))
                | toTseytin l (Or es) =
                    (case tseytinAndOr es l of
                        (_, [], _) => raise InsufficientVariables
                        | (vs, x::xs, t) => (Variable x, xs, (Equiv(Variable x, Or vs))::t))

            fun kno (Variable a) = [Variable a]
                | kno (Equiv (Variable a, Not (Variable b))) = [Or[Not(Variable a), Not (Variable b)],
                                                            Or [Variable a, Variable b]]
                | kno (Equiv (Variable a, Equiv (Variable b, Variable c))) = 
                    [Or [Not (Variable a), Not (Variable b), Variable c], Or [Not (Variable a), Variable b, Not (Variable c)],
                    Or [Variable a, Not (Variable b), Not (Variable c)], Or [Variable a, Variable b, Variable c]]
                | kno (Equiv (Variable a, Implies (Variable b, Variable c))) = 
                    [Or [Not (Variable a), Not (Variable b), Variable c], Or [Variable a, Variable b], Or [Variable a, Not (Variable c)]]
                | kno (Equiv (Variable a, Or l)) = (Or ((Not (Variable a))::l))::(map (fn x => Or [Variable a, Not (x)]) l)
                | kno (Equiv (Variable a, And l)) = (Or ((Variable a)::(map (fn x => Not(x)) l)))::(map (fn x => Or [Not(Variable a), x]) l)
                | kno e = raise InvalidExpression;

            fun glue [] = []
                | glue (x::xs) = x@(glue xs)

        in
            case simplify logicna_formula of
                True => True
                | False => False
                | e =>
                    let
                        val (var, _, tsey) = toTseytin
                            (checkDuplicates imena_spremenljivk (exprToVarList logicna_formula)) e
                    in
                        simplify (And (glue (map kno (var::tsey))))
                    end
        end ;

fun SATsolver newVariableList expr= 
    let
        fun KnoThird (Variable a) = true
            | KnoThird (Not (Variable a)) = true
            | KnoThird e = false

        fun KnoSecond (Variable a) = true
            | KnoSecond (Not (Variable a)) = true
            | KnoSecond (Or l) = List.all KnoThird l
            | KnoSecond e = false

        fun KnoFirst (Variable a) = true
            | KnoFirst (Not(Variable a)) = true
            | KnoFirst (Or l) = List.all KnoThird l
            | KnoFirst (And l) = List.all KnoSecond l
            | KnoFirst True = true
            | KnoFirst False = true
            | KnoFirst e = false

        fun findFree (Variable a) = [(a, true)]
            | findFree (Not (Variable a)) = [(a, false)]
            | findFree (And []) = []
            | findFree (And(x::xs)) = 
                (case x of
                    Variable a => (a, true)::(findFree (And xs))
                    | Not (Variable a) => (a, false)::(findFree (And xs))
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
                        | (true, false) => 
                            (case checkFree xs of
                                NONE => NONE
                                | SOME e => SOME (e))
                        | (false, false) => 
                            (case checkFree xs of
                                NONE => NONE
                                | SOME e => SOME ((x,b)::e))
                end

        fun changeVar [] a = a
            | changeVar ((a,var)::vs) (Variable b) =
                if a = b
                then (if var then True else False)
                else changeVar vs (Variable b) 

        fun simplifyExpr vars expr = 
            case expr of
                And l => simplify (And (map (simplifyExpr vars) l))
                | Or l => simplify (Or (map (simplifyExpr vars) l))
                | Not e => simplify (Not (simplifyExpr vars e))
                | Variable a => changeVar vars (Variable a)
                | e => simplify e

        fun findVar (Variable a) = SOME (Variable a)
            | findVar (Not (Variable a)) = SOME (Not (Variable a))
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

        fun stepOne expr = 
            case checkFree (findFree expr) of
                NONE => NONE
                | SOME [] => stepTwo expr
                | SOME a => 
                    case stepOne (simplifyExpr a expr) of
                        NONE => NONE
                        | SOME b => SOME (a@b)

        and stepTwo True = (SOME [])
            | stepTwo False = NONE
            | stepTwo (And[]) = (SOME [])
            | stepTwo (Or []) = NONE
            | stepTwo e = stepThree e

        and stepThree expr = 
            let
                val (var, value) = 
                    (case findVar expr of
                        NONE => raise NoVariables
                        | SOME (Variable a) => (a, true)
                        | SOME (Not(Variable a)) => (a, false))
            in
                case stepOne (simplifyExpr [(var, value)] expr) of
                    SOME a => SOME ((var,value)::a)
                    | NONE =>
                        (case stepOne (simplifyExpr [(var, not (value))] expr) of
                            NONE => NONE
                            | SOME a => SOME ((var,not(value))::a))
            end
    in
        if KnoFirst (simplify expr)
        then stepOne expr
        else stepOne (tseytinTransformation newVariableList expr)
    end ;

fun equivalentExpressions e1 e2 =
    let
        fun count e = 
            case e of
                Not a => (count a) + 1
                | (Implies (a,b) | Equiv (a,b)) => (count a) + (count b) + 1
                | ((And l) | (Or l) ) => (foldl (fn (x,y) => (count x) + y) 0 l) + 1
                | _ => 1

        fun createVarList 0 n = [n]
            | createVarList n1 n2 = n2::(createVarList (n1-1) (n2+1))

        val (e, n) = aExpToIntExp (Not(Equiv(e1,e2))) 0
        val s = tseytinTransformation (createVarList (count e) n) e
    in
        not (Option.isSome(SATsolver [] s))
    end;

type timetable = {day : string, time: int, course: string} list;
type student = {studentID : int, curriculum : string list};

fun problemReductionN n time studList = 
    let
        fun firstCondition (sid,[],t) = []
            | firstCondition (sid, x::xs, t:timetable) = 
                (Or (foldl (fn ({day = d,time = ti ,course = _},b) => 
                    [Variable (sid, x, ti, 1, d),Variable (sid, x, ti, 2, d),Variable (sid, x, ti, 3, d)]@b) 
                    [] (List.filter (fn {day = _,time = _ ,course = c} => c=x) t)))::(firstCondition (sid,xs,t))

        fun studentTraversal [] t = []
            | studentTraversal (({studentID=sid, curriculum=c}:student)::ss) t =
                (firstCondition (sid,c,t))@(studentTraversal ss t)

        fun secondFilter (s,_,t,_,d) = (fn (s1,_,t1,_,d1) => s = s1 andalso (abs (t - t1) < n) andalso d = d1)
        fun thirdFilter (_,c,t,p,d) = (fn (_,c1,t1,p1,d1) => c = c1 andalso t = t1 andalso d = d1 andalso p = p1)

        fun generalCondition [] f = []
            | generalCondition ((s,c,t,p,d)::xs) f = 
                (map (fn a => Or [Not (Variable (s,c,t,p,d)),Not (Variable a)])
                (List.filter (f (s,c,t,p,d)) xs))@(generalCondition xs f)

        val first = studentTraversal studList time
        val genCond = generalCondition (rmDup (exprToVarList (And(first))))
    in
        simplify (And(first@(genCond secondFilter)@(genCond thirdFilter)))
    end;

fun problemReduction time studList = problemReductionN 1 time studList; 

fun solutionRepresentation NONE = []
    | solutionRepresentation (SOME l) = 
        let
            fun oneStudent [] = []
                | oneStudent ((s,c,t,p,d)::xs) = 
                    let
                        val same = (s,c,t,p,d)::(List.filter (fn (s1,_,_,_,_) => s=s1) xs)
                        val different = (List.filter (fn (s1,_,_,_,_) => s<>s1) xs)  
                    in
                        ({studentID=s, curriculum = (rmDup (map (fn (_,c1,_,_,_)=>c1) same))},
                        (map (fn (_,c1,t1,_,d1) => {day=d1,time=t1,course=c1}) same))::(oneStudent different)
                    end
        in
            oneStudent (map (fn (a,b)=> a) (List.filter (fn (a,b) => b) l))
        end;

(* TESTS *)
(*
val _ = print ("\n");
val _ =  print ("UPORABLJENE SPREMENLJIVKE\n");
val _ = print ("\n");

use "unittest.sml";

val evaluateWith = eval [("a",true), ("a", false), ("b", false)];
val example = (Implies(And [Or [Variable "p", Variable "q"], Variable "r"], Not (Variable "s")));
val result = And
    [Variable "x4",Or [Not (Variable "x4"),Not (Variable "x2"),Variable "x3"],
     Or [Variable "x4",Variable "x2"],Or [Variable "x4",Not (Variable "x3")],
     Or [Variable "x2",Not (Variable "x1"),Not (Variable "r")],
     Or [Not (Variable "x2"),Variable "x1"],
     Or [Not (Variable "x2"),Variable "r"],
     Or [Not (Variable "x1"),Variable "p",Variable "q"],
     Or [Variable "x1",Not (Variable "p")],
     Or [Variable "x1",Not (Variable "q")],
     Or [Not (Variable "x3"),Not (Variable "s")],
     Or [Variable "x3",Variable "s"]];

val time = [{day = "torek", time = 7, course = "DS"},
            {day = "sreda", time = 10, course = "DS"},
            {day = "torek", time = 7, course = "P2"},
            {day = "ponedeljek", time = 12, course = "P2"}] : timetable;
val students = [{studentID = 63170000, curriculum = ["DS", "P2", "OPB", "RK"]},{studentID = 63160000, curriculum = ["P2", "RK", "ARS"]}] : student list;
val students2 = [{studentID = 0, curriculum = ["OPB"]},{studentID = 1, curriculum = ["OPB"]},
                {studentID = 2, curriculum = ["P2", "RK", "ARS", "OPB"]}, {studentID = 3, curriculum = ["OPB"]}] : student list;

val time2 = [{day = "torek", time = 7, course = "OPB"},
            {day = "sreda", time = 11, course = "RK"},
            {day = "petek", time = 14, course = "ARS"}]@time;

val _ = print ("\n");
val _ =  print ("TESTI\n");
val _ = print ("\n");
(* Zgoraj definirane funkcije vrzejo nekaj warningov, predvsem calling polyequal (kar zaradi polimorfizma ne moremo 
    spremeniti), ter match nonexhaustive, ki se pojavi v funkcijah, katere so narejene tako, da se klice le nekaj vrst izrazov*)

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
    assert(eval [(1,true), (2,true),(3,false),(4,false)] (Equiv(And[Implies(Variable 3, Variable 4), True], And []))),
	assert(eval [(1, false), (2, true)] (And [True,Or [Variable 1, Not (Not (Variable 2))],Implies (Variable 1, Variable 2)])),
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
    assert_equal(removeEmpty, (Equiv(And[], Or[Not(Variable 2)])), Equiv(True,Not(Variable 2))),
	assert_equal(removeEmpty, (Or[True, And[], Or[False]]), Or[True,True,False]),
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
    assert_equal(removeConstants, (Or [Variable 1, (Equiv (False, True)), Variable 2]), (Or [Variable 1, Variable 2])),
    assert_equal(removeConstants, (Equiv(And[True, Variable 1,Or[Variable 2, True], Not False] ,Or[Variable 2, False, And [True, True]])) ,Variable 1),
    assert_equal(removeConstants, (And [True, Variable 4, Equiv(False, Or[False, Variable 2, True])]), False)
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
    assert_equal(pushNegations, Not (Implies (Variable 1, Variable 2)), And [Variable 1, Not (Variable 2)]),
    assert_equal(pushNegations, Not (Implies (Variable 1, False)), And [Variable 1, True]),
    assert_equal(pushNegations, (Not (Or [True, False])), (And [False, True])),
    assert_equal(pushNegations, (Not (Or [])), (And [])),
    assert_equal(pushNegations, (Not (And [True, False])), (Or [False, True])),
    assert_equal(pushNegations, (Not (And [])), (Or [])),
    assert_equal(pushNegations, (Not (Implies (Not (Not (Variable "a")), Variable "b"))), And [Variable "a", Not (Variable "b")]),
	assert_equal(pushNegations, (Implies (Not (Not (Variable "a")), Variable "b")), Implies (Variable "a", Variable "b"))
]);

test("test-removeVars", [
    assert_equal (removeVars, True, True),
    assert_equal (removeVars, False, False),
    assert_equal (removeVars, (Variable 1), (Variable 1)),
    assert_equal (removeVars, (Not (True)), (Not(True))),
    assert_equal (removeVars, (Not (Equiv (Variable 1, Variable 1))), (Not(True))),
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
    assert_equal (removeVars, (And [Variable 1, Variable 2, Variable 1, True]), (And [Variable 2, Variable 1, True])),
    assert_equal (removeVars, (And [Variable 1, Variable 2, Not (Variable 1), True]), False),
    assert_equal (removeVars, (And [Variable "s"]), Variable "s"),
    assert_equal (removeVars, (And []), True),
    assert_equal (removeVars, (And [Not(Not(Variable 2)), Not(Not(Variable 2))]), Not(Not(Variable 2))),
    assert_equal (removeVars, (And [Variable 1, Variable 2, (Implies (Not(Variable 1), Variable 1))]), (And [Variable 2, Variable 1])),
    assert_equal (removeVars, (Or [Variable 1, Variable 2, Variable 1, True]), (Or [Variable 2, Variable 1, True])),
    assert_equal (removeVars, (Or [Variable 1, Variable 2, Not (Variable 1), True]), True),
    assert_equal (removeVars, (Or [Variable "s"]), Variable "s"),
    assert_equal (removeVars, (Or []), False),
    assert_equal (removeVars, (Or [Not(Not(Variable 2)), Not(Not(Variable 2))]), Not(Not(Variable 2))),
    assert_equal (removeVars, (Or [Variable 1, Variable 2, (Implies (Not(Variable 1), Variable 1))]), (Or [Variable 2, Variable 1])),
    assert_equal (removeVars, (Or [Variable 2, And[Variable 2, Variable 2], Variable 2, Implies(Variable 3, Variable 3)]) , Or[Variable 2, True]),
    assert_equal (removeVars, (Or [Variable 2, Not(And[Variable 2, Variable 2]), Variable 2, Implies(Variable 3, Variable 3)]) , True)
]);

test("test-simplify", [
    assert_equal (simplify, (And [Not(Not(Variable 2)), Not(Not(Variable 2))]), (Variable 2)),
    assert_equal (simplify, (And[Implies(Variable 2,Not (Variable 2)),Equiv(Not False,Variable 2)]), False),
    assert_equal (simplify, (Implies (Not(Equiv (Variable 1, True)), Implies (True, Variable 1))), (Variable 1)),
    assert_equal (simplify, (Or [Implies (Equiv (Variable 1, True), Implies (True, Variable 1))]), True),
    assert_equal (simplify, (And[Implies(Variable 2,Not (Variable 2)),Equiv(Not False,Variable 2)]), False),
    assert_equal (simplify, (Or[And[Variable 1,Variable 1,True,True], Variable 1, Variable 2]), Or[Variable 1, Variable 2]),
    assert_equal (simplify, (Not(And[Implies(Variable 2,Not (Variable 2)),Equiv(Not False,Variable 2)])), True)
]);

test("test-tseytinTransformation", [
    assert_equal (tseytinTransformation [], True, True),
    assert_equal (tseytinTransformation [], False, False),
    assert_equal (tseytinTransformation [], (Variable 1), (Variable 1)),
    assert_equal (tseytinTransformation [2], (Not (Variable 1)), And [Variable 2,Or[Not(Variable 2), Not (Variable 1)],
                                                            Or [Variable 2, Variable 1]]),
    assert_equal (tseytinTransformation [3], (Implies (Variable 1, Variable 2)),
        And [Variable 3,Or [Not (Variable 3), Not (Variable 1), Variable 2], Or [Variable 3, Variable 1], Or [Variable 3, Not (Variable 2)]]),
    assert_equal (tseytinTransformation [1], (Equiv (Variable 2, Variable 3)),
        And [Variable 1, Or [Not (Variable 1), Not (Variable 2), Variable 3], Or [Not (Variable 1), Variable 2, Not (Variable 3)],
                    Or [Variable 1, Not (Variable 2), Not (Variable 3)], Or [Variable 1, Variable 2, Variable 3]]),
    assert_equal (tseytinTransformation [1], (Or [Variable 2, Variable 3]),
        And [Variable 1, Or [Not (Variable 1), Variable 2, Variable 3], Or [Variable 1, Not(Variable 2)],
                    Or [Variable 1, Not (Variable 3)]]),
    assert_equal (tseytinTransformation [1], (And [Variable 2, Variable 3]),
        And [Variable 1, Or [Variable 1, Not(Variable 2),Not(Variable 3)], Or [Not(Variable 1), Variable 2],
                    Or [Not(Variable 1), Variable 3]]),                                
    assert_equal (tseytinTransformation ["x1","x2","x3","x4"], example, result),
    assert_raises (fn () => (tseytinTransformation ["x1","x2","x3"] example), InsufficientVariables),
    assert_raises (fn () => (tseytinTransformation ["x1","x2","x3","p"] example), InvalidVariable),
    assert_raises (fn () => (tseytinTransformation [4,4,3] (Implies (Not(Variable 1), Not(Variable 2)))), InvalidVariable )
]);

test("test-SATsolver", [
    assert_equal (SATsolver [], True, (SOME [])),
    assert_equal (SATsolver [], False, NONE),
    assert_equal (SATsolver [], (Variable 1), (SOME [(1,true)])),
    assert_equal (SATsolver [2], (Not (Variable 1)), (SOME [(1,false)])),
    assert_equal (SATsolver [1], (Implies (Variable 2, Variable 3)), (SOME [(1,true),(2,false)])),
    assert_equal (SATsolver [1], (Equiv (Variable 2, Variable 3)), (SOME [(1,true),(2,false),(3, false)])),
    assert_equal (SATsolver [], (And [Variable 1, Variable 2]), (SOME [(1,true),(2, true)])),
    assert_equal (SATsolver [], (Or [Variable 1, Variable 2]), (SOME [(1,true)])),
    assert_equal (SATsolver ["x1","x2","x3","x4"], example, (SOME
        [("x4",true),("x2",false),("x1",false),("p",false),("q",false),("x3",false),("s",true)]))
]);

test("test-equivalentExpressions", [
    assert (equivalentExpressions True True),
    assert (equivalentExpressions False False),
    assert_false (equivalentExpressions False True),
    assert (equivalentExpressions (Variable 1) (Variable 1)),
    assert_false (equivalentExpressions (Variable 1) (Variable 2)),
    assert (equivalentExpressions example example),
    assert (equivalentExpressions example (Implies (Not(example), False))),
    assert_false (equivalentExpressions example (Not(example))),
    assert (equivalentExpressions (Equiv (Not (Variable 1), (Variable 2))) (Equiv (Variable 1,Not (Variable 2)))),
    assert (equivalentExpressions (Not (And [Variable 1,Variable 2])) (Or [Not(Variable 1), Not(Variable 2)])),
    assert_false (equivalentExpressions (Not (Implies (Variable 1, Variable 2))) (Implies (Not (Variable 1),Not(Variable 2))))
]);

test("test-problemReduction + solutionRepresentation", [
    assert_equal (problemReduction time, students, False),
    assert_equal (solutionRepresentation,(SATsolver [] (problemReduction time students)), []), (* Not all subjects *)
    assert_equal (solutionRepresentation,(SATsolver [] (problemReductionN 2 time2 students)), []),
    assert_equal (solutionRepresentation,(SATsolver [] (problemReduction time2 students)),
    [({curriculum=["DS","P2","OPB","RK"],studentID=63170000},
    [{course="DS",day="sreda",time=10},{course="P2",day="ponedeljek",time=12},
     {course="OPB",day="torek",time=7},{course="RK",day="sreda",time=11}]),
    ({curriculum=["P2","RK","ARS"],studentID=63160000},
    [{course="P2",day="ponedeljek",time=12},{course="RK",day="sreda",time=11},
     {course="ARS",day="petek",time=14}])]),
    assert_equal (solutionRepresentation,(SATsolver [] (problemReduction time2 students2)), []) (* Not enough seats *)
]);
*)


(* pomozni primeri
val d = tseytinTransformation [3,4,5,6,7,8,9] (Not(Equiv(Variable 1, Variable 2)))
val c = removeVars (And [Not(Not(Variable 2)), Not(Not(Variable 2))])
val e = simplify (And[Implies(Variable 2,Not (Variable 2)),Equiv(Not False,Variable 2)]);
val time = [{day = "torek", time = 7, course = "DS"},
            {day = "sreda", time = 10, course = "DS"},
            {day = "petek", time = 14, course = "DS"},
            {day = "torek", time = 7, course = "P2"},
            {day = "ponedeljek", time = 12, course = "P2"}] : timetable;
val students = [{studentID = 63170000, curriculum = ["DS", "P2", "OPB", "RK"]},{studentID = 63160000, curriculum = ["P2", "RK", "ARS"]}] : student list;
val h = SATsolver [] (problemReduction time2 students);
val a = SATsolver [] (problemReductionN 2 time2 students);
val b = solutionRepresentation a;
val i = solutionRepresentation h;*)
(*
nekaj dodatinih testov za simplify
val lexp1 = Not (Equiv (Variable 7, False))
val lexp2 = And [Equiv (Not (True), Equiv (Equiv (Equiv (And [Not (False), Variable 9, Implies (Variable 4, And [Implies (False, Equiv (Not (False), False)), Implies (True, False), Or [False, True, Variable 3]]), Variable 0], Not (Or [False, True])), Variable 0), True)), Variable 3];
val lexp3 = And [Equiv (True, Or [Equiv (And [Implies (Equiv (Implies (And [], Not (True)), Or [False, Or [], True, Variable 4]), Variable 7), And [True, Equiv (True, Equiv (Equiv (True, Variable 9), And [Not (Variable 6), Variable 1]))], Variable 5], Equiv (False, Or [And [], Variable 5, True, True])), Not (Implies (False, And [True]))]), Not (Or [Equiv (Not (False), True), Implies (Or [False, Variable 2], False), True])];
val lexp4 = Implies (Or [Variable 0], Or [And [Variable 1, False], Equiv (Not (And [False, Implies (Not (False), Equiv (Not (Or [True, And [False, Not (True)], False, And [False]]), True)), True, Not (And [True, False, Variable 2])]), Implies (True, Equiv (Not (True), True))), Implies (And [Not (Equiv (False, Variable 3)), False, Not (And [False, Implies (True, And [Not (Not (Variable 8)), True]), Not (False)]), True], True), Not (Not (Implies (Variable 0, Equiv (Implies (False, True), Not (Variable 7)))))]);
val lexp5 = And [Equiv (Equiv (Or [False, Not (Variable 9), And [Variable 4, Or [And [False,Equiv (False,False)],And [True,True], And [True,False,Not (Variable 3)]]],Not (Variable 0)], And [True,False]),Variable 0),Variable 3];
val lexp6 = Not (Equiv (Not (And [Variable 4, Implies (Implies (Or [False, False], Implies (Not (Equiv (True, Or [])), Not (And [Variable 6, True, And [Not (True), Equiv (Not (Equiv (False, False)), False)]]))), Or [Equiv (And [Not (And []), Implies (And [Variable 3], Or [True, True]), Implies (Or [Variable 3, True], Equiv (Variable 0, Variable 7))], Equiv (Implies (Implies (True, And []), And [Equiv (False, Variable 0), Equiv (True, Or [And [False, False, Or [And [Variable 6, True, True], False]]]), And [], Not (Not (Implies (Or [True, Variable 3, Not (False)], Variable 9)))]), True)), Or [], Implies (And [Variable 6, Not (Or []), Or [False, Equiv (Equiv (Variable 9, Variable 3), Equiv (Variable 5, Equiv (Variable 7, Not (False)))), Variable 2], Implies (And [], Variable 4)], Implies (True, False)), Not (Equiv (True, Not (True)))]), Or [Variable 1, Variable 9, Equiv (Or [Not (Variable 4), Not (True), Equiv (False, Not (Implies (And [], Equiv (Equiv (False, True), False)))), Implies (Or [Implies (Equiv (And [Not (True), And [False, Implies (False, True)], Variable 7, And [Variable 1, False, False]], Or [Equiv (False, Variable 3), True]), Variable 5), True, True, False], True)], Not (True))], And [And [True, And [Not (True), Equiv (True, Not (Variable 4)), Implies (Implies (Not (Not (Not (Variable 4))), Variable 6), True), And [Variable 6, False]], And [And [False, Implies (True, Equiv (Or [Or [Not (True), Not (True), Not (True), False]], Variable 7)), True], And [], Equiv (Implies (And [], Or [Or [], Not (Variable 7)]), True), Implies (False, False)], Not (Equiv (Or [], Implies (And [False], False)))]]]), Or [Not (False)]));
val lexp7 = Equiv (Implies (Or [Or [False], And [], And [Or [Or [], Not (Equiv (True, Equiv (Not (Not (Equiv (Equiv (Variable 4, True), Implies (False, Variable 6)))), Equiv (True, Implies (Not (Implies (And [Variable 3, Implies (True, Variable 7), True, Variable 8], True)), False)))))], True], Variable 2], Or [Or [], Equiv (And [], Implies (Implies (Implies (And [Variable 9, Implies (Equiv (False, True), Equiv (Variable 9, Variable 4)), And [True, Not (Not (Implies (Variable 3, False))), Not (False), Not (Not (Variable 1))], Implies (True, Variable 3)], Not (Not (Implies (And [Variable 4, And [False, Variable 0, Variable 4, Variable 7]], False)))), Or [Implies (False, Equiv (Equiv (True, False), Variable 2)), And [Or [Variable 4], Equiv (True, Or [Not (Equiv (Implies (False, Variable 7), True)), Not (True), And [False, Variable 6, Implies (Variable 2, Variable 7)]]), Equiv (Variable 3, And [True])]]), Variable 1)), Not (And [Implies (Not (Not (True)), Not (And [Variable 8, And [Variable 4, And [Variable 5, Variable 2], Equiv (Variable 7, True)], Not (Or []), Implies (Not (Or [Variable 7, Variable 2, Variable 4, Variable 9]), Equiv (False, Equiv (Variable 1, Variable 0)))])), Implies (Or [Not (Variable 7), Or [Variable 0, Equiv (Variable 8, And [Or [False], Variable 2, Variable 1, And [And [Variable 6, Variable 1, And [Or [Variable 9, False, False, Variable 2], Variable 7, True], True]]])]], Equiv (Variable 1, Variable 4))]), And [Not (Not (Not (Or []))), Equiv (Implies (And [Not (And [Variable 0, Not (Variable 3), Variable 5]), Or [Implies (Implies (Variable 1, Not (False)), Variable 3)], Implies (And [And [Or [Variable 5], True]], Equiv (Equiv (True, Or []), Or []))], Implies (Implies (Implies (Or [False, Variable 5], And [Variable 6, False, False]), Implies (True, False)), Variable 5)), Implies (True, Variable 6)), Variable 1, Variable 0]]), And [Equiv (Implies (Variable 6, Variable 5), Implies (And [Variable 3, Not (Variable 7), Not (False), Or [False]], Or [Variable 1, Variable 7]))]);    
val a1 = simplify lexp1;
val a2 = simplify lexp2;
val a3 = simplify lexp3;
val a4 = simplify lexp4;
val a5 = simplify lexp5;
val a6 = simplify lexp6;
val a7 = simplify lexp7;*)