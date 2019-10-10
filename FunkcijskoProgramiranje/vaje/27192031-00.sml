fun next (n: int): int = n + 1;

fun add (a: int, b: int): int = a + b;

(*
use "unittest.sml";

test("tests", [
	assert_eq(next(1),2),
	assert_eq(next(~1),0),
	assert_eq(add(1,2),3),
    assert_eq(add(1256,68798), 70054),
    assert_eq(next(123456789), 123456790)
]);

val _ = OS.Process.exit(OS.Process.success);
*)