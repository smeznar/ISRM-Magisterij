use "unittest.sml";

test("tests", [
	assert(10 = 10),
	assert(true),
	assert(not false),
	assert_false(false),
	assert_false(10 > 20),

	assert_eq(100, 10 * 10),
	assert_eq("Danes je lep dan", "Danes " ^ "je " ^ "lep " ^ "dan"),
	assert_eq(true, 10 < 20 andalso false orelse 5 * 5 = 25),
	assert_eq_real(10.01, 5.0 + 0.01 + 5.0),
	assert_equal(fn x => x * 2, 20, 40)
]);

val _ = OS.Process.exit(OS.Process.success);
