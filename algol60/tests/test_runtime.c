/*
 * Test program for ALGOL 60+ runtime library
 * Demonstrates the runtime library without needing the compiler
 */

#include "../../libalgol60/algol60.h"

int main(int argc, char **argv)
{
	algol_integer n;
	algol_real x, y;

	/* Initialize runtime */
	algol_init(argc, argv);

	/* Test string output */
	algol_write_string("========================================");
	algol_write_newline();
	algol_write_string("  ALGOL 60+ Runtime Library Test");
	algol_write_newline();
	algol_write_string("========================================");
	algol_write_newline();
	algol_write_newline();

	/* Test 1: Hello World */
	algol_write_string("Test 1: Hello World");
	algol_write_newline();
	algol_write_string("Hello, ALGOL 60+!");
	algol_write_newline();
	algol_write_newline();

	/* Test 2: Integer output */
	algol_write_string("Test 2: Integer output");
	algol_write_newline();
	algol_write_string("The answer is: ");
	algol_write_integer(42);
	algol_write_newline();
	algol_write_newline();

	/* Test 3: Real number output */
	algol_write_string("Test 3: Real number output");
	algol_write_newline();
	algol_write_string("Pi = ");
	algol_write_real(3.14159265359);
	algol_write_newline();
	algol_write_newline();

	/* Test 4: Boolean output */
	algol_write_string("Test 4: Boolean output");
	algol_write_newline();
	algol_write_string("true = ");
	algol_write_boolean(ALGOL_TRUE);
	algol_write_string(", false = ");
	algol_write_boolean(ALGOL_FALSE);
	algol_write_newline();
	algol_write_newline();

	/* Test 5: Mathematical functions */
	algol_write_string("Test 5: Mathematical functions");
	algol_write_newline();

	x = 2.0;
	y = algol_sqrt(x);
	algol_write_string("sqrt(2.0) = ");
	algol_write_real(y);
	algol_write_newline();

	x = 1.5707963;  /* π/2 */
	y = algol_sin(x);
	algol_write_string("sin(π/2) = ");
	algol_write_real(y);
	algol_write_newline();

	x = 1.0;
	y = algol_exp(x);
	algol_write_string("exp(1.0) = ");
	algol_write_real(y);
	algol_write_newline();

	x = 2.71828;
	y = algol_ln(x);
	algol_write_string("ln(e) = ");
	algol_write_real(y);
	algol_write_newline();
	algol_write_newline();

	/* Test 6: Absolute value and sign */
	algol_write_string("Test 6: Absolute value and sign");
	algol_write_newline();

	n = -42;
	algol_write_string("abs(-42) = ");
	algol_write_integer(algol_abs_integer(n));
	algol_write_newline();

	algol_write_string("sign(-42) = ");
	algol_write_integer(algol_sign_integer(n));
	algol_write_newline();

	algol_write_string("sign(0) = ");
	algol_write_integer(algol_sign_integer(0));
	algol_write_newline();

	algol_write_string("sign(42) = ");
	algol_write_integer(algol_sign_integer(42));
	algol_write_newline();
	algol_write_newline();

	/* Test 7: Entier function */
	algol_write_string("Test 7: Entier (floor) function");
	algol_write_newline();

	x = 3.7;
	algol_write_string("entier(3.7) = ");
	algol_write_integer(algol_entier(x));
	algol_write_newline();

	x = -3.7;
	algol_write_string("entier(-3.7) = ");
	algol_write_integer(algol_entier(x));
	algol_write_newline();
	algol_write_newline();

	/* Test 8: String operations */
	algol_write_string("Test 8: String operations");
	algol_write_newline();

	algol_string s1 = algol_string_create("Hello");
	algol_string s2 = algol_string_create(" World");
	algol_string s3 = algol_string_concat(s1, s2);

	algol_write_string("concat('Hello', ' World') = '");
	algol_write_string(s3);
	algol_write_string("'");
	algol_write_newline();

	algol_write_string("length('Hello World') = ");
	algol_write_integer((algol_integer)algol_string_length(s3));
	algol_write_newline();

	algol_string_free(s1);
	algol_string_free(s2);
	algol_string_free(s3);
	algol_write_newline();

	/* Test 9: Arrays */
	algol_write_string("Test 9: Array operations");
	algol_write_newline();

	int lower[1] = {1};
	int upper[1] = {5};
	algol_array_t *arr = algol_array_create(1, lower, upper, sizeof(algol_integer));

	/* Fill array with squares */
	for (int i = 1; i <= 5; i++) {
		int indices[1] = {i};
		algol_integer *elem = (algol_integer *)algol_array_element(arr, indices);
		*elem = i * i;
	}

	/* Print array */
	algol_write_string("Array[1:5] of squares: ");
	for (int i = 1; i <= 5; i++) {
		int indices[1] = {i};
		algol_integer *elem = (algol_integer *)algol_array_element(arr, indices);
		algol_write_integer(*elem);
		algol_write_string(" ");
	}
	algol_write_newline();

	algol_array_free(arr);
	algol_write_newline();

	/* Test 10: Memory statistics */
	algol_write_string("Test 10: Memory statistics");
	algol_write_newline();
	algol_write_string("Peak memory allocated: ");
	algol_write_integer((algol_integer)algol_memory_peak());
	algol_write_string(" bytes");
	algol_write_newline();
	algol_write_newline();

	/* Finalize */
	algol_write_string("========================================");
	algol_write_newline();
	algol_write_string("  All tests completed successfully!");
	algol_write_newline();
	algol_write_string("========================================");
	algol_write_newline();

	algol_fini();
	return 0;
}
