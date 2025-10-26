/*
 * Test program for BASIC runtime library
 * Demonstrates usage of libbasic functions
 */

#include "../libbasic/basicrt.h"
#include <stdio.h>

int main(void)
{
	basic_string_t *s1, *s2, *result;
	int i;

	basic_runtime_init();

	/* Test I/O functions */
	basic_print_string("BASIC Runtime Library Test");
	basic_print_newline();
	basic_print_string("==========================");
	basic_print_newline();
	basic_print_newline();

	/* Test string functions */
	basic_print_string("String Functions:");
	basic_print_newline();

	s1 = basic_string_new("Hello, World!");
	basic_print_string("  Original: ");
	basic_print_string(s1->data);
	basic_print_newline();

	result = basic_left(s1, 5);
	basic_print_string("  LEFT$(\"Hello, World!\", 5) = ");
	basic_print_string(result->data);
	basic_print_newline();
	basic_string_free(result);

	result = basic_right(s1, 6);
	basic_print_string("  RIGHT$(\"Hello, World!\", 6) = ");
	basic_print_string(result->data);
	basic_print_newline();
	basic_string_free(result);

	result = basic_mid(s1, 8, 5);
	basic_print_string("  MID$(\"Hello, World!\", 8, 5) = ");
	basic_print_string(result->data);
	basic_print_newline();
	basic_string_free(result);

	basic_print_string("  LEN(\"Hello, World!\") = ");
	basic_print_int(basic_len(s1));
	basic_print_newline();

	s2 = basic_string_new("World");
	i = basic_instr(s1, s2);
	basic_print_string("  INSTR(\"Hello, World!\", \"World\") = ");
	basic_print_int(i);
	basic_print_newline();
	basic_string_free(s2);

	result = basic_ucase(s1);
	basic_print_string("  UCASE$(\"Hello, World!\") = ");
	basic_print_string(result->data);
	basic_print_newline();
	basic_string_free(result);

	result = basic_lcase(s1);
	basic_print_string("  LCASE$(\"Hello, World!\") = ");
	basic_print_string(result->data);
	basic_print_newline();
	basic_string_free(result);

	basic_string_free(s1);
	basic_print_newline();

	/* Test math functions */
	basic_print_string("Math Functions:");
	basic_print_newline();

	basic_print_string("  ABS(-42) = ");
	basic_print_int(basic_abs_i(-42));
	basic_print_newline();

	basic_print_string("  SGN(-5) = ");
	basic_print_int(basic_sgn(-5.0));
	basic_print_newline();

	basic_print_string("  SGN(0) = ");
	basic_print_int(basic_sgn(0.0));
	basic_print_newline();

	basic_print_string("  SGN(5) = ");
	basic_print_int(basic_sgn(5.0));
	basic_print_newline();

	basic_print_string("  SQR(16) = ");
	basic_print_double(basic_sqr(16.0));
	basic_print_newline();

	basic_print_string("  INT(3.7) = ");
	basic_print_int(basic_int(3.7));
	basic_print_newline();

	basic_print_string("  FIX(-3.7) = ");
	basic_print_int(basic_fix(-3.7));
	basic_print_newline();

	basic_print_newline();

	/* Test type conversion */
	basic_print_string("Type Conversion:");
	basic_print_newline();

	basic_print_string("  CINT(3.7) = ");
	basic_print_int(basic_cint(3.7));
	basic_print_newline();

	basic_print_string("  CLNG(1234567.89) = ");
	basic_print_long(basic_clng(1234567.89));
	basic_print_newline();

	basic_print_newline();

	/* Test system functions */
	basic_print_string("System Functions:");
	basic_print_newline();

	basic_print_string("  TIMER = ");
	basic_print_long(basic_timer());
	basic_print_string(" (seconds since midnight)");
	basic_print_newline();

	basic_print_newline();
	basic_print_string("Test completed successfully!");
	basic_print_newline();

	basic_runtime_cleanup();
	return 0;
}
