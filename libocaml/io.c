/*
 * Copyright (c) 2025 PCC OCaml Runtime Library
 *
 * I/O and conversion functions
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "ocaml_runtime.h"

/* ===== I/O Operations ===== */

/*
 * Print integer
 */
void
ocaml_print_int(ocaml_value_t val)
{
	if (!IS_INT(val)) {
		fprintf(stderr, "ocaml_print_int: not an integer\n");
		return;
	}

	printf("%ld", (long)INT_VAL(val));
	fflush(stdout);
}

/*
 * Print float
 */
void
ocaml_print_float(double val)
{
	printf("%g", val);
	fflush(stdout);
}

/*
 * Print character
 */
void
ocaml_print_char(ocaml_value_t val)
{
	if (!IS_INT(val)) {
		fprintf(stderr, "ocaml_print_char: not a character\n");
		return;
	}

	printf("%c", (char)INT_VAL(val));
	fflush(stdout);
}

/*
 * Print string
 */
void
ocaml_print_string(ocaml_value_t val)
{
	if (!IS_BLOCK(val) || TAG(val) != TAG_STRING) {
		fprintf(stderr, "ocaml_print_string: not a string\n");
		return;
	}

	printf("%s", (const char *)val);
	fflush(stdout);
}

/*
 * Print newline
 */
void
ocaml_print_newline(void)
{
	printf("\n");
	fflush(stdout);
}

/*
 * Read integer from stdin
 */
ocaml_value_t
ocaml_read_int(void)
{
	long val;

	if (scanf("%ld", &val) != 1) {
		fprintf(stderr, "ocaml_read_int: invalid input\n");
		return VAL_INT(0);
	}

	return VAL_INT(val);
}

/*
 * Read line from stdin
 */
ocaml_value_t
ocaml_read_line(void)
{
	char buffer[4096];

	if (fgets(buffer, sizeof(buffer), stdin) == NULL) {
		return ocaml_string_make("");
	}

	/* Remove trailing newline */
	size_t len = strlen(buffer);
	if (len > 0 && buffer[len-1] == '\n')
		buffer[len-1] = '\0';

	return ocaml_string_make(buffer);
}

/* ===== Conversion Functions ===== */

/*
 * Convert float to int
 */
ocaml_value_t
ocaml_int_of_float(double f)
{
	return VAL_INT((intptr_t)f);
}

/*
 * Convert int to float
 */
double
ocaml_float_of_int(ocaml_value_t i)
{
	if (!IS_INT(i)) {
		fprintf(stderr, "ocaml_float_of_int: not an integer\n");
		return 0.0;
	}

	return (double)INT_VAL(i);
}

/*
 * Convert int to char
 */
ocaml_value_t
ocaml_char_of_int(ocaml_value_t i)
{
	if (!IS_INT(i)) {
		fprintf(stderr, "ocaml_char_of_int: not an integer\n");
		return VAL_INT(0);
	}

	intptr_t val = INT_VAL(i);
	if (val < 0 || val > 255) {
		fprintf(stderr, "ocaml_char_of_int: value out of range\n");
		return VAL_INT(0);
	}

	return VAL_INT(val);
}

/*
 * Convert char to int
 */
ocaml_value_t
ocaml_int_of_char(ocaml_value_t c)
{
	if (!IS_INT(c)) {
		fprintf(stderr, "ocaml_int_of_char: not a character\n");
		return VAL_INT(0);
	}

	return c;
}

/*
 * Convert int to string
 */
ocaml_value_t
ocaml_string_of_int(ocaml_value_t i)
{
	char buffer[32];

	if (!IS_INT(i)) {
		fprintf(stderr, "ocaml_string_of_int: not an integer\n");
		return ocaml_string_make("");
	}

	snprintf(buffer, sizeof(buffer), "%ld", (long)INT_VAL(i));

	return ocaml_string_make(buffer);
}

/*
 * Convert string to int
 */
ocaml_value_t
ocaml_int_of_string(ocaml_value_t s)
{
	const char *str;
	long val;
	char *endptr;

	if (!IS_BLOCK(s) || TAG(s) != TAG_STRING) {
		fprintf(stderr, "ocaml_int_of_string: not a string\n");
		return VAL_INT(0);
	}

	str = (const char *)s;
	val = strtol(str, &endptr, 10);

	if (*endptr != '\0') {
		fprintf(stderr, "ocaml_int_of_string: invalid integer\n");
		return VAL_INT(0);
	}

	return VAL_INT(val);
}

/* ===== Math Functions ===== */

double
ocaml_sqrt(double x)
{
	return sqrt(x);
}

double
ocaml_sin(double x)
{
	return sin(x);
}

double
ocaml_cos(double x)
{
	return cos(x);
}

double
ocaml_exp(double x)
{
	return exp(x);
}

double
ocaml_log(double x)
{
	return log(x);
}
