/*
 * Copyright (c) 2025 PCC Pascal Runtime Library
 *
 * Pascal Mathematical Functions
 *
 * Implements standard mathematical functions for Pascal programs.
 */

#include <math.h>
#include <stdlib.h>
#include "pascalrt.h"

/*
 * Absolute Value Functions
 */

int
pascal_abs_int(int x)
{
	return abs(x);
}

double
pascal_abs_real(double x)
{
	return fabs(x);
}

/*
 * Square Functions
 */

int
pascal_sqr_int(int x)
{
	return x * x;
}

double
pascal_sqr_real(double x)
{
	return x * x;
}

/*
 * Square Root
 */

double
pascal_sqrt(double x)
{
	if (x < 0.0) {
		pascal_runtime_error(PASCAL_ERR_INVALID_OPERATION,
		                     "Square root of negative number");
		return 0.0;
	}
	return sqrt(x);
}

/*
 * Trigonometric Functions
 */

double
pascal_sin(double x)
{
	return sin(x);
}

double
pascal_cos(double x)
{
	return cos(x);
}

double
pascal_arctan(double x)
{
	return atan(x);
}

/*
 * Exponential and Logarithmic Functions
 */

double
pascal_exp(double x)
{
	return exp(x);
}

double
pascal_ln(double x)
{
	if (x <= 0.0) {
		pascal_runtime_error(PASCAL_ERR_INVALID_OPERATION,
		                     "Natural logarithm of non-positive number");
		return 0.0;
	}
	return log(x);
}

/*
 * Rounding Functions
 */

int
pascal_round(double x)
{
	/* Pascal-style rounding: rounds to nearest integer */
	/* Ties (0.5) round away from zero */
	if (x >= 0.0) {
		return (int)(x + 0.5);
	} else {
		return (int)(x - 0.5);
	}
}

int
pascal_trunc(double x)
{
	/* Truncate towards zero */
	return (int)x;
}

/*
 * Ordinal Functions
 */

int
pascal_odd(int x)
{
	return (x & 1) != 0;
}

int
pascal_ord_char(char c)
{
	return (int)(unsigned char)c;
}

int
pascal_ord_bool(int b)
{
	return b ? 1 : 0;
}

char
pascal_chr(int x)
{
	if (x < 0 || x > 255) {
		pascal_runtime_error(PASCAL_ERR_RANGE_CHECK, "Chr value out of range");
		return 0;
	}
	return (char)x;
}

/*
 * Successor and Predecessor Functions
 */

int
pascal_succ_int(int x)
{
	return x + 1;
}

int
pascal_pred_int(int x)
{
	return x - 1;
}

char
pascal_succ_char(char c)
{
	if (c == 255) {
		pascal_runtime_error(PASCAL_ERR_RANGE_CHECK, "Succ of maximum char");
		return c;
	}
	return c + 1;
}

char
pascal_pred_char(char c)
{
	if (c == 0) {
		pascal_runtime_error(PASCAL_ERR_RANGE_CHECK, "Pred of minimum char");
		return c;
	}
	return c - 1;
}

/*
 * Type Conversion Functions
 */

void
pascal_int_to_str(int value, char *str)
{
	sprintf(str, "%d", value);
}

void
pascal_real_to_str(double value, char *str)
{
	sprintf(str, "%g", value);
}

int
pascal_str_to_int(const char *str, int *error)
{
	char *endptr;
	long value;

	*error = 0;
	value = strtol(str, &endptr, 10);

	if (*endptr != '\0') {
		*error = 1;  /* Conversion error */
		return 0;
	}

	return (int)value;
}

double
pascal_str_to_real(const char *str, int *error)
{
	char *endptr;
	double value;

	*error = 0;
	value = strtod(str, &endptr);

	if (*endptr != '\0') {
		*error = 1;  /* Conversion error */
		return 0.0;
	}

	return value;
}

/*
 * Utility Functions (Turbo/Delphi extensions)
 */

void
pascal_inc_int(int *x, int delta)
{
	*x += delta;
}

void
pascal_dec_int(int *x, int delta)
{
	*x -= delta;
}

uint8_t
pascal_hi_byte(uint16_t x)
{
	return (uint8_t)(x >> 8);
}

uint8_t
pascal_lo_byte(uint16_t x)
{
	return (uint8_t)(x & 0xFF);
}

uint16_t
pascal_swap_word(uint16_t x)
{
	return (x >> 8) | (x << 8);
}

/*
 * Range Checking Functions
 */

int
pascal_range_check_int(int value, int min, int max)
{
	if (value < min || value > max) {
		pascal_runtime_error(PASCAL_ERR_RANGE_CHECK, "Value out of range");
		return min;  /* Return minimum value as fallback */
	}
	return value;
}

char
pascal_range_check_char(int value)
{
	if (value < 0 || value > 255) {
		pascal_runtime_error(PASCAL_ERR_RANGE_CHECK, "Character value out of range");
		return 0;
	}
	return (char)value;
}
