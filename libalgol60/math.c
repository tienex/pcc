/*
 * Copyright (c) 2025 PCC ALGOL 60+ Runtime Library
 *
 * Mathematical functions for ALGOL 60+ programs
 * Implements the standard ALGOL 60 transfer functions
 */

#include <math.h>
#include <stdlib.h>
#include <errno.h>
#include <float.h>
#include "algol60.h"

/*
 * Absolute value functions
 */

algol_integer
algol_abs_integer(algol_integer x)
{
	return (x < 0) ? -x : x;
}

algol_real
algol_abs_real(algol_real x)
{
	return fabs(x);
}

/*
 * Sign function
 * Returns -1 if x < 0, 0 if x == 0, +1 if x > 0
 */

algol_integer
algol_sign_integer(algol_integer x)
{
	if (x < 0)
		return -1;
	else if (x > 0)
		return 1;
	else
		return 0;
}

algol_integer
algol_sign_real(algol_real x)
{
	if (x < 0.0)
		return -1;
	else if (x > 0.0)
		return 1;
	else
		return 0;
}

/*
 * Square root
 */

algol_real
algol_sqrt(algol_real x)
{
	if (x < 0.0) {
		algol_error(ALGOL_ERR_DOMAIN, "sqrt: argument must be non-negative");
		return 0.0;
	}
	return sqrt(x);
}

/*
 * Trigonometric functions
 */

algol_real
algol_sin(algol_real x)
{
	return sin(x);
}

algol_real
algol_cos(algol_real x)
{
	return cos(x);
}

algol_real
algol_tan(algol_real x)
{
	return tan(x);
}

algol_real
algol_arctan(algol_real x)
{
	return atan(x);
}

/*
 * Exponential and logarithm functions
 */

algol_real
algol_exp(algol_real x)
{
	algol_real result;

	errno = 0;
	result = exp(x);

	if (errno == ERANGE) {
		if (result == HUGE_VAL || result == -HUGE_VAL) {
			algol_error(ALGOL_ERR_RANGE, "exp: overflow");
		}
	}

	return result;
}

algol_real
algol_ln(algol_real x)
{
	if (x <= 0.0) {
		algol_error(ALGOL_ERR_DOMAIN, "ln: argument must be positive");
		return 0.0;
	}
	return log(x);
}

algol_real
algol_log10(algol_real x)
{
	if (x <= 0.0) {
		algol_error(ALGOL_ERR_DOMAIN, "log10: argument must be positive");
		return 0.0;
	}
	return log10(x);
}

/*
 * Entier function (floor)
 * Returns the greatest integer less than or equal to x
 */

algol_integer
algol_entier(algol_real x)
{
	return (algol_integer)floor(x);
}

/*
 * Power function
 */

algol_real
algol_power(algol_real base, algol_real exponent)
{
	algol_real result;

	/* Special cases */
	if (base == 0.0) {
		if (exponent <= 0.0) {
			algol_error(ALGOL_ERR_DOMAIN, "power: 0^0 or 0^(negative) is undefined");
			return 0.0;
		}
		return 0.0;
	}

	if (base < 0.0 && floor(exponent) != exponent) {
		algol_error(ALGOL_ERR_DOMAIN, "power: negative base with non-integer exponent");
		return 0.0;
	}

	errno = 0;
	result = pow(base, exponent);

	if (errno == ERANGE) {
		algol_error(ALGOL_ERR_RANGE, "power: overflow or underflow");
	} else if (errno == EDOM) {
		algol_error(ALGOL_ERR_DOMAIN, "power: domain error");
	}

	return result;
}

algol_integer
algol_power_int(algol_integer base, algol_integer exponent)
{
	algol_integer result;
	algol_integer i;

	/* Special cases */
	if (exponent < 0) {
		algol_error(ALGOL_ERR_DOMAIN, "power: negative exponent for integer power");
		return 0;
	}

	if (exponent == 0)
		return 1;

	if (base == 0)
		return 0;

	/* Simple iterative multiplication */
	result = 1;
	for (i = 0; i < exponent; i++) {
		/* Check for overflow */
		if (result > INT32_MAX / base || result < INT32_MIN / base) {
			algol_error(ALGOL_ERR_RANGE, "power: integer overflow");
			return 0;
		}
		result *= base;
	}

	return result;
}

/*
 * Type conversions
 */

algol_real
algol_int_to_real(algol_integer i)
{
	return (algol_real)i;
}

algol_integer
algol_real_to_int(algol_real r)
{
	/* Truncate towards zero */
	return (algol_integer)r;
}

algol_integer
algol_bool_to_int(algol_boolean b)
{
	return b ? 1 : 0;
}

algol_boolean
algol_int_to_bool(algol_integer i)
{
	return (i != 0) ? ALGOL_TRUE : ALGOL_FALSE;
}

/*
 * Random number generation (extension)
 */

algol_real
algol_random(void)
{
	/* Return random value between 0.0 and 1.0 */
	return (algol_real)rand() / (algol_real)RAND_MAX;
}

algol_integer
algol_random_int(algol_integer min, algol_integer max)
{
	if (min > max) {
		algol_error(ALGOL_ERR_DOMAIN, "random_int: min > max");
		return min;
	}

	return min + (rand() % (max - min + 1));
}

/*
 * Time function (extension)
 */

#include <time.h>

algol_real
algol_time(void)
{
	return (algol_real)time(NULL);
}
