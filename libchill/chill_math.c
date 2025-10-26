/*
 * Copyright (c) 2025 PCC CHILL Runtime Library
 *
 * CHILL mathematical functions
 */

#include "chill.h"
#include <math.h>

/*
 * Integer mathematical functions
 */

chill_int_t
chill_abs_int(chill_int_t x)
{
	return (x < 0) ? -x : x;
}

chill_int_t
chill_sign_int(chill_int_t x)
{
	if (x < 0) return -1;
	if (x > 0) return 1;
	return 0;
}

chill_int_t
chill_max_int(chill_int_t a, chill_int_t b)
{
	return (a > b) ? a : b;
}

chill_int_t
chill_min_int(chill_int_t a, chill_int_t b)
{
	return (a < b) ? a : b;
}

/*
 * Real mathematical functions
 */

chill_real_t
chill_abs_real(chill_real_t x)
{
	return fabsf(x);
}

chill_real_t
chill_sign_real(chill_real_t x)
{
	if (x < 0.0f) return -1.0f;
	if (x > 0.0f) return 1.0f;
	return 0.0f;
}

chill_real_t
chill_max_real(chill_real_t a, chill_real_t b)
{
	return (a > b) ? a : b;
}

chill_real_t
chill_min_real(chill_real_t a, chill_real_t b)
{
	return (a < b) ? a : b;
}

/*
 * Trigonometric functions
 */

chill_real_t
chill_sin(chill_real_t x)
{
	return sinf(x);
}

chill_real_t
chill_cos(chill_real_t x)
{
	return cosf(x);
}

chill_real_t
chill_tan(chill_real_t x)
{
	return tanf(x);
}

chill_real_t
chill_arcsin(chill_real_t x)
{
	if (x < -1.0f || x > 1.0f) {
		chill_raise(CHILL_EXC_RANGEFAIL, "arcsin argument out of range");
		return 0.0f;
	}
	return asinf(x);
}

chill_real_t
chill_arccos(chill_real_t x)
{
	if (x < -1.0f || x > 1.0f) {
		chill_raise(CHILL_EXC_RANGEFAIL, "arccos argument out of range");
		return 0.0f;
	}
	return acosf(x);
}

chill_real_t
chill_arctan(chill_real_t x)
{
	return atanf(x);
}

/*
 * Exponential and logarithmic functions
 */

chill_real_t
chill_sqrt(chill_real_t x)
{
	if (x < 0.0f) {
		chill_raise(CHILL_EXC_RANGEFAIL, "sqrt of negative number");
		return 0.0f;
	}
	return sqrtf(x);
}

chill_real_t
chill_exp(chill_real_t x)
{
	return expf(x);
}

chill_real_t
chill_ln(chill_real_t x)
{
	if (x <= 0.0f) {
		chill_raise(CHILL_EXC_RANGEFAIL, "ln of non-positive number");
		return 0.0f;
	}
	return logf(x);
}

chill_real_t
chill_log10(chill_real_t x)
{
	if (x <= 0.0f) {
		chill_raise(CHILL_EXC_RANGEFAIL, "log10 of non-positive number");
		return 0.0f;
	}
	return log10f(x);
}

chill_real_t
chill_pow(chill_real_t base, chill_real_t exp)
{
	return powf(base, exp);
}

/*
 * Type conversion functions
 */

chill_int_t
chill_num(chill_char_t ch)
{
	return (chill_int_t)ch;
}

chill_char_t
chill_char(chill_int_t n)
{
	if (n < 0 || n > 255) {
		chill_raise(CHILL_EXC_RANGEFAIL, "char conversion out of range");
		return '\0';
	}
	return (chill_char_t)n;
}

chill_int_t
chill_pred(chill_int_t x)
{
	return x - 1;
}

chill_int_t
chill_succ(chill_int_t x)
{
	return x + 1;
}

chill_int_t
chill_real_to_int(chill_real_t x)
{
	return (chill_int_t)x;
}

chill_real_t
chill_int_to_real(chill_int_t x)
{
	return (chill_real_t)x;
}
