/*
 * Copyright (c) 2025 PCC PL/I Runtime Library
 *
 * Mathematical functions implementation
 */

#include "pli_runtime.h"
#include <math.h>
#include <stdlib.h>

/* Absolute value */
pli_fixed_t pli_abs_fixed(pli_fixed_t x) {
	return abs(x);
}

pli_float_long_t pli_abs_float(pli_float_long_t x) {
	return fabs(x);
}

/* Ceiling and floor */
pli_fixed_t pli_ceil(pli_float_long_t x) {
	return (pli_fixed_t)ceil(x);
}

pli_fixed_t pli_floor(pli_float_long_t x) {
	return (pli_fixed_t)floor(x);
}

/* Maximum and minimum */
pli_fixed_t pli_max_fixed(pli_fixed_t a, pli_fixed_t b) {
	return (a > b) ? a : b;
}

pli_fixed_t pli_min_fixed(pli_fixed_t a, pli_fixed_t b) {
	return (a < b) ? a : b;
}

pli_float_long_t pli_max_float(pli_float_long_t a, pli_float_long_t b) {
	return (a > b) ? a : b;
}

pli_float_long_t pli_min_float(pli_float_long_t a, pli_float_long_t b) {
	return (a < b) ? a : b;
}

/* Modulo */
pli_fixed_t pli_mod(pli_fixed_t a, pli_fixed_t b) {
	if (b == 0) {
		pli_signal("ZERODIVIDE");
		return 0;
	}
	return a % b;
}

/* Rounding and truncation */
pli_fixed_t pli_round(pli_float_long_t x) {
	return (pli_fixed_t)round(x);
}

pli_fixed_t pli_sign(pli_fixed_t x) {
	if (x > 0) return 1;
	if (x < 0) return -1;
	return 0;
}

pli_fixed_t pli_trunc(pli_float_long_t x) {
	return (pli_fixed_t)trunc(x);
}

/* Trigonometric functions */
pli_float_long_t pli_acos(pli_float_long_t x) {
	return acos(x);
}

pli_float_long_t pli_asin(pli_float_long_t x) {
	return asin(x);
}

pli_float_long_t pli_atan(pli_float_long_t x) {
	return atan(x);
}

pli_float_long_t pli_atanh(pli_float_long_t x) {
	return atanh(x);
}

pli_float_long_t pli_cos(pli_float_long_t x) {
	return cos(x);
}

pli_float_long_t pli_cosh(pli_float_long_t x) {
	return cosh(x);
}

pli_float_long_t pli_sin(pli_float_long_t x) {
	return sin(x);
}

pli_float_long_t pli_sinh(pli_float_long_t x) {
	return sinh(x);
}

pli_float_long_t pli_tan(pli_float_long_t x) {
	return tan(x);
}

pli_float_long_t pli_tanh(pli_float_long_t x) {
	return tanh(x);
}

/* Exponential and logarithmic */
pli_float_long_t pli_exp(pli_float_long_t x) {
	return exp(x);
}

pli_float_long_t pli_log(pli_float_long_t x) {
	if (x <= 0.0) {
		pli_signal("ERROR");
		return 0.0;
	}
	return log(x);
}

pli_float_long_t pli_log10(pli_float_long_t x) {
	if (x <= 0.0) {
		pli_signal("ERROR");
		return 0.0;
	}
	return log10(x);
}

pli_float_long_t pli_log2(pli_float_long_t x) {
	if (x <= 0.0) {
		pli_signal("ERROR");
		return 0.0;
	}
	return log2(x);
}

pli_float_long_t pli_sqrt(pli_float_long_t x) {
	if (x < 0.0) {
		pli_signal("ERROR");
		return 0.0;
	}
	return sqrt(x);
}
