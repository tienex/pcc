/*
 * Copyright (c) 2025 PCC Wirth Languages Runtime Library
 *
 * Mathematical Functions
 * - Standard math functions (common across all Wirth languages)
 * - Rounding and truncation
 * - Bit operations (Modula-2, Oberon, Ada)
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "wirthrt.h"

/*
 * Standard Mathematical Functions
 */

int wirth_abs_int(int x) {
	return abs(x);
}

double wirth_abs_real(double x) {
	return fabs(x);
}

int wirth_odd(int x) {
	return x & 1;
}

double wirth_sqrt(double x) {
	if (x < 0.0) {
		wirth_runtime_error(WIRTH_ERR_INVALID_OPERATION, "SQRT of negative number");
		return 0.0;
	}
	return sqrt(x);
}

double wirth_sin(double x) {
	return sin(x);
}

double wirth_cos(double x) {
	return cos(x);
}

double wirth_tan(double x) {
	return tan(x);
}

double wirth_arctan(double x) {
	return atan(x);
}

double wirth_arctan2(double y, double x) {
	return atan2(y, x);
}

double wirth_exp(double x) {
	double result = exp(x);
	if (result == HUGE_VAL) {
		wirth_runtime_error(WIRTH_ERR_REAL_OVERFLOW, "EXP overflow");
	}
	return result;
}

double wirth_ln(double x) {
	if (x <= 0.0) {
		wirth_runtime_error(WIRTH_ERR_INVALID_OPERATION, "LN of non-positive number");
		return 0.0;
	}
	return log(x);
}

double wirth_log10(double x) {
	if (x <= 0.0) {
		wirth_runtime_error(WIRTH_ERR_INVALID_OPERATION, "LOG of non-positive number");
		return 0.0;
	}
	return log10(x);
}

double wirth_power(double x, double y) {
	double result = pow(x, y);
	if (result == HUGE_VAL) {
		wirth_runtime_error(WIRTH_ERR_REAL_OVERFLOW, "POWER overflow");
	} else if (result == 0.0 && x != 0.0) {
		wirth_runtime_error(WIRTH_ERR_REAL_UNDERFLOW, "POWER underflow");
	}
	return result;
}

/*
 * Rounding and Truncation Functions
 */

int wirth_floor(double x) {
	return (int)floor(x);
}

int wirth_ceil(double x) {
	return (int)ceil(x);
}

int wirth_round(double x) {
	return (int)round(x);
}

int wirth_trunc(double x) {
	return (int)trunc(x);
}

int wirth_entier(double x) {
	/* Modula-2 ENTIER is same as FLOOR */
	return (int)floor(x);
}

/*
 * Bit Operations
 */

unsigned int wirth_bit_and(unsigned int a, unsigned int b) {
	return a & b;
}

unsigned int wirth_bit_or(unsigned int a, unsigned int b) {
	return a | b;
}

unsigned int wirth_bit_xor(unsigned int a, unsigned int b) {
	return a ^ b;
}

unsigned int wirth_bit_not(unsigned int a) {
	return ~a;
}

unsigned int wirth_lsh(unsigned int value, int shift) {
	/* Logical shift (left for positive, right for negative) */
	if (shift >= 0) {
		return value << shift;
	} else {
		return value >> (-shift);
	}
}

unsigned int wirth_rot(unsigned int value, int shift) {
	/* Rotation (assumes 32-bit unsigned int) */
	const int bits = 32;
	shift = shift % bits;
	if (shift < 0) {
		shift += bits;
	}

	return (value << shift) | (value >> (bits - shift));
}
