/*
 * Copyright (c) 2025 PCC Xbase++ Runtime Library
 *
 * Numeric functions
 */

#include "xbrt.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

/* ABS() - Absolute value */
double xb_abs(double x) { return fabs(x); }

/* INT() - Integer part */
double xb_int(double x) { return floor(x); }

/* ROUND() - Round to N decimals */
double xb_round(double x, int decimals) {
	double mult = pow(10.0, decimals);
	return round(x * mult) / mult;
}

/* SQRT() - Square root */
double xb_sqrt(double x) { return sqrt(x); }

/* EXP() - Exponential */
double xb_exp(double x) { return exp(x); }

/* LOG() - Natural logarithm */
double xb_log(double x) { return log(x); }

/* LOG10() - Base-10 logarithm */
double xb_log10(double x) { return log10(x); }

/* SIN() - Sine */
double xb_sin(double x) { return sin(x); }

/* COS() - Cosine */
double xb_cos(double x) { return cos(x); }

/* TAN() - Tangent */
double xb_tan(double x) { return tan(x); }

/* MIN() - Minimum of two values */
double xb_min(double a, double b) { return a < b ? a : b; }

/* MAX() - Maximum of two values */
double xb_max(double a, double b) { return a > b ? a : b; }

/* MOD() - Modulo */
double xb_mod(double a, double b) { return fmod(a, b); }

/* RAND() - Random number [0,1) */
double xb_rand(void) { return (double)rand() / (RAND_MAX + 1.0); }

/* STR() - Convert number to string */
char *xb_str(double num, int width, int decimals) {
	char format[32];
	char *result;

	if (width <= 0) width = 10;
	if (decimals < 0) decimals = 0;

	snprintf(format, sizeof(format), "%%%d.%df", width, decimals);
	result = (char *)malloc(width + 10);
	if (result)
		snprintf(result, width + 10, format, num);

	return result ? result : strdup("");
}

/* VAL() - Convert string to number */
double xb_val(const char *str) {
	return str ? atof(str) : 0.0;
}
