/*
 * Copyright (c) 2025 PCC BASIC Compiler
 *
 * BASIC Runtime Library - Math Functions
 */

#include "basicrt.h"

int rnd_initialized = 0;

/*
 * ABS - Absolute value
 */

double
basic_abs_d(double x)
{
	return fabs(x);
}

int
basic_abs_i(int x)
{
	return abs(x);
}

/*
 * Trigonometric functions
 */

double
basic_sin(double x)
{
	return sin(x);
}

double
basic_cos(double x)
{
	return cos(x);
}

double
basic_tan(double x)
{
	return tan(x);
}

double
basic_atn(double x)
{
	return atan(x);
}

/*
 * Exponential and logarithmic functions
 */

double
basic_exp(double x)
{
	return exp(x);
}

double
basic_log(double x)
{
	return log(x);
}

/*
 * SQR - Square root
 */

double
basic_sqr(double x)
{
	return sqrt(x);
}

/*
 * SGN - Sign function
 * Returns -1 if x < 0, 0 if x = 0, 1 if x > 0
 */

int
basic_sgn(double x)
{
	if (x < 0.0)
		return -1;
	if (x > 0.0)
		return 1;
	return 0;
}

/*
 * FIX - Truncate to integer (towards zero)
 */

int
basic_fix(double x)
{
	return (int)x;
}

/*
 * INT - Floor (largest integer <= x)
 */

int
basic_int(double x)
{
	return (int)floor(x);
}

/*
 * RND - Random number [0,1)
 */

double
basic_rnd(void)
{
	if (!rnd_initialized) {
		basic_randomize();
	}
	return (double)rand() / (double)RAND_MAX;
}

/*
 * RANDOMIZE - Seed random number generator
 */

void
basic_randomize(void)
{
	srand((unsigned int)time(NULL));
	rnd_initialized = 1;
}

/*
 * Type conversion functions
 */

int
basic_cint(double x)
{
	return (int)(x + (x >= 0 ? 0.5 : -0.5));  /* Round to nearest */
}

long
basic_clng(double x)
{
	return (long)(x + (x >= 0 ? 0.5 : -0.5));  /* Round to nearest */
}

float
basic_csng(double x)
{
	return (float)x;
}

double
basic_cdbl(float x)
{
	return (double)x;
}
