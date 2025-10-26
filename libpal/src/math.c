/*
 * Copyright (c) 2025 PCC Paradox PAL Runtime Library
 *
 * Math and numeric functions
 */

#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "../include/palrt.h"

/* Random number generator initialized flag */
static int random_initialized = 0;

PAL_Number pal_abs(PAL_Number x)
{
	return fabs(x);
}

PAL_Number pal_sqrt(PAL_Number x)
{
	if (x < 0.0)
		return 0.0;  /* PAL returns 0 for negative sqrt */
	return sqrt(x);
}

PAL_Number pal_sin(PAL_Number x)
{
	return sin(x);
}

PAL_Number pal_cos(PAL_Number x)
{
	return cos(x);
}

PAL_Number pal_tan(PAL_Number x)
{
	return tan(x);
}

PAL_Number pal_exp(PAL_Number x)
{
	return exp(x);
}

PAL_Number pal_ln(PAL_Number x)
{
	if (x <= 0.0)
		return 0.0;  /* PAL returns 0 for invalid ln */
	return log(x);
}

PAL_Number pal_log(PAL_Number x)
{
	if (x <= 0.0)
		return 0.0;  /* PAL returns 0 for invalid log */
	return log10(x);
}

PAL_Number pal_round(PAL_Number x)
{
	return round(x);
}

PAL_Number pal_trunc(PAL_Number x)
{
	return trunc(x);
}

PAL_LongInt pal_int(PAL_Number x)
{
	return (PAL_LongInt)trunc(x);
}

PAL_Number pal_frac(PAL_Number x)
{
	return x - trunc(x);
}

PAL_Number pal_random(void)
{
	if (!random_initialized) {
		srand((unsigned int)time(NULL));
		random_initialized = 1;
	}
	return (PAL_Number)rand() / (PAL_Number)RAND_MAX;
}

PAL_Number pal_max(PAL_Number a, PAL_Number b)
{
	return (a > b) ? a : b;
}

PAL_Number pal_min(PAL_Number a, PAL_Number b)
{
	return (a < b) ? a : b;
}

PAL_Number pal_power(PAL_Number base, PAL_Number exponent)
{
	return pow(base, exponent);
}

PAL_LongInt pal_mod(PAL_LongInt a, PAL_LongInt b)
{
	if (b == 0)
		return 0;  /* PAL returns 0 for division by zero */
	return a % b;
}
