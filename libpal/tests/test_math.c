/*
 * Test program for PAL runtime library - Math functions
 */

#include <stdio.h>
#include <assert.h>
#include <math.h>
#include "../include/palrt.h"

#define EPSILON 0.0001

void test_basic_math(void)
{
	PAL_Number result;

	printf("Testing basic math functions...\n");

	/* Absolute value */
	assert(pal_abs(-5.5) == 5.5);
	assert(pal_abs(3.2) == 3.2);
	printf("  pal_abs: OK\n");

	/* Square root */
	result = pal_sqrt(16.0);
	assert(fabs(result - 4.0) < EPSILON);
	printf("  pal_sqrt: OK\n");

	/* Power */
	result = pal_power(2.0, 3.0);
	assert(fabs(result - 8.0) < EPSILON);
	printf("  pal_power: OK\n");

	/* Max/Min */
	assert(pal_max(5.0, 10.0) == 10.0);
	assert(pal_min(5.0, 10.0) == 5.0);
	printf("  pal_max/pal_min: OK\n");

	printf("Basic math tests: PASSED\n\n");
}

void test_trig_functions(void)
{
	PAL_Number result;

	printf("Testing trigonometric functions...\n");

	/* Sine */
	result = pal_sin(0.0);
	assert(fabs(result - 0.0) < EPSILON);
	printf("  pal_sin: OK\n");

	/* Cosine */
	result = pal_cos(0.0);
	assert(fabs(result - 1.0) < EPSILON);
	printf("  pal_cos: OK\n");

	/* Tangent */
	result = pal_tan(0.0);
	assert(fabs(result - 0.0) < EPSILON);
	printf("  pal_tan: OK\n");

	printf("Trigonometric tests: PASSED\n\n");
}

void test_rounding(void)
{
	printf("Testing rounding functions...\n");

	/* Round */
	assert(pal_round(3.4) == 3.0);
	assert(pal_round(3.6) == 4.0);
	printf("  pal_round: OK\n");

	/* Truncate */
	assert(pal_trunc(3.7) == 3.0);
	assert(pal_trunc(-3.7) == -3.0);
	printf("  pal_trunc: OK\n");

	/* Integer part */
	assert(pal_int(3.7) == 3);
	assert(pal_int(-3.7) == -3);
	printf("  pal_int: OK\n");

	/* Fractional part */
	assert(fabs(pal_frac(3.7) - 0.7) < EPSILON);
	printf("  pal_frac: OK\n");

	printf("Rounding tests: PASSED\n\n");
}

void test_logarithms(void)
{
	PAL_Number result;

	printf("Testing logarithm functions...\n");

	/* Natural logarithm */
	result = pal_ln(2.718281828);
	assert(fabs(result - 1.0) < 0.001);
	printf("  pal_ln: OK\n");

	/* Base-10 logarithm */
	result = pal_log(100.0);
	assert(fabs(result - 2.0) < EPSILON);
	printf("  pal_log: OK\n");

	/* Exponential */
	result = pal_exp(1.0);
	assert(fabs(result - 2.718281828) < 0.001);
	printf("  pal_exp: OK\n");

	printf("Logarithm tests: PASSED\n\n");
}

int main(void)
{
	printf("PAL Runtime Library - Math Function Tests\n");
	printf("========================================\n\n");

	pal_runtime_init();

	test_basic_math();
	test_trig_functions();
	test_rounding();
	test_logarithms();

	pal_runtime_cleanup();

	printf("All math tests PASSED!\n");
	return 0;
}
