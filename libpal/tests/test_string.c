/*
 * Test program for PAL runtime library - String functions
 */

#include <stdio.h>
#include <assert.h>
#include "../include/palrt.h"

void test_string_basic(void)
{
	PAL_String *s1, *s2, *result;

	printf("Testing basic string operations...\n");

	/* Create string */
	s1 = pal_string_new("Hello, World!");
	assert(s1 != NULL);
	assert(pal_strlen(s1) == 13);
	printf("  pal_string_new: OK\n");

	/* Upper/lower */
	result = pal_upper(s1);
	assert(strcmp(pal_string_cstr(result), "HELLO, WORLD!") == 0);
	pal_string_free(result);
	printf("  pal_upper: OK\n");

	result = pal_lower(s1);
	assert(strcmp(pal_string_cstr(result), "hello, world!") == 0);
	pal_string_free(result);
	printf("  pal_lower: OK\n");

	/* Substring */
	result = pal_substr(s1, 8, 5);  /* "World" */
	assert(strcmp(pal_string_cstr(result), "World") == 0);
	pal_string_free(result);
	printf("  pal_substr: OK\n");

	/* Concatenation */
	s2 = pal_string_new(" Welcome!");
	result = pal_concat(s1, s2);
	assert(strcmp(pal_string_cstr(result), "Hello, World! Welcome!") == 0);
	pal_string_free(result);
	printf("  pal_concat: OK\n");

	/* String position */
	assert(pal_strpos(s1, pal_string_new("World")) == 8);
	assert(pal_strpos(s1, pal_string_new("xyz")) == 0);
	printf("  pal_strpos: OK\n");

	pal_string_free(s1);
	pal_string_free(s2);

	printf("String tests: PASSED\n\n");
}

void test_string_trim(void)
{
	PAL_String *s, *result;

	printf("Testing string trim functions...\n");

	s = pal_string_new("  Hello World  ");

	result = pal_trim(s);
	assert(strcmp(pal_string_cstr(result), "Hello World") == 0);
	pal_string_free(result);
	printf("  pal_trim: OK\n");

	result = pal_ltrim(s);
	assert(strcmp(pal_string_cstr(result), "Hello World  ") == 0);
	pal_string_free(result);
	printf("  pal_ltrim: OK\n");

	result = pal_rtrim(s);
	assert(strcmp(pal_string_cstr(result), "  Hello World") == 0);
	pal_string_free(result);
	printf("  pal_rtrim: OK\n");

	pal_string_free(s);

	printf("Trim tests: PASSED\n\n");
}

void test_string_fill(void)
{
	PAL_String *result;

	printf("Testing pal_fill...\n");

	result = pal_fill('*', 10);
	assert(pal_strlen(result) == 10);
	assert(strcmp(pal_string_cstr(result), "**********") == 0);
	pal_string_free(result);

	printf("  pal_fill: OK\n");
	printf("Fill tests: PASSED\n\n");
}

int main(void)
{
	printf("PAL Runtime Library - String Function Tests\n");
	printf("==========================================\n\n");

	pal_runtime_init();

	test_string_basic();
	test_string_trim();
	test_string_fill();

	pal_runtime_cleanup();

	printf("All string tests PASSED!\n");
	return 0;
}
