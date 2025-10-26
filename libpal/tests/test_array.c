/*
 * Test program for PAL runtime library - Array functions
 */

#include <stdio.h>
#include <assert.h>
#include "../include/palrt.h"

void test_array_creation(void)
{
	PAL_Array *arr;

	printf("Testing array creation...\n");

	arr = pal_array_new(sizeof(int), 10);
	assert(arr != NULL);
	assert(pal_arraysize(arr) == 10);
	printf("  pal_array_new: OK\n");

	pal_array_free(arr);

	printf("Array creation tests: PASSED\n\n");
}

void test_array_access(void)
{
	PAL_Array *arr;
	int value, *ptr;

	printf("Testing array access...\n");

	arr = pal_array_new(sizeof(int), 5);

	/* Set values */
	for (int i = 0; i < 5; i++) {
		value = i * 10;
		pal_arrayset(arr, i, &value);
	}
	printf("  pal_arrayset: OK\n");

	/* Get values */
	for (int i = 0; i < 5; i++) {
		ptr = (int *)pal_arrayget(arr, i);
		assert(ptr != NULL);
		assert(*ptr == i * 10);
	}
	printf("  pal_arrayget: OK\n");

	pal_array_free(arr);

	printf("Array access tests: PASSED\n\n");
}

void test_array_insert_delete(void)
{
	PAL_Array *arr;
	int value, *ptr;

	printf("Testing array insert/delete...\n");

	arr = pal_array_new(sizeof(int), 3);

	/* Initial values: 10, 20, 30 */
	value = 10; pal_arrayset(arr, 0, &value);
	value = 20; pal_arrayset(arr, 1, &value);
	value = 30; pal_arrayset(arr, 2, &value);

	/* Insert 15 at position 1 */
	value = 15;
	pal_arrayinsert(arr, 1, &value);
	assert(pal_arraysize(arr) == 4);
	ptr = (int *)pal_arrayget(arr, 1);
	assert(*ptr == 15);
	printf("  pal_arrayinsert: OK\n");

	/* Delete element at position 1 */
	pal_arraydelete(arr, 1);
	assert(pal_arraysize(arr) == 3);
	ptr = (int *)pal_arrayget(arr, 1);
	assert(*ptr == 20);
	printf("  pal_arraydelete: OK\n");

	pal_array_free(arr);

	printf("Array insert/delete tests: PASSED\n\n");
}

void test_array_resize(void)
{
	PAL_Array *arr;
	int value;

	printf("Testing array resize...\n");

	arr = pal_array_new(sizeof(int), 5);

	/* Resize to larger */
	pal_arrayresize(arr, 10);
	assert(pal_arraysize(arr) == 10);
	printf("  Resize larger: OK\n");

	/* Resize to smaller */
	pal_arrayresize(arr, 3);
	assert(pal_arraysize(arr) == 3);
	printf("  Resize smaller: OK\n");

	pal_array_free(arr);

	printf("Array resize tests: PASSED\n\n");
}

int main(void)
{
	printf("PAL Runtime Library - Array Function Tests\n");
	printf("=========================================\n\n");

	pal_runtime_init();

	test_array_creation();
	test_array_access();
	test_array_insert_delete();
	test_array_resize();

	pal_runtime_cleanup();

	printf("All array tests PASSED!\n");
	return 0;
}
