/*
 * Integration test for BLISS Runtime Memory functions
 */

#include <blissrt.h>
#include <stdio.h>
#include <stdlib.h>

long bliss_main(void)
{
	int tests_passed = 0;
	int tests_total = 0;
	bliss_vector_t *vec;
	void *ptr;

	printf("=== BLISS Runtime Memory Tests ===\n\n");

	/* Test 1: malloc/free */
	tests_total++;
	printf("Test 1: malloc/free... ");
	ptr = bliss_malloc(100);
	if (ptr != NULL) {
		bliss_free(ptr);
		printf("OK\n");
		tests_passed++;
	} else {
		printf("FAILED\n");
	}

	/* Test 2: vector allocation */
	tests_total++;
	printf("Test 2: vector allocation... ");
	vec = bliss_alloc_vector(10, sizeof(bliss_word_t));
	if (vec != NULL && vec->size == 10) {
		printf("OK\n");
		tests_passed++;
	} else {
		printf("FAILED\n");
	}

	/* Test 3: vector initialization */
	tests_total++;
	printf("Test 3: vector initialization... ");
	if (vec != NULL) {
		vec->data[0] = 42;
		vec->data[9] = 99;
		if (vec->data[0] == 42 && vec->data[9] == 99) {
			printf("OK\n");
			tests_passed++;
		} else {
			printf("FAILED\n");
		}
	} else {
		printf("SKIPPED (vector is NULL)\n");
	}

	/* Test 4: vector free */
	tests_total++;
	printf("Test 4: vector free... ");
	if (vec != NULL) {
		bliss_free_vector(vec);
		printf("OK\n");
		tests_passed++;
	} else {
		printf("SKIPPED (vector is NULL)\n");
	}

	printf("\n=== Results ===\n");
	printf("Tests passed: %d/%d\n", tests_passed, tests_total);

	return (tests_passed == tests_total) ? 0 : 1;
}
