/*
 * Vector Operations using BLISS Runtime Library
 * Demonstrates vector allocation and manipulation
 */

#include <blissrt.h>

/* BLISS main routine */
long bliss_main(void)
{
	bliss_vector_t *vec;
	int i;

	bliss_puts("BLISS Vector Operations");
	bliss_putcrlf();
	bliss_putcrlf();

	/* Allocate a vector of 10 words */
	vec = bliss_alloc_vector(10, sizeof(bliss_word_t));
	if (vec == NULL) {
		bliss_puts("Error: failed to allocate vector");
		bliss_putcrlf();
		return 1;
	}

	/* Initialize vector with values */
	for (i = 0; i < 10; i++) {
		vec->data[i] = i * 10;
	}

	/* Display vector contents */
	bliss_puts("Vector contents:");
	bliss_putcrlf();
	for (i = 0; i < 10; i++) {
		bliss_puts("  vec[");
		bliss_put_decimal(i);
		bliss_puts("] = ");
		bliss_put_decimal(vec->data[i]);
		bliss_putcrlf();
	}

	/* Calculate sum */
	bliss_word_t sum = 0;
	for (i = 0; i < 10; i++) {
		sum += vec->data[i];
	}

	bliss_putcrlf();
	bliss_puts("Sum of all elements: ");
	bliss_put_decimal(sum);
	bliss_putcrlf();

	/* Free vector */
	bliss_free_vector(vec);

	return 0;
}
