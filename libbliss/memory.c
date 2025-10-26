/*
 * Copyright (c) 2025 PCC BLISS Runtime Library
 *
 * Memory Management Implementation
 */

#include "blissrt.h"
#include <stdlib.h>
#include <string.h>

/*
 * Allocate memory for a vector
 */
bliss_vector_t *bliss_alloc_vector(size_t size, size_t element_size)
{
	bliss_vector_t *vec;

	vec = (bliss_vector_t *)malloc(sizeof(bliss_vector_t));
	if (vec == NULL)
		return NULL;

	vec->size = size;
	vec->element_size = element_size;
	vec->data = (bliss_word_t *)calloc(size, element_size);

	if (vec->data == NULL) {
		free(vec);
		return NULL;
	}

	return vec;
}

/*
 * Free a vector
 */
void bliss_free_vector(bliss_vector_t *vec)
{
	if (vec == NULL)
		return;

	if (vec->data)
		free(vec->data);

	free(vec);
}

/*
 * Allocate memory
 */
void *bliss_malloc(size_t size)
{
	return malloc(size);
}

/*
 * Free allocated memory
 */
void bliss_free(void *ptr)
{
	free(ptr);
}

/*
 * Reallocate memory
 */
void *bliss_realloc(void *ptr, size_t size)
{
	return realloc(ptr, size);
}
