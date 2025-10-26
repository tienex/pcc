/*
 * Copyright (c) 2025 PCC CHILL Runtime Library
 *
 * CHILL memory management functions
 */

#include "chill.h"
#include <stdlib.h>
#include <string.h>

/*
 * Memory allocation and deallocation
 */

void *
chill_allocate(size_t size)
{
	void *ptr;

	if (size == 0) {
		return NULL;
	}

	ptr = malloc(size);
	if (ptr == NULL) {
		chill_raise(CHILL_EXC_OUTOFMEM, "Memory allocation failed");
		return NULL;
	}

	/* Initialize memory to zero (CHILL semantics) */
	memset(ptr, 0, size);

	return ptr;
}

void
chill_free(void *ptr)
{
	if (ptr != NULL) {
		free(ptr);
	}
}
