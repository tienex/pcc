/*
 * COBOL Runtime Library - Memory Management
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cobolrt.h"

void *
__cobol_malloc(size_t size)
{
	void *ptr = malloc(size);

	if (!ptr && size > 0) {
		__cobol_fatal_error("Out of memory");
	}

	return ptr;
}

void
__cobol_free(void *ptr)
{
	if (ptr)
		free(ptr);
}

void *
__cobol_realloc(void *ptr, size_t size)
{
	void *new_ptr = realloc(ptr, size);

	if (!new_ptr && size > 0) {
		__cobol_fatal_error("Out of memory");
	}

	return new_ptr;
}
