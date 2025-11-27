/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart Runtime Library - Memory Management
 */

#include "dart.h"
#include <stdlib.h>
#include <stdio.h>

static size_t total_allocated = 0;
static size_t total_freed = 0;

void *
dart_malloc(size_t size)
{
	void *ptr = malloc(size);
	if (!ptr && size > 0) {
		fprintf(stderr, "dart_malloc: out of memory\n");
		exit(1);
	}
	total_allocated += size;
	return ptr;
}

void *
dart_calloc(size_t count, size_t size)
{
	void *ptr = calloc(count, size);
	if (!ptr && count > 0 && size > 0) {
		fprintf(stderr, "dart_calloc: out of memory\n");
		exit(1);
	}
	total_allocated += (count * size);
	return ptr;
}

void *
dart_realloc(void *ptr, size_t size)
{
	void *new_ptr = realloc(ptr, size);
	if (!new_ptr && size > 0) {
		fprintf(stderr, "dart_realloc: out of memory\n");
		exit(1);
	}
	return new_ptr;
}

void
dart_free(void *ptr)
{
	if (ptr) {
		free(ptr);
		/* Note: We don't track exact freed size, but could with a wrapper */
	}
}

void
dart_memory_stats(void)
{
	printf("Memory statistics:\n");
	printf("  Total allocated: %zu bytes\n", total_allocated);
	printf("  Total freed: %zu bytes\n", total_freed);
	printf("  Difference: %zd bytes\n", (ssize_t)(total_allocated - total_freed));
}
