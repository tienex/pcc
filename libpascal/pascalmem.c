/*
 * Copyright (c) 2025 PCC Pascal Runtime Library
 *
 * Pascal Memory Management
 *
 * Implements memory allocation and manipulation functions for Pascal programs.
 */

#include <stdlib.h>
#include <string.h>
#include "pascalrt.h"

/*
 * Dynamic Memory Allocation
 */

void *
pascal_new(size_t size)
{
	void *ptr = malloc(size);
	if (ptr == NULL) {
		pascal_runtime_error(PASCAL_ERR_HEAP_OVERFLOW, "Out of memory");
		return NULL;
	}
	/* Initialize to zero (Pascal standard behavior) */
	memset(ptr, 0, size);
	return ptr;
}

void
pascal_dispose(void *ptr)
{
	if (ptr == NULL) {
		pascal_runtime_error(PASCAL_ERR_NIL_POINTER, "Dispose of nil pointer");
		return;
	}
	free(ptr);
}

void *
pascal_getmem(size_t size)
{
	void *ptr = malloc(size);
	if (ptr == NULL) {
		pascal_runtime_error(PASCAL_ERR_HEAP_OVERFLOW, "Out of memory");
		return NULL;
	}
	return ptr;
}

void
pascal_freemem(void *ptr, size_t size)
{
	(void)size;  /* Size parameter is for compatibility */
	if (ptr == NULL) {
		pascal_runtime_error(PASCAL_ERR_NIL_POINTER, "FreeMem of nil pointer");
		return;
	}
	free(ptr);
}

/*
 * Memory Block Operations
 */

void
pascal_fillchar(void *dest, size_t count, uint8_t value)
{
	if (dest == NULL) {
		pascal_runtime_error(PASCAL_ERR_NIL_POINTER, "FillChar with nil pointer");
		return;
	}
	memset(dest, value, count);
}

void
pascal_move(const void *src, void *dest, size_t count)
{
	if (src == NULL || dest == NULL) {
		pascal_runtime_error(PASCAL_ERR_NIL_POINTER, "Move with nil pointer");
		return;
	}
	memmove(dest, src, count);
}

/*
 * Set Operations
 */

void
pascal_set_init(PascalSet s)
{
	memset(s, 0, sizeof(PascalSet));
}

void
pascal_set_add(PascalSet s, int element)
{
	if (element < 0 || element > 255) {
		pascal_runtime_error(PASCAL_ERR_RANGE_CHECK, "Set element out of range");
		return;
	}
	int word = element / 32;
	int bit = element % 32;
	s[word] |= (1U << bit);
}

void
pascal_set_remove(PascalSet s, int element)
{
	if (element < 0 || element > 255) {
		pascal_runtime_error(PASCAL_ERR_RANGE_CHECK, "Set element out of range");
		return;
	}
	int word = element / 32;
	int bit = element % 32;
	s[word] &= ~(1U << bit);
}

int
pascal_set_contains(const PascalSet s, int element)
{
	if (element < 0 || element > 255) {
		return 0;
	}
	int word = element / 32;
	int bit = element % 32;
	return (s[word] & (1U << bit)) != 0;
}

void
pascal_set_union(PascalSet result, const PascalSet s1, const PascalSet s2)
{
	for (int i = 0; i < 8; i++) {
		result[i] = s1[i] | s2[i];
	}
}

void
pascal_set_intersection(PascalSet result, const PascalSet s1, const PascalSet s2)
{
	for (int i = 0; i < 8; i++) {
		result[i] = s1[i] & s2[i];
	}
}

void
pascal_set_difference(PascalSet result, const PascalSet s1, const PascalSet s2)
{
	for (int i = 0; i < 8; i++) {
		result[i] = s1[i] & ~s2[i];
	}
}

int
pascal_set_equal(const PascalSet s1, const PascalSet s2)
{
	for (int i = 0; i < 8; i++) {
		if (s1[i] != s2[i]) {
			return 0;
		}
	}
	return 1;
}

int
pascal_set_subset(const PascalSet s1, const PascalSet s2)
{
	/* Check if s1 is a subset of s2 */
	for (int i = 0; i < 8; i++) {
		if ((s1[i] & ~s2[i]) != 0) {
			return 0;
		}
	}
	return 1;
}
