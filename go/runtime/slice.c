/*
 * Copyright (c) 2025 PCC Go Runtime Library
 *
 * Slice operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

/*
 * Create new slice with specified length and capacity
 */
go_slice
go_slice_new(go_int len, go_int cap, size_t elem_size)
{
	go_slice s;

	if (len < 0) len = 0;
	if (cap < len) cap = len;

	s.len = len;
	s.cap = cap;

	if (cap > 0) {
		s.data = go_calloc(cap, elem_size);
	} else {
		s.data = NULL;
	}

	return s;
}

/*
 * make() for slices
 */
go_slice
go_slice_make(go_int len, go_int cap, size_t elem_size)
{
	return go_slice_new(len, cap, elem_size);
}

/*
 * Append element to slice
 */
go_slice
go_slice_append(go_slice s, void *elem, size_t elem_size)
{
	go_slice result;

	/* Check if we need to grow */
	if (s.len >= s.cap) {
		/* Double capacity, or use 1 if cap is 0 */
		go_int new_cap = s.cap * 2;
		if (new_cap == 0)
			new_cap = 1;

		/* Allocate new backing array */
		void *new_data = go_malloc(new_cap * elem_size);

		/* Copy old data */
		if (s.len > 0 && s.data != NULL)
			memcpy(new_data, s.data, s.len * elem_size);

		/* Update slice */
		result.data = new_data;
		result.len = s.len + 1;
		result.cap = new_cap;

		/* Free old data if allocated */
		if (s.data != NULL)
			go_free(s.data);
	} else {
		/* Enough capacity */
		result.data = s.data;
		result.len = s.len + 1;
		result.cap = s.cap;
	}

	/* Copy new element */
	memcpy((char *)result.data + s.len * elem_size, elem, elem_size);

	return result;
}

/*
 * Copy slice elements
 */
go_slice
go_copy_slice(go_slice dst, go_slice src, size_t elem_size)
{
	go_int n = dst.len < src.len ? dst.len : src.len;

	if (n > 0 && dst.data != NULL && src.data != NULL)
		memcpy(dst.data, src.data, n * elem_size);

	return dst;
}

/*
 * Sub-slice operation
 */
go_slice
go_slice_sub(go_slice s, go_int low, go_int high, size_t elem_size)
{
	go_slice result;

	/* Bounds checking */
	if (low < 0) low = 0;
	if (high > s.cap) high = s.cap;
	if (low > high) low = high;

	result.data = (char *)s.data + low * elem_size;
	result.len = high - low;
	result.cap = s.cap - low;

	return result;
}

/*
 * Index into slice
 */
void *
go_slice_index(go_slice s, go_int i, size_t elem_size)
{
	/* Bounds checking */
	if (i < 0 || i >= s.len) {
		fprintf(stderr, "runtime error: index out of range [%lld] with length %lld\n",
		    (long long)i, (long long)s.len);
		go_panic((go_interface){ NULL, NULL });
	}

	return (char *)s.data + i * elem_size;
}
