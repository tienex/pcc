/*
 * Copyright (c) 2025 PCC Go Runtime Library
 *
 * Built-in functions implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <inttypes.h>
#include "runtime.h"

/*
 * make() for slices
 */
go_slice
go_make_slice(go_type *elem_type, go_int len, go_int cap)
{
	if (elem_type == NULL) {
		fprintf(stderr, "runtime error: make slice with nil type\n");
		go_panic((go_interface){ NULL, NULL });
	}

	return go_slice_make(len, cap, elem_type->size);
}

/*
 * make() for maps
 */
go_map
go_make_map(go_type *key_type, go_type *val_type, go_int hint)
{
	if (key_type == NULL || val_type == NULL) {
		fprintf(stderr, "runtime error: make map with nil type\n");
		go_panic((go_interface){ NULL, NULL });
	}

	return go_map_new(key_type->size, val_type->size, hint > 0 ? hint : 0);
}

/*
 * make() for channels
 */
go_channel *
go_make_chan(go_type *elem_type, go_int buffer)
{
	if (elem_type == NULL) {
		fprintf(stderr, "runtime error: make channel with nil type\n");
		go_panic((go_interface){ NULL, NULL });
	}

	if (buffer < 0)
		buffer = 0;

	return go_chan_new(elem_type->size, buffer);
}

/*
 * new() - allocate zeroed memory
 */
void *
go_new(go_type *typ)
{
	if (typ == NULL) {
		fprintf(stderr, "runtime error: new with nil type\n");
		go_panic((go_interface){ NULL, NULL });
	}

	return go_calloc(1, typ->size);
}

/*
 * len() for strings
 */
go_int
go_len_string(go_string s)
{
	return s.len;
}

/*
 * len() for slices
 */
go_int
go_len_slice(go_slice s)
{
	return s.len;
}

/*
 * len() for maps
 */
go_int
go_len_map(go_map m)
{
	return go_map_len(m);
}

/*
 * len() for channels
 */
go_int
go_len_chan(go_channel *ch)
{
	go_int len;

	if (ch == NULL)
		return 0;

	pthread_mutex_lock(&ch->lock);
	len = ch->count;
	pthread_mutex_unlock(&ch->lock);

	return len;
}

/*
 * cap() for slices
 */
go_int
go_cap_slice(go_slice s)
{
	return s.cap;
}

/*
 * cap() for channels
 */
go_int
go_cap_chan(go_channel *ch)
{
	if (ch == NULL)
		return 0;

	return ch->buffer_size;
}

/*
 * append() for slices
 */
go_slice
go_append(go_slice s, void *elems, go_int count, size_t elem_size)
{
	go_slice result = s;
	go_int i;
	char *elem_ptr = (char *)elems;

	/* Append each element */
	for (i = 0; i < count; i++) {
		result = go_slice_append(result, elem_ptr, elem_size);
		elem_ptr += elem_size;
	}

	return result;
}

/*
 * copy() for slices
 */
go_int
go_copy_slice(go_slice dst, go_slice src, size_t elem_size)
{
	go_int n = dst.len < src.len ? dst.len : src.len;

	if (n > 0)
		go_slice_copy(dst, src, elem_size);

	return n;
}

/*
 * delete() for maps
 */
void
go_delete_map(go_map m, void *key)
{
	go_map_delete(m, key);
}

/*
 * close() for channels
 */
void
go_close_chan(go_channel *ch)
{
	go_chan_close(ch);
}

/*
 * print() - variadic print
 */
void
go_print(go_int nargs, ...)
{
	/* This is a placeholder - actual implementation would need
	 * type information for each argument passed by the compiler */
	fprintf(stderr, "print: %lld arguments\n", (long long)nargs);
}

/*
 * println() - variadic println
 */
void
go_println(go_int nargs, ...)
{
	/* This is a placeholder - actual implementation would need
	 * type information for each argument passed by the compiler */
	fprintf(stderr, "println: %lld arguments\n", (long long)nargs);
}

/*
 * Print functions for specific types
 */
void
go_print_bool(go_bool val)
{
	printf("%s", val ? "true" : "false");
}

void
go_print_int(go_int64 val)
{
	printf("%" PRId64, val);
}

void
go_print_uint(go_uint64 val)
{
	printf("%" PRIu64, val);
}

void
go_print_float(go_float64 val)
{
	printf("%g", val);
}

void
go_print_string(go_string s)
{
	printf("%.*s", (int)s.len, s.data);
}

void
go_print_newline(void)
{
	printf("\n");
}
