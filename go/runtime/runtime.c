/*
 * Copyright (c) 2025 PCC Go Runtime Library
 *
 * Runtime initialization and core functionality
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

/* Global runtime state */
static struct {
	int argc;
	char **argv;
	bool initialized;
} runtime_state = { 0, NULL, false };

/* Predefined type descriptors */
go_type go_type_bool    = { sizeof(go_bool), _Alignof(go_bool), 0, 1, "bool" };
go_type go_type_int8    = { sizeof(go_int8), _Alignof(go_int8), 0, 2, "int8" };
go_type go_type_uint8   = { sizeof(go_uint8), _Alignof(go_uint8), 0, 3, "uint8" };
go_type go_type_int16   = { sizeof(go_int16), _Alignof(go_int16), 0, 4, "int16" };
go_type go_type_uint16  = { sizeof(go_uint16), _Alignof(go_uint16), 0, 5, "uint16" };
go_type go_type_int32   = { sizeof(go_int32), _Alignof(go_int32), 0, 6, "int32" };
go_type go_type_uint32  = { sizeof(go_uint32), _Alignof(go_uint32), 0, 7, "uint32" };
go_type go_type_int64   = { sizeof(go_int64), _Alignof(go_int64), 0, 8, "int64" };
go_type go_type_uint64  = { sizeof(go_uint64), _Alignof(go_uint64), 0, 9, "uint64" };
go_type go_type_int     = { sizeof(go_int), _Alignof(go_int), 0, 10, "int" };
go_type go_type_uint    = { sizeof(go_uint), _Alignof(go_uint), 0, 11, "uint" };
go_type go_type_uintptr = { sizeof(go_uintptr), _Alignof(go_uintptr), 0, 12, "uintptr" };
go_type go_type_float32 = { sizeof(go_float32), _Alignof(go_float32), 0, 13, "float32" };
go_type go_type_float64 = { sizeof(go_float64), _Alignof(go_float64), 0, 14, "float64" };
go_type go_type_string  = { sizeof(go_string), _Alignof(go_string), 0, 15, "string" };

/*
 * Initialize Go runtime
 */
void
go_runtime_init(int argc, char **argv)
{
	if (runtime_state.initialized)
		return;

	runtime_state.argc = argc;
	runtime_state.argv = argv;
	runtime_state.initialized = true;

	/* Initialize subsystems */
	go_gc_init();
	go_sched_init();
}

/*
 * Exit Go runtime
 */
void
go_runtime_exit(int code)
{
	/* Run deferred functions */
	go_defer_run();

	/* Cleanup */
	exit(code);
}

/*
 * Memory allocation wrappers
 */
void *
go_malloc(size_t size)
{
	void *ptr = malloc(size);
	if (ptr == NULL && size > 0) {
		fprintf(stderr, "runtime: out of memory\n");
		go_runtime_exit(2);
	}
	return ptr;
}

void *
go_calloc(size_t nmemb, size_t size)
{
	void *ptr = calloc(nmemb, size);
	if (ptr == NULL && nmemb > 0 && size > 0) {
		fprintf(stderr, "runtime: out of memory\n");
		go_runtime_exit(2);
	}
	return ptr;
}

void *
go_realloc(void *ptr, size_t size)
{
	void *new_ptr = realloc(ptr, size);
	if (new_ptr == NULL && size > 0) {
		fprintf(stderr, "runtime: out of memory\n");
		go_runtime_exit(2);
	}
	return new_ptr;
}

void
go_free(void *ptr)
{
	free(ptr);
}

/*
 * Type reflection
 */
go_type *
go_typeof(go_interface iface)
{
	return (go_type *)iface.type;
}

go_string
go_type_name(go_type *typ)
{
	if (typ == NULL)
		return (go_string){ "<nil>", 5 };
	return (go_string){ typ->name, strlen(typ->name) };
}

size_t
go_type_size(go_type *typ)
{
	if (typ == NULL)
		return 0;
	return typ->size;
}

/*
 * Hash functions
 */
uint32_t
go_hash_string(go_string s)
{
	return go_hash_bytes(s.data, s.len);
}

uint32_t
go_hash_int(int64_t val)
{
	return go_hash_bytes(&val, sizeof(val));
}

uint32_t
go_hash_ptr(void *ptr)
{
	return go_hash_bytes(&ptr, sizeof(ptr));
}

/* FNV-1a hash */
uint32_t
go_hash_bytes(const void *data, size_t len)
{
	const uint8_t *bytes = (const uint8_t *)data;
	uint32_t hash = 2166136261u;
	size_t i;

	for (i = 0; i < len; i++) {
		hash ^= bytes[i];
		hash *= 16777619u;
	}

	return hash;
}

/*
 * Range iteration helpers
 */
go_range_iter
go_range_init(go_int len)
{
	go_range_iter it;
	it.index = 0;
	it.len = len;
	return it;
}

bool
go_range_next(go_range_iter *it)
{
	if (it->index < it->len) {
		it->index++;
		return true;
	}
	return false;
}
