/*
 * Copyright (c) 2025 PCC Go Runtime Library
 *
 * Atomic operations
 * Using GCC/Clang built-ins
 */

#include <stdint.h>
#include <stdbool.h>
#include "runtime.h"

/*
 * Atomic add operations
 */
int32_t
go_atomic_add_int32(int32_t *ptr, int32_t delta)
{
	return __sync_add_and_fetch(ptr, delta);
}

int64_t
go_atomic_add_int64(int64_t *ptr, int64_t delta)
{
	return __sync_add_and_fetch(ptr, delta);
}

uint32_t
go_atomic_add_uint32(uint32_t *ptr, uint32_t delta)
{
	return __sync_add_and_fetch(ptr, delta);
}

uint64_t
go_atomic_add_uint64(uint64_t *ptr, uint64_t delta)
{
	return __sync_add_and_fetch(ptr, delta);
}

/*
 * Atomic compare-and-swap operations
 */
bool
go_atomic_cas_int32(int32_t *ptr, int32_t old, int32_t new)
{
	return __sync_bool_compare_and_swap(ptr, old, new);
}

bool
go_atomic_cas_int64(int64_t *ptr, int64_t old, int64_t new)
{
	return __sync_bool_compare_and_swap(ptr, old, new);
}

bool
go_atomic_cas_ptr(void **ptr, void *old, void *new)
{
	return __sync_bool_compare_and_swap(ptr, old, new);
}

/*
 * Atomic load operations
 */
int32_t
go_atomic_load_int32(int32_t *ptr)
{
	__sync_synchronize();
	return *ptr;
}

int64_t
go_atomic_load_int64(int64_t *ptr)
{
	__sync_synchronize();
	return *ptr;
}

void *
go_atomic_load_ptr(void **ptr)
{
	__sync_synchronize();
	return *ptr;
}

/*
 * Atomic store operations
 */
void
go_atomic_store_int32(int32_t *ptr, int32_t val)
{
	__sync_synchronize();
	*ptr = val;
	__sync_synchronize();
}

void
go_atomic_store_int64(int64_t *ptr, int64_t val)
{
	__sync_synchronize();
	*ptr = val;
	__sync_synchronize();
}

void
go_atomic_store_ptr(void **ptr, void *val)
{
	__sync_synchronize();
	*ptr = val;
	__sync_synchronize();
}
