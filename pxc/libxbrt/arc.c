/*
 * Copyright (c) 2025 PCC Xbase++ Runtime Library
 *
 * Automatic Reference Counting (ARC) for memory management
 */

#include "xbrt.h"
#include <stdlib.h>
#include <string.h>

/* Reference counted object header */
typedef struct arc_header {
	size_t ref_count;
	void (*destructor)(void*);
} ARC_HEADER;

#define ARC_HEADER_SIZE sizeof(ARC_HEADER)

/*
 * Allocate reference-counted memory
 */
void *
xb_arc_alloc(size_t size, void (*destructor)(void*))
{
	ARC_HEADER *hdr = malloc(ARC_HEADER_SIZE + size);
	if (!hdr) return NULL;

	hdr->ref_count = 1;
	hdr->destructor = destructor;

	return (void*)((char*)hdr + ARC_HEADER_SIZE);
}

/*
 * Get header from object pointer
 */
static ARC_HEADER *
get_header(void *ptr)
{
	if (!ptr) return NULL;
	return (ARC_HEADER*)((char*)ptr - ARC_HEADER_SIZE);
}

/*
 * Retain (increment reference count)
 */
void *
xb_arc_retain(void *ptr)
{
	ARC_HEADER *hdr = get_header(ptr);
	if (hdr) {
		hdr->ref_count++;
	}
	return ptr;
}

/*
 * Release (decrement reference count, free if zero)
 */
void
xb_arc_release(void *ptr)
{
	ARC_HEADER *hdr = get_header(ptr);
	if (!hdr) return;

	hdr->ref_count--;
	if (hdr->ref_count == 0) {
		/* Call destructor if present */
		if (hdr->destructor) {
			hdr->destructor(ptr);
		}
		/* Free memory */
		free(hdr);
	}
}

/*
 * Get reference count
 */
size_t
xb_arc_ref_count(void *ptr)
{
	ARC_HEADER *hdr = get_header(ptr);
	return hdr ? hdr->ref_count : 0;
}

/*
 * Autorelease pool (for automatic cleanup)
 */
typedef struct autorelease_pool {
	void **objects;
	size_t count;
	size_t capacity;
	struct autorelease_pool *parent;
} AUTORELEASE_POOL;

static AUTORELEASE_POOL *current_pool = NULL;

/*
 * Create autorelease pool
 */
AUTORELEASE_POOL *
xb_arc_pool_create(void)
{
	AUTORELEASE_POOL *pool = malloc(sizeof(AUTORELEASE_POOL));
	if (!pool) return NULL;

	pool->objects = NULL;
	pool->count = 0;
	pool->capacity = 0;
	pool->parent = current_pool;
	current_pool = pool;

	return pool;
}

/*
 * Add object to autorelease pool
 */
void *
xb_arc_autorelease(void *ptr)
{
	if (!current_pool || !ptr) return ptr;

	/* Expand pool if needed */
	if (current_pool->count >= current_pool->capacity) {
		size_t new_cap = current_pool->capacity ? current_pool->capacity * 2 : 16;
		void **new_objs = realloc(current_pool->objects, new_cap * sizeof(void*));
		if (!new_objs) return ptr;

		current_pool->objects = new_objs;
		current_pool->capacity = new_cap;
	}

	current_pool->objects[current_pool->count++] = ptr;
	return ptr;
}

/*
 * Drain autorelease pool
 */
void
xb_arc_pool_drain(AUTORELEASE_POOL *pool)
{
	if (!pool) return;

	size_t i;
	for (i = 0; i < pool->count; i++) {
		xb_arc_release(pool->objects[i]);
	}

	free(pool->objects);
	current_pool = pool->parent;
	free(pool);
}

/*
 * Value destructor for ARC
 */
static void
value_destructor(void *ptr)
{
	xb_value_t *val = (xb_value_t*)ptr;
	/* Free value contents but not the value itself (handled by ARC) */
	if (val->type == XB_CHARACTER && val->data.string) {
		free(val->data.string);
	}
}

/*
 * Create ARC-managed value
 */
xb_value_t *
xb_value_new_arc(xb_type_t type)
{
	xb_value_t *val = xb_arc_alloc(sizeof(xb_value_t), value_destructor);
	if (val) {
		memset(val, 0, sizeof(xb_value_t));
		val->type = type;
	}
	return val;
}
