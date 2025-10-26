/*
 * Copyright (c) 2025 PCC Common LISP Runtime Library
 *
 * Memory management implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "liblisp.h"

/* Simple memory pool for objects */
#define POOL_SIZE 10000
static lisp_object_t object_pool[POOL_SIZE];
static int pool_index = 0;

/* Singleton NIL and T objects */
lisp_object_t *LISP_NIL = NULL;
lisp_object_t *LISP_T = NULL;

/*
 * Initialize garbage collector (simple pool-based allocator)
 */
void
lisp_gc_init(void)
{
	pool_index = 0;

	/* Initialize NIL */
	LISP_NIL = &object_pool[pool_index++];
	LISP_NIL->type = LISP_TYPE_NIL;

	/* Initialize T */
	LISP_T = &object_pool[pool_index++];
	LISP_T->type = LISP_TYPE_T;
}

/*
 * Allocate a new LISP object
 */
lisp_object_t *
lisp_alloc(void)
{
	if (pool_index >= POOL_SIZE) {
		fprintf(stderr, "Error: Object pool exhausted\n");
		exit(1);
	}

	return &object_pool[pool_index++];
}

/*
 * Free a LISP object (no-op in simple pool allocator)
 */
void
lisp_free(lisp_object_t *obj)
{
	/* In a real implementation, this would mark the object as free */
	/* For now, we just use a simple bump allocator */
}

/*
 * Collect garbage (no-op in simple implementation)
 */
void
lisp_gc_collect(void)
{
	/* In a real implementation, this would do mark-and-sweep */
}

/*
 * Get heap size
 */
size_t
lisp_gc_get_heap_size(void)
{
	return pool_index * sizeof(lisp_object_t);
}

/*
 * Create NIL object
 */
lisp_object_t *
lisp_make_nil(void)
{
	return LISP_NIL;
}

/*
 * Create T (true) object
 */
lisp_object_t *
lisp_make_t(void)
{
	return LISP_T;
}

/*
 * Create integer object
 */
lisp_object_t *
lisp_make_integer(long value)
{
	lisp_object_t *obj = lisp_alloc();
	obj->type = LISP_TYPE_INTEGER;
	obj->value.integer = value;
	return obj;
}

/*
 * Create floating-point object
 */
lisp_object_t *
lisp_make_float(double value)
{
	lisp_object_t *obj = lisp_alloc();
	obj->type = LISP_TYPE_FLOAT;
	obj->value.floating = value;
	return obj;
}

/*
 * Create string object
 */
lisp_object_t *
lisp_make_string(const char *str)
{
	lisp_object_t *obj = lisp_alloc();
	obj->type = LISP_TYPE_STRING;
	obj->value.string = strdup(str);
	return obj;
}

/*
 * Create symbol object
 */
lisp_object_t *
lisp_make_symbol(const char *name)
{
	lisp_object_t *obj = lisp_alloc();
	obj->type = LISP_TYPE_SYMBOL;
	obj->value.symbol = strdup(name);
	return obj;
}

/*
 * Create cons cell (pair)
 */
lisp_object_t *
lisp_make_cons(lisp_object_t *car, lisp_object_t *cdr)
{
	lisp_object_t *obj = lisp_alloc();
	obj->type = LISP_TYPE_CONS;
	obj->value.cons.car = car;
	obj->value.cons.cdr = cdr;
	return obj;
}
