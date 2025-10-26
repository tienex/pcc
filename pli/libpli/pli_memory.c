/*
 * Copyright (c) 2025 PCC PL/I Runtime Library
 *
 * Memory management functions
 */

#include "pli_runtime.h"
#include <stdlib.h>
#include <string.h>

/* Allocation tracking structure */
typedef struct alloc_entry {
	void *ptr;
	size_t size;
	int allocated;
	struct alloc_entry *next;
} alloc_entry_t;

static alloc_entry_t *alloc_list = NULL;

/* Find allocation entry */
static alloc_entry_t *find_allocation(void *ptr) {
	for (alloc_entry_t *e = alloc_list; e != NULL; e = e->next) {
		if (e->ptr == ptr) {
			return e;
		}
	}
	return NULL;
}

/* Add allocation entry */
static void add_allocation(void *ptr, size_t size) {
	alloc_entry_t *e = malloc(sizeof(alloc_entry_t));
	if (!e) return;

	e->ptr = ptr;
	e->size = size;
	e->allocated = 1;
	e->next = alloc_list;
	alloc_list = e;
}

/* Remove allocation entry */
static void remove_allocation(void *ptr) {
	alloc_entry_t **pp = &alloc_list;

	while (*pp) {
		if ((*pp)->ptr == ptr) {
			alloc_entry_t *e = *pp;
			*pp = e->next;
			free(e);
			return;
		}
		pp = &(*pp)->next;
	}
}

/* ALLOCATE - Allocate memory */
void *pli_allocate(size_t size) {
	void *ptr = malloc(size);
	if (ptr) {
		memset(ptr, 0, size);
		add_allocation(ptr, size);
	} else {
		pli_signal("ERROR");  /* Out of memory */
	}
	return ptr;
}

/* FREE - Free allocated memory */
void pli_free(void *ptr) {
	if (!ptr) return;

	alloc_entry_t *e = find_allocation(ptr);
	if (e && e->allocated) {
		free(ptr);
		remove_allocation(ptr);
	} else {
		pli_signal("ERROR");  /* Invalid free */
	}
}

/* ADDR - Get address of variable */
void *pli_addr(void *var) {
	return var;
}

/* NULL - Return null pointer */
void *pli_null(void) {
	return NULL;
}

/* SIZE - Get size of allocated block */
size_t pli_size(void *ptr) {
	if (!ptr) return 0;

	alloc_entry_t *e = find_allocation(ptr);
	if (e) {
		return e->size;
	}
	return 0;
}

/* ALLOCATION - Check if pointer is allocated (returns 1 if allocated) */
pli_fixed_t pli_allocation(void *ptr) {
	if (!ptr) return 0;

	alloc_entry_t *e = find_allocation(ptr);
	if (e && e->allocated) {
		return 1;
	}
	return 0;
}
