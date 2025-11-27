/*
 * Copyright (c) 2025 PCC Wirth Languages Runtime Library
 *
 * Memory Management
 * - Generic allocation/deallocation
 * - Modula-3 traced/untraced heaps
 * - Type system support for Oberon-2, Component Pascal, Ada
 * - Memory operations (fill, copy, compare)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "wirthrt.h"

/*
 * Generic Memory Allocation
 */

void *wirth_new(size_t size) {
	void *ptr = malloc(size);
	if (!ptr && size > 0) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Memory allocation failed");
		return NULL;
	}
	/* Initialize to zero (Pascal/Modula-2 convention) */
	if (ptr) {
		memset(ptr, 0, size);
	}
	return ptr;
}

void wirth_dispose(void *ptr) {
	if (ptr) {
		free(ptr);
	}
}

void *wirth_allocate(size_t size) {
	/* Modula-2 ALLOCATE - similar to NEW but doesn't initialize */
	void *ptr = malloc(size);
	if (!ptr && size > 0) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Memory allocation failed");
	}
	return ptr;
}

void wirth_deallocate(void *ptr, size_t size) {
	/* Modula-2 DEALLOCATE - size parameter for compatibility */
	(void)size;  /* Size not used in this implementation */
	if (ptr) {
		free(ptr);
	}
}

/*
 * Modula-3 Traced/Untraced Heaps
 *
 * Note: This is a simplified implementation. A full implementation
 * would include garbage collection for traced heap.
 */

void *wirth_new_traced(size_t size, WirthTypeDescriptor *type) {
	/* For now, allocate from regular heap */
	/* A full GC implementation would track this allocation */
	(void)type;  /* Type descriptor for future GC implementation */

	void *ptr = malloc(size);
	if (!ptr && size > 0) {
		wirth_runtime_error(WIRTH_ERR_STORAGE_ERROR, "Traced heap allocation failed");
		return NULL;
	}
	if (ptr) {
		memset(ptr, 0, size);
	}
	return ptr;
}

void *wirth_new_untraced(size_t size) {
	/* Untraced heap - manual deallocation required */
	void *ptr = malloc(size);
	if (!ptr && size > 0) {
		wirth_runtime_error(WIRTH_ERR_STORAGE_ERROR, "Untraced heap allocation failed");
		return NULL;
	}
	if (ptr) {
		memset(ptr, 0, size);
	}
	return ptr;
}

/*
 * Memory Operations
 */

void wirth_fill(void *dest, size_t count, uint8_t value) {
	if (dest && count > 0) {
		memset(dest, value, count);
	}
}

void wirth_copy(const void *src, void *dest, size_t count) {
	if (src && dest && count > 0) {
		memmove(dest, src, count);  /* Use memmove for overlapping regions */
	}
}

int wirth_compare_mem(const void *s1, const void *s2, size_t count) {
	if (!s1 || !s2) {
		return 0;
	}
	return memcmp(s1, s2, count);
}

/*
 * Type System Support (for OOP features)
 */

int wirth_is_type(void *obj, WirthTypeDescriptor *type) {
	if (!obj || !type) {
		return 0;
	}

	/* In a full implementation, would check the object's type descriptor
	 * against the provided type and its base types */

	/* For now, return true (type checking done at compile time) */
	return 1;
}

void *wirth_type_guard(void *obj, WirthTypeDescriptor *type) {
	if (!obj) {
		wirth_runtime_error(WIRTH_ERR_NIL_POINTER, "Type guard on nil pointer");
		return NULL;
	}

	if (!type) {
		wirth_runtime_error(WIRTH_ERR_TYPE_GUARD, "Invalid type descriptor");
		return NULL;
	}

	/* In a full implementation, would check if object is of the specified type */
	if (!wirth_is_type(obj, type)) {
		wirth_runtime_error(WIRTH_ERR_TYPE_GUARD, "Type guard failed");
		return NULL;
	}

	return obj;
}

WirthTypeDescriptor *wirth_type_of(void *obj) {
	if (!obj) {
		return NULL;
	}

	/* In a full implementation, would return the actual type descriptor
	 * stored with the object */

	/* For now, return NULL (type checking done at compile time) */
	return NULL;
}
