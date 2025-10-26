/*
 * Copyright (c) 2025 PCC Generic Garbage Collector
 *
 * Memory pool implementation for small object optimization
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "gc.h"
#include "gc_internal.h"

/* Pool size classes (powers of 2) */
#define POOL_MIN_SIZE 16
#define POOL_MAX_SIZE 256
#define POOL_NUM_CLASSES 5  /* 16, 32, 64, 128, 256 */

/* Free list node */
typedef struct pool_node {
	struct pool_node *next;
} pool_node_t;

/* Memory pool for a specific size class */
typedef struct memory_pool {
	size_t object_size;
	pool_node_t *free_list;
	size_t num_free;
	size_t num_allocated;
} memory_pool_t;

/* Pool manager */
struct gc_pool_manager {
	memory_pool_t pools[POOL_NUM_CLASSES];
	int enabled;
};

/*
 * Get size class index for a given size
 * Returns -1 if size is too large for pooling
 */
static int
get_size_class(size_t size)
{
	if (size <= 16) return 0;
	if (size <= 32) return 1;
	if (size <= 64) return 2;
	if (size <= 128) return 3;
	if (size <= 256) return 4;
	return -1;
}

/*
 * Get object size for a size class
 */
static size_t
get_class_size(int class)
{
	return POOL_MIN_SIZE << class;  /* 16 * 2^class */
}

/*
 * Initialize pool manager
 */
gc_pool_manager_t *
gc_pool_init(void)
{
	gc_pool_manager_t *pm;
	int i;

	pm = calloc(1, sizeof(gc_pool_manager_t));
	if (!pm)
		return NULL;

	/* Initialize each pool */
	for (i = 0; i < POOL_NUM_CLASSES; i++) {
		pm->pools[i].object_size = get_class_size(i);
		pm->pools[i].free_list = NULL;
		pm->pools[i].num_free = 0;
		pm->pools[i].num_allocated = 0;
	}

	pm->enabled = 1;
	return pm;
}

/*
 * Destroy pool manager
 */
void
gc_pool_destroy(gc_pool_manager_t *pm)
{
	int i;
	pool_node_t *node, *next;

	if (!pm)
		return;

	/* Free all nodes in free lists */
	for (i = 0; i < POOL_NUM_CLASSES; i++) {
		node = pm->pools[i].free_list;
		while (node) {
			next = node->next;
			free(node);
			node = next;
		}
	}

	free(pm);
}

/*
 * Allocate from pool
 * Returns NULL if size is not suitable for pooling
 */
void *
gc_pool_alloc(gc_pool_manager_t *pm, size_t size)
{
	int class;
	memory_pool_t *pool;
	pool_node_t *node;
	gc_object_t *obj;
	void *ptr;

	if (!pm || !pm->enabled)
		return NULL;

	/* Get size class */
	class = get_size_class(size + sizeof(gc_object_t));
	if (class < 0)
		return NULL;

	pool = &pm->pools[class];

	/* Try to get from free list */
	if (pool->free_list) {
		node = pool->free_list;
		pool->free_list = node->next;
		pool->num_free--;

		/* Initialize object header */
		obj = (gc_object_t *)node;
		memset(obj, 0, sizeof(gc_object_t));
		obj->size = size;
		obj->flags = 0x80;  /* Mark as pooled */

		ptr = (void *)(obj + 1);
		return ptr;
	}

	/* Allocate new memory */
	obj = calloc(1, pool->object_size);
	if (!obj)
		return NULL;

	obj->size = size;
	obj->flags = 0x80;  /* Mark as pooled */
	pool->num_allocated++;

	ptr = (void *)(obj + 1);
	return ptr;
}

/*
 * Return object to pool
 */
void
gc_pool_free(gc_pool_manager_t *pm, void *obj)
{
	gc_object_t *header;
	pool_node_t *node;
	int class;

	if (!pm || !obj)
		return;

	header = gc_get_header(obj);
	if (!header || !(header->flags & 0x80))
		return;  /* Not a pooled object */

	/* Get size class */
	class = get_size_class(header->size + sizeof(gc_object_t));
	if (class < 0)
		return;

	/* Add to free list */
	node = (pool_node_t *)header;
	node->next = pm->pools[class].free_list;
	pm->pools[class].free_list = node;
	pm->pools[class].num_free++;
}

/*
 * Check if object is from pool
 */
int
gc_pool_is_pooled(void *obj)
{
	gc_object_t *header;

	if (!obj)
		return 0;

	header = gc_get_header(obj);
	if (!header)
		return 0;

	return (header->flags & 0x80) != 0;
}

/*
 * Get pool statistics
 */
void
gc_pool_stats(gc_pool_manager_t *pm)
{
	int i;
	memory_pool_t *pool;

	if (!pm)
		return;

	printf("Memory Pool Statistics:\n");
	printf("  Enabled: %s\n", pm->enabled ? "yes" : "no");
	printf("  Size Classes:\n");

	for (i = 0; i < POOL_NUM_CLASSES; i++) {
		pool = &pm->pools[i];
		printf("    %3zu bytes: %zu allocated, %zu free\n",
		       pool->object_size, pool->num_allocated, pool->num_free);
	}
}
