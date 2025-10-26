/*
 * Copyright (c) 2025 PCC Generic Garbage Collector
 *
 * Core GC implementation
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "gc.h"
#include "gc_internal.h"

/* Root set for GC */
#define MAX_ROOTS 4096

/* GC context structure */
struct gc_context {
	/* Configuration */
	gc_config_t config;

	/* Object list */
	gc_object_t *objects;

	/* Root set */
	root_entry_t *roots;
	size_t num_roots;

	/* Weak references */
	gc_weak_t *weak_refs;

	/* Memory pools */
	gc_pool_manager_t *pool_manager;

	/* Callbacks */
	gc_mark_fn mark_callback;
	void *mark_userdata;
	gc_finalize_fn finalize_callback;
	void *finalize_userdata;

	/* Statistics */
	gc_stats_t stats;

	/* Automatic GC */
	int auto_gc;
};

/* Forward declarations for weak reference support */
void gc_weak_invalidate(gc_context_t *gc, void *obj);
void gc_weak_cleanup(gc_context_t *gc);

/* Internal helper to access weak_refs field */
gc_weak_t **
gc_get_weak_refs(gc_context_t *gc)
{
	return &gc->weak_refs;
}

/*
 * Initialize GC with configuration
 */
gc_context_t *
gc_init(const gc_config_t *config)
{
	gc_context_t *gc;

	gc = calloc(1, sizeof(gc_context_t));
	if (!gc)
		return NULL;

	/* Set configuration */
	if (config) {
		gc->config = *config;
	} else {
		gc_config_t default_config = GC_DEFAULT_CONFIG;
		gc->config = default_config;
	}

	/* Initialize stats */
	gc->stats.heap_size = gc->config.heap_size;

	/* Initialize memory pools */
	if (gc->config.enable_pools) {
		gc->pool_manager = gc_pool_init();
		if (!gc->pool_manager) {
			free(gc);
			return NULL;
		}
	}

	/* Enable auto GC by default */
	gc->auto_gc = 1;

	if (gc->config.verbose) {
		printf("GC initialized: heap_size=%zu, threshold=%zu, pools=%s\n",
		       gc->config.heap_size, gc->config.gc_threshold,
		       gc->config.enable_pools ? "enabled" : "disabled");
	}

	return gc;
}

/*
 * Destroy GC context
 */
void
gc_destroy(gc_context_t *gc)
{
	gc_object_t *obj, *next;
	root_entry_t *root, *root_next;

	if (!gc)
		return;

	/* Free all objects */
	obj = gc->objects;
	while (obj) {
		next = obj->next;

		/* Call finalizer if set */
		if (gc->finalize_callback) {
			gc->finalize_callback(gc_get_object(obj), gc->finalize_userdata);
		}

		free(obj);
		obj = next;
	}

	/* Free roots */
	root = gc->roots;
	while (root) {
		root_next = root->next;
		free(root);
		root = root_next;
	}

	/* Clean up weak references */
	gc_weak_cleanup(gc);

	/* Destroy pool manager */
	if (gc->pool_manager) {
		gc_pool_destroy(gc->pool_manager);
	}

	free(gc);
}

/*
 * Allocate object from GC heap
 */
void *
gc_alloc(gc_context_t *gc, size_t size)
{
	return gc_alloc_aligned(gc, size, sizeof(void *));
}

/*
 * Allocate object with alignment
 */
void *
gc_alloc_aligned(gc_context_t *gc, size_t size, size_t align)
{
	gc_object_t *obj;
	void *ptr;
	size_t total_size;

	if (!gc)
		return NULL;

	/* Check if GC is needed */
	if (gc->auto_gc && gc->stats.current_usage > gc->config.gc_threshold) {
		gc_collect(gc);
	}

	/* Try pool allocation for small objects first */
	if (gc->pool_manager && align <= sizeof(void *)) {
		ptr = gc_pool_alloc(gc->pool_manager, size);
		if (ptr) {
			obj = gc_get_header(ptr);
			/* Add to object list */
			obj->next = gc->objects;
			gc->objects = obj;

			/* Update stats */
			total_size = sizeof(gc_object_t) + size;
			gc->stats.total_allocated += total_size;
			gc->stats.current_usage += total_size;
			gc->stats.num_objects++;

			return ptr;
		}
	}

	/* Allocate header + data */
	total_size = sizeof(gc_object_t) + size;

	/* Align size */
	if (align > 1) {
		total_size = (total_size + align - 1) & ~(align - 1);
	}

	obj = calloc(1, total_size);
	if (!obj) {
		/* Try GC and retry */
		gc_collect(gc);
		obj = calloc(1, total_size);
		if (!obj) {
			fprintf(stderr, "gc_alloc: out of memory\n");
			return NULL;
		}
	}

	/* Initialize header */
	obj->size = size;
	obj->marked = 0;
	obj->flags = 0;

	/* Add to object list */
	obj->next = gc->objects;
	gc->objects = obj;

	/* Update stats */
	gc->stats.total_allocated += total_size;
	gc->stats.current_usage += total_size;
	gc->stats.num_objects++;

	/* Return pointer to data (after header) */
	ptr = (void *)(obj + 1);

	return ptr;
}

/*
 * Register a root
 */
void
gc_register_root(gc_context_t *gc, void **root)
{
	root_entry_t *entry;

	if (!gc || !root)
		return;

	entry = malloc(sizeof(root_entry_t));
	if (!entry)
		return;

	entry->root = root;
	entry->next = gc->roots;
	gc->roots = entry;
	gc->num_roots++;
}

/*
 * Unregister a root
 */
void
gc_unregister_root(gc_context_t *gc, void **root)
{
	root_entry_t *entry, *prev;

	if (!gc || !root)
		return;

	prev = NULL;
	entry = gc->roots;

	while (entry) {
		if (entry->root == root) {
			if (prev)
				prev->next = entry->next;
			else
				gc->roots = entry->next;

			free(entry);
			gc->num_roots--;
			return;
		}
		prev = entry;
		entry = entry->next;
	}
}

/*
 * Set mark callback
 */
void
gc_set_mark_callback(gc_context_t *gc, gc_mark_fn mark_fn, void *userdata)
{
	if (gc) {
		gc->mark_callback = mark_fn;
		gc->mark_userdata = userdata;
	}
}

/*
 * Set finalize callback
 */
void
gc_set_finalize_callback(gc_context_t *gc, gc_finalize_fn finalize_fn, void *userdata)
{
	if (gc) {
		gc->finalize_callback = finalize_fn;
		gc->finalize_userdata = userdata;
	}
}

/*
 * Mark an object
 */
void
gc_mark(gc_context_t *gc, void *obj)
{
	gc_object_t *header;

	if (!gc || !obj)
		return;

	header = gc_get_header(obj);

	/* Already marked? */
	if (header->marked)
		return;

	/* Mark this object */
	header->marked = 1;

	/* Call mark callback to mark children */
	if (gc->mark_callback) {
		gc->mark_callback(gc, obj, gc->mark_userdata);
	}
}

/*
 * Garbage collection
 */
size_t
gc_collect(gc_context_t *gc)
{
	root_entry_t *root;
	gc_object_t *obj, *prev, *next;
	size_t freed = 0;
	size_t freed_bytes = 0;

	if (!gc)
		return 0;

	if (gc->config.verbose) {
		printf("GC: Starting collection (usage: %zu bytes, %zu objects)\n",
		       gc->stats.current_usage, gc->stats.num_objects);
	}

	/* Mark phase: mark all reachable objects from roots */
	root = gc->roots;
	while (root) {
		if (root->root && *root->root) {
			gc_mark(gc, *root->root);
		}
		root = root->next;
	}

	/* Sweep phase: free unmarked objects */
	prev = NULL;
	obj = gc->objects;

	while (obj) {
		next = obj->next;

		if (!obj->marked) {
			/* Free unmarked object */
			void *obj_ptr = gc_get_object(obj);

			/* Invalidate weak references */
			gc_weak_invalidate(gc, obj_ptr);

			/* Call finalizer if set */
			if (gc->finalize_callback) {
				gc->finalize_callback(obj_ptr, gc->finalize_userdata);
			}

			/* Remove from list */
			if (prev)
				prev->next = next;
			else
				gc->objects = next;

			/* Update stats */
			size_t obj_size = sizeof(gc_object_t) + obj->size;
			freed_bytes += obj_size;
			freed++;

			/* Free object (return to pool if pooled) */
			if (gc->pool_manager && gc_pool_is_pooled(obj_ptr)) {
				gc_pool_free(gc->pool_manager, obj_ptr);
			} else {
				free(obj);
			}
		} else {
			/* Reset mark for next GC */
			obj->marked = 0;
			prev = obj;
		}

		obj = next;
	}

	/* Update stats */
	gc->stats.total_freed += freed_bytes;
	gc->stats.current_usage -= freed_bytes;
	gc->stats.num_objects -= freed;
	gc->stats.num_collections++;

	if (gc->config.verbose) {
		printf("GC: Freed %zu objects (%zu bytes)\n", freed, freed_bytes);
		printf("GC: Current usage: %zu bytes, %zu objects\n",
		       gc->stats.current_usage, gc->stats.num_objects);
	}

	return freed;
}

/*
 * Get object header
 */
gc_object_t *
gc_get_header(void *obj)
{
	if (!obj)
		return NULL;

	return ((gc_object_t *)obj) - 1;
}

/*
 * Get object pointer from header
 */
void *
gc_get_object(gc_object_t *header)
{
	if (!header)
		return NULL;

	return (void *)(header + 1);
}

/*
 * Set object flags
 */
void
gc_set_flags(void *obj, uint8_t flags)
{
	gc_object_t *header = gc_get_header(obj);
	if (header)
		header->flags = flags;
}

/*
 * Get object flags
 */
uint8_t
gc_get_flags(void *obj)
{
	gc_object_t *header = gc_get_header(obj);
	return header ? header->flags : 0;
}

/*
 * Get GC statistics
 */
void
gc_get_stats(gc_context_t *gc, gc_stats_t *stats)
{
	if (gc && stats)
		*stats = gc->stats;
}

/*
 * Print GC statistics
 */
void
gc_print_stats(gc_context_t *gc)
{
	if (!gc)
		return;

	printf("GC Statistics:\n");
	printf("  Total allocated:  %zu bytes\n", gc->stats.total_allocated);
	printf("  Total freed:      %zu bytes\n", gc->stats.total_freed);
	printf("  Current usage:    %zu bytes\n", gc->stats.current_usage);
	printf("  Number of objects: %zu\n", gc->stats.num_objects);
	printf("  Collections:      %zu\n", gc->stats.num_collections);
	printf("  Heap size:        %zu bytes\n", gc->stats.heap_size);
}

/*
 * Enable/disable automatic GC
 */
void
gc_set_auto(gc_context_t *gc, int enabled)
{
	if (gc)
		gc->auto_gc = enabled;
}

/*
 * Get current heap usage
 */
size_t
gc_usage(gc_context_t *gc)
{
	return gc ? gc->stats.current_usage : 0;
}

/*
 * Resize heap
 */
int
gc_resize_heap(gc_context_t *gc, size_t new_size)
{
	if (!gc)
		return -1;

	gc->config.heap_size = new_size;
	gc->stats.heap_size = new_size;

	return 0;
}
