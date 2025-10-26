/*
 * Copyright (c) 2025 PCC Generic Garbage Collector
 *
 * Generic mark-and-sweep garbage collector
 * Language-agnostic implementation for use across PCC language runtimes
 */

#ifndef PCC_GC_H
#define PCC_GC_H

#include <stddef.h>
#include <stdint.h>

/* Opaque GC context */
typedef struct gc_context gc_context_t;

/* GC object header */
typedef struct gc_object {
	size_t size;           /* Size in bytes */
	uint8_t marked;        /* Mark bit for GC */
	uint8_t flags;         /* User-defined flags */
	struct gc_object *next;
} gc_object_t;

/* GC statistics */
typedef struct gc_stats {
	size_t total_allocated;
	size_t total_freed;
	size_t current_usage;
	size_t num_objects;
	size_t num_collections;
	size_t heap_size;
} gc_stats_t;

/* GC configuration */
typedef struct gc_config {
	size_t heap_size;           /* Initial heap size (0 = default) */
	size_t gc_threshold;        /* Trigger GC when usage exceeds this */
	float growth_factor;        /* Heap growth factor (default: 1.5) */
	int enable_compaction;      /* Enable heap compaction */
	int enable_pools;           /* Enable memory pools for small objects */
	int verbose;                /* Print GC debug messages */
} gc_config_t;

/* Callback for marking referenced objects
 * Called during GC mark phase for each live object
 * Should call gc_mark() on all objects referenced by this object
 */
typedef void (*gc_mark_fn)(gc_context_t *gc, void *obj, void *userdata);

/* Callback for finalizing objects
 * Called before an object is freed
 */
typedef void (*gc_finalize_fn)(void *obj, void *userdata);

/* Initialize GC with configuration */
gc_context_t *gc_init(const gc_config_t *config);

/* Destroy GC context and free all memory */
void gc_destroy(gc_context_t *gc);

/* Allocate object from GC heap */
void *gc_alloc(gc_context_t *gc, size_t size);

/* Allocate object with specific alignment */
void *gc_alloc_aligned(gc_context_t *gc, size_t size, size_t align);

/* Register a root object for GC */
void gc_register_root(gc_context_t *gc, void **root);

/* Unregister a root object */
void gc_unregister_root(gc_context_t *gc, void **root);

/* Set mark callback for object traversal */
void gc_set_mark_callback(gc_context_t *gc, gc_mark_fn mark_fn, void *userdata);

/* Set finalize callback for object cleanup */
void gc_set_finalize_callback(gc_context_t *gc, gc_finalize_fn finalize_fn, void *userdata);

/* Mark an object as reachable (called from mark callback) */
void gc_mark(gc_context_t *gc, void *obj);

/* Trigger garbage collection */
size_t gc_collect(gc_context_t *gc);

/* Force heap compaction */
size_t gc_compact(gc_context_t *gc);

/* Get GC statistics */
void gc_get_stats(gc_context_t *gc, gc_stats_t *stats);

/* Get object header from object pointer */
gc_object_t *gc_get_header(void *obj);

/* Get object pointer from header */
void *gc_get_object(gc_object_t *header);

/* Set/get object flags (user-defined) */
void gc_set_flags(void *obj, uint8_t flags);
uint8_t gc_get_flags(void *obj);

/* Print GC statistics (for debugging) */
void gc_print_stats(gc_context_t *gc);

/* Enable/disable automatic GC */
void gc_set_auto(gc_context_t *gc, int enabled);

/* Get current heap usage */
size_t gc_usage(gc_context_t *gc);

/* Resize heap */
int gc_resize_heap(gc_context_t *gc, size_t new_size);

/* Weak reference support */
typedef struct gc_weak gc_weak_t;

/* Create a weak reference to an object */
gc_weak_t *gc_weak_create(gc_context_t *gc, void *obj);

/* Get object from weak reference (returns NULL if collected) */
void *gc_weak_get(gc_weak_t *weak);

/* Release weak reference */
void gc_weak_release(gc_weak_t *weak);

/* Default configuration */
#define GC_DEFAULT_CONFIG { \
	.heap_size = 16 * 1024 * 1024,  /* 16MB */ \
	.gc_threshold = 8 * 1024 * 1024, /* 8MB */ \
	.growth_factor = 1.5, \
	.enable_compaction = 0, \
	.enable_pools = 1, \
	.verbose = 0 \
}

#endif /* PCC_GC_H */
