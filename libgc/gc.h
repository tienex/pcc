/*
 * Copyright (c) 2025 PCC Project
 *
 * Garbage Collector and Automatic Reference Counting Library
 *
 * Reusable across compiler projects. Provides:
 * - Mark-and-sweep garbage collection
 * - Generational GC
 * - Automatic Reference Counting (ARC)
 * - Concurrent and incremental collection
 * - Conservative and precise GC modes
 */

#ifndef _PCC_GC_H_
#define _PCC_GC_H_

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * GC Modes
 */
typedef enum {
	GC_MODE_NONE = 0,         /* No GC - manual only */
	GC_MODE_REFCOUNT,         /* Reference counting */
	GC_MODE_MARK_SWEEP,       /* Mark and sweep */
	GC_MODE_COPYING,          /* Copying collector */
	GC_MODE_GENERATIONAL,     /* Generational GC */
	GC_MODE_INCREMENTAL,      /* Incremental GC */
	GC_MODE_CONCURRENT        /* Concurrent GC */
} gc_mode_t;

/*
 * GC Precision
 */
typedef enum {
	GC_CONSERVATIVE = 0,      /* Conservative - scan all words */
	GC_PRECISE                /* Precise - use type information */
} gc_precision_t;

/*
 * Object Flags
 */
typedef enum {
	GC_FLAG_NONE = 0,
	GC_FLAG_ATOMIC = 1,       /* Object contains no pointers */
	GC_FLAG_PINNED = 2,       /* Cannot be moved */
	GC_FLAG_FINALIZABLE = 4,  /* Has finalizer */
	GC_FLAG_WEAK = 8          /* Weak reference */
} gc_flags_t;

/*
 * GC Configuration
 */
typedef struct {
	gc_mode_t mode;
	gc_precision_t precision;
	size_t initial_heap_size;
	size_t max_heap_size;
	int num_generations;      /* For generational GC */
	int incremental_step_ms;  /* For incremental GC */
	int enable_parallel;      /* Use multiple threads */
	int num_threads;          /* Number of GC threads */
} gc_config_t;

/*
 * Type Descriptor for Precise GC
 */
typedef struct gc_type_desc {
	const char *name;
	size_t size;
	size_t pointer_count;
	size_t *pointer_offsets;  /* Offsets of pointers in object */
	void (*finalizer)(void *obj);
} gc_type_desc_t;

/*
 * GC Statistics
 */
typedef struct {
	size_t total_allocated;
	size_t total_freed;
	size_t current_heap_size;
	size_t num_collections;
	size_t num_objects;
	uint64_t total_gc_time_us;
	double fragmentation_ratio;
} gc_stats_t;

/*
 * Initialization and Configuration
 */

/* Initialize GC with configuration */
int gc_init(const gc_config_t *config);

/* Shutdown GC and free all resources */
void gc_shutdown(void);

/* Set GC mode at runtime */
int gc_set_mode(gc_mode_t mode);

/* Get current GC configuration */
const gc_config_t *gc_get_config(void);

/*
 * Memory Allocation
 */

/* Allocate GC-managed memory */
void *gc_alloc(size_t size);

/* Allocate with specific flags */
void *gc_alloc_flags(size_t size, gc_flags_t flags);

/* Allocate with type descriptor (precise GC) */
void *gc_alloc_typed(const gc_type_desc_t *type);

/* Allocate atomic (no pointers) */
void *gc_alloc_atomic(size_t size);

/* Reallocate GC-managed memory */
void *gc_realloc(void *ptr, size_t new_size);

/* Free GC-managed memory (decrements refcount or marks for collection) */
void gc_free(void *ptr);

/*
 * Reference Counting (ARC)
 */

/* Increment reference count */
void *gc_retain(void *ptr);

/* Decrement reference count */
void gc_release(void *ptr);

/* Get reference count */
size_t gc_refcount(void *ptr);

/* Create weak reference */
void *gc_weak_ref(void *ptr);

/* Check if weak reference is still valid */
int gc_weak_valid(void *weak_ref);

/*
 * Collection Control
 */

/* Trigger immediate collection */
void gc_collect(void);

/* Trigger minor collection (young generation only) */
void gc_collect_minor(void);

/* Trigger full collection */
void gc_collect_full(void);

/* Disable automatic collection */
void gc_disable(void);

/* Enable automatic collection */
void gc_enable(void);

/* Check if GC is enabled */
int gc_is_enabled(void);

/*
 * Roots and References
 */

/* Register a root pointer */
void gc_add_root(void *ptr);

/* Unregister a root pointer */
void gc_remove_root(void *ptr);

/* Register stack base for conservative scanning */
void gc_register_thread_stack(void *stack_base);

/* Unregister thread stack */
void gc_unregister_thread_stack(void);

/*
 * Finalizers
 */

/* Register finalizer for object */
void gc_register_finalizer(void *ptr, void (*finalizer)(void *obj));

/* Unregister finalizer */
void gc_unregister_finalizer(void *ptr);

/*
 * Write Barriers (for incremental/concurrent GC)
 */

/* Notify GC of pointer write */
void gc_write_barrier(void *obj, void **field, void *value);

/* Batch write barrier */
void gc_write_barrier_range(void *obj, void **start, void **end);

/*
 * Statistics and Monitoring
 */

/* Get GC statistics */
const gc_stats_t *gc_get_stats(void);

/* Print GC statistics */
void gc_print_stats(void);

/* Reset statistics */
void gc_reset_stats(void);

/*
 * Advanced Features
 */

/* Pin object (prevent moving) */
void gc_pin(void *ptr);

/* Unpin object */
void gc_unpin(void *ptr);

/* Get object size */
size_t gc_size(void *ptr);

/* Check if pointer is GC-managed */
int gc_is_managed(void *ptr);

/* Get object generation (for generational GC) */
int gc_get_generation(void *ptr);

/* Promote object to older generation */
void gc_promote(void *ptr);

/*
 * Callbacks
 */

/* Called before collection starts */
typedef void (*gc_pre_collect_callback_t)(void);

/* Called after collection completes */
typedef void (*gc_post_collect_callback_t)(const gc_stats_t *stats);

/* Called when out of memory */
typedef void (*gc_oom_callback_t)(size_t requested_size);

/* Register callbacks */
void gc_set_pre_collect_callback(gc_pre_collect_callback_t callback);
void gc_set_post_collect_callback(gc_post_collect_callback_t callback);
void gc_set_oom_callback(gc_oom_callback_t callback);

/*
 * Debugging
 */

/* Enable GC debugging */
void gc_enable_debug(int level);

/* Dump GC heap */
void gc_dump_heap(void);

/* Verify heap consistency */
int gc_verify_heap(void);

/* Get heap fragmentation info */
double gc_get_fragmentation(void);

#ifdef __cplusplus
}
#endif

#endif /* _PCC_GC_H_ */
