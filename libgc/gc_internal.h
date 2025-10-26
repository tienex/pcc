/*
 * Copyright (c) 2025 PCC Generic Garbage Collector
 *
 * Internal header for GC implementation
 */

#ifndef PCC_GC_INTERNAL_H
#define PCC_GC_INTERNAL_H

#include "gc.h"

/* Forward declarations */
typedef struct gc_pool_manager gc_pool_manager_t;

/* Root set entry */
typedef struct root_entry {
	void **root;
	struct root_entry *next;
} root_entry_t;

/* Weak reference structure (same as gc_weak_t from gc.h) */
struct gc_weak {
	void *object;               /* Weak pointer to object */
	struct gc_weak *next;       /* Next in list */
	struct gc_weak *prev;       /* Prev in list */
	int valid;                  /* 1 if object still alive */
};

/* Extended GC context (internal use only) */
typedef struct gc_context_internal {
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
} gc_context_internal_t;

/* Pool management functions */
gc_pool_manager_t *gc_pool_init(void);
void gc_pool_destroy(gc_pool_manager_t *pm);
void *gc_pool_alloc(gc_pool_manager_t *pm, size_t size);
void gc_pool_free(gc_pool_manager_t *pm, void *obj);
int gc_pool_is_pooled(void *obj);
void gc_pool_stats(gc_pool_manager_t *pm);

/* Weak reference internal functions */
void gc_weak_invalidate(gc_context_t *gc, void *obj);
void gc_weak_cleanup(gc_context_t *gc);

/* Helper to access weak_refs field (gc_context is opaque) */
gc_weak_t **gc_get_weak_refs(gc_context_t *gc);

#endif /* PCC_GC_INTERNAL_H */
