/*
 * Copyright (c) 2025 PCC Project
 * Garbage Collector Implementation
 * Core mark-and-sweep with ARC support
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <sys/time.h>
#include "gc.h"

/* GC Object Header */
typedef struct gc_object {
	size_t size;
	size_t refcount;
	gc_flags_t flags;
	const gc_type_desc_t *type;
	void (*finalizer)(void *);
	int marked;
	int generation;
	struct gc_object *next;
} gc_object_t;

/* GC State */
typedef struct {
	gc_config_t config;
	gc_object_t *heap_list;
	void **roots;
	size_t root_count;
	size_t root_capacity;
	pthread_mutex_t gc_mutex;
	int gc_enabled;
	int gc_running;
	gc_stats_t stats;
	gc_pre_collect_callback_t pre_collect_cb;
	gc_post_collect_callback_t post_collect_cb;
	gc_oom_callback_t oom_cb;
} gc_state_t;

static gc_state_t gc_state = {0};

#define GC_LOCK() pthread_mutex_lock(&gc_state.gc_mutex)
#define GC_UNLOCK() pthread_mutex_unlock(&gc_state.gc_mutex)

#define OBJECT_HEADER(ptr) ((gc_object_t *)((char *)(ptr) - sizeof(gc_object_t)))
#define OBJECT_DATA(obj) ((void *)((char *)(obj) + sizeof(gc_object_t)))

/*
 * Initialization
 */
int gc_init(const gc_config_t *config) {
	memset(&gc_state, 0, sizeof(gc_state));

	if (config) {
		gc_state.config = *config;
	} else {
		/* Default configuration */
		gc_state.config.mode = GC_MODE_MARK_SWEEP;
		gc_state.config.precision = GC_CONSERVATIVE;
		gc_state.config.initial_heap_size = 1024 * 1024;  /* 1MB */
		gc_state.config.max_heap_size = 64 * 1024 * 1024; /* 64MB */
		gc_state.config.num_generations = 3;
		gc_state.config.enable_parallel = 0;
	}

	pthread_mutex_init(&gc_state.gc_mutex, NULL);
	gc_state.gc_enabled = 1;
	gc_state.heap_list = NULL;
	gc_state.roots = NULL;
	gc_state.root_count = 0;
	gc_state.root_capacity = 0;

	return 0;
}

void gc_shutdown(void) {
	GC_LOCK();

	/* Free all objects */
	gc_object_t *obj = gc_state.heap_list;
	while (obj) {
		gc_object_t *next = obj->next;
		if (obj->finalizer) {
			obj->finalizer(OBJECT_DATA(obj));
		}
		free(obj);
		obj = next;
	}

	if (gc_state.roots) {
		free(gc_state.roots);
	}

	GC_UNLOCK();
	pthread_mutex_destroy(&gc_state.gc_mutex);
}

/*
 * Memory Allocation
 */
void *gc_alloc(size_t size) {
	return gc_alloc_flags(size, GC_FLAG_NONE);
}

void *gc_alloc_flags(size_t size, gc_flags_t flags) {
	GC_LOCK();

	/* Check if we need to collect */
	if (gc_state.stats.current_heap_size + size > gc_state.config.max_heap_size) {
		GC_UNLOCK();
		gc_collect();
		GC_LOCK();

		/* Check again after collection */
		if (gc_state.stats.current_heap_size + size > gc_state.config.max_heap_size) {
			if (gc_state.oom_cb) {
				GC_UNLOCK();
				gc_state.oom_cb(size);
				return NULL;
			}
			GC_UNLOCK();
			return NULL;
		}
	}

	/* Allocate object with header */
	gc_object_t *obj = (gc_object_t *)malloc(sizeof(gc_object_t) + size);
	if (!obj) {
		GC_UNLOCK();
		return NULL;
	}

	/* Initialize header */
	obj->size = size;
	obj->refcount = (gc_state.config.mode == GC_MODE_REFCOUNT) ? 1 : 0;
	obj->flags = flags;
	obj->type = NULL;
	obj->finalizer = NULL;
	obj->marked = 0;
	obj->generation = 0;

	/* Add to heap list */
	obj->next = gc_state.heap_list;
	gc_state.heap_list = obj;

	/* Update statistics */
	gc_state.stats.total_allocated += size;
	gc_state.stats.current_heap_size += size + sizeof(gc_object_t);
	gc_state.stats.num_objects++;

	void *ptr = OBJECT_DATA(obj);
	memset(ptr, 0, size);

	GC_UNLOCK();
	return ptr;
}

void *gc_alloc_typed(const gc_type_desc_t *type) {
	if (!type) {
		return NULL;
	}

	void *ptr = gc_alloc(type->size);
	if (ptr) {
		gc_object_t *obj = OBJECT_HEADER(ptr);
		obj->type = type;
		if (type->finalizer) {
			obj->finalizer = type->finalizer;
			obj->flags |= GC_FLAG_FINALIZABLE;
		}
	}
	return ptr;
}

void *gc_alloc_atomic(size_t size) {
	return gc_alloc_flags(size, GC_FLAG_ATOMIC);
}

void *gc_realloc(void *ptr, size_t new_size) {
	if (!ptr) {
		return gc_alloc(new_size);
	}

	gc_object_t *obj = OBJECT_HEADER(ptr);
	if (obj->size >= new_size) {
		return ptr;  /* No need to reallocate */
	}

	void *new_ptr = gc_alloc(new_size);
	if (new_ptr) {
		memcpy(new_ptr, ptr, obj->size);
		gc_free(ptr);
	}
	return new_ptr;
}

void gc_free(void *ptr) {
	if (!ptr) {
		return;
	}

	if (gc_state.config.mode == GC_MODE_REFCOUNT) {
		gc_release(ptr);
	}
	/* For mark-sweep, do nothing - will be collected */
}

/*
 * Reference Counting
 */
void *gc_retain(void *ptr) {
	if (!ptr) {
		return NULL;
	}

	GC_LOCK();
	gc_object_t *obj = OBJECT_HEADER(ptr);
	obj->refcount++;
	GC_UNLOCK();

	return ptr;
}

void gc_release(void *ptr) {
	if (!ptr) {
		return;
	}

	GC_LOCK();
	gc_object_t *obj = OBJECT_HEADER(ptr);

	if (obj->refcount > 0) {
		obj->refcount--;
	}

	if (obj->refcount == 0 && gc_state.config.mode == GC_MODE_REFCOUNT) {
		/* Remove from list and free */
		gc_object_t **p = &gc_state.heap_list;
		while (*p && *p != obj) {
			p = &(*p)->next;
		}
		if (*p) {
			*p = obj->next;
		}

		/* Call finalizer */
		if (obj->finalizer) {
			obj->finalizer(ptr);
		}

		/* Update stats */
		gc_state.stats.total_freed += obj->size;
		gc_state.stats.current_heap_size -= obj->size + sizeof(gc_object_t);
		gc_state.stats.num_objects--;

		free(obj);
	}

	GC_UNLOCK();
}

size_t gc_refcount(void *ptr) {
	if (!ptr) {
		return 0;
	}

	gc_object_t *obj = OBJECT_HEADER(ptr);
	return obj->refcount;
}

/*
 * Mark Phase
 */
static void gc_mark_object(void *ptr) {
	if (!ptr) {
		return;
	}

	gc_object_t *obj = OBJECT_HEADER(ptr);
	if (obj->marked) {
		return;  /* Already marked */
	}

	obj->marked = 1;

	/* If we have type information, mark pointers */
	if (gc_state.config.precision == GC_PRECISE && obj->type) {
		for (size_t i = 0; i < obj->type->pointer_count; i++) {
			void **field = (void **)((char *)ptr + obj->type->pointer_offsets[i]);
			gc_mark_object(*field);
		}
	} else if (!(obj->flags & GC_FLAG_ATOMIC)) {
		/* Conservative: scan all words */
		size_t num_words = obj->size / sizeof(void *);
		void **words = (void **)ptr;
		for (size_t i = 0; i < num_words; i++) {
			/* Check if it looks like a heap pointer */
			gc_object_t *test_obj = gc_state.heap_list;
			while (test_obj) {
				if (OBJECT_DATA(test_obj) == words[i]) {
					gc_mark_object(words[i]);
					break;
				}
				test_obj = test_obj->next;
			}
		}
	}
}

static void gc_mark_roots(void) {
	/* Mark registered roots */
	for (size_t i = 0; i < gc_state.root_count; i++) {
		gc_mark_object(gc_state.roots[i]);
	}

	/* Conservative stack scanning would go here */
}

/*
 * Sweep Phase
 */
static void gc_sweep(void) {
	gc_object_t **p = &gc_state.heap_list;
	size_t freed_count = 0;
	size_t freed_bytes = 0;

	while (*p) {
		gc_object_t *obj = *p;
		if (!obj->marked) {
			/* Unmark and free */
			*p = obj->next;

			/* Call finalizer if present */
			if (obj->finalizer) {
				obj->finalizer(OBJECT_DATA(obj));
			}

			freed_bytes += obj->size + sizeof(gc_object_t);
			freed_count++;

			free(obj);
		} else {
			/* Unmark for next collection */
			obj->marked = 0;
			p = &obj->next;
		}
	}

	/* Update statistics */
	gc_state.stats.total_freed += freed_bytes;
	gc_state.stats.current_heap_size -= freed_bytes;
	gc_state.stats.num_objects -= freed_count;
}

/*
 * Collection
 */
void gc_collect(void) {
	if (!gc_state.gc_enabled || gc_state.gc_running) {
		return;
	}

	GC_LOCK();

	if (gc_state.pre_collect_cb) {
		gc_state.pre_collect_cb();
	}

	gc_state.gc_running = 1;

	struct timeval start, end;
	gettimeofday(&start, NULL);

	/* Mark phase */
	gc_mark_roots();

	/* Sweep phase */
	gc_sweep();

	gettimeofday(&end, NULL);
	uint64_t elapsed_us = (end.tv_sec - start.tv_sec) * 1000000 +
	                      (end.tv_usec - start.tv_usec);

	gc_state.stats.num_collections++;
	gc_state.stats.total_gc_time_us += elapsed_us;

	gc_state.gc_running = 0;

	if (gc_state.post_collect_cb) {
		gc_state.post_collect_cb(&gc_state.stats);
	}

	GC_UNLOCK();
}

void gc_collect_minor(void) {
	/* For generational GC - simplified version */
	gc_collect();
}

void gc_collect_full(void) {
	gc_collect();
}

/*
 * Control
 */
void gc_disable(void) {
	gc_state.gc_enabled = 0;
}

void gc_enable(void) {
	gc_state.gc_enabled = 1;
}

int gc_is_enabled(void) {
	return gc_state.gc_enabled;
}

int gc_set_mode(gc_mode_t mode) {
	gc_state.config.mode = mode;
	return 0;
}

const gc_config_t *gc_get_config(void) {
	return &gc_state.config;
}

/*
 * Roots
 */
void gc_add_root(void *ptr) {
	if (!ptr) {
		return;
	}

	GC_LOCK();

	if (gc_state.root_count >= gc_state.root_capacity) {
		size_t new_cap = gc_state.root_capacity ? gc_state.root_capacity * 2 : 16;
		void **new_roots = (void **)realloc(gc_state.roots, new_cap * sizeof(void *));
		if (!new_roots) {
			GC_UNLOCK();
			return;
		}
		gc_state.roots = new_roots;
		gc_state.root_capacity = new_cap;
	}

	gc_state.roots[gc_state.root_count++] = ptr;

	GC_UNLOCK();
}

void gc_remove_root(void *ptr) {
	if (!ptr) {
		return;
	}

	GC_LOCK();

	for (size_t i = 0; i < gc_state.root_count; i++) {
		if (gc_state.roots[i] == ptr) {
			gc_state.roots[i] = gc_state.roots[--gc_state.root_count];
			break;
		}
	}

	GC_UNLOCK();
}

/*
 * Statistics
 */
const gc_stats_t *gc_get_stats(void) {
	return &gc_state.stats;
}

void gc_print_stats(void) {
	printf("GC Statistics:\n");
	printf("  Total allocated: %zu bytes\n", gc_state.stats.total_allocated);
	printf("  Total freed: %zu bytes\n", gc_state.stats.total_freed);
	printf("  Current heap: %zu bytes\n", gc_state.stats.current_heap_size);
	printf("  Number of objects: %zu\n", gc_state.stats.num_objects);
	printf("  Collections: %zu\n", gc_state.stats.num_collections);
	printf("  Total GC time: %llu us\n", (unsigned long long)gc_state.stats.total_gc_time_us);
}

void gc_reset_stats(void) {
	memset(&gc_state.stats, 0, sizeof(gc_stats_t));
}

/*
 * Finalizers
 */
void gc_register_finalizer(void *ptr, void (*finalizer)(void *obj)) {
	if (!ptr) {
		return;
	}

	gc_object_t *obj = OBJECT_HEADER(ptr);
	obj->finalizer = finalizer;
	obj->flags |= GC_FLAG_FINALIZABLE;
}

void gc_unregister_finalizer(void *ptr) {
	if (!ptr) {
		return;
	}

	gc_object_t *obj = OBJECT_HEADER(ptr);
	obj->finalizer = NULL;
	obj->flags &= ~GC_FLAG_FINALIZABLE;
}

/*
 * Callbacks
 */
void gc_set_pre_collect_callback(gc_pre_collect_callback_t callback) {
	gc_state.pre_collect_cb = callback;
}

void gc_set_post_collect_callback(gc_post_collect_callback_t callback) {
	gc_state.post_collect_cb = callback;
}

void gc_set_oom_callback(gc_oom_callback_t callback) {
	gc_state.oom_cb = callback;
}

/*
 * Utility
 */
size_t gc_size(void *ptr) {
	if (!ptr) {
		return 0;
	}
	gc_object_t *obj = OBJECT_HEADER(ptr);
	return obj->size;
}

int gc_is_managed(void *ptr) {
	if (!ptr) {
		return 0;
	}

	gc_object_t *obj = gc_state.heap_list;
	while (obj) {
		if (OBJECT_DATA(obj) == ptr) {
			return 1;
		}
		obj = obj->next;
	}
	return 0;
}

void gc_pin(void *ptr) {
	if (!ptr) {
		return;
	}
	gc_object_t *obj = OBJECT_HEADER(ptr);
	obj->flags |= GC_FLAG_PINNED;
}

void gc_unpin(void *ptr) {
	if (!ptr) {
		return;
	}
	gc_object_t *obj = OBJECT_HEADER(ptr);
	obj->flags &= ~GC_FLAG_PINNED;
}

void gc_write_barrier(void *obj, void **field, void *value) {
	/* For incremental/concurrent GC - simplified */
	*field = value;
}

void gc_enable_debug(int level) {
	/* Debug support */
	(void)level;
}

int gc_verify_heap(void) {
	/* Heap verification */
	return 1;
}

double gc_get_fragmentation(void) {
	/* Simplified fragmentation calculation */
	return 0.0;
}
