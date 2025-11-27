/*	$Id$	*/
/*
 * Copyright (c) 2025 JavaScript Runtime Library
 * All rights reserved.
 */

/*
 * JavaScript Garbage Collector
 *
 * Implements mark-and-sweep garbage collection with reference counting
 * optimization for immediate deallocation.
 */

#ifndef _JSGC_H
#define _JSGC_H

#include "jsrt.h"

/* GC Configuration */
#define JS_GC_THRESHOLD 1000		/* Trigger GC after N allocations */
#define JS_GC_GROWTH_FACTOR 2.0		/* Grow threshold by this factor */

/* GC State */
typedef struct js_gc_state {
	/* All allocated values (for mark-and-sweep) */
	js_value_t *all_values;
	size_t value_count;

	/* GC statistics */
	size_t allocations_since_gc;
	size_t gc_threshold;
	size_t total_collections;
	size_t objects_freed;

	/* Root set (global variables, stack values) */
	js_value_t **roots;
	size_t root_count;
	size_t root_capacity;

} js_gc_state_t;

/* Initialize GC */
void js_gc_init(void);

/* Cleanup GC */
void js_gc_cleanup(void);

/* Allocate a new value (with GC tracking) */
js_value_t *js_gc_alloc_value(void);

/* Add a root (GC will not collect) */
void js_gc_add_root(js_value_t *val);

/* Remove a root */
void js_gc_remove_root(js_value_t *val);

/* Trigger garbage collection */
void js_gc_collect(void);

/* Mark-and-sweep implementation */
void js_gc_mark(js_value_t *val);
void js_gc_sweep(void);

/* Debug */
void js_gc_dump_stats(FILE *fp);

#endif /* _JSGC_H */
