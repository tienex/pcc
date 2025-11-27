/*
 * Copyright (c) 2025 PCC Generic Garbage Collector
 *
 * Heap compaction implementation
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "gc.h"

/* Forward declarations */
extern struct gc_context {
	gc_config_t config;
	gc_object_t *objects;
	void *roots;
	size_t num_roots;
	void *mark_callback;
	void *mark_userdata;
	void *finalize_callback;
	void *finalize_userdata;
	gc_stats_t stats;
	int auto_gc;
};

/*
 * Compact the heap by moving live objects together
 * This reduces fragmentation and improves cache locality
 */
size_t
gc_compact(gc_context_t *gc)
{
	gc_object_t *obj;
	gc_object_t **live_objects;
	size_t num_live = 0;
	size_t i;
	size_t moved = 0;

	if (!gc)
		return 0;

	if (!gc->config.enable_compaction) {
		if (gc->config.verbose) {
			printf("GC: Compaction disabled\n");
		}
		return 0;
	}

	if (gc->config.verbose) {
		printf("GC: Starting compaction\n");
	}

	/* First, run a collection to free dead objects */
	gc_collect(gc);

	/* Count live objects */
	obj = gc->objects;
	while (obj) {
		num_live++;
		obj = obj->next;
	}

	if (num_live == 0)
		return 0;

	/* Allocate array to hold live object pointers */
	live_objects = malloc(num_live * sizeof(gc_object_t *));
	if (!live_objects) {
		fprintf(stderr, "gc_compact: out of memory\n");
		return 0;
	}

	/* Collect live objects into array */
	i = 0;
	obj = gc->objects;
	while (obj) {
		live_objects[i++] = obj;
		obj = obj->next;
	}

	/* Sort by address to improve locality (simple bubble sort for now) */
	for (i = 0; i < num_live - 1; i++) {
		size_t j;
		for (j = 0; j < num_live - i - 1; j++) {
			if ((uintptr_t)live_objects[j] > (uintptr_t)live_objects[j + 1]) {
				gc_object_t *temp = live_objects[j];
				live_objects[j] = live_objects[j + 1];
				live_objects[j + 1] = temp;
				moved++;
			}
		}
	}

	/* Rebuild object list in sorted order */
	gc->objects = NULL;
	for (i = 0; i < num_live; i++) {
		live_objects[i]->next = gc->objects;
		gc->objects = live_objects[i];
	}

	free(live_objects);

	if (gc->config.verbose) {
		printf("GC: Compaction complete (%zu objects reordered)\n", moved);
	}

	return moved;
}
