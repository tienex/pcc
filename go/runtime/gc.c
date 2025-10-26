/*
 * Copyright (c) 2025 PCC Go Runtime Library
 *
 * Garbage collection (stub implementation)
 * For production use, a real GC would be needed
 */

#include <stdio.h>
#include <stdlib.h>
#include "runtime.h"

/* GC tracking structure */
typedef struct gc_object {
	void *ptr;
	size_t size;
	bool marked;
	struct gc_object *next;
} gc_object;

/* GC state */
static struct {
	gc_object *objects;
	size_t num_objects;
	size_t total_allocated;
	bool initialized;
} gc_state = { NULL, 0, 0, false };

/*
 * Initialize garbage collector
 */
void
go_gc_init(void)
{
	if (gc_state.initialized)
		return;

	gc_state.objects = NULL;
	gc_state.num_objects = 0;
	gc_state.total_allocated = 0;
	gc_state.initialized = true;
}

/*
 * Register object for GC tracking
 */
void
go_gc_register(void *ptr, size_t size)
{
	gc_object *obj;

	if (ptr == NULL)
		return;

	obj = (gc_object *)malloc(sizeof(gc_object));
	if (obj == NULL)
		return;

	obj->ptr = ptr;
	obj->size = size;
	obj->marked = false;
	obj->next = gc_state.objects;

	gc_state.objects = obj;
	gc_state.num_objects++;
	gc_state.total_allocated += size;
}

/*
 * Unregister object from GC tracking
 */
void
go_gc_unregister(void *ptr)
{
	gc_object *obj, *prev;

	if (ptr == NULL)
		return;

	prev = NULL;
	for (obj = gc_state.objects; obj != NULL; obj = obj->next) {
		if (obj->ptr == ptr) {
			/* Remove from list */
			if (prev == NULL)
				gc_state.objects = obj->next;
			else
				prev->next = obj->next;

			gc_state.num_objects--;
			gc_state.total_allocated -= obj->size;

			free(obj);
			return;
		}
		prev = obj;
	}
}

/*
 * Run garbage collection (stub)
 * In a real implementation, this would:
 * 1. Mark all reachable objects from roots
 * 2. Sweep unmarked objects
 * 3. Compact memory
 */
void
go_gc_run(void)
{
	/* This is a stub - no actual collection happens */

	/* In a real GC, we would:
	 * - Stop the world
	 * - Mark phase: trace from roots
	 * - Sweep phase: free unmarked objects
	 * - Start the world
	 */

#ifdef GC_DEBUG
	fprintf(stderr, "GC: %zu objects, %zu bytes allocated\n",
	    gc_state.num_objects, gc_state.total_allocated);
#endif
}
