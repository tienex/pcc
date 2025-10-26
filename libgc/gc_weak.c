/*
 * Copyright (c) 2025 PCC Generic Garbage Collector
 *
 * Weak reference implementation
 */

#include <stdlib.h>
#include <string.h>
#include "gc.h"
#include "gc_internal.h"

/*
 * Create a weak reference to an object
 */
gc_weak_t *
gc_weak_create(gc_context_t *gc, void *obj)
{
	gc_weak_t **weak_refs;
	gc_weak_t *weak;

	if (!gc || !obj)
		return NULL;

	weak_refs = gc_get_weak_refs(gc);

	weak = malloc(sizeof(gc_weak_t));
	if (!weak)
		return NULL;

	weak->object = obj;
	weak->valid = 1;
	weak->next = *weak_refs;
	weak->prev = NULL;

	if (*weak_refs)
		(*weak_refs)->prev = weak;

	*weak_refs = weak;

	return weak;
}

/*
 * Get object from weak reference
 * Returns NULL if object has been collected
 */
void *
gc_weak_get(gc_weak_t *weak)
{
	if (!weak || !weak->valid)
		return NULL;

	return weak->object;
}

/*
 * Release weak reference
 */
void
gc_weak_release(gc_weak_t *weak)
{
	/* Note: We don't actually remove from the GC's list here
	 * because we don't have a reference to the GC context.
	 * Instead, we just mark it as invalid and it will be
	 * cleaned up during GC or gc_destroy().
	 */
	if (!weak)
		return;

	/* Mark as invalid */
	weak->valid = 0;
	weak->object = NULL;

	/* Note: Memory is not freed here - it will be freed during gc_weak_cleanup */
}

/*
 * Invalidate weak references to collected objects
 * Called during GC sweep phase
 */
void
gc_weak_invalidate(gc_context_t *gc, void *obj)
{
	gc_weak_t **weak_refs;
	gc_weak_t *weak;

	if (!gc || !obj)
		return;

	weak_refs = gc_get_weak_refs(gc);

	/* Find all weak references to this object */
	weak = *weak_refs;
	while (weak) {
		if (weak->object == obj) {
			weak->object = NULL;
			weak->valid = 0;
		}
		weak = weak->next;
	}
}

/*
 * Clean up all weak references
 */
void
gc_weak_cleanup(gc_context_t *gc)
{
	gc_weak_t **weak_refs;
	gc_weak_t *weak, *next;

	if (!gc)
		return;

	weak_refs = gc_get_weak_refs(gc);

	weak = *weak_refs;
	while (weak) {
		next = weak->next;
		free(weak);
		weak = next;
	}

	*weak_refs = NULL;
}
