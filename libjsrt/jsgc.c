/*	$Id$	*/
/*
 * Copyright (c) 2025 JavaScript Runtime Library
 */

/*
 * JavaScript Garbage Collector Implementation
 */

#include "jsgc.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static js_gc_state_t gc_state = {0};

void
js_gc_init(void)
{
	gc_state.all_values = NULL;
	gc_state.value_count = 0;
	gc_state.allocations_since_gc = 0;
	gc_state.gc_threshold = JS_GC_THRESHOLD;
	gc_state.total_collections = 0;
	gc_state.objects_freed = 0;
	gc_state.root_capacity = 256;
	gc_state.roots = calloc(gc_state.root_capacity, sizeof(js_value_t *));
	gc_state.root_count = 0;
}

void
js_gc_cleanup(void)
{
	/* Free all remaining values */
	js_value_t *val = gc_state.all_values;
	while (val) {
		js_value_t *next = val->gc_next;
		free(val);
		val = next;
	}
	free(gc_state.roots);
}

js_value_t *
js_gc_alloc_value(void)
{
	js_value_t *val = calloc(1, sizeof(js_value_t));

	/* Add to GC tracking list */
	val->gc_next = gc_state.all_values;
	gc_state.all_values = val;
	gc_state.value_count++;

	/* Check if GC should run */
	gc_state.allocations_since_gc++;
	if (gc_state.allocations_since_gc >= gc_state.gc_threshold) {
		js_gc_collect();
	}

	return val;
}

void
js_gc_add_root(js_value_t *val)
{
	if (gc_state.root_count >= gc_state.root_capacity) {
		gc_state.root_capacity *= 2;
		gc_state.roots = realloc(gc_state.roots,
		                         gc_state.root_capacity * sizeof(js_value_t *));
	}
	gc_state.roots[gc_state.root_count++] = val;
}

void
js_gc_remove_root(js_value_t *val)
{
	for (size_t i = 0; i < gc_state.root_count; i++) {
		if (gc_state.roots[i] == val) {
			gc_state.roots[i] = gc_state.roots[--gc_state.root_count];
			return;
		}
	}
}

void
js_gc_mark(js_value_t *val)
{
	if (!val || val->gc_mark)
		return;

	val->gc_mark = 1;

	/* Mark referenced values */
	switch (val->type) {
	case JS_TYPE_OBJECT:
		if (val->u.object) {
			val->u.object->gc_mark = 1;

			/* Mark properties */
			for (size_t i = 0; i < val->u.object->property_capacity; i++) {
				js_property_t *prop = val->u.object->properties[i];
				while (prop) {
					js_gc_mark(prop->value);
					prop = prop->next;
				}
			}

			/* Mark array elements */
			for (size_t i = 0; i < val->u.object->length; i++) {
				if (val->u.object->elements[i])
					js_gc_mark(val->u.object->elements[i]);
			}

			/* Mark prototype */
			if (val->u.object->prototype) {
				/* Create temporary value wrapper for prototype */
				js_value_t proto_val = {
					.type = JS_TYPE_OBJECT,
					.u.object = val->u.object->prototype
				};
				js_gc_mark(&proto_val);
			}
		}
		break;

	case JS_TYPE_FUNCTION:
		if (val->u.function && val->u.function->scope) {
			/* Mark closure scope */
			js_value_t scope_val = {
				.type = JS_TYPE_OBJECT,
				.u.object = val->u.function->scope
			};
			js_gc_mark(&scope_val);
		}
		break;

	default:
		break;
	}
}

void
js_gc_sweep(void)
{
	js_value_t **ptr = &gc_state.all_values;
	size_t freed = 0;

	while (*ptr) {
		js_value_t *val = *ptr;

		if (!val->gc_mark && val->refcount == 0) {
			/* Remove from list and free */
			*ptr = val->gc_next;

			switch (val->type) {
			case JS_TYPE_STRING:
				if (val->u.string)
					js_string_free(val->u.string);
				break;
			case JS_TYPE_OBJECT:
				if (val->u.object)
					js_object_free(val->u.object);
				break;
			case JS_TYPE_FUNCTION:
				if (val->u.function)
					js_function_free(val->u.function);
				break;
			default:
				break;
			}

			free(val);
			freed++;
			gc_state.value_count--;
		} else {
			/* Clear mark for next GC */
			val->gc_mark = 0;
			if (val->u.object)
				val->u.object->gc_mark = 0;
			ptr = &val->gc_next;
		}
	}

	gc_state.objects_freed += freed;
}

void
js_gc_collect(void)
{
	/* Mark phase - mark all reachable values from roots */
	for (size_t i = 0; i < gc_state.root_count; i++) {
		js_gc_mark(gc_state.roots[i]);
	}

	/* Mark global object and special values */
	extern js_object_t *js_global_object;
	extern js_value_t *js_undefined_value;
	extern js_value_t *js_null_value;

	if (js_global_object) {
		js_value_t global_val = {.type = JS_TYPE_OBJECT, .u.object = js_global_object};
		js_gc_mark(&global_val);
	}

	if (js_undefined_value) js_gc_mark(js_undefined_value);
	if (js_null_value) js_gc_mark(js_null_value);

	/* Sweep phase - free unmarked values */
	js_gc_sweep();

	/* Update stats */
	gc_state.total_collections++;
	gc_state.allocations_since_gc = 0;
	gc_state.gc_threshold = (size_t)(gc_state.gc_threshold * JS_GC_GROWTH_FACTOR);
}

void
js_gc_dump_stats(FILE *fp)
{
	fprintf(fp, "=== GC Statistics ===\n");
	fprintf(fp, "Total values: %zu\n", gc_state.value_count);
	fprintf(fp, "Root count: %zu\n", gc_state.root_count);
	fprintf(fp, "Collections: %zu\n", gc_state.total_collections);
	fprintf(fp, "Objects freed: %zu\n", gc_state.objects_freed);
	fprintf(fp, "GC threshold: %zu\n", gc_state.gc_threshold);
	fprintf(fp, "Allocations since last GC: %zu\n", gc_state.allocations_since_gc);
}
