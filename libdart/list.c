/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart Runtime Library - List Implementation
 */

#include "dart.h"
#include <stdlib.h>
#include <string.h>

#define INITIAL_CAPACITY 16

static void
dart_list_destructor(DartObject *obj)
{
	DartList *list = (DartList *)obj;
	if (list->elements) {
		/* Release all elements */
		for (size_t i = 0; i < list->length; i++) {
			dart_object_release(list->elements[i]);
		}
		dart_free(list->elements);
	}
}

static void
dart_list_ensure_capacity(DartList *list, size_t min_capacity)
{
	if (list->capacity >= min_capacity) {
		return;
	}

	size_t new_capacity = list->capacity == 0 ? INITIAL_CAPACITY : list->capacity * 2;
	while (new_capacity < min_capacity) {
		new_capacity *= 2;
	}

	DartObject **new_elements = dart_realloc(list->elements,
	                                          new_capacity * sizeof(DartObject *));
	list->elements = new_elements;
	list->capacity = new_capacity;
}

DartList *
dart_list_new(void)
{
	return dart_list_new_with_capacity(INITIAL_CAPACITY);
}

DartList *
dart_list_new_with_capacity(size_t capacity)
{
	DartList *list = (DartList *)dart_object_new(DART_TYPE_LIST, sizeof(DartList));
	list->length = 0;
	list->capacity = capacity;
	list->elements = dart_calloc(capacity, sizeof(DartObject *));
	list->base.destructor = dart_list_destructor;
	return list;
}

size_t
dart_list_length(DartList *list)
{
	return list ? list->length : 0;
}

DartObject *
dart_list_get(DartList *list, size_t index)
{
	if (!list || index >= list->length) {
		return dart_null();
	}
	return list->elements[index];
}

void
dart_list_set(DartList *list, size_t index, DartObject *value)
{
	if (!list || index >= list->length) {
		return;
	}

	/* Release old value */
	dart_object_release(list->elements[index]);

	/* Retain new value */
	dart_object_retain(value);
	list->elements[index] = value;
}

void
dart_list_add(DartList *list, DartObject *value)
{
	if (!list) {
		return;
	}

	dart_list_ensure_capacity(list, list->length + 1);
	dart_object_retain(value);
	list->elements[list->length++] = value;
}

void
dart_list_insert(DartList *list, size_t index, DartObject *value)
{
	if (!list || index > list->length) {
		return;
	}

	dart_list_ensure_capacity(list, list->length + 1);

	/* Shift elements */
	memmove(&list->elements[index + 1], &list->elements[index],
	        (list->length - index) * sizeof(DartObject *));

	dart_object_retain(value);
	list->elements[index] = value;
	list->length++;
}

void
dart_list_remove_at(DartList *list, size_t index)
{
	if (!list || index >= list->length) {
		return;
	}

	/* Release element */
	dart_object_release(list->elements[index]);

	/* Shift remaining elements */
	memmove(&list->elements[index], &list->elements[index + 1],
	        (list->length - index - 1) * sizeof(DartObject *));

	list->length--;
}

void
dart_list_clear(DartList *list)
{
	if (!list) {
		return;
	}

	/* Release all elements */
	for (size_t i = 0; i < list->length; i++) {
		dart_object_release(list->elements[i]);
	}

	list->length = 0;
}

bool
dart_list_contains(DartList *list, DartObject *value)
{
	return dart_list_index_of(list, value) >= 0;
}

int
dart_list_index_of(DartList *list, DartObject *value)
{
	if (!list) {
		return -1;
	}

	for (size_t i = 0; i < list->length; i++) {
		if (dart_object_equals(list->elements[i], value)) {
			return (int)i;
		}
	}

	return -1;
}

void
dart_list_sort(DartList *list, int (*compare)(DartObject *, DartObject *))
{
	if (!list || list->length < 2 || !compare) {
		return;
	}

	/* Simple bubble sort for now */
	for (size_t i = 0; i < list->length - 1; i++) {
		for (size_t j = 0; j < list->length - i - 1; j++) {
			if (compare(list->elements[j], list->elements[j + 1]) > 0) {
				/* Swap */
				DartObject *temp = list->elements[j];
				list->elements[j] = list->elements[j + 1];
				list->elements[j + 1] = temp;
			}
		}
	}
}

DartList *
dart_list_sublist(DartList *list, size_t start, size_t end)
{
	if (!list || start >= list->length) {
		return dart_list_new();
	}

	if (end > list->length) {
		end = list->length;
	}
	if (end <= start) {
		return dart_list_new();
	}

	size_t length = end - start;
	DartList *result = dart_list_new_with_capacity(length);

	for (size_t i = start; i < end; i++) {
		dart_list_add(result, list->elements[i]);
	}

	return result;
}

DartList *
dart_list_reversed(DartList *list)
{
	if (!list) {
		return dart_list_new();
	}

	DartList *result = dart_list_new_with_capacity(list->length);

	for (size_t i = list->length; i > 0; i--) {
		dart_list_add(result, list->elements[i - 1]);
	}

	return result;
}
