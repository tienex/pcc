/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart Runtime Library - Set Implementation
 */

#include "dart.h"
#include <stdlib.h>

#define INITIAL_SET_CAPACITY 16
#define LOAD_FACTOR 0.75

static void
dart_set_destructor(DartObject *obj)
{
	DartSet *set = (DartSet *)obj;
	if (set->buckets) {
		for (size_t i = 0; i < set->capacity; i++) {
			DartSetEntry *entry = set->buckets[i];
			while (entry) {
				DartSetEntry *next = entry->next;
				dart_object_release(entry->value);
				dart_free(entry);
				entry = next;
			}
		}
		dart_free(set->buckets);
	}
}

static size_t
dart_set_bucket_index(DartSet *set, DartObject *value)
{
	int hash = dart_object_hash(value);
	return (size_t)(hash < 0 ? -hash : hash) % set->capacity;
}

static void
dart_set_resize(DartSet *set, size_t new_capacity)
{
	DartSetEntry **old_buckets = set->buckets;
	size_t old_capacity = set->capacity;

	set->buckets = dart_calloc(new_capacity, sizeof(DartSetEntry *));
	set->capacity = new_capacity;
	set->size = 0;

	/* Rehash all entries */
	for (size_t i = 0; i < old_capacity; i++) {
		DartSetEntry *entry = old_buckets[i];
		while (entry) {
			DartSetEntry *next = entry->next;
			dart_set_add(set, entry->value);
			dart_object_release(entry->value);
			dart_free(entry);
			entry = next;
		}
	}

	dart_free(old_buckets);
}

DartSet *
dart_set_new(void)
{
	return dart_set_new_with_capacity(INITIAL_SET_CAPACITY);
}

DartSet *
dart_set_new_with_capacity(size_t capacity)
{
	DartSet *set = (DartSet *)dart_object_new(DART_TYPE_SET, sizeof(DartSet));
	set->size = 0;
	set->capacity = capacity;
	set->buckets = dart_calloc(capacity, sizeof(DartSetEntry *));
	set->base.destructor = dart_set_destructor;
	return set;
}

size_t
dart_set_size(DartSet *set)
{
	return set ? set->size : 0;
}

void
dart_set_add(DartSet *set, DartObject *value)
{
	if (!set || !value) {
		return;
	}

	/* Check if already exists */
	if (dart_set_contains(set, value)) {
		return;
	}

	/* Check load factor */
	if ((double)set->size / set->capacity > LOAD_FACTOR) {
		dart_set_resize(set, set->capacity * 2);
	}

	size_t index = dart_set_bucket_index(set, value);

	/* Create new entry */
	DartSetEntry *new_entry = dart_malloc(sizeof(DartSetEntry));
	dart_object_retain(value);
	new_entry->value = value;
	new_entry->next = set->buckets[index];
	set->buckets[index] = new_entry;
	set->size++;
}

void
dart_set_remove(DartSet *set, DartObject *value)
{
	if (!set || !value) {
		return;
	}

	size_t index = dart_set_bucket_index(set, value);
	DartSetEntry *entry = set->buckets[index];
	DartSetEntry *prev = NULL;

	while (entry) {
		if (dart_object_equals(entry->value, value)) {
			if (prev) {
				prev->next = entry->next;
			} else {
				set->buckets[index] = entry->next;
			}

			dart_object_release(entry->value);
			dart_free(entry);
			set->size--;
			return;
		}
		prev = entry;
		entry = entry->next;
	}
}

bool
dart_set_contains(DartSet *set, DartObject *value)
{
	if (!set || !value) {
		return false;
	}

	size_t index = dart_set_bucket_index(set, value);
	DartSetEntry *entry = set->buckets[index];

	while (entry) {
		if (dart_object_equals(entry->value, value)) {
			return true;
		}
		entry = entry->next;
	}

	return false;
}

void
dart_set_clear(DartSet *set)
{
	if (!set) {
		return;
	}

	for (size_t i = 0; i < set->capacity; i++) {
		DartSetEntry *entry = set->buckets[i];
		while (entry) {
			DartSetEntry *next = entry->next;
			dart_object_release(entry->value);
			dart_free(entry);
			entry = next;
		}
		set->buckets[i] = NULL;
	}

	set->size = 0;
}

DartList *
dart_set_to_list(DartSet *set)
{
	DartList *result = dart_list_new();
	if (!set) {
		return result;
	}

	for (size_t i = 0; i < set->capacity; i++) {
		DartSetEntry *entry = set->buckets[i];
		while (entry) {
			dart_list_add(result, entry->value);
			entry = entry->next;
		}
	}

	return result;
}

DartSet *
dart_set_union(DartSet *a, DartSet *b)
{
	DartSet *result = dart_set_new();

	if (a) {
		DartList *list_a = dart_set_to_list(a);
		for (size_t i = 0; i < dart_list_length(list_a); i++) {
			dart_set_add(result, dart_list_get(list_a, i));
		}
		dart_object_release((DartObject *)list_a);
	}

	if (b) {
		DartList *list_b = dart_set_to_list(b);
		for (size_t i = 0; i < dart_list_length(list_b); i++) {
			dart_set_add(result, dart_list_get(list_b, i));
		}
		dart_object_release((DartObject *)list_b);
	}

	return result;
}

DartSet *
dart_set_intersection(DartSet *a, DartSet *b)
{
	DartSet *result = dart_set_new();

	if (!a || !b) {
		return result;
	}

	DartList *list_a = dart_set_to_list(a);
	for (size_t i = 0; i < dart_list_length(list_a); i++) {
		DartObject *value = dart_list_get(list_a, i);
		if (dart_set_contains(b, value)) {
			dart_set_add(result, value);
		}
	}
	dart_object_release((DartObject *)list_a);

	return result;
}

DartSet *
dart_set_difference(DartSet *a, DartSet *b)
{
	DartSet *result = dart_set_new();

	if (!a) {
		return result;
	}

	DartList *list_a = dart_set_to_list(a);
	for (size_t i = 0; i < dart_list_length(list_a); i++) {
		DartObject *value = dart_list_get(list_a, i);
		if (!b || !dart_set_contains(b, value)) {
			dart_set_add(result, value);
		}
	}
	dart_object_release((DartObject *)list_a);

	return result;
}
