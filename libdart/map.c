/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart Runtime Library - Map Implementation
 */

#include "dart.h"
#include <stdlib.h>
#include <string.h>

#define INITIAL_MAP_CAPACITY 16
#define LOAD_FACTOR 0.75

static void
dart_map_destructor(DartObject *obj)
{
	DartMap *map = (DartMap *)obj;
	if (map->buckets) {
		for (size_t i = 0; i < map->capacity; i++) {
			DartMapEntry *entry = map->buckets[i];
			while (entry) {
				DartMapEntry *next = entry->next;
				dart_object_release(entry->key);
				dart_object_release(entry->value);
				dart_free(entry);
				entry = next;
			}
		}
		dart_free(map->buckets);
	}
}

static size_t
dart_map_bucket_index(DartMap *map, DartObject *key)
{
	int hash = dart_object_hash(key);
	return (size_t)(hash < 0 ? -hash : hash) % map->capacity;
}

static void
dart_map_resize(DartMap *map, size_t new_capacity)
{
	DartMapEntry **old_buckets = map->buckets;
	size_t old_capacity = map->capacity;

	map->buckets = dart_calloc(new_capacity, sizeof(DartMapEntry *));
	map->capacity = new_capacity;
	map->size = 0;

	/* Rehash all entries */
	for (size_t i = 0; i < old_capacity; i++) {
		DartMapEntry *entry = old_buckets[i];
		while (entry) {
			DartMapEntry *next = entry->next;
			dart_map_set(map, entry->key, entry->value);
			dart_object_release(entry->key);
			dart_object_release(entry->value);
			dart_free(entry);
			entry = next;
		}
	}

	dart_free(old_buckets);
}

DartMap *
dart_map_new(void)
{
	return dart_map_new_with_capacity(INITIAL_MAP_CAPACITY);
}

DartMap *
dart_map_new_with_capacity(size_t capacity)
{
	DartMap *map = (DartMap *)dart_object_new(DART_TYPE_MAP, sizeof(DartMap));
	map->size = 0;
	map->capacity = capacity;
	map->buckets = dart_calloc(capacity, sizeof(DartMapEntry *));
	map->base.destructor = dart_map_destructor;
	return map;
}

size_t
dart_map_size(DartMap *map)
{
	return map ? map->size : 0;
}

DartObject *
dart_map_get(DartMap *map, DartObject *key)
{
	if (!map || !key) {
		return dart_null();
	}

	size_t index = dart_map_bucket_index(map, key);
	DartMapEntry *entry = map->buckets[index];

	while (entry) {
		if (dart_object_equals(entry->key, key)) {
			return entry->value;
		}
		entry = entry->next;
	}

	return dart_null();
}

void
dart_map_set(DartMap *map, DartObject *key, DartObject *value)
{
	if (!map || !key) {
		return;
	}

	/* Check load factor */
	if ((double)map->size / map->capacity > LOAD_FACTOR) {
		dart_map_resize(map, map->capacity * 2);
	}

	size_t index = dart_map_bucket_index(map, key);
	DartMapEntry *entry = map->buckets[index];

	/* Check if key exists */
	while (entry) {
		if (dart_object_equals(entry->key, key)) {
			/* Update existing value */
			dart_object_release(entry->value);
			dart_object_retain(value);
			entry->value = value;
			return;
		}
		entry = entry->next;
	}

	/* Create new entry */
	DartMapEntry *new_entry = dart_malloc(sizeof(DartMapEntry));
	dart_object_retain(key);
	dart_object_retain(value);
	new_entry->key = key;
	new_entry->value = value;
	new_entry->next = map->buckets[index];
	map->buckets[index] = new_entry;
	map->size++;
}

void
dart_map_remove(DartMap *map, DartObject *key)
{
	if (!map || !key) {
		return;
	}

	size_t index = dart_map_bucket_index(map, key);
	DartMapEntry *entry = map->buckets[index];
	DartMapEntry *prev = NULL;

	while (entry) {
		if (dart_object_equals(entry->key, key)) {
			if (prev) {
				prev->next = entry->next;
			} else {
				map->buckets[index] = entry->next;
			}

			dart_object_release(entry->key);
			dart_object_release(entry->value);
			dart_free(entry);
			map->size--;
			return;
		}
		prev = entry;
		entry = entry->next;
	}
}

bool
dart_map_contains_key(DartMap *map, DartObject *key)
{
	return !dart_is_null(dart_map_get(map, key));
}

void
dart_map_clear(DartMap *map)
{
	if (!map) {
		return;
	}

	for (size_t i = 0; i < map->capacity; i++) {
		DartMapEntry *entry = map->buckets[i];
		while (entry) {
			DartMapEntry *next = entry->next;
			dart_object_release(entry->key);
			dart_object_release(entry->value);
			dart_free(entry);
			entry = next;
		}
		map->buckets[i] = NULL;
	}

	map->size = 0;
}

DartList *
dart_map_keys(DartMap *map)
{
	DartList *result = dart_list_new();
	if (!map) {
		return result;
	}

	for (size_t i = 0; i < map->capacity; i++) {
		DartMapEntry *entry = map->buckets[i];
		while (entry) {
			dart_list_add(result, entry->key);
			entry = entry->next;
		}
	}

	return result;
}

DartList *
dart_map_values(DartMap *map)
{
	DartList *result = dart_list_new();
	if (!map) {
		return result;
	}

	for (size_t i = 0; i < map->capacity; i++) {
		DartMapEntry *entry = map->buckets[i];
		while (entry) {
			dart_list_add(result, entry->value);
			entry = entry->next;
		}
	}

	return result;
}
