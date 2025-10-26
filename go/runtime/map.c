/*
 * Copyright (c) 2025 PCC Go Runtime Library
 *
 * Map (hashmap) operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

#define MAP_INITIAL_BUCKETS 16
#define MAP_LOAD_FACTOR 0.75

/* Map entry */
typedef struct map_entry {
	void *key;
	void *val;
	uint32_t hash;
	struct map_entry *next;
} map_entry;

/* Map structure */
typedef struct go_map_impl {
	map_entry **buckets;
	size_t num_buckets;
	size_t count;
	size_t key_size;
	size_t val_size;
} go_map_impl;

/* Map iterator */
struct go_map_iter {
	go_map_impl *map;
	size_t bucket_idx;
	map_entry *current;
};

/*
 * Create new map
 */
go_map
go_map_new(size_t key_size, size_t val_size, size_t initial_cap)
{
	go_map_impl *m;
	size_t num_buckets;

	if (initial_cap < MAP_INITIAL_BUCKETS)
		num_buckets = MAP_INITIAL_BUCKETS;
	else
		num_buckets = initial_cap * 2;  /* Account for load factor */

	m = (go_map_impl *)go_malloc(sizeof(go_map_impl));
	m->buckets = (map_entry **)go_calloc(num_buckets, sizeof(map_entry *));
	m->num_buckets = num_buckets;
	m->count = 0;
	m->key_size = key_size;
	m->val_size = val_size;

	return (go_map)m;
}

/*
 * Compute bucket index
 */
static size_t
bucket_index(go_map_impl *m, uint32_t hash)
{
	return hash % m->num_buckets;
}

/*
 * Resize map (grow buckets)
 */
static void
map_resize(go_map_impl *m)
{
	size_t old_num_buckets = m->num_buckets;
	map_entry **old_buckets = m->buckets;
	size_t i;

	/* Double the number of buckets */
	m->num_buckets *= 2;
	m->buckets = (map_entry **)go_calloc(m->num_buckets, sizeof(map_entry *));

	/* Rehash all entries */
	for (i = 0; i < old_num_buckets; i++) {
		map_entry *entry = old_buckets[i];
		while (entry != NULL) {
			map_entry *next = entry->next;
			size_t new_idx = bucket_index(m, entry->hash);

			/* Insert at head of new bucket */
			entry->next = m->buckets[new_idx];
			m->buckets[new_idx] = entry;

			entry = next;
		}
	}

	go_free(old_buckets);
}

/*
 * Insert or update map entry
 */
void
go_map_insert(go_map map, void *key, void *val)
{
	go_map_impl *m = (go_map_impl *)map;
	uint32_t hash;
	size_t idx;
	map_entry *entry;

	if (m == NULL)
		return;

	/* Compute hash */
	hash = go_hash_bytes(key, m->key_size);
	idx = bucket_index(m, hash);

	/* Search for existing entry */
	for (entry = m->buckets[idx]; entry != NULL; entry = entry->next) {
		if (entry->hash == hash &&
		    memcmp(entry->key, key, m->key_size) == 0) {
			/* Update existing entry */
			memcpy(entry->val, val, m->val_size);
			return;
		}
	}

	/* Create new entry */
	entry = (map_entry *)go_malloc(sizeof(map_entry));
	entry->key = go_malloc(m->key_size);
	entry->val = go_malloc(m->val_size);
	entry->hash = hash;

	memcpy(entry->key, key, m->key_size);
	memcpy(entry->val, val, m->val_size);

	/* Insert at head of bucket */
	entry->next = m->buckets[idx];
	m->buckets[idx] = entry;

	m->count++;

	/* Check load factor */
	if ((double)m->count / m->num_buckets > MAP_LOAD_FACTOR)
		map_resize(m);
}

/*
 * Look up map entry
 */
void *
go_map_lookup(go_map map, void *key)
{
	go_map_impl *m = (go_map_impl *)map;
	uint32_t hash;
	size_t idx;
	map_entry *entry;

	if (m == NULL)
		return NULL;

	hash = go_hash_bytes(key, m->key_size);
	idx = bucket_index(m, hash);

	/* Search for entry */
	for (entry = m->buckets[idx]; entry != NULL; entry = entry->next) {
		if (entry->hash == hash &&
		    memcmp(entry->key, key, m->key_size) == 0) {
			return entry->val;
		}
	}

	return NULL;
}

/*
 * Delete map entry
 */
bool
go_map_delete(go_map map, void *key)
{
	go_map_impl *m = (go_map_impl *)map;
	uint32_t hash;
	size_t idx;
	map_entry *entry, *prev;

	if (m == NULL)
		return false;

	hash = go_hash_bytes(key, m->key_size);
	idx = bucket_index(m, hash);

	/* Search for entry */
	prev = NULL;
	for (entry = m->buckets[idx]; entry != NULL; entry = entry->next) {
		if (entry->hash == hash &&
		    memcmp(entry->key, key, m->key_size) == 0) {
			/* Found - remove it */
			if (prev == NULL)
				m->buckets[idx] = entry->next;
			else
				prev->next = entry->next;

			go_free(entry->key);
			go_free(entry->val);
			go_free(entry);

			m->count--;
			return true;
		}
		prev = entry;
	}

	return false;
}

/*
 * Get map length
 */
go_int
go_map_len(go_map map)
{
	go_map_impl *m = (go_map_impl *)map;

	if (m == NULL)
		return 0;

	return m->count;
}

/*
 * Clear all map entries
 */
void
go_map_clear(go_map map)
{
	go_map_impl *m = (go_map_impl *)map;
	size_t i;

	if (m == NULL)
		return;

	for (i = 0; i < m->num_buckets; i++) {
		map_entry *entry = m->buckets[i];
		while (entry != NULL) {
			map_entry *next = entry->next;
			go_free(entry->key);
			go_free(entry->val);
			go_free(entry);
			entry = next;
		}
		m->buckets[i] = NULL;
	}

	m->count = 0;
}

/*
 * Free map
 */
void
go_map_free(go_map map)
{
	go_map_impl *m = (go_map_impl *)map;

	if (m == NULL)
		return;

	go_map_clear(map);
	go_free(m->buckets);
	go_free(m);
}

/*
 * Create map iterator
 */
go_map_iter *
go_map_iter_new(go_map map)
{
	go_map_impl *m = (go_map_impl *)map;
	go_map_iter *it;

	if (m == NULL)
		return NULL;

	it = (go_map_iter *)go_malloc(sizeof(go_map_iter));
	it->map = m;
	it->bucket_idx = 0;
	it->current = NULL;

	return it;
}

/*
 * Get next map entry
 */
bool
go_map_iter_next(go_map_iter *it, void *key, void *val)
{
	go_map_impl *m;

	if (it == NULL || it->map == NULL)
		return false;

	m = it->map;

	/* Continue from current position */
	if (it->current != NULL) {
		it->current = it->current->next;
	}

	/* Find next non-empty bucket */
	while (it->current == NULL && it->bucket_idx < m->num_buckets) {
		it->current = m->buckets[it->bucket_idx];
		it->bucket_idx++;
	}

	if (it->current == NULL)
		return false;

	/* Copy key and value */
	if (key != NULL)
		memcpy(key, it->current->key, m->key_size);
	if (val != NULL)
		memcpy(val, it->current->val, m->val_size);

	return true;
}

/*
 * Free map iterator
 */
void
go_map_iter_free(go_map_iter *it)
{
	if (it != NULL)
		go_free(it);
}
