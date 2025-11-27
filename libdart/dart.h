/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart Runtime Library - Main Header
 */

#ifndef _DART_H_
#define _DART_H_

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

/* Dart object types */
typedef enum {
	DART_TYPE_NULL,
	DART_TYPE_BOOL,
	DART_TYPE_INT,
	DART_TYPE_DOUBLE,
	DART_TYPE_STRING,
	DART_TYPE_LIST,
	DART_TYPE_MAP,
	DART_TYPE_SET,
	DART_TYPE_FUNCTION,
	DART_TYPE_OBJECT
} DartType;

/* Forward declarations */
typedef struct DartObject DartObject;
typedef struct DartString DartString;
typedef struct DartList DartList;
typedef struct DartMap DartMap;
typedef struct DartSet DartSet;
typedef struct DartException DartException;

/* Base Dart object */
struct DartObject {
	DartType type;
	int ref_count;
	void *data;
	void (*destructor)(DartObject *);
};

/* Dart String */
struct DartString {
	DartObject base;
	size_t length;
	size_t capacity;
	char *data;
};

/* Dart List */
struct DartList {
	DartObject base;
	size_t length;
	size_t capacity;
	DartObject **elements;
};

/* Dart Map (hash map) */
typedef struct DartMapEntry {
	DartObject *key;
	DartObject *value;
	struct DartMapEntry *next;
} DartMapEntry;

struct DartMap {
	DartObject base;
	size_t size;
	size_t capacity;
	DartMapEntry **buckets;
};

/* Dart Set (hash set) */
typedef struct DartSetEntry {
	DartObject *value;
	struct DartSetEntry *next;
} DartSetEntry;

struct DartSet {
	DartObject base;
	size_t size;
	size_t capacity;
	DartSetEntry **buckets;
};

/* Dart Exception */
struct DartException {
	DartObject base;
	DartString *message;
	DartString *stack_trace;
};

/* Object management */
DartObject *dart_object_new(DartType type, size_t size);
void dart_object_retain(DartObject *obj);
void dart_object_release(DartObject *obj);
bool dart_object_equals(DartObject *a, DartObject *b);
int dart_object_hash(DartObject *obj);
DartString *dart_object_to_string(DartObject *obj);

/* Null */
DartObject *dart_null(void);
bool dart_is_null(DartObject *obj);

/* Boolean */
DartObject *dart_bool_new(bool value);
bool dart_bool_value(DartObject *obj);

/* Integer */
DartObject *dart_int_new(int64_t value);
int64_t dart_int_value(DartObject *obj);

/* Double */
DartObject *dart_double_new(double value);
double dart_double_value(DartObject *obj);

/* String */
DartString *dart_string_new(const char *str);
DartString *dart_string_new_with_length(const char *str, size_t length);
DartString *dart_string_empty(void);
const char *dart_string_cstr(DartString *str);
size_t dart_string_length(DartString *str);
DartString *dart_string_concat(DartString *a, DartString *b);
DartString *dart_string_substring(DartString *str, size_t start, size_t end);
int dart_string_compare(DartString *a, DartString *b);
bool dart_string_equals(DartString *a, DartString *b);
int dart_string_index_of(DartString *str, DartString *pattern);
DartString *dart_string_replace(DartString *str, DartString *from, DartString *to);
DartString *dart_string_to_upper(DartString *str);
DartString *dart_string_to_lower(DartString *str);
DartString *dart_string_trim(DartString *str);
DartList *dart_string_split(DartString *str, DartString *separator);

/* List */
DartList *dart_list_new(void);
DartList *dart_list_new_with_capacity(size_t capacity);
size_t dart_list_length(DartList *list);
DartObject *dart_list_get(DartList *list, size_t index);
void dart_list_set(DartList *list, size_t index, DartObject *value);
void dart_list_add(DartList *list, DartObject *value);
void dart_list_insert(DartList *list, size_t index, DartObject *value);
void dart_list_remove_at(DartList *list, size_t index);
void dart_list_clear(DartList *list);
bool dart_list_contains(DartList *list, DartObject *value);
int dart_list_index_of(DartList *list, DartObject *value);
void dart_list_sort(DartList *list, int (*compare)(DartObject *, DartObject *));
DartList *dart_list_sublist(DartList *list, size_t start, size_t end);
DartList *dart_list_reversed(DartList *list);

/* Map */
DartMap *dart_map_new(void);
DartMap *dart_map_new_with_capacity(size_t capacity);
size_t dart_map_size(DartMap *map);
DartObject *dart_map_get(DartMap *map, DartObject *key);
void dart_map_set(DartMap *map, DartObject *key, DartObject *value);
void dart_map_remove(DartMap *map, DartObject *key);
bool dart_map_contains_key(DartMap *map, DartObject *key);
void dart_map_clear(DartMap *map);
DartList *dart_map_keys(DartMap *map);
DartList *dart_map_values(DartMap *map);

/* Set */
DartSet *dart_set_new(void);
DartSet *dart_set_new_with_capacity(size_t capacity);
size_t dart_set_size(DartSet *set);
void dart_set_add(DartSet *set, DartObject *value);
void dart_set_remove(DartSet *set, DartObject *value);
bool dart_set_contains(DartSet *set, DartObject *value);
void dart_set_clear(DartSet *set);
DartList *dart_set_to_list(DartSet *set);
DartSet *dart_set_union(DartSet *a, DartSet *b);
DartSet *dart_set_intersection(DartSet *a, DartSet *b);
DartSet *dart_set_difference(DartSet *a, DartSet *b);

/* Exception */
DartException *dart_exception_new(const char *message);
void dart_exception_throw(DartException *ex);
DartException *dart_exception_catch(void);

/* Core I/O */
void dart_print(DartObject *obj);
void dart_print_string(const char *str);
DartString *dart_read_line(void);

/* Type checking */
bool dart_is_type(DartObject *obj, DartType type);
DartType dart_get_type(DartObject *obj);
const char *dart_type_name(DartType type);

/* Runtime initialization */
void dart_runtime_init(void);
void dart_runtime_cleanup(void);

/* Memory management */
void *dart_malloc(size_t size);
void *dart_calloc(size_t count, size_t size);
void *dart_realloc(void *ptr, size_t size);
void dart_free(void *ptr);

/* Iterators */
typedef struct DartIterator {
	DartObject *collection;
	size_t position;
	void *internal;
} DartIterator;

DartIterator *dart_iterator_new(DartObject *collection);
bool dart_iterator_has_next(DartIterator *iter);
DartObject *dart_iterator_next(DartIterator *iter);
void dart_iterator_free(DartIterator *iter);

#endif /* _DART_H_ */
