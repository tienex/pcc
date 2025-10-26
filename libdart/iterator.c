/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart Runtime Library - Iterator Support
 */

#include "dart.h"
#include <stdlib.h>

DartIterator *
dart_iterator_new(DartObject *collection)
{
	if (!collection) {
		return NULL;
	}

	DartIterator *iter = dart_malloc(sizeof(DartIterator));
	iter->collection = collection;
	iter->position = 0;
	iter->internal = NULL;

	dart_object_retain(collection);
	return iter;
}

bool
dart_iterator_has_next(DartIterator *iter)
{
	if (!iter || !iter->collection) {
		return false;
	}

	switch (iter->collection->type) {
	case DART_TYPE_LIST: {
		DartList *list = (DartList *)iter->collection;
		return iter->position < list->length;
	}
	case DART_TYPE_STRING: {
		DartString *str = (DartString *)iter->collection;
		return iter->position < str->length;
	}
	default:
		return false;
	}
}

DartObject *
dart_iterator_next(DartIterator *iter)
{
	if (!iter || !iter->collection) {
		return dart_null();
	}

	switch (iter->collection->type) {
	case DART_TYPE_LIST: {
		DartList *list = (DartList *)iter->collection;
		if (iter->position >= list->length) {
			return dart_null();
		}
		return list->elements[iter->position++];
	}
	case DART_TYPE_STRING: {
		DartString *str = (DartString *)iter->collection;
		if (iter->position >= str->length) {
			return dart_null();
		}
		char ch = str->data[iter->position++];
		return (DartObject *)dart_string_new_with_length(&ch, 1);
	}
	default:
		return dart_null();
	}
}

void
dart_iterator_free(DartIterator *iter)
{
	if (!iter) {
		return;
	}

	dart_object_release(iter->collection);
	dart_free(iter);
}
