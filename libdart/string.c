/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart Runtime Library - String Implementation
 */

#include "dart.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

static void
dart_string_destructor(DartObject *obj)
{
	DartString *str = (DartString *)obj;
	if (str->data) {
		dart_free(str->data);
	}
}

DartString *
dart_string_new(const char *cstr)
{
	if (!cstr) {
		return dart_string_empty();
	}
	return dart_string_new_with_length(cstr, strlen(cstr));
}

DartString *
dart_string_new_with_length(const char *cstr, size_t length)
{
	DartString *str = (DartString *)dart_object_new(DART_TYPE_STRING, sizeof(DartString));
	str->length = length;
	str->capacity = length + 1;
	str->data = dart_malloc(str->capacity);

	if (cstr) {
		memcpy(str->data, cstr, length);
	}
	str->data[length] = '\0';

	str->base.destructor = dart_string_destructor;
	return str;
}

DartString *
dart_string_empty(void)
{
	return dart_string_new_with_length("", 0);
}

const char *
dart_string_cstr(DartString *str)
{
	return str ? str->data : "";
}

size_t
dart_string_length(DartString *str)
{
	return str ? str->length : 0;
}

DartString *
dart_string_concat(DartString *a, DartString *b)
{
	if (!a) return b;
	if (!b) return a;

	size_t new_length = a->length + b->length;
	DartString *result = dart_string_new_with_length(NULL, new_length);

	memcpy(result->data, a->data, a->length);
	memcpy(result->data + a->length, b->data, b->length);
	result->data[new_length] = '\0';

	return result;
}

DartString *
dart_string_substring(DartString *str, size_t start, size_t end)
{
	if (!str || start >= str->length) {
		return dart_string_empty();
	}

	if (end > str->length) {
		end = str->length;
	}
	if (end <= start) {
		return dart_string_empty();
	}

	size_t length = end - start;
	return dart_string_new_with_length(str->data + start, length);
}

int
dart_string_compare(DartString *a, DartString *b)
{
	if (!a && !b) return 0;
	if (!a) return -1;
	if (!b) return 1;

	return strcmp(a->data, b->data);
}

bool
dart_string_equals(DartString *a, DartString *b)
{
	if (a == b) return true;
	if (!a || !b) return false;
	if (a->length != b->length) return false;

	return memcmp(a->data, b->data, a->length) == 0;
}

int
dart_string_index_of(DartString *str, DartString *pattern)
{
	if (!str || !pattern || pattern->length == 0 || pattern->length > str->length) {
		return -1;
	}

	for (size_t i = 0; i <= str->length - pattern->length; i++) {
		if (memcmp(str->data + i, pattern->data, pattern->length) == 0) {
			return (int)i;
		}
	}

	return -1;
}

DartString *
dart_string_replace(DartString *str, DartString *from, DartString *to)
{
	if (!str || !from || !to || from->length == 0) {
		return str;
	}

	int index = dart_string_index_of(str, from);
	if (index < 0) {
		return str;
	}

	/* Calculate new size */
	size_t new_length = str->length - from->length + to->length;
	DartString *result = dart_string_new_with_length(NULL, new_length);

	/* Copy before match */
	memcpy(result->data, str->data, index);

	/* Copy replacement */
	memcpy(result->data + index, to->data, to->length);

	/* Copy after match */
	memcpy(result->data + index + to->length,
	       str->data + index + from->length,
	       str->length - index - from->length);

	result->data[new_length] = '\0';
	return result;
}

DartString *
dart_string_to_upper(DartString *str)
{
	if (!str) {
		return dart_string_empty();
	}

	DartString *result = dart_string_new_with_length(NULL, str->length);
	for (size_t i = 0; i < str->length; i++) {
		result->data[i] = toupper(str->data[i]);
	}
	result->data[str->length] = '\0';

	return result;
}

DartString *
dart_string_to_lower(DartString *str)
{
	if (!str) {
		return dart_string_empty();
	}

	DartString *result = dart_string_new_with_length(NULL, str->length);
	for (size_t i = 0; i < str->length; i++) {
		result->data[i] = tolower(str->data[i]);
	}
	result->data[str->length] = '\0';

	return result;
}

DartString *
dart_string_trim(DartString *str)
{
	if (!str || str->length == 0) {
		return dart_string_empty();
	}

	size_t start = 0;
	size_t end = str->length;

	/* Trim from start */
	while (start < end && isspace(str->data[start])) {
		start++;
	}

	/* Trim from end */
	while (end > start && isspace(str->data[end - 1])) {
		end--;
	}

	if (start == end) {
		return dart_string_empty();
	}

	return dart_string_substring(str, start, end);
}

DartList *
dart_string_split(DartString *str, DartString *separator)
{
	DartList *result = dart_list_new();

	if (!str || str->length == 0) {
		return result;
	}

	if (!separator || separator->length == 0) {
		/* Split into characters */
		for (size_t i = 0; i < str->length; i++) {
			DartString *ch = dart_string_new_with_length(str->data + i, 1);
			dart_list_add(result, (DartObject *)ch);
		}
		return result;
	}

	size_t start = 0;
	int index;

	while ((index = dart_string_index_of(
		dart_string_substring(str, start, str->length), separator)) >= 0) {
		size_t pos = start + index;
		DartString *part = dart_string_substring(str, start, pos);
		dart_list_add(result, (DartObject *)part);
		start = pos + separator->length;
	}

	/* Add remaining part */
	if (start < str->length) {
		DartString *part = dart_string_substring(str, start, str->length);
		dart_list_add(result, (DartObject *)part);
	}

	return result;
}
