/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart Runtime Library - Object System
 */

#include "dart.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Null singleton */
static DartObject dart_null_singleton = {
	.type = DART_TYPE_NULL,
	.ref_count = -1,  /* Never freed */
	.data = NULL,
	.destructor = NULL
};

DartObject *
dart_object_new(DartType type, size_t size)
{
	DartObject *obj = dart_calloc(1, size);
	obj->type = type;
	obj->ref_count = 1;
	obj->data = NULL;
	obj->destructor = NULL;
	return obj;
}

void
dart_object_retain(DartObject *obj)
{
	if (obj && obj->ref_count >= 0) {
		obj->ref_count++;
	}
}

void
dart_object_release(DartObject *obj)
{
	if (!obj || obj->ref_count < 0) {
		return;
	}

	obj->ref_count--;
	if (obj->ref_count == 0) {
		if (obj->destructor) {
			obj->destructor(obj);
		}
		dart_free(obj);
	}
}

bool
dart_object_equals(DartObject *a, DartObject *b)
{
	if (a == b) {
		return true;
	}
	if (!a || !b) {
		return false;
	}
	if (a->type != b->type) {
		return false;
	}

	switch (a->type) {
	case DART_TYPE_NULL:
		return true;
	case DART_TYPE_BOOL:
		return dart_bool_value(a) == dart_bool_value(b);
	case DART_TYPE_INT:
		return dart_int_value(a) == dart_int_value(b);
	case DART_TYPE_DOUBLE:
		return dart_double_value(a) == dart_double_value(b);
	case DART_TYPE_STRING:
		return dart_string_equals((DartString *)a, (DartString *)b);
	default:
		return a == b;  /* Reference equality */
	}
}

int
dart_object_hash(DartObject *obj)
{
	if (!obj) {
		return 0;
	}

	switch (obj->type) {
	case DART_TYPE_NULL:
		return 0;
	case DART_TYPE_BOOL:
		return dart_bool_value(obj) ? 1 : 0;
	case DART_TYPE_INT:
		return (int)dart_int_value(obj);
	case DART_TYPE_DOUBLE: {
		double d = dart_double_value(obj);
		return *(int *)&d;
	}
	case DART_TYPE_STRING: {
		DartString *str = (DartString *)obj;
		int hash = 0;
		for (size_t i = 0; i < str->length; i++) {
			hash = hash * 31 + str->data[i];
		}
		return hash;
	}
	default:
		return (int)(uintptr_t)obj;
	}
}

DartString *
dart_object_to_string(DartObject *obj)
{
	char buf[256];

	if (!obj) {
		return dart_string_new("null");
	}

	switch (obj->type) {
	case DART_TYPE_NULL:
		return dart_string_new("null");
	case DART_TYPE_BOOL:
		return dart_string_new(dart_bool_value(obj) ? "true" : "false");
	case DART_TYPE_INT:
		snprintf(buf, sizeof(buf), "%lld", (long long)dart_int_value(obj));
		return dart_string_new(buf);
	case DART_TYPE_DOUBLE:
		snprintf(buf, sizeof(buf), "%g", dart_double_value(obj));
		return dart_string_new(buf);
	case DART_TYPE_STRING:
		return (DartString *)obj;
	case DART_TYPE_LIST:
		return dart_string_new("[...]");
	case DART_TYPE_MAP:
		return dart_string_new("{...}");
	case DART_TYPE_SET:
		return dart_string_new("{...}");
	default:
		snprintf(buf, sizeof(buf), "Object@%p", obj);
		return dart_string_new(buf);
	}
}

/* Null */
DartObject *
dart_null(void)
{
	return &dart_null_singleton;
}

bool
dart_is_null(DartObject *obj)
{
	return obj == NULL || obj == &dart_null_singleton;
}

/* Boolean */
DartObject *
dart_bool_new(bool value)
{
	DartObject *obj = dart_object_new(DART_TYPE_BOOL, sizeof(DartObject) + sizeof(bool));
	bool *data = dart_malloc(sizeof(bool));
	*data = value;
	obj->data = data;
	return obj;
}

bool
dart_bool_value(DartObject *obj)
{
	if (!obj || obj->type != DART_TYPE_BOOL) {
		return false;
	}
	return *(bool *)obj->data;
}

/* Integer */
DartObject *
dart_int_new(int64_t value)
{
	DartObject *obj = dart_object_new(DART_TYPE_INT, sizeof(DartObject) + sizeof(int64_t));
	int64_t *data = dart_malloc(sizeof(int64_t));
	*data = value;
	obj->data = data;
	return obj;
}

int64_t
dart_int_value(DartObject *obj)
{
	if (!obj || obj->type != DART_TYPE_INT) {
		return 0;
	}
	return *(int64_t *)obj->data;
}

/* Double */
DartObject *
dart_double_new(double value)
{
	DartObject *obj = dart_object_new(DART_TYPE_DOUBLE, sizeof(DartObject) + sizeof(double));
	double *data = dart_malloc(sizeof(double));
	*data = value;
	obj->data = data;
	return obj;
}

double
dart_double_value(DartObject *obj)
{
	if (!obj || obj->type != DART_TYPE_DOUBLE) {
		return 0.0;
	}
	return *(double *)obj->data;
}

/* Type checking */
bool
dart_is_type(DartObject *obj, DartType type)
{
	return obj && obj->type == type;
}

DartType
dart_get_type(DartObject *obj)
{
	return obj ? obj->type : DART_TYPE_NULL;
}

const char *
dart_type_name(DartType type)
{
	switch (type) {
	case DART_TYPE_NULL:
		return "Null";
	case DART_TYPE_BOOL:
		return "bool";
	case DART_TYPE_INT:
		return "int";
	case DART_TYPE_DOUBLE:
		return "double";
	case DART_TYPE_STRING:
		return "String";
	case DART_TYPE_LIST:
		return "List";
	case DART_TYPE_MAP:
		return "Map";
	case DART_TYPE_SET:
		return "Set";
	case DART_TYPE_FUNCTION:
		return "Function";
	case DART_TYPE_OBJECT:
		return "Object";
	default:
		return "Unknown";
	}
}
