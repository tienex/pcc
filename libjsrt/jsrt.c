/*	$Id$	*/
/*
 * Copyright (c) 2025 JavaScript Runtime Library
 * All rights reserved.
 */

/*
 * JavaScript Runtime Library - Core Implementation
 *
 * Implements the JavaScript value system, type operations, and core runtime.
 */

#include "jsrt.h"
#include "jsgc.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <setjmp.h>

/* Global runtime state */
js_object_t *js_global_object = NULL;

/* Builtin prototypes */
js_object_t *js_object_prototype = NULL;
js_object_t *js_function_prototype = NULL;
js_object_t *js_array_prototype = NULL;
js_object_t *js_string_prototype = NULL;
js_object_t *js_number_prototype = NULL;
js_object_t *js_boolean_prototype = NULL;
js_object_t *js_error_prototype = NULL;

/* Builtin constructors */
js_function_t *js_object_constructor = NULL;
js_function_t *js_function_constructor = NULL;
js_function_t *js_array_constructor = NULL;
js_function_t *js_string_constructor = NULL;
js_function_t *js_number_constructor = NULL;
js_function_t *js_boolean_constructor = NULL;
js_function_t *js_error_constructor = NULL;
js_function_t *js_type_error_constructor = NULL;
js_function_t *js_reference_error_constructor = NULL;
js_function_t *js_range_error_constructor = NULL;

/* Special values */
js_value_t *js_undefined_value = NULL;
js_value_t *js_null_value = NULL;
js_value_t *js_true_value = NULL;
js_value_t *js_false_value = NULL;
js_value_t *js_nan_value = NULL;
js_value_t *js_infinity_value = NULL;
js_value_t *js_neg_infinity_value = NULL;

/* Exception handling */
js_try_state_t *js_current_try_state = NULL;

/* String interning */
static js_string_t *string_intern_table[1024];

/* Symbol registry */
static uint32_t next_symbol_id = 1;

/* Statistics */
static js_memory_stats_t memory_stats = {0};

/*
 * Initialize the JavaScript runtime
 */
void
js_runtime_init(void)
{
	/* Create special values */
	js_undefined_value = malloc(sizeof(js_value_t));
	js_undefined_value->type = JS_TYPE_UNDEFINED;
	js_undefined_value->refcount = 1000000; /* Never free */
	js_undefined_value->gc_mark = 0;

	js_null_value = malloc(sizeof(js_value_t));
	js_null_value->type = JS_TYPE_NULL;
	js_null_value->refcount = 1000000;
	js_null_value->gc_mark = 0;

	js_true_value = malloc(sizeof(js_value_t));
	js_true_value->type = JS_TYPE_BOOLEAN;
	js_true_value->u.boolean = 1;
	js_true_value->refcount = 1000000;
	js_true_value->gc_mark = 0;

	js_false_value = malloc(sizeof(js_value_t));
	js_false_value->type = JS_TYPE_BOOLEAN;
	js_false_value->u.boolean = 0;
	js_false_value->refcount = 1000000;
	js_false_value->gc_mark = 0;

	js_nan_value = malloc(sizeof(js_value_t));
	js_nan_value->type = JS_TYPE_NUMBER;
	js_nan_value->u.number = NAN;
	js_nan_value->refcount = 1000000;
	js_nan_value->gc_mark = 0;

	js_infinity_value = malloc(sizeof(js_value_t));
	js_infinity_value->type = JS_TYPE_NUMBER;
	js_infinity_value->u.number = INFINITY;
	js_infinity_value->refcount = 1000000;
	js_infinity_value->gc_mark = 0;

	js_neg_infinity_value = malloc(sizeof(js_value_t));
	js_neg_infinity_value->type = JS_TYPE_NUMBER;
	js_neg_infinity_value->u.number = -INFINITY;
	js_neg_infinity_value->refcount = 1000000;
	js_neg_infinity_value->gc_mark = 0;

	/* Initialize GC */
	js_gc_init();

	/* Initialize string intern table */
	memset(string_intern_table, 0, sizeof(string_intern_table));

	/* Create global object and prototypes */
	js_object_prototype = js_object_create(NULL);
	js_global_object = js_object_create(js_object_prototype);

	/* Initialize built-in objects (declared extern, implemented in jsbuiltin.c) */
	extern void js_init_builtins(void);
	js_init_builtins();
}

/*
 * Cleanup the JavaScript runtime
 */
void
js_runtime_cleanup(void)
{
	/* Run final GC */
	js_gc_collect();

	/* Free special values */
	/* (Skip for now - they're never freed) */

	/* Clean up GC */
	js_gc_cleanup();
}

/*
 * Value Creation Functions
 */

js_value_t *
js_value_undefined(void)
{
	return js_undefined_value;
}

js_value_t *
js_value_null(void)
{
	return js_null_value;
}

js_value_t *
js_value_boolean(int val)
{
	return val ? js_true_value : js_false_value;
}

js_value_t *
js_value_number(double val)
{
	if (isnan(val))
		return js_nan_value;
	if (isinf(val))
		return val > 0 ? js_infinity_value : js_neg_infinity_value;

	js_value_t *v = js_gc_alloc_value();
	v->type = JS_TYPE_NUMBER;
	v->u.number = val;
	v->refcount = 1;
	memory_stats.values_allocated++;
	return v;
}

js_value_t *
js_value_bigint(uint64_t val)
{
	js_value_t *v = js_gc_alloc_value();
	v->type = JS_TYPE_BIGINT;
	v->u.bigint = val;
	v->refcount = 1;
	memory_stats.values_allocated++;
	return v;
}

js_value_t *
js_value_string(const char *str)
{
	return js_value_string_len(str, strlen(str));
}

js_value_t *
js_value_string_len(const char *str, size_t len)
{
	js_value_t *v = js_gc_alloc_value();
	v->type = JS_TYPE_STRING;
	v->u.string = js_string_create_len(str, len);
	v->refcount = 1;
	memory_stats.values_allocated++;
	return v;
}

js_value_t *
js_value_object(void)
{
	js_value_t *v = js_gc_alloc_value();
	v->type = JS_TYPE_OBJECT;
	v->u.object = js_object_create(js_object_prototype);
	v->refcount = 1;
	memory_stats.values_allocated++;
	return v;
}

js_value_t *
js_value_array(size_t length)
{
	js_value_t *v = js_gc_alloc_value();
	v->type = JS_TYPE_OBJECT;
	v->u.object = js_object_create(js_array_prototype);
	v->u.object->elements = calloc(length, sizeof(js_value_t *));
	v->u.object->length = length;
	v->u.object->element_capacity = length;
	v->refcount = 1;
	memory_stats.values_allocated++;
	return v;
}

js_value_t *
js_value_function(js_function_t *fn)
{
	js_value_t *v = js_gc_alloc_value();
	v->type = JS_TYPE_FUNCTION;
	v->u.function = fn;
	v->refcount = 1;
	memory_stats.values_allocated++;
	return v;
}

js_value_t *
js_value_symbol(const char *description)
{
	js_value_t *v = js_gc_alloc_value();
	v->type = JS_TYPE_SYMBOL;
	v->u.symbol_id = next_symbol_id++;
	v->refcount = 1;
	memory_stats.values_allocated++;
	return v;
}

/*
 * Type Checking Functions
 */

int js_is_undefined(js_value_t *val) { return val->type == JS_TYPE_UNDEFINED; }
int js_is_null(js_value_t *val) { return val->type == JS_TYPE_NULL; }
int js_is_boolean(js_value_t *val) { return val->type == JS_TYPE_BOOLEAN; }
int js_is_number(js_value_t *val) { return val->type == JS_TYPE_NUMBER; }
int js_is_bigint(js_value_t *val) { return val->type == JS_TYPE_BIGINT; }
int js_is_string(js_value_t *val) { return val->type == JS_TYPE_STRING; }
int js_is_symbol(js_value_t *val) { return val->type == JS_TYPE_SYMBOL; }
int js_is_function(js_value_t *val) { return val->type == JS_TYPE_FUNCTION; }

int
js_is_object(js_value_t *val)
{
	return val->type == JS_TYPE_OBJECT || val->type == JS_TYPE_FUNCTION;
}

int
js_is_array(js_value_t *val)
{
	if (val->type != JS_TYPE_OBJECT)
		return 0;
	/* Check if prototype is Array.prototype */
	return val->u.object->prototype == js_array_prototype;
}

int
js_is_nullish(js_value_t *val)
{
	return js_is_null(val) || js_is_undefined(val);
}

int
js_is_primitive(js_value_t *val)
{
	return val->type != JS_TYPE_OBJECT && val->type != JS_TYPE_FUNCTION;
}

/*
 * Raw Value Extraction
 */

int
js_get_boolean(js_value_t *val)
{
	if (val->type == JS_TYPE_BOOLEAN)
		return val->u.boolean;
	/* Type coercion handled by js_to_boolean */
	return 0;
}

double
js_get_number(js_value_t *val)
{
	if (val->type == JS_TYPE_NUMBER)
		return val->u.number;
	return NAN;
}

uint64_t
js_get_bigint(js_value_t *val)
{
	if (val->type == JS_TYPE_BIGINT)
		return val->u.bigint;
	return 0;
}

const char *
js_get_string(js_value_t *val)
{
	if (val->type == JS_TYPE_STRING)
		return val->u.string->data;
	return NULL;
}

js_object_t *
js_get_object(js_value_t *val)
{
	if (val->type == JS_TYPE_OBJECT)
		return val->u.object;
	return NULL;
}

js_function_t *
js_get_function(js_value_t *val)
{
	if (val->type == JS_TYPE_FUNCTION)
		return val->u.function;
	return NULL;
}

/*
 * String Operations
 */

static uint32_t
hash_string(const char *str, size_t len)
{
	uint32_t hash = 5381;
	for (size_t i = 0; i < len; i++)
		hash = ((hash << 5) + hash) + (unsigned char)str[i];
	return hash;
}

js_string_t *
js_string_create(const char *str)
{
	return js_string_create_len(str, strlen(str));
}

js_string_t *
js_string_create_len(const char *str, size_t len)
{
	js_string_t *s = malloc(sizeof(js_string_t));
	s->refcount = 1;
	s->length = len;
	s->hash = hash_string(str, len);
	s->data = malloc(len + 1);
	memcpy(s->data, str, len);
	s->data[len] = '\0';
	s->intern_next = NULL;
	memory_stats.strings_allocated++;
	return s;
}

js_string_t *
js_string_concat(js_string_t *a, js_string_t *b)
{
	size_t total_len = a->length + b->length;
	js_string_t *result = malloc(sizeof(js_string_t));
	result->refcount = 1;
	result->length = total_len;
	result->data = malloc(total_len + 1);
	memcpy(result->data, a->data, a->length);
	memcpy(result->data + a->length, b->data, b->length);
	result->data[total_len] = '\0';
	result->hash = hash_string(result->data, total_len);
	result->intern_next = NULL;
	memory_stats.strings_allocated++;
	return result;
}

js_string_t *
js_string_concat_cstr(js_string_t *a, const char *b)
{
	js_string_t *b_str = js_string_create(b);
	js_string_t *result = js_string_concat(a, b_str);
	js_string_free(b_str);
	return result;
}

int
js_string_compare(js_string_t *a, js_string_t *b)
{
	if (a == b)
		return 0;
	if (a->length != b->length)
		return (int)a->length - (int)b->length;
	return memcmp(a->data, b->data, a->length);
}

js_string_t *
js_string_substring(js_string_t *str, size_t start, size_t len)
{
	if (start >= str->length)
		return js_string_create("");
	if (start + len > str->length)
		len = str->length - start;
	return js_string_create_len(str->data + start, len);
}

js_string_t *
js_string_intern(const char *str)
{
	uint32_t hash = hash_string(str, strlen(str));
	size_t index = hash % 1024;

	/* Check if already interned */
	for (js_string_t *s = string_intern_table[index]; s; s = s->intern_next) {
		if (strcmp(s->data, str) == 0) {
			s->refcount++;
			return s;
		}
	}

	/* Create new interned string */
	js_string_t *s = js_string_create(str);
	s->intern_next = string_intern_table[index];
	string_intern_table[index] = s;
	return s;
}

void
js_string_free(js_string_t *str)
{
	if (--str->refcount == 0) {
		free(str->data);
		free(str);
		memory_stats.strings_allocated--;
	}
}

/*
 * Object Operations
 */

js_object_t *
js_object_create(js_object_t *prototype)
{
	js_object_t *obj = calloc(1, sizeof(js_object_t));
	obj->refcount = 1;
	obj->property_capacity = 16;
	obj->properties = calloc(obj->property_capacity, sizeof(js_property_t *));
	obj->property_count = 0;
	obj->prototype = prototype;
	obj->elements = NULL;
	obj->length = 0;
	obj->element_capacity = 0;
	obj->function = NULL;
	obj->constructor = NULL;
	obj->internal_data = NULL;
	obj->gc_mark = 0;
	memory_stats.objects_allocated++;
	return obj;
}

static js_property_t *
find_property(js_object_t *obj, const char *key)
{
	uint32_t hash = hash_string(key, strlen(key));
	size_t index = hash % obj->property_capacity;

	for (js_property_t *prop = obj->properties[index]; prop; prop = prop->next) {
		if (strcmp(prop->name->data, key) == 0)
			return prop;
	}
	return NULL;
}

js_value_t *
js_object_get(js_object_t *obj, const char *key)
{
	js_property_t *prop = find_property(obj, key);
	if (prop) {
		if (prop->getter)
			return js_function_call(prop->getter, js_value_object(), NULL, 0);
		return prop->value;
	}

	/* Search prototype chain */
	if (obj->prototype)
		return js_object_get(obj->prototype, key);

	return js_undefined_value;
}

js_value_t *
js_object_get_str(js_object_t *obj, js_string_t *key)
{
	return js_object_get(obj, key->data);
}

void
js_object_set(js_object_t *obj, const char *key, js_value_t *val)
{
	js_property_t *prop = find_property(obj, key);

	if (prop) {
		if (!prop->writable)
			return; /* Silently fail in non-strict mode */
		if (prop->setter) {
			js_value_t *args[1] = {val};
			js_function_call(prop->setter, js_value_object(), args, 1);
			return;
		}
		js_value_release(prop->value);
		prop->value = val;
		js_value_retain(val);
		return;
	}

	/* Create new property */
	uint32_t hash = hash_string(key, strlen(key));
	size_t index = hash % obj->property_capacity;

	js_property_t *new_prop = malloc(sizeof(js_property_t));
	new_prop->name = js_string_intern(key);
	new_prop->value = val;
	js_value_retain(val);
	new_prop->writable = 1;
	new_prop->enumerable = 1;
	new_prop->configurable = 1;
	new_prop->getter = NULL;
	new_prop->setter = NULL;
	new_prop->next = obj->properties[index];
	obj->properties[index] = new_prop;
	obj->property_count++;
}

void
js_object_set_str(js_object_t *obj, js_string_t *key, js_value_t *val)
{
	js_object_set(obj, key->data, val);
}

int
js_object_has(js_object_t *obj, const char *key)
{
	if (find_property(obj, key))
		return 1;
	if (obj->prototype)
		return js_object_has(obj->prototype, key);
	return 0;
}

int
js_object_delete(js_object_t *obj, const char *key)
{
	uint32_t hash = hash_string(key, strlen(key));
	size_t index = hash % obj->property_capacity;
	js_property_t **ptr = &obj->properties[index];

	while (*ptr) {
		if (strcmp((*ptr)->name->data, key) == 0) {
			if (!(*ptr)->configurable)
				return 0;
			js_property_t *to_delete = *ptr;
			*ptr = to_delete->next;
			js_value_release(to_delete->value);
			js_string_free(to_delete->name);
			free(to_delete);
			obj->property_count--;
			return 1;
		}
		ptr = &(*ptr)->next;
	}
	return 1; /* Deleting non-existent property succeeds */
}

void
js_object_free(js_object_t *obj)
{
	if (--obj->refcount == 0) {
		/* Free properties */
		for (size_t i = 0; i < obj->property_capacity; i++) {
			js_property_t *prop = obj->properties[i];
			while (prop) {
				js_property_t *next = prop->next;
				js_value_release(prop->value);
				js_string_free(prop->name);
				free(prop);
				prop = next;
			}
		}
		free(obj->properties);

		/* Free array elements */
		if (obj->elements) {
			for (size_t i = 0; i < obj->length; i++)
				if (obj->elements[i])
					js_value_release(obj->elements[i]);
			free(obj->elements);
		}

		free(obj);
		memory_stats.objects_allocated--;
	}
}

/*
 * Reference Counting
 */

void
js_value_retain(js_value_t *val)
{
	if (val && val->refcount < 1000000)
		val->refcount++;
}

void
js_value_release(js_value_t *val)
{
	if (!val || val->refcount >= 1000000)
		return;

	if (--val->refcount == 0) {
		/* Free the value */
		switch (val->type) {
		case JS_TYPE_STRING:
			js_string_free(val->u.string);
			break;
		case JS_TYPE_OBJECT:
			js_object_free(val->u.object);
			break;
		case JS_TYPE_FUNCTION:
			js_function_free(val->u.function);
			break;
		default:
			break;
		}
		free(val);
		memory_stats.values_allocated--;
	}
}

/*
 * Typeof Operator
 */

const char *
js_typeof(js_value_t *val)
{
	switch (val->type) {
	case JS_TYPE_UNDEFINED: return "undefined";
	case JS_TYPE_NULL: return "object"; /* Historical quirk */
	case JS_TYPE_BOOLEAN: return "boolean";
	case JS_TYPE_NUMBER: return "number";
	case JS_TYPE_BIGINT: return "bigint";
	case JS_TYPE_STRING: return "string";
	case JS_TYPE_SYMBOL: return "symbol";
	case JS_TYPE_FUNCTION: return "function";
	case JS_TYPE_OBJECT: return "object";
	default: return "unknown";
	}
}

/*
 * Utility Functions
 */

void
js_value_print(js_value_t *val, FILE *fp)
{
	switch (val->type) {
	case JS_TYPE_UNDEFINED:
		fprintf(fp, "undefined");
		break;
	case JS_TYPE_NULL:
		fprintf(fp, "null");
		break;
	case JS_TYPE_BOOLEAN:
		fprintf(fp, "%s", val->u.boolean ? "true" : "false");
		break;
	case JS_TYPE_NUMBER:
		if (isnan(val->u.number))
			fprintf(fp, "NaN");
		else if (isinf(val->u.number))
			fprintf(fp, "%sInfinity", val->u.number < 0 ? "-" : "");
		else
			fprintf(fp, "%g", val->u.number);
		break;
	case JS_TYPE_BIGINT:
		fprintf(fp, "%llun", (unsigned long long)val->u.bigint);
		break;
	case JS_TYPE_STRING:
		fprintf(fp, "\"%s\"", val->u.string->data);
		break;
	case JS_TYPE_SYMBOL:
		fprintf(fp, "Symbol(%u)", val->u.symbol_id);
		break;
	case JS_TYPE_FUNCTION:
		fprintf(fp, "[Function]");
		break;
	case JS_TYPE_OBJECT:
		if (js_is_array(val))
			fprintf(fp, "[Array]");
		else
			fprintf(fp, "[Object]");
		break;
	}
}

void
js_get_memory_stats(js_memory_stats_t *stats)
{
	*stats = memory_stats;
}
