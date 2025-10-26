/*	$Id$	*/
/*
 * Copyright (c) 2025 JavaScript Runtime Library
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 */

/*
 * JavaScript Runtime Library - Core Header
 *
 * This header defines the JavaScript value representation, type system,
 * and core runtime functions for compiled JavaScript code.
 */

#ifndef _JSRT_H
#define _JSRT_H

#include <stdint.h>
#include <stddef.h>
#include <stdio.h>

/* JavaScript Types */
typedef enum {
	JS_TYPE_UNDEFINED = 0,
	JS_TYPE_NULL,
	JS_TYPE_BOOLEAN,
	JS_TYPE_NUMBER,
	JS_TYPE_BIGINT,
	JS_TYPE_STRING,
	JS_TYPE_SYMBOL,
	JS_TYPE_OBJECT,
	JS_TYPE_FUNCTION,
} js_type_t;

/* Forward declarations */
struct js_value;
struct js_object;
struct js_property;
struct js_function;
struct js_string;

/* JavaScript Value Representation */
typedef struct js_value {
	js_type_t type;

	/* Reference counting for GC */
	uint32_t refcount;

	/* GC marking */
	uint8_t gc_mark;

	/* Value storage */
	union {
		/* Primitive values */
		int boolean;
		double number;
		uint64_t bigint;

		/* Reference types */
		struct js_string *string;
		struct js_object *object;
		struct js_function *function;
		uint32_t symbol_id;
	} u;

	/* GC linked list */
	struct js_value *gc_next;
} js_value_t;

/* JavaScript String */
typedef struct js_string {
	uint32_t refcount;
	size_t length;
	uint32_t hash;
	char *data;

	/* For interned strings */
	struct js_string *intern_next;
} js_string_t;

/* JavaScript Object Property */
typedef struct js_property {
	js_string_t *name;
	js_value_t *value;

	/* Property attributes */
	uint8_t writable : 1;
	uint8_t enumerable : 1;
	uint8_t configurable : 1;

	/* Getter/setter */
	struct js_function *getter;
	struct js_function *setter;

	struct js_property *next;
} js_property_t;

/* JavaScript Object */
typedef struct js_object {
	uint32_t refcount;

	/* Property hash table */
	js_property_t **properties;
	size_t property_count;
	size_t property_capacity;

	/* Prototype chain */
	struct js_object *prototype;

	/* For arrays */
	js_value_t **elements;
	size_t length;
	size_t element_capacity;

	/* For functions */
	struct js_function *function;

	/* Constructor */
	struct js_function *constructor;

	/* Internal slots */
	void *internal_data;

	/* GC */
	uint8_t gc_mark;
} js_object_t;

/* JavaScript Function */
typedef struct js_function {
	uint32_t refcount;

	/* Function pointer (for native functions) */
	js_value_t *(*native_fn)(js_value_t *this_val, js_value_t **args, int argc);

	/* For JS functions (compiled code) */
	void *code_ptr;

	/* Closure environment */
	struct js_object *scope;

	/* Function properties */
	js_string_t *name;
	int arity;

	/* Bound function */
	js_value_t *bound_this;
	js_value_t **bound_args;
	int bound_argc;

	/* Flags */
	uint8_t is_constructor : 1;
	uint8_t is_native : 1;
	uint8_t is_arrow : 1;
	uint8_t is_generator : 1;
	uint8_t is_async : 1;

	/* Prototype for constructor functions */
	js_object_t *prototype_obj;
} js_function_t;

/* JavaScript Symbols */
typedef struct js_symbol {
	uint32_t id;
	js_string_t *description;
} js_symbol_t;

/*
 * Core Runtime Functions
 */

/* Initialization and cleanup */
void js_runtime_init(void);
void js_runtime_cleanup(void);

/* Value creation */
js_value_t *js_value_undefined(void);
js_value_t *js_value_null(void);
js_value_t *js_value_boolean(int val);
js_value_t *js_value_number(double val);
js_value_t *js_value_bigint(uint64_t val);
js_value_t *js_value_string(const char *str);
js_value_t *js_value_string_len(const char *str, size_t len);
js_value_t *js_value_object(void);
js_value_t *js_value_array(size_t length);
js_value_t *js_value_function(js_function_t *fn);
js_value_t *js_value_symbol(const char *description);

/* Type checking */
int js_is_undefined(js_value_t *val);
int js_is_null(js_value_t *val);
int js_is_boolean(js_value_t *val);
int js_is_number(js_value_t *val);
int js_is_bigint(js_value_t *val);
int js_is_string(js_value_t *val);
int js_is_symbol(js_value_t *val);
int js_is_object(js_value_t *val);
int js_is_function(js_value_t *val);
int js_is_array(js_value_t *val);
int js_is_nullish(js_value_t *val);  /* null or undefined */
int js_is_primitive(js_value_t *val);

/* Type conversion (abstract operations from ES spec) */
js_value_t *js_to_primitive(js_value_t *val, const char *hint);
js_value_t *js_to_boolean(js_value_t *val);
js_value_t *js_to_number(js_value_t *val);
js_value_t *js_to_bigint(js_value_t *val);
js_value_t *js_to_string(js_value_t *val);
js_value_t *js_to_object(js_value_t *val);

/* Raw value extraction */
int js_get_boolean(js_value_t *val);
double js_get_number(js_value_t *val);
uint64_t js_get_bigint(js_value_t *val);
const char *js_get_string(js_value_t *val);
js_object_t *js_get_object(js_value_t *val);
js_function_t *js_get_function(js_value_t *val);

/* String operations */
js_string_t *js_string_create(const char *str);
js_string_t *js_string_create_len(const char *str, size_t len);
js_string_t *js_string_concat(js_string_t *a, js_string_t *b);
js_string_t *js_string_concat_cstr(js_string_t *a, const char *b);
int js_string_compare(js_string_t *a, js_string_t *b);
js_string_t *js_string_substring(js_string_t *str, size_t start, size_t len);
js_string_t *js_string_intern(const char *str);
void js_string_free(js_string_t *str);

/* Object operations */
js_object_t *js_object_create(js_object_t *prototype);
js_value_t *js_object_get(js_object_t *obj, const char *key);
js_value_t *js_object_get_str(js_object_t *obj, js_string_t *key);
void js_object_set(js_object_t *obj, const char *key, js_value_t *val);
void js_object_set_str(js_object_t *obj, js_string_t *key, js_value_t *val);
int js_object_has(js_object_t *obj, const char *key);
int js_object_delete(js_object_t *obj, const char *key);
js_value_t **js_object_keys(js_object_t *obj, size_t *count);
js_value_t **js_object_values(js_object_t *obj, size_t *count);
void js_object_free(js_object_t *obj);

/* Property descriptor operations */
void js_object_define_property(js_object_t *obj, const char *key,
                                js_value_t *value, int writable,
                                int enumerable, int configurable);
void js_object_define_accessor(js_object_t *obj, const char *key,
                                js_function_t *getter, js_function_t *setter,
                                int enumerable, int configurable);

/* Array operations */
js_value_t *js_array_get(js_object_t *arr, size_t index);
void js_array_set(js_object_t *arr, size_t index, js_value_t *val);
void js_array_push(js_object_t *arr, js_value_t *val);
js_value_t *js_array_pop(js_object_t *arr);
void js_array_unshift(js_object_t *arr, js_value_t *val);
js_value_t *js_array_shift(js_object_t *arr);
size_t js_array_length(js_object_t *arr);

/* Function operations */
js_function_t *js_function_create_native(js_value_t *(*fn)(js_value_t *, js_value_t **, int),
                                          const char *name, int arity);
js_function_t *js_function_create_js(void *code_ptr, js_object_t *scope,
                                      const char *name, int arity);
js_value_t *js_function_call(js_function_t *fn, js_value_t *this_val,
                             js_value_t **args, int argc);
js_value_t *js_function_construct(js_function_t *fn, js_value_t **args, int argc);
js_function_t *js_function_bind(js_function_t *fn, js_value_t *this_val,
                                js_value_t **args, int argc);
void js_function_free(js_function_t *fn);

/* Reference counting */
void js_value_retain(js_value_t *val);
void js_value_release(js_value_t *val);

/* Typeof operator */
const char *js_typeof(js_value_t *val);

/* Instanceof operator */
int js_instanceof(js_value_t *val, js_function_t *constructor);

/* In operator */
int js_in(js_value_t *prop, js_value_t *obj);

/* Delete operator */
int js_delete(js_object_t *obj, const char *key);

/* Error creation */
js_value_t *js_error(const char *message);
js_value_t *js_type_error(const char *message);
js_value_t *js_reference_error(const char *message);
js_value_t *js_range_error(const char *message);
js_value_t *js_syntax_error(const char *message);

/* Throw exception */
void js_throw(js_value_t *error) __attribute__((noreturn));

/* Exception handling support */
typedef struct js_try_state {
	void *jmp_buf;
	js_value_t *exception;
	struct js_try_state *prev;
} js_try_state_t;

extern js_try_state_t *js_current_try_state;

#define JS_TRY(state) \
	js_try_state_t state; \
	state.prev = js_current_try_state; \
	js_current_try_state = &state; \
	if (setjmp(state.jmp_buf) == 0)

#define JS_CATCH(state, exception) \
	js_current_try_state = state.prev; \
	else for (js_value_t *exception = state.exception; exception; exception = NULL)

#define JS_FINALLY(state) \
	js_current_try_state = state.prev;

/* Global object */
extern js_object_t *js_global_object;

/* Builtin prototypes */
extern js_object_t *js_object_prototype;
extern js_object_t *js_function_prototype;
extern js_object_t *js_array_prototype;
extern js_object_t *js_string_prototype;
extern js_object_t *js_number_prototype;
extern js_object_t *js_boolean_prototype;
extern js_object_t *js_error_prototype;

/* Builtin constructors */
extern js_function_t *js_object_constructor;
extern js_function_t *js_function_constructor;
extern js_function_t *js_array_constructor;
extern js_function_t *js_string_constructor;
extern js_function_t *js_number_constructor;
extern js_function_t *js_boolean_constructor;
extern js_function_t *js_error_constructor;
extern js_function_t *js_type_error_constructor;
extern js_function_t *js_reference_error_constructor;
extern js_function_t *js_range_error_constructor;

/* Special values */
extern js_value_t *js_undefined_value;
extern js_value_t *js_null_value;
extern js_value_t *js_true_value;
extern js_value_t *js_false_value;
extern js_value_t *js_nan_value;
extern js_value_t *js_infinity_value;
extern js_value_t *js_neg_infinity_value;

/* Utility macros */
#define JS_UNDEFINED() (js_undefined_value)
#define JS_NULL() (js_null_value)
#define JS_TRUE() (js_true_value)
#define JS_FALSE() (js_false_value)
#define JS_BOOL(x) ((x) ? js_true_value : js_false_value)
#define JS_NUM(x) (js_value_number(x))
#define JS_STR(x) (js_value_string(x))

/* Debug and utilities */
void js_value_print(js_value_t *val, FILE *fp);
char *js_value_to_cstring(js_value_t *val);
void js_dump_object(js_object_t *obj, FILE *fp);

/* Memory statistics */
typedef struct js_memory_stats {
	size_t values_allocated;
	size_t objects_allocated;
	size_t strings_allocated;
	size_t functions_allocated;
	size_t total_memory_bytes;
	size_t gc_collections;
} js_memory_stats_t;

void js_get_memory_stats(js_memory_stats_t *stats);

#endif /* _JSRT_H */
