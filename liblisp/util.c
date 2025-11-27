/*
 * Copyright (c) 2025 PCC Common LISP Runtime Library
 *
 * Utility functions implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include "liblisp.h"

/*
 * Report a runtime error
 */
void
lisp_error(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	fprintf(stderr, "LISP Runtime Error: ");
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
}

/*
 * Test if object is true (anything except NIL is true)
 */
int
lisp_is_true(lisp_object_t *obj)
{
	return (obj != NULL && obj->type != LISP_TYPE_NIL);
}

/*
 * Test if object is NIL
 */
int
lisp_is_nil(lisp_object_t *obj)
{
	return (obj == NULL || obj->type == LISP_TYPE_NIL);
}

/*
 * Initialize runtime
 */
void
lisp_runtime_init(void)
{
	lisp_gc_init();
}

/*
 * Cleanup runtime
 */
void
lisp_runtime_cleanup(void)
{
	/* Cleanup resources */
}

/*
 * Convert object to string
 */
lisp_object_t *
lisp_string(lisp_object_t *obj)
{
	char buffer[256];

	switch (obj->type) {
	case LISP_TYPE_STRING:
		return obj;

	case LISP_TYPE_SYMBOL:
		return lisp_make_string(obj->value.symbol);

	case LISP_TYPE_INTEGER:
		snprintf(buffer, sizeof(buffer), "%ld", obj->value.integer);
		return lisp_make_string(buffer);

	case LISP_TYPE_FLOAT:
		snprintf(buffer, sizeof(buffer), "%g", obj->value.floating);
		return lisp_make_string(buffer);

	case LISP_TYPE_NIL:
		return lisp_make_string("NIL");

	case LISP_TYPE_T:
		return lisp_make_string("T");

	default:
		return lisp_make_string("");
	}
}

/*
 * Convert string to uppercase
 */
lisp_object_t *
lisp_string_upcase(lisp_object_t *str)
{
	if (str->type != LISP_TYPE_STRING) {
		lisp_error("STRING-UPCASE: argument must be a string");
		return LISP_NIL;
	}

	char *result = strdup(str->value.string);
	for (char *p = result; *p; p++)
		*p = toupper(*p);

	return lisp_make_string(result);
}

/*
 * Convert string to lowercase
 */
lisp_object_t *
lisp_string_downcase(lisp_object_t *str)
{
	if (str->type != LISP_TYPE_STRING) {
		lisp_error("STRING-DOWNCASE: argument must be a string");
		return LISP_NIL;
	}

	char *result = strdup(str->value.string);
	for (char *p = result; *p; p++)
		*p = tolower(*p);

	return lisp_make_string(result);
}

/*
 * String equality
 */
lisp_object_t *
lisp_string_equal(lisp_object_t *a, lisp_object_t *b)
{
	if (a->type != LISP_TYPE_STRING || b->type != LISP_TYPE_STRING) {
		lisp_error("STRING=: arguments must be strings");
		return LISP_NIL;
	}

	return (strcmp(a->value.string, b->value.string) == 0) ? LISP_T : LISP_NIL;
}

/*
 * String less than
 */
lisp_object_t *
lisp_string_less(lisp_object_t *a, lisp_object_t *b)
{
	if (a->type != LISP_TYPE_STRING || b->type != LISP_TYPE_STRING) {
		lisp_error("STRING<: arguments must be strings");
		return LISP_NIL;
	}

	return (strcmp(a->value.string, b->value.string) < 0) ? LISP_T : LISP_NIL;
}

/*
 * Concatenate strings
 */
lisp_object_t *
lisp_concatenate(int n, ...)
{
	va_list ap;
	char buffer[4096] = "";
	int i;

	va_start(ap, n);

	for (i = 0; i < n; i++) {
		lisp_object_t *str = va_arg(ap, lisp_object_t *);
		if (str->type != LISP_TYPE_STRING) {
			lisp_error("CONCATENATE: all arguments must be strings");
			va_end(ap);
			return LISP_NIL;
		}
		strcat(buffer, str->value.string);
	}

	va_end(ap);
	return lisp_make_string(buffer);
}
