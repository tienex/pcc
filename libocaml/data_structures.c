/*
 * Copyright (c) 2025 PCC OCaml Runtime Library
 *
 * Core data structures: lists, arrays, strings
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "ocaml_runtime.h"

/* ===== List Operations ===== */

/*
 * Create a cons cell (head :: tail)
 */
ocaml_value_t
ocaml_cons(ocaml_value_t head, ocaml_value_t tail)
{
	ocaml_value_t cell;

	cell = ocaml_alloc(2, TAG_CONS);
	FIELD(cell, 0) = head;
	FIELD(cell, 1) = tail;

	return cell;
}

/*
 * Get head of list
 */
ocaml_value_t
ocaml_list_hd(ocaml_value_t list)
{
	if (list == VAL_NIL) {
		fprintf(stderr, "ocaml_list_hd: empty list\n");
		exit(1);
	}

	return FIELD(list, 0);
}

/*
 * Get tail of list
 */
ocaml_value_t
ocaml_list_tl(ocaml_value_t list)
{
	if (list == VAL_NIL) {
		fprintf(stderr, "ocaml_list_tl: empty list\n");
		exit(1);
	}

	return FIELD(list, 1);
}

/*
 * Get length of list
 */
ocaml_value_t
ocaml_list_length(ocaml_value_t list)
{
	intptr_t len = 0;

	while (list != VAL_NIL) {
		len++;
		list = FIELD(list, 1);
	}

	return VAL_INT(len);
}

/*
 * Get nth element of list (0-indexed)
 */
ocaml_value_t
ocaml_list_nth(ocaml_value_t list, ocaml_value_t n)
{
	intptr_t index = INT_VAL(n);
	intptr_t i;

	for (i = 0; i < index; i++) {
		if (list == VAL_NIL) {
			fprintf(stderr, "ocaml_list_nth: index out of bounds\n");
			exit(1);
		}
		list = FIELD(list, 1);
	}

	if (list == VAL_NIL) {
		fprintf(stderr, "ocaml_list_nth: index out of bounds\n");
		exit(1);
	}

	return FIELD(list, 0);
}

/* ===== Array Operations ===== */

/*
 * Create an array of given size, initialized with value
 */
ocaml_value_t
ocaml_array_make(ocaml_value_t size, ocaml_value_t init)
{
	ocaml_value_t array;
	intptr_t len = INT_VAL(size);
	intptr_t i;

	if (len < 0) {
		fprintf(stderr, "ocaml_array_make: negative size\n");
		exit(1);
	}

	array = ocaml_alloc(len, TAG_ARRAY);

	for (i = 0; i < len; i++) {
		FIELD(array, i) = init;
	}

	return array;
}

/*
 * Get length of array
 */
ocaml_value_t
ocaml_array_length(ocaml_value_t array)
{
	if (!IS_BLOCK(array) || TAG(array) != TAG_ARRAY) {
		fprintf(stderr, "ocaml_array_length: not an array\n");
		exit(1);
	}

	return VAL_INT(SIZE(array));
}

/*
 * Get element from array
 */
ocaml_value_t
ocaml_array_get(ocaml_value_t array, ocaml_value_t index)
{
	intptr_t idx = INT_VAL(index);

	if (!IS_BLOCK(array) || TAG(array) != TAG_ARRAY) {
		fprintf(stderr, "ocaml_array_get: not an array\n");
		exit(1);
	}

	if (idx < 0 || idx >= (intptr_t)SIZE(array)) {
		fprintf(stderr, "ocaml_array_get: index out of bounds\n");
		exit(1);
	}

	return FIELD(array, idx);
}

/*
 * Set element in array
 */
void
ocaml_array_set(ocaml_value_t array, ocaml_value_t index, ocaml_value_t val)
{
	intptr_t idx = INT_VAL(index);

	if (!IS_BLOCK(array) || TAG(array) != TAG_ARRAY) {
		fprintf(stderr, "ocaml_array_set: not an array\n");
		exit(1);
	}

	if (idx < 0 || idx >= (intptr_t)SIZE(array)) {
		fprintf(stderr, "ocaml_array_set: index out of bounds\n");
		exit(1);
	}

	FIELD(array, idx) = val;
}

/* ===== String Operations ===== */

/*
 * Create a string from C string
 */
ocaml_value_t
ocaml_string_make(const char *str)
{
	ocaml_value_t s;
	size_t len = strlen(str);
	size_t words = (len + sizeof(ocaml_value_t)) / sizeof(ocaml_value_t);
	char *data;

	s = ocaml_alloc(words, TAG_STRING);
	data = (char *)s;

	strcpy(data, str);

	return s;
}

/*
 * Get length of string
 */
ocaml_value_t
ocaml_string_length(ocaml_value_t str)
{
	if (!IS_BLOCK(str) || TAG(str) != TAG_STRING) {
		fprintf(stderr, "ocaml_string_length: not a string\n");
		exit(1);
	}

	return VAL_INT(strlen((char *)str));
}

/*
 * Get character from string
 */
ocaml_value_t
ocaml_string_get(ocaml_value_t str, ocaml_value_t index)
{
	intptr_t idx = INT_VAL(index);
	char *data;
	size_t len;

	if (!IS_BLOCK(str) || TAG(str) != TAG_STRING) {
		fprintf(stderr, "ocaml_string_get: not a string\n");
		exit(1);
	}

	data = (char *)str;
	len = strlen(data);

	if (idx < 0 || idx >= (intptr_t)len) {
		fprintf(stderr, "ocaml_string_get: index out of bounds\n");
		exit(1);
	}

	return VAL_INT(data[idx]);
}

/*
 * Get C string data (for printing)
 */
const char *
ocaml_string_data(ocaml_value_t str)
{
	if (!IS_BLOCK(str) || TAG(str) != TAG_STRING) {
		fprintf(stderr, "ocaml_string_data: not a string\n");
		exit(1);
	}

	return (const char *)str;
}

/*
 * Concatenate list of strings with separator
 */
ocaml_value_t
ocaml_string_concat(ocaml_value_t sep, ocaml_value_t list)
{
	const char *sep_str;
	size_t sep_len;
	size_t total_len = 0;
	ocaml_value_t curr;
	char *result, *ptr;
	ocaml_value_t result_val;
	int first = 1;

	if (!IS_BLOCK(sep) || TAG(sep) != TAG_STRING) {
		fprintf(stderr, "ocaml_string_concat: separator not a string\n");
		exit(1);
	}

	sep_str = (const char *)sep;
	sep_len = strlen(sep_str);

	/* Calculate total length */
	curr = list;
	while (curr != VAL_NIL) {
		ocaml_value_t str = FIELD(curr, 0);
		if (!IS_BLOCK(str) || TAG(str) != TAG_STRING) {
			fprintf(stderr, "ocaml_string_concat: list element not a string\n");
			exit(1);
		}
		total_len += strlen((const char *)str);
		curr = FIELD(curr, 1);
		if (curr != VAL_NIL)
			total_len += sep_len;
	}

	/* Allocate result */
	result = malloc(total_len + 1);
	if (!result) {
		fprintf(stderr, "ocaml_string_concat: out of memory\n");
		exit(1);
	}

	/* Build result */
	ptr = result;
	curr = list;
	while (curr != VAL_NIL) {
		const char *str = (const char *)FIELD(curr, 0);
		size_t len = strlen(str);

		if (!first) {
			memcpy(ptr, sep_str, sep_len);
			ptr += sep_len;
		}
		first = 0;

		memcpy(ptr, str, len);
		ptr += len;

		curr = FIELD(curr, 1);
	}
	*ptr = '\0';

	/* Convert to OCaml string */
	result_val = ocaml_string_make(result);
	free(result);

	return result_val;
}

/* ===== Reference Operations ===== */

/*
 * Create a reference
 */
ocaml_value_t
ocaml_ref(ocaml_value_t val)
{
	ocaml_value_t ref = ocaml_alloc(1, 0);
	FIELD(ref, 0) = val;
	return ref;
}

/*
 * Dereference
 */
ocaml_value_t
ocaml_deref(ocaml_value_t ref)
{
	if (!IS_BLOCK(ref)) {
		fprintf(stderr, "ocaml_deref: not a reference\n");
		exit(1);
	}

	return FIELD(ref, 0);
}

/*
 * Assign to reference
 */
void
ocaml_assign(ocaml_value_t ref, ocaml_value_t val)
{
	if (!IS_BLOCK(ref)) {
		fprintf(stderr, "ocaml_assign: not a reference\n");
		exit(1);
	}

	FIELD(ref, 0) = val;
}
