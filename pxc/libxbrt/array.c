/*
 * Copyright (c) 2025 PCC Xbase++ Runtime Library
 *
 * Array functions
 */

#include "xbrt.h"
#include <stdlib.h>
#include <string.h>

/* ALEN() - Array length */
int xb_alen(const xb_array_t *arr) {
	return arr ? arr->length : 0;
}

/* ASIZE() - Resize array */
void xb_asize(xb_array_t *arr, int new_size) {
	if (!arr || new_size < 0)
		return;

	if (new_size > arr->capacity) {
		size_t new_cap = new_size * 2;
		xb_value_t *new_elems = realloc(arr->elements,
		                                 new_cap * sizeof(xb_value_t));
		if (new_elems) {
			arr->elements = new_elems;
			arr->capacity = new_cap;
			memset(arr->elements + arr->length, 0,
			       (new_size - arr->length) * sizeof(xb_value_t));
		}
	}

	arr->length = new_size;
}

/* AADD() - Add element to array */
void xb_aadd(xb_array_t *arr, const xb_value_t *val) {
	if (!arr)
		return;

	xb_asize(arr, arr->length + 1);
	if (val)
		arr->elements[arr->length - 1] = *val;
}

/* AINS() - Insert element */
void xb_ains(xb_array_t *arr, int pos) {
	if (!arr || pos < 1 || pos > arr->length)
		return;

	xb_asize(arr, arr->length + 1);

	/* Shift elements right */
	memmove(&arr->elements[pos], &arr->elements[pos - 1],
	        (arr->length - pos) * sizeof(xb_value_t));

	/* Clear inserted element */
	memset(&arr->elements[pos - 1], 0, sizeof(xb_value_t));
}

/* ADEL() - Delete element */
void xb_adel(xb_array_t *arr, int pos) {
	if (!arr || pos < 1 || pos > arr->length)
		return;

	/* Shift elements left */
	memmove(&arr->elements[pos - 1], &arr->elements[pos],
	        (arr->length - pos) * sizeof(xb_value_t));

	arr->length--;
}

/* ASORT() - Sort array (simple numeric/string sort) */
void xb_asort(xb_array_t *arr) {
	/* TODO: Implement proper sorting */
	(void)arr;
}

/* ASCAN() - Scan for value */
int xb_ascan(const xb_array_t *arr, const xb_value_t *val) {
	if (!arr || !val)
		return 0;

	size_t i;
	for (i = 0; i < arr->length; i++) {
		/* Simple comparison - TODO: implement proper value comparison */
		if (arr->elements[i].type == val->type)
			return i + 1;  /* 1-based */
	}

	return 0;
}

/* AFILL() - Fill array with value */
void xb_afill(xb_array_t *arr, const xb_value_t *val, int start, int count) {
	if (!arr || !val || start < 1)
		return;

	if (count < 0)
		count = arr->length - start + 1;

	size_t i;
	for (i = start - 1; i < arr->length && count > 0; i++, count--) {
		arr->elements[i] = *val;
	}
}

/* ACLONE() - Clone array */
xb_array_t *xb_aclone(const xb_array_t *arr) {
	if (!arr)
		return NULL;

	xb_array_t *clone = malloc(sizeof(xb_array_t));
	if (!clone)
		return NULL;

	clone->length = arr->length;
	clone->capacity = arr->capacity;
	clone->elements = calloc(arr->capacity, sizeof(xb_value_t));

	if (clone->elements) {
		memcpy(clone->elements, arr->elements,
		       arr->length * sizeof(xb_value_t));
	}

	return clone;
}
