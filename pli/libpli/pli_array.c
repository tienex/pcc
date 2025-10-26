/*
 * Copyright (c) 2025 PCC PL/I Runtime Library
 *
 * Array manipulation functions
 */

#include "pli_runtime.h"
#include <stdlib.h>
#include <string.h>

/* Create array descriptor */
pli_array_t *pli_array_create(int32_t ndims, int32_t *lower, int32_t *upper, size_t elem_size) {
	pli_array_t *arr = malloc(sizeof(pli_array_t));
	if (!arr) return NULL;

	arr->ndims = ndims;
	arr->elem_size = elem_size;

	/* Allocate bounds arrays */
	arr->lower_bounds = malloc(ndims * sizeof(int32_t));
	arr->upper_bounds = malloc(ndims * sizeof(int32_t));

	if (!arr->lower_bounds || !arr->upper_bounds) {
		free(arr->lower_bounds);
		free(arr->upper_bounds);
		free(arr);
		return NULL;
	}

	memcpy(arr->lower_bounds, lower, ndims * sizeof(int32_t));
	memcpy(arr->upper_bounds, upper, ndims * sizeof(int32_t));

	/* Calculate total size and allocate data */
	size_t total_elements = 1;
	for (int32_t i = 0; i < ndims; i++) {
		total_elements *= (upper[i] - lower[i] + 1);
	}

	arr->data = calloc(total_elements, elem_size);
	if (!arr->data) {
		free(arr->lower_bounds);
		free(arr->upper_bounds);
		free(arr);
		return NULL;
	}

	return arr;
}

/* Destroy array */
void pli_array_destroy(pli_array_t *arr) {
	if (!arr) return;

	if (arr->data) free(arr->data);
	if (arr->lower_bounds) free(arr->lower_bounds);
	if (arr->upper_bounds) free(arr->upper_bounds);
	free(arr);
}

/* DIM - Get dimension extent */
pli_fixed_t pli_dim(pli_array_t *arr, pli_fixed_t dimension) {
	if (!arr || dimension < 1 || dimension > arr->ndims) {
		return 0;
	}

	int32_t dim_idx = dimension - 1;  /* Convert to 0-based */
	return arr->upper_bounds[dim_idx] - arr->lower_bounds[dim_idx] + 1;
}

/* HBOUND - Get upper bound */
pli_fixed_t pli_hbound(pli_array_t *arr, pli_fixed_t dimension) {
	if (!arr || dimension < 1 || dimension > arr->ndims) {
		return 0;
	}

	int32_t dim_idx = dimension - 1;
	return arr->upper_bounds[dim_idx];
}

/* LBOUND - Get lower bound */
pli_fixed_t pli_lbound(pli_array_t *arr, pli_fixed_t dimension) {
	if (!arr || dimension < 1 || dimension > arr->ndims) {
		return 0;
	}

	int32_t dim_idx = dimension - 1;
	return arr->lower_bounds[dim_idx];
}

/* SUM - Sum all array elements (assumes FIXED BINARY elements) */
pli_fixed_t pli_sum(pli_array_t *arr) {
	if (!arr || arr->elem_size != sizeof(pli_fixed_t)) {
		return 0;
	}

	size_t total_elements = 1;
	for (int32_t i = 0; i < arr->ndims; i++) {
		total_elements *= (arr->upper_bounds[i] - arr->lower_bounds[i] + 1);
	}

	pli_fixed_t sum = 0;
	pli_fixed_t *data = (pli_fixed_t *)arr->data;

	for (size_t i = 0; i < total_elements; i++) {
		sum += data[i];
	}

	return sum;
}

/* PROD - Product of all array elements (assumes FIXED BINARY elements) */
pli_fixed_t pli_prod(pli_array_t *arr) {
	if (!arr || arr->elem_size != sizeof(pli_fixed_t)) {
		return 0;
	}

	size_t total_elements = 1;
	for (int32_t i = 0; i < arr->ndims; i++) {
		total_elements *= (arr->upper_bounds[i] - arr->lower_bounds[i] + 1);
	}

	pli_fixed_t prod = 1;
	pli_fixed_t *data = (pli_fixed_t *)arr->data;

	for (size_t i = 0; i < total_elements; i++) {
		prod *= data[i];
	}

	return prod;
}
