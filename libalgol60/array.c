/*
 * Copyright (c) 2025 PCC ALGOL 60+ Runtime Library
 *
 * Array operations and memory management for ALGOL 60+ programs
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "algol60.h"

/*
 * Array allocation
 */

algol_array_t *
algol_array_create(int ndims, int *lower_bounds, int *upper_bounds, size_t elem_size)
{
	algol_array_t *array;
	size_t total_elements;
	int i;

	if (ndims <= 0) {
		algol_error(ALGOL_ERR_BOUNDS, "array_create: invalid number of dimensions");
		return NULL;
	}

	/* Allocate array descriptor */
	array = (algol_array_t *)algol_malloc(sizeof(algol_array_t));
	if (array == NULL) {
		algol_fatal_error(ALGOL_ERR_MEMORY, "array_create: out of memory");
		return NULL;
	}

	array->ndims = ndims;
	array->elem_size = elem_size;

	/* Allocate bounds arrays */
	array->lower_bounds = (int *)algol_malloc(ndims * sizeof(int));
	array->upper_bounds = (int *)algol_malloc(ndims * sizeof(int));

	if (array->lower_bounds == NULL || array->upper_bounds == NULL) {
		algol_free(array->lower_bounds);
		algol_free(array->upper_bounds);
		algol_free(array);
		algol_fatal_error(ALGOL_ERR_MEMORY, "array_create: out of memory");
		return NULL;
	}

	/* Copy bounds and compute total size */
	total_elements = 1;
	for (i = 0; i < ndims; i++) {
		if (upper_bounds[i] < lower_bounds[i]) {
			algol_error(ALGOL_ERR_BOUNDS,
			    "array_create: upper bound < lower bound for dimension %d", i);
			algol_free(array->lower_bounds);
			algol_free(array->upper_bounds);
			algol_free(array);
			return NULL;
		}

		array->lower_bounds[i] = lower_bounds[i];
		array->upper_bounds[i] = upper_bounds[i];
		total_elements *= (upper_bounds[i] - lower_bounds[i] + 1);
	}

	/* Allocate data */
	array->total_size = total_elements * elem_size;
	array->data = algol_calloc(total_elements, elem_size);

	if (array->data == NULL) {
		algol_free(array->lower_bounds);
		algol_free(array->upper_bounds);
		algol_free(array);
		algol_fatal_error(ALGOL_ERR_MEMORY, "array_create: out of memory for data");
		return NULL;
	}

	return array;
}

/*
 * Array deallocation
 */

void
algol_array_free(algol_array_t *array)
{
	if (array == NULL) {
		return;
	}

	algol_free(array->data);
	algol_free(array->lower_bounds);
	algol_free(array->upper_bounds);
	algol_free(array);
}

/*
 * Array bounds checking
 */

int
algol_array_bounds_check(algol_array_t *array, int dim, int index)
{
	if (array == NULL) {
		algol_error(ALGOL_ERR_NULL_PTR, "bounds_check: null array");
		return 0;
	}

	if (dim < 0 || dim >= array->ndims) {
		algol_error(ALGOL_ERR_BOUNDS, "bounds_check: invalid dimension %d", dim);
		return 0;
	}

	if (index < array->lower_bounds[dim] || index > array->upper_bounds[dim]) {
		algol_error(ALGOL_ERR_BOUNDS,
		    "bounds_check: index %d out of bounds [%d:%d] for dimension %d",
		    index, array->lower_bounds[dim], array->upper_bounds[dim], dim);
		return 0;
	}

	return 1;
}

/*
 * Array element access
 * Returns pointer to element at given indices
 */

void *
algol_array_element(algol_array_t *array, int *indices)
{
	size_t offset;
	size_t multiplier;
	int i;
	char *ptr;

	if (array == NULL) {
		algol_error(ALGOL_ERR_NULL_PTR, "array_element: null array");
		return NULL;
	}

	if (indices == NULL) {
		algol_error(ALGOL_ERR_NULL_PTR, "array_element: null indices");
		return NULL;
	}

	/* Compute linear offset using row-major order */
	offset = 0;
	multiplier = 1;

	for (i = array->ndims - 1; i >= 0; i--) {
		/* Bounds check */
		if (!algol_array_bounds_check(array, i, indices[i])) {
			return NULL;
		}

		/* Add contribution of this dimension */
		offset += (indices[i] - array->lower_bounds[i]) * multiplier;

		/* Update multiplier for next dimension */
		multiplier *= (array->upper_bounds[i] - array->lower_bounds[i] + 1);
	}

	/* Return pointer to element */
	ptr = (char *)array->data;
	return (void *)(ptr + offset * array->elem_size);
}

/*
 * Memory management with tracking
 */

static size_t total_allocated = 0;
static size_t peak_allocated = 0;

void *
algol_malloc(size_t size)
{
	void *ptr;

	ptr = malloc(size);
	if (ptr != NULL) {
		total_allocated += size;
		if (total_allocated > peak_allocated) {
			peak_allocated = total_allocated;
		}
	}

	return ptr;
}

void *
algol_calloc(size_t nmemb, size_t size)
{
	void *ptr;
	size_t total_size = nmemb * size;

	ptr = calloc(nmemb, size);
	if (ptr != NULL) {
		total_allocated += total_size;
		if (total_allocated > peak_allocated) {
			peak_allocated = total_allocated;
		}
	}

	return ptr;
}

void *
algol_realloc(void *old_ptr, size_t size)
{
	void *ptr;

	ptr = realloc(old_ptr, size);
	if (ptr != NULL) {
		/* Note: tracking realloc size changes is complex,
		 * this is a simplified version */
		total_allocated += size;
		if (total_allocated > peak_allocated) {
			peak_allocated = total_allocated;
		}
	}

	return ptr;
}

void
algol_free(void *ptr)
{
	if (ptr != NULL) {
		free(ptr);
		/* Note: we don't track size on free, so we can't update total_allocated */
	}
}

size_t
algol_memory_allocated(void)
{
	return total_allocated;
}

size_t
algol_memory_peak(void)
{
	return peak_allocated;
}
