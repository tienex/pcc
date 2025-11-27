/*
 * Copyright (c) 2025 PCC Paradox PAL Runtime Library
 *
 * Array and memory management functions
 */

#include <stdlib.h>
#include <string.h>
#include "../include/palrt.h"

PAL_Array *pal_array_new(size_t elem_size, int32_t length)
{
	PAL_Array *arr;

	if (length < 0)
		return NULL;

	arr = (PAL_Array *)malloc(sizeof(PAL_Array));
	if (!arr)
		return NULL;

	arr->elem_size = elem_size;
	arr->length = length;
	arr->capacity = length;
	arr->data = calloc(length, elem_size);

	if (!arr->data && length > 0) {
		free(arr);
		return NULL;
	}

	return arr;
}

void pal_array_free(PAL_Array *arr)
{
	if (arr) {
		if (arr->data)
			free(arr->data);
		free(arr);
	}
}

int32_t pal_arraysize(PAL_Array *arr)
{
	if (!arr)
		return 0;
	return arr->length;
}

void *pal_arrayget(PAL_Array *arr, int32_t index)
{
	if (!arr || index < 0 || index >= arr->length)
		return NULL;

	return (char *)arr->data + (index * arr->elem_size);
}

void pal_arrayset(PAL_Array *arr, int32_t index, const void *value)
{
	void *dest;

	if (!arr || !value || index < 0 || index >= arr->length)
		return;

	dest = (char *)arr->data + (index * arr->elem_size);
	memcpy(dest, value, arr->elem_size);
}

int pal_arrayinsert(PAL_Array *arr, int32_t index, const void *value)
{
	void *new_data;
	int32_t new_capacity;

	if (!arr || !value || index < 0 || index > arr->length)
		return -1;

	/* Check if we need to resize */
	if (arr->length >= arr->capacity) {
		new_capacity = arr->capacity * 2;
		if (new_capacity < 8)
			new_capacity = 8;

		new_data = realloc(arr->data, new_capacity * arr->elem_size);
		if (!new_data)
			return -1;

		arr->data = new_data;
		arr->capacity = new_capacity;
	}

	/* Shift elements after index */
	if (index < arr->length) {
		memmove((char *)arr->data + ((index + 1) * arr->elem_size),
		        (char *)arr->data + (index * arr->elem_size),
		        (arr->length - index) * arr->elem_size);
	}

	/* Insert new element */
	memcpy((char *)arr->data + (index * arr->elem_size),
	       value, arr->elem_size);

	arr->length++;
	return 0;
}

int pal_arraydelete(PAL_Array *arr, int32_t index)
{
	if (!arr || index < 0 || index >= arr->length)
		return -1;

	/* Shift elements after index */
	if (index < arr->length - 1) {
		memmove((char *)arr->data + (index * arr->elem_size),
		        (char *)arr->data + ((index + 1) * arr->elem_size),
		        (arr->length - index - 1) * arr->elem_size);
	}

	arr->length--;
	return 0;
}

int pal_arrayresize(PAL_Array *arr, int32_t new_size)
{
	void *new_data;

	if (!arr || new_size < 0)
		return -1;

	if (new_size > arr->capacity) {
		new_data = realloc(arr->data, new_size * arr->elem_size);
		if (!new_data)
			return -1;

		/* Zero-initialize new elements */
		memset((char *)new_data + (arr->length * arr->elem_size),
		       0, (new_size - arr->length) * arr->elem_size);

		arr->data = new_data;
		arr->capacity = new_size;
	}

	arr->length = new_size;
	return 0;
}

void *pal_malloc(size_t size)
{
	return malloc(size);
}

void *pal_realloc(void *ptr, size_t size)
{
	return realloc(ptr, size);
}

void pal_free(void *ptr)
{
	free(ptr);
}
