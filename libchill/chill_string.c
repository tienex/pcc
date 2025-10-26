/*
 * Copyright (c) 2025 PCC CHILL Runtime Library
 *
 * CHILL string manipulation functions
 */

#include "chill.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/*
 * String creation and management
 */

chill_chars_t *
chill_chars_create(const char *str)
{
	chill_chars_t *result;
	size_t len;

	if (str == NULL) {
		str = "";
	}

	len = strlen(str);
	result = (chill_chars_t *)malloc(sizeof(chill_chars_t));
	if (result == NULL) {
		chill_raise(CHILL_EXC_OUTOFMEM, "Out of memory creating string");
		return NULL;
	}

	result->data = (char *)malloc(len + 1);
	if (result->data == NULL) {
		free(result);
		chill_raise(CHILL_EXC_OUTOFMEM, "Out of memory allocating string data");
		return NULL;
	}

	strcpy(result->data, str);
	result->length = len;
	result->capacity = len + 1;

	return result;
}

chill_chars_t *
chill_chars_alloc(size_t length)
{
	chill_chars_t *result;

	result = (chill_chars_t *)malloc(sizeof(chill_chars_t));
	if (result == NULL) {
		chill_raise(CHILL_EXC_OUTOFMEM, "Out of memory allocating string");
		return NULL;
	}

	result->data = (char *)malloc(length + 1);
	if (result->data == NULL) {
		free(result);
		chill_raise(CHILL_EXC_OUTOFMEM, "Out of memory allocating string data");
		return NULL;
	}

	result->data[0] = '\0';
	result->length = 0;
	result->capacity = length + 1;

	return result;
}

void
chill_chars_free(chill_chars_t *str)
{
	if (str != NULL) {
		if (str->data != NULL) {
			free(str->data);
		}
		free(str);
	}
}

size_t
chill_chars_length(const chill_chars_t *str)
{
	if (str == NULL) {
		return 0;
	}
	return str->length;
}

/*
 * String operations
 */

chill_chars_t *
chill_chars_concat(const chill_chars_t *s1, const chill_chars_t *s2)
{
	chill_chars_t *result;
	size_t len1, len2, total_len;

	len1 = (s1 != NULL) ? s1->length : 0;
	len2 = (s2 != NULL) ? s2->length : 0;
	total_len = len1 + len2;

	result = chill_chars_alloc(total_len);
	if (result == NULL) {
		return NULL;
	}

	if (s1 != NULL && s1->data != NULL && len1 > 0) {
		memcpy(result->data, s1->data, len1);
	}

	if (s2 != NULL && s2->data != NULL && len2 > 0) {
		memcpy(result->data + len1, s2->data, len2);
	}

	result->data[total_len] = '\0';
	result->length = total_len;

	return result;
}

chill_chars_t *
chill_chars_substr(const chill_chars_t *str, size_t start, size_t len)
{
	chill_chars_t *result;

	if (str == NULL || str->data == NULL) {
		return chill_chars_create("");
	}

	/* Check bounds */
	if (start >= str->length) {
		chill_raise(CHILL_EXC_RANGEFAIL, "Substring start out of range");
		return chill_chars_create("");
	}

	/* Adjust length if it exceeds string bounds */
	if (start + len > str->length) {
		len = str->length - start;
	}

	result = chill_chars_alloc(len);
	if (result == NULL) {
		return NULL;
	}

	memcpy(result->data, str->data + start, len);
	result->data[len] = '\0';
	result->length = len;

	return result;
}

chill_chars_t *
chill_chars_upper(const chill_chars_t *str)
{
	chill_chars_t *result;
	size_t i;

	if (str == NULL) {
		return chill_chars_create("");
	}

	result = chill_chars_alloc(str->length);
	if (result == NULL) {
		return NULL;
	}

	for (i = 0; i < str->length; i++) {
		result->data[i] = toupper((unsigned char)str->data[i]);
	}
	result->data[str->length] = '\0';
	result->length = str->length;

	return result;
}

chill_chars_t *
chill_chars_lower(const chill_chars_t *str)
{
	chill_chars_t *result;
	size_t i;

	if (str == NULL) {
		return chill_chars_create("");
	}

	result = chill_chars_alloc(str->length);
	if (result == NULL) {
		return NULL;
	}

	for (i = 0; i < str->length; i++) {
		result->data[i] = tolower((unsigned char)str->data[i]);
	}
	result->data[str->length] = '\0';
	result->length = str->length;

	return result;
}

int
chill_chars_compare(const chill_chars_t *s1, const chill_chars_t *s2)
{
	size_t len1, len2, min_len;
	int cmp;

	len1 = (s1 != NULL) ? s1->length : 0;
	len2 = (s2 != NULL) ? s2->length : 0;
	min_len = (len1 < len2) ? len1 : len2;

	if (len1 == 0 && len2 == 0) {
		return 0;
	}

	if (len1 == 0) {
		return -1;
	}

	if (len2 == 0) {
		return 1;
	}

	cmp = memcmp(s1->data, s2->data, min_len);
	if (cmp != 0) {
		return cmp;
	}

	/* If prefixes are equal, longer string is greater */
	if (len1 < len2) {
		return -1;
	} else if (len1 > len2) {
		return 1;
	}

	return 0;
}
