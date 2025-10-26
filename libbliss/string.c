/*
 * Copyright (c) 2025 PCC BLISS Runtime Library
 *
 * String Operations Implementation
 */

#include "blissrt.h"
#include <stdlib.h>
#include <string.h>

/*
 * Create a BLISS string from C string
 */
bliss_string_t bliss_string_from_cstr(const char *str)
{
	bliss_string_t result;

	if (str == NULL) {
		result.data = NULL;
		result.length = 0;
		return result;
	}

	result.length = strlen(str);
	result.data = (char *)malloc(result.length + 1);
	if (result.data) {
		memcpy(result.data, str, result.length);
		result.data[result.length] = '\0';
	} else {
		result.length = 0;
	}

	return result;
}

/*
 * Create a C string from BLISS string
 */
char *bliss_string_to_cstr(bliss_string_t str)
{
	char *result;

	if (str.data == NULL || str.length == 0) {
		result = (char *)malloc(1);
		if (result) {
			result[0] = '\0';
		}
		return result;
	}

	result = (char *)malloc(str.length + 1);
	if (result) {
		memcpy(result, str.data, str.length);
		result[str.length] = '\0';
	}

	return result;
}

/*
 * Compare two BLISS strings
 */
int bliss_string_compare(bliss_string_t s1, bliss_string_t s2)
{
	size_t min_len = s1.length < s2.length ? s1.length : s2.length;
	int cmp;

	if (s1.data == NULL && s2.data == NULL)
		return 0;
	if (s1.data == NULL)
		return -1;
	if (s2.data == NULL)
		return 1;

	cmp = memcmp(s1.data, s2.data, min_len);
	if (cmp != 0)
		return cmp;

	/* If equal up to min_len, longer string is greater */
	if (s1.length < s2.length)
		return -1;
	if (s1.length > s2.length)
		return 1;

	return 0;
}

/*
 * Concatenate two BLISS strings
 */
bliss_string_t bliss_string_concat(bliss_string_t s1, bliss_string_t s2)
{
	bliss_string_t result;

	result.length = s1.length + s2.length;
	result.data = (char *)malloc(result.length + 1);

	if (result.data == NULL) {
		result.length = 0;
		return result;
	}

	if (s1.data && s1.length > 0)
		memcpy(result.data, s1.data, s1.length);

	if (s2.data && s2.length > 0)
		memcpy(result.data + s1.length, s2.data, s2.length);

	result.data[result.length] = '\0';

	return result;
}

/*
 * Get substring
 */
bliss_string_t bliss_string_substr(bliss_string_t str, size_t start, size_t length)
{
	bliss_string_t result;

	/* Bounds checking */
	if (str.data == NULL || start >= str.length) {
		result.data = NULL;
		result.length = 0;
		return result;
	}

	/* Adjust length if it extends past end of string */
	if (start + length > str.length)
		length = str.length - start;

	result.length = length;
	result.data = (char *)malloc(length + 1);

	if (result.data == NULL) {
		result.length = 0;
		return result;
	}

	memcpy(result.data, str.data + start, length);
	result.data[length] = '\0';

	return result;
}

/*
 * Free a BLISS string
 */
void bliss_string_free(bliss_string_t str)
{
	if (str.data)
		free(str.data);
}
