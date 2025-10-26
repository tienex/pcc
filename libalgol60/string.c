/*
 * Copyright (c) 2025 PCC ALGOL 60+ Runtime Library
 *
 * String operations for ALGOL 60+ programs
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "algol60.h"

/*
 * String creation and destruction
 */

algol_string
algol_string_create(const char *str)
{
	algol_string s;

	if (str == NULL) {
		return NULL;
	}

	s = (algol_string)algol_malloc(strlen(str) + 1);
	if (s == NULL) {
		algol_fatal_error(ALGOL_ERR_MEMORY, "string_create: out of memory");
		return NULL;
	}

	strcpy(s, str);
	return s;
}

algol_string
algol_string_create_n(const char *str, size_t len)
{
	algol_string s;

	if (str == NULL) {
		return NULL;
	}

	s = (algol_string)algol_malloc(len + 1);
	if (s == NULL) {
		algol_fatal_error(ALGOL_ERR_MEMORY, "string_create_n: out of memory");
		return NULL;
	}

	strncpy(s, str, len);
	s[len] = '\0';
	return s;
}

void
algol_string_free(algol_string str)
{
	if (str != NULL) {
		algol_free(str);
	}
}

/*
 * String operations
 */

size_t
algol_string_length(algol_string str)
{
	if (str == NULL) {
		return 0;
	}
	return strlen(str);
}

algol_string
algol_string_concat(algol_string s1, algol_string s2)
{
	algol_string result;
	size_t len1, len2;

	if (s1 == NULL && s2 == NULL) {
		return algol_string_create("");
	}
	if (s1 == NULL) {
		return algol_string_create(s2);
	}
	if (s2 == NULL) {
		return algol_string_create(s1);
	}

	len1 = strlen(s1);
	len2 = strlen(s2);

	result = (algol_string)algol_malloc(len1 + len2 + 1);
	if (result == NULL) {
		algol_fatal_error(ALGOL_ERR_MEMORY, "string_concat: out of memory");
		return NULL;
	}

	strcpy(result, s1);
	strcat(result, s2);

	return result;
}

algol_string
algol_string_substring(algol_string str, int start, int length)
{
	size_t str_len;
	algol_string result;

	if (str == NULL) {
		return algol_string_create("");
	}

	str_len = strlen(str);

	/* Bounds checking */
	if (start < 0 || start >= (int)str_len) {
		algol_error(ALGOL_ERR_BOUNDS, "substring: start index out of bounds");
		return algol_string_create("");
	}

	if (length < 0) {
		algol_error(ALGOL_ERR_BOUNDS, "substring: negative length");
		return algol_string_create("");
	}

	/* Adjust length if it extends beyond string */
	if (start + length > (int)str_len) {
		length = str_len - start;
	}

	result = algol_string_create_n(str + start, length);
	return result;
}

int
algol_string_compare(algol_string s1, algol_string s2)
{
	if (s1 == NULL && s2 == NULL) {
		return 0;
	}
	if (s1 == NULL) {
		return -1;
	}
	if (s2 == NULL) {
		return 1;
	}

	return strcmp(s1, s2);
}
