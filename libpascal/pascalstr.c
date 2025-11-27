/*
 * Copyright (c) 2025 PCC Pascal Runtime Library
 *
 * Pascal String Operations
 *
 * Implements short and long string operations for Pascal programs.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "pascalrt.h"

/*
 * Short String Operations (Turbo Pascal/Delphi style)
 */

void
pascal_str_assign(PascalString *dest, const char *src)
{
	size_t len = strlen(src);
	if (len > 255) {
		len = 255;
	}
	dest->length = (uint8_t)len;
	memcpy(dest->data, src, len);
	dest->data[len] = '\0';
}

void
pascal_str_concat(PascalString *dest, const PascalString *s1, const PascalString *s2)
{
	size_t total_len = s1->length + s2->length;
	if (total_len > 255) {
		total_len = 255;
	}

	dest->length = (uint8_t)total_len;

	if (s1->length <= 255) {
		memcpy(dest->data, s1->data, s1->length);
	}

	size_t remaining = total_len - s1->length;
	if (remaining > 0) {
		memcpy(dest->data + s1->length, s2->data, remaining);
	}

	dest->data[total_len] = '\0';
}

void
pascal_str_copy(PascalString *dest, const PascalString *src, int index, int count)
{
	/* Pascal uses 1-based indexing */
	if (index < 1 || index > src->length) {
		dest->length = 0;
		dest->data[0] = '\0';
		return;
	}

	/* Convert to 0-based */
	int start = index - 1;
	int available = src->length - start;

	if (count > available) {
		count = available;
	}
	if (count < 0) {
		count = 0;
	}
	if (count > 255) {
		count = 255;
	}

	dest->length = (uint8_t)count;
	memcpy(dest->data, src->data + start, count);
	dest->data[count] = '\0';
}

int
pascal_str_pos(const PascalString *substr, const PascalString *str)
{
	if (substr->length == 0 || substr->length > str->length) {
		return 0;
	}

	for (int i = 0; i <= str->length - substr->length; i++) {
		if (memcmp(str->data + i, substr->data, substr->length) == 0) {
			return i + 1;  /* 1-based index */
		}
	}

	return 0;  /* Not found */
}

int
pascal_str_length(const PascalString *str)
{
	return str->length;
}

void
pascal_str_upcase(PascalString *str)
{
	for (int i = 0; i < str->length; i++) {
		str->data[i] = toupper((unsigned char)str->data[i]);
	}
}

void
pascal_str_lowercase(PascalString *str)
{
	for (int i = 0; i < str->length; i++) {
		str->data[i] = tolower((unsigned char)str->data[i]);
	}
}

void
pascal_str_trim(PascalString *str)
{
	int start = 0;
	int end = str->length - 1;

	/* Trim leading spaces */
	while (start < str->length && isspace((unsigned char)str->data[start])) {
		start++;
	}

	/* Trim trailing spaces */
	while (end >= start && isspace((unsigned char)str->data[end])) {
		end--;
	}

	int new_length = end - start + 1;
	if (new_length < 0) {
		new_length = 0;
	}

	if (start > 0 && new_length > 0) {
		memmove(str->data, str->data + start, new_length);
	}

	str->length = (uint8_t)new_length;
	str->data[new_length] = '\0';
}

void
pascal_str_insert(PascalString *str, const PascalString *substr, int index)
{
	/* Pascal uses 1-based indexing */
	if (index < 1 || index > str->length + 1) {
		return;
	}

	/* Convert to 0-based */
	int pos = index - 1;

	int new_length = str->length + substr->length;
	if (new_length > 255) {
		new_length = 255;
	}

	/* Calculate how much of substr we can actually insert */
	int insert_len = new_length - str->length;
	if (insert_len <= 0) {
		return;
	}

	/* Move existing characters to make room */
	int chars_to_move = str->length - pos;
	if (chars_to_move > 0) {
		int dest_pos = pos + insert_len;
		if (dest_pos < 255) {
			int move_count = chars_to_move;
			if (dest_pos + move_count > 255) {
				move_count = 255 - dest_pos;
			}
			memmove(str->data + dest_pos, str->data + pos, move_count);
		}
	}

	/* Insert the substring */
	memcpy(str->data + pos, substr->data, insert_len);
	str->length = (uint8_t)new_length;
	str->data[new_length] = '\0';
}

void
pascal_str_delete(PascalString *str, int index, int count)
{
	/* Pascal uses 1-based indexing */
	if (index < 1 || index > str->length || count <= 0) {
		return;
	}

	/* Convert to 0-based */
	int pos = index - 1;

	/* Limit count to available characters */
	int available = str->length - pos;
	if (count > available) {
		count = available;
	}

	/* Move remaining characters */
	int remaining = str->length - pos - count;
	if (remaining > 0) {
		memmove(str->data + pos, str->data + pos + count, remaining);
	}

	str->length -= count;
	str->data[str->length] = '\0';
}

int
pascal_str_compare(const PascalString *s1, const PascalString *s2)
{
	int min_len = s1->length < s2->length ? s1->length : s2->length;
	int cmp = memcmp(s1->data, s2->data, min_len);

	if (cmp != 0) {
		return cmp;
	}

	/* If common prefix is equal, longer string is greater */
	if (s1->length < s2->length) {
		return -1;
	} else if (s1->length > s2->length) {
		return 1;
	}

	return 0;
}

int
pascal_str_compare_nocase(const PascalString *s1, const PascalString *s2)
{
	int min_len = s1->length < s2->length ? s1->length : s2->length;

	for (int i = 0; i < min_len; i++) {
		int c1 = tolower((unsigned char)s1->data[i]);
		int c2 = tolower((unsigned char)s2->data[i]);
		if (c1 != c2) {
			return c1 - c2;
		}
	}

	/* If common prefix is equal, longer string is greater */
	if (s1->length < s2->length) {
		return -1;
	} else if (s1->length > s2->length) {
		return 1;
	}

	return 0;
}

/*
 * Long String Operations (Delphi style)
 */

PascalLongString *
pascal_lstr_create(const char *src)
{
	PascalLongString *str = malloc(sizeof(PascalLongString));
	if (str == NULL) {
		pascal_runtime_error(PASCAL_ERR_HEAP_OVERFLOW, "Out of memory");
		return NULL;
	}

	size_t len = strlen(src);
	str->length = len;
	str->capacity = len + 1;
	str->data = malloc(str->capacity);

	if (str->data == NULL) {
		free(str);
		pascal_runtime_error(PASCAL_ERR_HEAP_OVERFLOW, "Out of memory");
		return NULL;
	}

	strcpy(str->data, src);
	return str;
}

void
pascal_lstr_free(PascalLongString *str)
{
	if (str != NULL) {
		if (str->data != NULL) {
			free(str->data);
		}
		free(str);
	}
}

void
pascal_lstr_assign(PascalLongString *dest, const char *src)
{
	size_t len = strlen(src);

	if (len + 1 > dest->capacity) {
		char *new_data = realloc(dest->data, len + 1);
		if (new_data == NULL) {
			pascal_runtime_error(PASCAL_ERR_HEAP_OVERFLOW, "Out of memory");
			return;
		}
		dest->data = new_data;
		dest->capacity = len + 1;
	}

	dest->length = len;
	strcpy(dest->data, src);
}

void
pascal_lstr_concat(PascalLongString *dest, const PascalLongString *s1, const PascalLongString *s2)
{
	size_t new_len = s1->length + s2->length;

	if (new_len + 1 > dest->capacity) {
		char *new_data = realloc(dest->data, new_len + 1);
		if (new_data == NULL) {
			pascal_runtime_error(PASCAL_ERR_HEAP_OVERFLOW, "Out of memory");
			return;
		}
		dest->data = new_data;
		dest->capacity = new_len + 1;
	}

	memcpy(dest->data, s1->data, s1->length);
	memcpy(dest->data + s1->length, s2->data, s2->length);
	dest->data[new_len] = '\0';
	dest->length = new_len;
}

void
pascal_lstr_setlength(PascalLongString *str, size_t newlen)
{
	if (newlen + 1 > str->capacity) {
		char *new_data = realloc(str->data, newlen + 1);
		if (new_data == NULL) {
			pascal_runtime_error(PASCAL_ERR_HEAP_OVERFLOW, "Out of memory");
			return;
		}
		str->data = new_data;
		str->capacity = newlen + 1;
	}

	if (newlen > str->length) {
		/* Fill new space with zeros */
		memset(str->data + str->length, 0, newlen - str->length);
	}

	str->length = newlen;
	str->data[newlen] = '\0';
}
