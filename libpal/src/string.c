/*
 * Copyright (c) 2025 PCC Paradox PAL Runtime Library
 *
 * String manipulation functions
 */

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include "../include/palrt.h"

PAL_String *pal_string_new(const char *str)
{
	PAL_String *s;
	size_t len;

	if (!str)
		return NULL;

	s = (PAL_String *)malloc(sizeof(PAL_String));
	if (!s)
		return NULL;

	len = strlen(str);
	s->length = (uint16_t)len;
	s->capacity = (uint16_t)(len + 1);
	s->data = (char *)malloc(s->capacity);
	if (!s->data) {
		free(s);
		return NULL;
	}

	strcpy(s->data, str);
	return s;
}

PAL_String *pal_string_alloc(size_t capacity)
{
	PAL_String *s;

	s = (PAL_String *)malloc(sizeof(PAL_String));
	if (!s)
		return NULL;

	s->length = 0;
	s->capacity = (uint16_t)capacity;
	s->data = (char *)malloc(capacity);
	if (!s->data) {
		free(s);
		return NULL;
	}

	s->data[0] = '\0';
	return s;
}

void pal_string_free(PAL_String *str)
{
	if (str) {
		if (str->data)
			free(str->data);
		free(str);
	}
}

const char *pal_string_cstr(PAL_String *str)
{
	if (!str || !str->data)
		return "";
	return str->data;
}

int32_t pal_strlen(PAL_String *str)
{
	if (!str)
		return 0;
	return (int32_t)str->length;
}

PAL_String *pal_upper(PAL_String *str)
{
	PAL_String *result;
	size_t i;

	if (!str)
		return NULL;

	result = pal_string_alloc(str->capacity);
	if (!result)
		return NULL;

	for (i = 0; i < str->length; i++) {
		result->data[i] = toupper((unsigned char)str->data[i]);
	}
	result->data[str->length] = '\0';
	result->length = str->length;

	return result;
}

PAL_String *pal_lower(PAL_String *str)
{
	PAL_String *result;
	size_t i;

	if (!str)
		return NULL;

	result = pal_string_alloc(str->capacity);
	if (!result)
		return NULL;

	for (i = 0; i < str->length; i++) {
		result->data[i] = tolower((unsigned char)str->data[i]);
	}
	result->data[str->length] = '\0';
	result->length = str->length;

	return result;
}

PAL_String *pal_substr(PAL_String *str, int32_t start, int32_t length)
{
	PAL_String *result;
	int32_t actual_start, actual_length;

	if (!str || start < 1 || length < 0)
		return pal_string_new("");

	/* Convert 1-based to 0-based */
	actual_start = start - 1;

	/* Bounds checking */
	if (actual_start >= (int32_t)str->length)
		return pal_string_new("");

	actual_length = length;
	if (actual_start + actual_length > (int32_t)str->length)
		actual_length = str->length - actual_start;

	result = pal_string_alloc(actual_length + 1);
	if (!result)
		return NULL;

	memcpy(result->data, str->data + actual_start, actual_length);
	result->data[actual_length] = '\0';
	result->length = actual_length;

	return result;
}

PAL_String *pal_trim(PAL_String *str)
{
	PAL_String *result;
	int start, end;

	if (!str || str->length == 0)
		return pal_string_new("");

	/* Find first non-whitespace */
	start = 0;
	while (start < (int)str->length && isspace((unsigned char)str->data[start]))
		start++;

	/* Find last non-whitespace */
	end = str->length - 1;
	while (end >= start && isspace((unsigned char)str->data[end]))
		end--;

	if (start > end)
		return pal_string_new("");

	result = pal_string_alloc(end - start + 2);
	if (!result)
		return NULL;

	memcpy(result->data, str->data + start, end - start + 1);
	result->data[end - start + 1] = '\0';
	result->length = end - start + 1;

	return result;
}

PAL_String *pal_ltrim(PAL_String *str)
{
	PAL_String *result;
	int start;

	if (!str || str->length == 0)
		return pal_string_new("");

	/* Find first non-whitespace */
	start = 0;
	while (start < (int)str->length && isspace((unsigned char)str->data[start]))
		start++;

	result = pal_string_alloc(str->length - start + 1);
	if (!result)
		return NULL;

	memcpy(result->data, str->data + start, str->length - start);
	result->data[str->length - start] = '\0';
	result->length = str->length - start;

	return result;
}

PAL_String *pal_rtrim(PAL_String *str)
{
	PAL_String *result;
	int end;

	if (!str || str->length == 0)
		return pal_string_new("");

	/* Find last non-whitespace */
	end = str->length - 1;
	while (end >= 0 && isspace((unsigned char)str->data[end]))
		end--;

	if (end < 0)
		return pal_string_new("");

	result = pal_string_alloc(end + 2);
	if (!result)
		return NULL;

	memcpy(result->data, str->data, end + 1);
	result->data[end + 1] = '\0';
	result->length = end + 1;

	return result;
}

int32_t pal_strpos(PAL_String *haystack, PAL_String *needle)
{
	const char *pos;

	if (!haystack || !needle || needle->length == 0)
		return 0;

	pos = strstr(haystack->data, needle->data);
	if (!pos)
		return 0;

	/* Return 1-based position */
	return (int32_t)(pos - haystack->data) + 1;
}

PAL_String *pal_concat(PAL_String *s1, PAL_String *s2)
{
	PAL_String *result;
	size_t total_len;

	if (!s1 && !s2)
		return pal_string_new("");
	if (!s1)
		return pal_string_new(s2->data);
	if (!s2)
		return pal_string_new(s1->data);

	total_len = s1->length + s2->length;
	result = pal_string_alloc(total_len + 1);
	if (!result)
		return NULL;

	memcpy(result->data, s1->data, s1->length);
	memcpy(result->data + s1->length, s2->data, s2->length);
	result->data[total_len] = '\0';
	result->length = total_len;

	return result;
}

char pal_chr(int32_t code)
{
	if (code < 0 || code > 255)
		return '\0';
	return (char)code;
}

int32_t pal_asc(char c)
{
	return (int32_t)(unsigned char)c;
}

PAL_String *pal_format(PAL_Number value, const char *fmt)
{
	char buffer[256];
	PAL_String *result;

	if (!fmt || fmt[0] == '\0') {
		snprintf(buffer, sizeof(buffer), "%.2f", value);
	} else {
		/* Simple format support */
		snprintf(buffer, sizeof(buffer), fmt, value);
	}

	result = pal_string_new(buffer);
	return result;
}

PAL_String *pal_fill(char c, int32_t count)
{
	PAL_String *result;
	int32_t i;

	if (count < 0)
		count = 0;

	result = pal_string_alloc(count + 1);
	if (!result)
		return NULL;

	for (i = 0; i < count; i++) {
		result->data[i] = c;
	}
	result->data[count] = '\0';
	result->length = count;

	return result;
}

int32_t pal_strcmp(PAL_String *s1, PAL_String *s2)
{
	if (!s1 && !s2)
		return 0;
	if (!s1)
		return -1;
	if (!s2)
		return 1;

	/* Case-insensitive comparison (PAL default) */
	return strcasecmp(s1->data, s2->data);
}
