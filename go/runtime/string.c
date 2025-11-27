/*
 * Copyright (c) 2025 PCC Go Runtime Library
 *
 * String operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

/*
 * Create Go string from C string
 */
go_string
go_string_new(const char *str)
{
	go_string s;

	if (str == NULL) {
		s.data = "";
		s.len = 0;
	} else {
		s.len = strlen(str);
		if (s.len > 0) {
			char *buf = (char *)go_malloc(s.len);
			memcpy(buf, str, s.len);
			s.data = buf;
		} else {
			s.data = "";
		}
	}

	return s;
}

/*
 * Create Go string from byte array
 */
go_string
go_string_from_bytes(const char *data, go_int len)
{
	go_string s;

	if (len <= 0 || data == NULL) {
		s.data = "";
		s.len = 0;
	} else {
		char *buf = (char *)go_malloc(len);
		memcpy(buf, data, len);
		s.data = buf;
		s.len = len;
	}

	return s;
}

/*
 * Concatenate two strings
 */
go_string
go_string_concat(go_string s1, go_string s2)
{
	go_string result;
	char *buf;

	if (s1.len == 0)
		return s2;
	if (s2.len == 0)
		return s1;

	result.len = s1.len + s2.len;
	buf = (char *)go_malloc(result.len);

	memcpy(buf, s1.data, s1.len);
	memcpy(buf + s1.len, s2.data, s2.len);

	result.data = buf;
	return result;
}

/*
 * Compare two strings
 */
go_int
go_string_compare(go_string s1, go_string s2)
{
	go_int min_len = s1.len < s2.len ? s1.len : s2.len;
	int cmp;

	if (min_len > 0) {
		cmp = memcmp(s1.data, s2.data, min_len);
		if (cmp != 0)
			return cmp;
	}

	return s1.len - s2.len;
}

/*
 * Check if two strings are equal
 */
go_bool
go_string_equal(go_string s1, go_string s2)
{
	if (s1.len != s2.len)
		return false;

	if (s1.len == 0)
		return true;

	return memcmp(s1.data, s2.data, s1.len) == 0;
}

/*
 * Slice a string
 */
go_string
go_string_slice(go_string s, go_int low, go_int high)
{
	go_string result;

	/* Bounds checking */
	if (low < 0) low = 0;
	if (high > s.len) high = s.len;
	if (low > high) low = high;

	result.data = s.data + low;
	result.len = high - low;

	return result;
}

/*
 * Convert string to NULL-terminated C string
 */
const char *
go_string_cstr(go_string s)
{
	char *buf;

	if (s.len == 0)
		return "";

	buf = (char *)go_malloc(s.len + 1);
	memcpy(buf, s.data, s.len);
	buf[s.len] = '\0';

	return buf;
}

/*
 * Convert integer to string
 */
go_string
go_int_to_string(go_int64 val)
{
	char buf[32];
	int len;

	len = snprintf(buf, sizeof(buf), "%lld", (long long)val);
	return go_string_from_bytes(buf, len);
}

/*
 * Convert unsigned integer to string
 */
go_string
go_uint_to_string(go_uint64 val)
{
	char buf[32];
	int len;

	len = snprintf(buf, sizeof(buf), "%llu", (unsigned long long)val);
	return go_string_from_bytes(buf, len);
}

/*
 * Convert float to string
 */
go_string
go_float_to_string(go_float64 val)
{
	char buf[64];
	int len;

	len = snprintf(buf, sizeof(buf), "%g", val);
	return go_string_from_bytes(buf, len);
}

/*
 * Convert bool to string
 */
go_string
go_bool_to_string(go_bool val)
{
	if (val)
		return (go_string){ "true", 4 };
	else
		return (go_string){ "false", 5 };
}
