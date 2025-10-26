/*
 * Copyright (c) 2025 PCC BASIC Compiler
 *
 * BASIC Runtime Library - String Functions
 */

#include "basicrt.h"
#include <ctype.h>

/*
 * String management
 */

basic_string_t *
basic_string_new(const char *str)
{
	basic_string_t *s = malloc(sizeof(basic_string_t));
	if (str != NULL) {
		s->length = strlen(str);
		s->allocated = s->length + 1;
		s->data = strdup(str);
	} else {
		s->length = 0;
		s->allocated = 1;
		s->data = malloc(1);
		s->data[0] = '\0';
	}
	return s;
}

void
basic_string_free(basic_string_t *str)
{
	if (str != NULL) {
		free(str->data);
		free(str);
	}
}

/*
 * LEFT$(string, n) - Returns leftmost n characters
 */
basic_string_t *
basic_left(basic_string_t *str, int n)
{
	basic_string_t *result;
	int len;

	if (str == NULL || n <= 0)
		return basic_string_new("");

	len = (n < str->length) ? n : str->length;
	result = malloc(sizeof(basic_string_t));
	result->length = len;
	result->allocated = len + 1;
	result->data = malloc(result->allocated);
	strncpy(result->data, str->data, len);
	result->data[len] = '\0';

	return result;
}

/*
 * RIGHT$(string, n) - Returns rightmost n characters
 */
basic_string_t *
basic_right(basic_string_t *str, int n)
{
	basic_string_t *result;
	int len, start;

	if (str == NULL || n <= 0)
		return basic_string_new("");

	len = (n < str->length) ? n : str->length;
	start = str->length - len;
	result = malloc(sizeof(basic_string_t));
	result->length = len;
	result->allocated = len + 1;
	result->data = strdup(str->data + start);

	return result;
}

/*
 * MID$(string, start, length) - Returns substring
 */
basic_string_t *
basic_mid(basic_string_t *str, int start, int length)
{
	basic_string_t *result;
	int len;

	if (str == NULL || start < 1 || start > str->length)
		return basic_string_new("");

	/* BASIC uses 1-based indexing */
	start--;

	len = (start + length > str->length) ? str->length - start : length;
	if (len <= 0)
		return basic_string_new("");

	result = malloc(sizeof(basic_string_t));
	result->length = len;
	result->allocated = len + 1;
	result->data = malloc(result->allocated);
	strncpy(result->data, str->data + start, len);
	result->data[len] = '\0';

	return result;
}

/*
 * LEN(string) - Returns length of string
 */
int
basic_len(basic_string_t *str)
{
	return (str != NULL) ? str->length : 0;
}

/*
 * String concatenation
 */
basic_string_t *
basic_concat(basic_string_t *s1, basic_string_t *s2)
{
	basic_string_t *result;
	int len1 = (s1 != NULL) ? s1->length : 0;
	int len2 = (s2 != NULL) ? s2->length : 0;

	result = malloc(sizeof(basic_string_t));
	result->length = len1 + len2;
	result->allocated = result->length + 1;
	result->data = malloc(result->allocated);

	if (s1 != NULL)
		strcpy(result->data, s1->data);
	else
		result->data[0] = '\0';

	if (s2 != NULL)
		strcat(result->data, s2->data);

	return result;
}

/*
 * INSTR(haystack, needle) - Find substring position (1-based)
 */
int
basic_instr(basic_string_t *haystack, basic_string_t *needle)
{
	char *pos;

	if (haystack == NULL || needle == NULL)
		return 0;

	pos = strstr(haystack->data, needle->data);
	if (pos == NULL)
		return 0;

	return (int)(pos - haystack->data) + 1;  /* 1-based index */
}

/*
 * CHR$(code) - Convert ASCII code to character
 */
basic_string_t *
basic_chr(int code)
{
	basic_string_t *result = malloc(sizeof(basic_string_t));
	result->length = 1;
	result->allocated = 2;
	result->data = malloc(2);
	result->data[0] = (char)code;
	result->data[1] = '\0';
	return result;
}

/*
 * ASC(string) - Get ASCII code of first character
 */
int
basic_asc(basic_string_t *str)
{
	if (str == NULL || str->length == 0)
		return 0;
	return (unsigned char)str->data[0];
}

/*
 * STR$(value) - Convert number to string
 */
basic_string_t *
basic_str(double value)
{
	char buffer[64];
	snprintf(buffer, sizeof(buffer), " %g", value);  /* BASIC adds leading space */
	return basic_string_new(buffer);
}

/*
 * VAL(string) - Convert string to number
 */
double
basic_val(basic_string_t *str)
{
	if (str == NULL)
		return 0.0;
	return atof(str->data);
}

/*
 * UCASE$(string) - Convert to uppercase
 */
basic_string_t *
basic_ucase(basic_string_t *str)
{
	basic_string_t *result;
	int i;

	if (str == NULL)
		return basic_string_new("");

	result = malloc(sizeof(basic_string_t));
	result->length = str->length;
	result->allocated = str->allocated;
	result->data = strdup(str->data);

	for (i = 0; i < result->length; i++)
		result->data[i] = toupper((unsigned char)result->data[i]);

	return result;
}

/*
 * LCASE$(string) - Convert to lowercase
 */
basic_string_t *
basic_lcase(basic_string_t *str)
{
	basic_string_t *result;
	int i;

	if (str == NULL)
		return basic_string_new("");

	result = malloc(sizeof(basic_string_t));
	result->length = str->length;
	result->allocated = str->allocated;
	result->data = strdup(str->data);

	for (i = 0; i < result->length; i++)
		result->data[i] = tolower((unsigned char)result->data[i]);

	return result;
}

/*
 * LTRIM$(string) - Remove leading spaces
 */
basic_string_t *
basic_ltrim(basic_string_t *str)
{
	int i;

	if (str == NULL)
		return basic_string_new("");

	for (i = 0; i < str->length && isspace((unsigned char)str->data[i]); i++)
		;

	return basic_string_new(str->data + i);
}

/*
 * RTRIM$(string) - Remove trailing spaces
 */
basic_string_t *
basic_rtrim(basic_string_t *str)
{
	basic_string_t *result;
	int i;

	if (str == NULL)
		return basic_string_new("");

	result = malloc(sizeof(basic_string_t));
	result->data = strdup(str->data);
	result->length = str->length;
	result->allocated = str->allocated;

	for (i = result->length - 1; i >= 0 && isspace((unsigned char)result->data[i]); i--)
		result->data[i] = '\0';

	result->length = i + 1;
	return result;
}

/*
 * SPACE$(n) - Create string of n spaces
 */
basic_string_t *
basic_space(int n)
{
	basic_string_t *result;
	int i;

	if (n <= 0)
		return basic_string_new("");

	result = malloc(sizeof(basic_string_t));
	result->length = n;
	result->allocated = n + 1;
	result->data = malloc(result->allocated);

	for (i = 0; i < n; i++)
		result->data[i] = ' ';
	result->data[n] = '\0';

	return result;
}

/*
 * STRING$(n, ch) - Create string of n characters
 */
basic_string_t *
basic_string_repeat(int n, const char *ch)
{
	basic_string_t *result;
	int i;
	char c;

	if (n <= 0 || ch == NULL)
		return basic_string_new("");

	c = ch[0];
	result = malloc(sizeof(basic_string_t));
	result->length = n;
	result->allocated = n + 1;
	result->data = malloc(result->allocated);

	for (i = 0; i < n; i++)
		result->data[i] = c;
	result->data[n] = '\0';

	return result;
}
