/*
 * Copyright (c) 2025 PCC Xbase++ Runtime Library
 *
 * String manipulation functions
 */

#include "xbrt.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

/*
 * LEN() - Return length of string
 */
int
xb_len(const char *str)
{
	return str ? strlen(str) : 0;
}

/*
 * SUBSTR() - Extract substring
 */
char *
xb_substr(const char *str, int start, int len)
{
	if (!str || start < 1)
		return strdup("");

	int slen = strlen(str);
	start--; /* Convert to 0-based */

	if (start >= slen)
		return strdup("");

	if (len < 0 || start + len > slen)
		len = slen - start;

	char *result = (char *)malloc(len + 1);
	if (result) {
		memcpy(result, str + start, len);
		result[len] = '\0';
	}

	return result ? result : strdup("");
}

/*
 * LEFT() - Extract left substring
 */
char *
xb_left(const char *str, int len)
{
	if (!str || len <= 0)
		return strdup("");

	int slen = strlen(str);
	if (len > slen)
		len = slen;

	char *result = (char *)malloc(len + 1);
	if (result) {
		memcpy(result, str, len);
		result[len] = '\0';
	}

	return result ? result : strdup("");
}

/*
 * RIGHT() - Extract right substring
 */
char *
xb_right(const char *str, int len)
{
	if (!str || len <= 0)
		return strdup("");

	int slen = strlen(str);
	if (len > slen)
		len = slen;

	return strdup(str + slen - len);
}

/*
 * UPPER() - Convert to uppercase
 */
char *
xb_upper(const char *str)
{
	if (!str)
		return strdup("");

	int len = strlen(str);
	char *result = (char *)malloc(len + 1);
	if (!result)
		return strdup("");

	int i;
	for (i = 0; i < len; i++)
		result[i] = toupper((unsigned char)str[i]);
	result[len] = '\0';

	return result;
}

/*
 * LOWER() - Convert to lowercase
 */
char *
xb_lower(const char *str)
{
	if (!str)
		return strdup("");

	int len = strlen(str);
	char *result = (char *)malloc(len + 1);
	if (!result)
		return strdup("");

	int i;
	for (i = 0; i < len; i++)
		result[i] = tolower((unsigned char)str[i]);
	result[len] = '\0';

	return result;
}

/*
 * TRIM() - Remove trailing spaces
 */
char *
xb_trim(const char *str)
{
	return xb_rtrim(str);
}

/*
 * LTRIM() - Remove leading spaces
 */
char *
xb_ltrim(const char *str)
{
	if (!str)
		return strdup("");

	while (*str && isspace((unsigned char)*str))
		str++;

	return strdup(str);
}

/*
 * RTRIM() - Remove trailing spaces
 */
char *
xb_rtrim(const char *str)
{
	if (!str)
		return strdup("");

	int len = strlen(str);
	while (len > 0 && isspace((unsigned char)str[len - 1]))
		len--;

	char *result = (char *)malloc(len + 1);
	if (result) {
		memcpy(result, str, len);
		result[len] = '\0';
	}

	return result ? result : strdup("");
}

/*
 * ALLTRIM() - Remove leading and trailing spaces
 */
char *
xb_alltrim(const char *str)
{
	char *tmp = xb_ltrim(str);
	char *result = xb_rtrim(tmp);
	free(tmp);
	return result;
}

/*
 * SPACE() - Create string of N spaces
 */
char *
xb_space(int n)
{
	if (n < 0)
		n = 0;

	char *result = (char *)malloc(n + 1);
	if (result) {
		memset(result, ' ', n);
		result[n] = '\0';
	}

	return result ? result : strdup("");
}

/*
 * REPLICATE() - Replicate string N times
 */
char *
xb_replicate(const char *str, int n)
{
	if (!str || n <= 0)
		return strdup("");

	int slen = strlen(str);
	int total_len = slen * n;

	char *result = (char *)malloc(total_len + 1);
	if (!result)
		return strdup("");

	int i;
	for (i = 0; i < n; i++)
		memcpy(result + i * slen, str, slen);
	result[total_len] = '\0';

	return result;
}

/*
 * STUFF() - Insert substring into string
 */
char *
xb_stuff(const char *str, int pos, int del, const char *ins)
{
	if (!str || pos < 1)
		return strdup(str ? str : "");

	if (!ins)
		ins = "";

	int slen = strlen(str);
	int ilen = strlen(ins);

	pos--; /* Convert to 0-based */

	if (pos > slen)
		pos = slen;
	if (del < 0)
		del = 0;
	if (pos + del > slen)
		del = slen - pos;

	int result_len = slen - del + ilen;
	char *result = (char *)malloc(result_len + 1);
	if (!result)
		return strdup("");

	/* Copy before insertion point */
	memcpy(result, str, pos);

	/* Copy insertion */
	memcpy(result + pos, ins, ilen);

	/* Copy after deletion point */
	memcpy(result + pos + ilen, str + pos + del, slen - pos - del);

	result[result_len] = '\0';

	return result;
}

/*
 * AT() - Find substring position
 */
int
xb_at(const char *needle, const char *haystack)
{
	if (!needle || !haystack)
		return 0;

	const char *found = strstr(haystack, needle);
	return found ? (found - haystack + 1) : 0;  /* 1-based */
}

/*
 * RAT() - Find last occurrence of substring
 */
int
xb_rat(const char *needle, const char *haystack)
{
	if (!needle || !haystack)
		return 0;

	int nlen = strlen(needle);
	int hlen = strlen(haystack);
	int i;

	for (i = hlen - nlen; i >= 0; i--) {
		if (memcmp(haystack + i, needle, nlen) == 0)
			return i + 1;  /* 1-based */
	}

	return 0;
}

/*
 * STRTRAN() - Replace substring
 */
char *
xb_strtran(const char *str, const char *find, const char *replace)
{
	if (!str || !find)
		return strdup(str ? str : "");

	if (!replace)
		replace = "";

	int slen = strlen(str);
	int flen = strlen(find);
	int rlen = strlen(replace);

	/* Count occurrences */
	int count = 0;
	const char *p = str;
	while ((p = strstr(p, find)) != NULL) {
		count++;
		p += flen;
	}

	if (count == 0)
		return strdup(str);

	/* Allocate result */
	int result_len = slen + count * (rlen - flen);
	char *result = (char *)malloc(result_len + 1);
	if (!result)
		return strdup("");

	/* Perform replacement */
	char *dst = result;
	p = str;
	while (*p) {
		const char *found = strstr(p, find);
		if (found == p) {
			memcpy(dst, replace, rlen);
			dst += rlen;
			p += flen;
		} else {
			*dst++ = *p++;
		}
	}
	*dst = '\0';

	return result;
}

/*
 * CHR() - Character from ASCII code
 */
char *
xb_chr(int code)
{
	char *result = (char *)malloc(2);
	if (result) {
		result[0] = (char)(code & 0xFF);
		result[1] = '\0';
	}
	return result ? result : strdup("");
}

/*
 * ASC() - ASCII code from character
 */
int
xb_asc(const char *str)
{
	return (str && str[0]) ? (unsigned char)str[0] : 0;
}

/*
 * ISALPHA() - Check if character is alphabetic
 */
int
xb_isalpha(int c)
{
	return isalpha(c);
}

/*
 * ISDIGIT() - Check if character is digit
 */
int
xb_isdigit(int c)
{
	return isdigit(c);
}
