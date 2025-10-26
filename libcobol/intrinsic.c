/*
 * COBOL Runtime Library - Intrinsic Functions
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include "cobolrt.h"

int
__cobol_length(cobol_field_t *field)
{
	if (!field)
		return 0;

	return field->size;
}

void
__cobol_upper_case(cobol_field_t *dst, cobol_field_t *src)
{
	size_t i;
	char *src_data, *dst_data;

	if (!dst || !dst->data || !src || !src->data)
		return;

	src_data = (char *)src->data;
	dst_data = (char *)dst->data;

	for (i = 0; i < src->size && i < dst->size; i++) {
		dst_data[i] = toupper(src_data[i]);
	}

	/* Pad if needed */
	if (dst->size > src->size)
		memset(dst_data + src->size, ' ', dst->size - src->size);
}

void
__cobol_lower_case(cobol_field_t *dst, cobol_field_t *src)
{
	size_t i;
	char *src_data, *dst_data;

	if (!dst || !dst->data || !src || !src->data)
		return;

	src_data = (char *)src->data;
	dst_data = (char *)dst->data;

	for (i = 0; i < src->size && i < dst->size; i++) {
		dst_data[i] = tolower(src_data[i]);
	}

	/* Pad if needed */
	if (dst->size > src->size)
		memset(dst_data + src->size, ' ', dst->size - src->size);
}

void
__cobol_reverse(cobol_field_t *dst, cobol_field_t *src)
{
	size_t i;
	char *src_data, *dst_data;

	if (!dst || !dst->data || !src || !src->data)
		return;

	src_data = (char *)src->data;
	dst_data = (char *)dst->data;

	for (i = 0; i < src->size && i < dst->size; i++) {
		dst_data[i] = src_data[src->size - 1 - i];
	}

	/* Pad if needed */
	if (dst->size > src->size)
		memset(dst_data + src->size, ' ', dst->size - src->size);
}

int
__cobol_numval(cobol_field_t *field)
{
	return __cobol_get_int(field);
}

void
__cobol_current_date(cobol_field_t *field)
{
	time_t now;
	struct tm *tm_info;
	char buffer[32];

	if (!field || !field->data)
		return;

	now = time(NULL);
	tm_info = localtime(&now);

	/* Format: YYYYMMDDHHMMSS */
	snprintf(buffer, sizeof(buffer), "%04d%02d%02d%02d%02d%02d",
	         tm_info->tm_year + 1900,
	         tm_info->tm_mon + 1,
	         tm_info->tm_mday,
	         tm_info->tm_hour,
	         tm_info->tm_min,
	         tm_info->tm_sec);

	/* Copy to field */
	if (strlen(buffer) > field->size)
		buffer[field->size] = '\0';

	memcpy(field->data, buffer, strlen(buffer));

	/* Pad with spaces if needed */
	if (strlen(buffer) < field->size)
		memset((char *)field->data + strlen(buffer), ' ', field->size - strlen(buffer));
}
