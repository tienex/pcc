/*
 * COBOL Runtime Library - String Operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "cobolrt.h"

void
__cobol_move(cobol_field_t *dst, cobol_field_t *src)
{
	size_t copy_len;

	if (!dst || !dst->data || !src || !src->data)
		return;

	/* Handle numeric to numeric */
	if (dst->type == COB_TYPE_NUMERIC && src->type == COB_TYPE_NUMERIC) {
		double val = __cobol_get_double(src);
		__cobol_set_double(dst, val);
		return;
	}

	/* Handle alphanumeric move */
	copy_len = src->size < dst->size ? src->size : dst->size;

	if (dst->type == COB_TYPE_NUMERIC) {
		/* Right justify */
		if (src->size < dst->size)
			memset(dst->data, '0', dst->size - src->size);
		memcpy((char *)dst->data + (dst->size - copy_len), src->data, copy_len);
	} else {
		/* Left justify */
		memcpy(dst->data, src->data, copy_len);
		if (copy_len < dst->size)
			memset((char *)dst->data + copy_len, ' ', dst->size - copy_len);
	}
}

void
__cobol_move_all(cobol_field_t *dst, cobol_field_t *src)
{
	size_t i, src_len;
	char *dst_data = (char *)dst->data;
	char *src_data = (char *)src->data;

	if (!dst || !dst->data || !src || !src->data || src->size == 0)
		return;

	/* Repeat source pattern to fill destination */
	src_len = src->size;
	for (i = 0; i < dst->size; i++) {
		dst_data[i] = src_data[i % src_len];
	}
}

void
__cobol_string(cobol_field_t *dst, cobol_field_t *sources[], int count)
{
	size_t pos = 0;
	int i;
	char *dst_data = (char *)dst->data;

	if (!dst || !dst->data)
		return;

	/* Clear destination */
	memset(dst_data, ' ', dst->size);

	/* Concatenate sources */
	for (i = 0; i < count && pos < dst->size; i++) {
		cobol_field_t *src = sources[i];
		size_t copy_len;
		size_t src_len = src->size;

		/* Trim trailing spaces from source */
		while (src_len > 0 && ((char *)src->data)[src_len-1] == ' ')
			src_len--;

		copy_len = src_len;
		if (pos + copy_len > dst->size)
			copy_len = dst->size - pos;

		memcpy(dst_data + pos, src->data, copy_len);
		pos += copy_len;
	}
}

void
__cobol_unstring(cobol_field_t *src, cobol_field_t *dests[], int count, const char *delim)
{
	char *src_data = (char *)src->data;
	size_t src_len = src->size;
	size_t pos = 0;
	int dest_idx = 0;
	size_t delim_len = delim ? strlen(delim) : 0;

	if (!src || !src->data)
		return;

	/* Trim trailing spaces */
	while (src_len > 0 && src_data[src_len-1] == ' ')
		src_len--;

	/* Split by delimiter */
	while (pos < src_len && dest_idx < count) {
		cobol_field_t *dst = dests[dest_idx];
		size_t start = pos;
		size_t len;

		/* Find delimiter or end */
		if (delim_len > 0) {
			while (pos < src_len) {
				if (memcmp(src_data + pos, delim, delim_len) == 0)
					break;
				pos++;
			}
		} else {
			/* No delimiter - split by spaces */
			while (pos < src_len && src_data[pos] != ' ')
				pos++;
		}

		len = pos - start;

		/* Copy to destination */
		if (len > dst->size)
			len = dst->size;
		memcpy(dst->data, src_data + start, len);
		if (len < dst->size)
			memset((char *)dst->data + len, ' ', dst->size - len);

		/* Skip delimiter */
		if (delim_len > 0 && pos < src_len)
			pos += delim_len;
		else
			pos++;

		dest_idx++;
	}
}

void
__cobol_inspect_replacing(cobol_field_t *field, const char *pattern, const char *replacement)
{
	char *data = (char *)field->data;
	size_t pat_len = strlen(pattern);
	size_t rep_len = strlen(replacement);
	size_t i;

	if (!field || !field->data || pat_len == 0)
		return;

	/* Simple replacement - same length only */
	if (pat_len != rep_len)
		return;

	for (i = 0; i <= field->size - pat_len; i++) {
		if (memcmp(data + i, pattern, pat_len) == 0) {
			memcpy(data + i, replacement, rep_len);
			i += rep_len - 1;
		}
	}
}

int
__cobol_compare(cobol_field_t *f1, cobol_field_t *f2)
{
	size_t len1, len2, min_len;
	int result;

	if (!f1 || !f1->data || !f2 || !f2->data)
		return 0;

	/* Numeric comparison */
	if (f1->type == COB_TYPE_NUMERIC && f2->type == COB_TYPE_NUMERIC) {
		double v1 = __cobol_get_double(f1);
		double v2 = __cobol_get_double(f2);
		if (v1 < v2) return -1;
		if (v1 > v2) return 1;
		return 0;
	}

	/* String comparison */
	len1 = f1->size;
	len2 = f2->size;
	min_len = len1 < len2 ? len1 : len2;

	result = memcmp(f1->data, f2->data, min_len);
	if (result != 0)
		return result;

	/* If equal up to min_len, compare lengths */
	if (len1 < len2) return -1;
	if (len1 > len2) return 1;
	return 0;
}
