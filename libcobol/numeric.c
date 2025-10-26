/*
 * COBOL Runtime Library - Numeric Operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "cobolrt.h"

void
__cobol_add(cobol_field_t *result, cobol_field_t *op1, cobol_field_t *op2)
{
	double val1 = __cobol_get_double(op1);
	double val2 = __cobol_get_double(op2);
	__cobol_set_double(result, val1 + val2);
}

void
__cobol_subtract(cobol_field_t *result, cobol_field_t *op1, cobol_field_t *op2)
{
	double val1 = __cobol_get_double(op1);
	double val2 = __cobol_get_double(op2);
	__cobol_set_double(result, val1 - val2);
}

void
__cobol_multiply(cobol_field_t *result, cobol_field_t *op1, cobol_field_t *op2)
{
	double val1 = __cobol_get_double(op1);
	double val2 = __cobol_get_double(op2);
	__cobol_set_double(result, val1 * val2);
}

void
__cobol_divide(cobol_field_t *result, cobol_field_t *op1, cobol_field_t *op2)
{
	double val1 = __cobol_get_double(op1);
	double val2 = __cobol_get_double(op2);

	if (val2 == 0.0) {
		__cobol_set_exception(1, "Division by zero");
		__cobol_set_double(result, 0.0);
		return;
	}

	__cobol_set_double(result, val1 / val2);
}

void
__cobol_compute(cobol_field_t *result, double value)
{
	__cobol_set_double(result, value);
}

int
__cobol_get_int(cobol_field_t *field)
{
	char buffer[32];
	char *data = (char *)field->data;
	int i, val = 0;
	int sign = 1;

	if (!field || !field->data)
		return 0;

	/* Handle numeric field */
	if (field->type == COB_TYPE_NUMERIC) {
		/* Extract digits */
		for (i = 0; i < field->size && i < sizeof(buffer)-1; i++) {
			if (isdigit(data[i]))
				buffer[i] = data[i];
			else if (data[i] == '-')
				sign = -1;
			else if (data[i] == ' ')
				buffer[i] = '0';
			else
				buffer[i] = '0';
		}
		buffer[i] = '\0';
		val = atoi(buffer);
	} else {
		/* Alphanumeric - parse as string */
		memcpy(buffer, data, field->size < sizeof(buffer) ? field->size : sizeof(buffer)-1);
		buffer[field->size < sizeof(buffer) ? field->size : sizeof(buffer)-1] = '\0';
		val = atoi(buffer);
	}

	return val * sign;
}

long long
__cobol_get_long(cobol_field_t *field)
{
	char buffer[64];
	char *data = (char *)field->data;
	long long val = 0;
	int sign = 1;
	size_t i;

	if (!field || !field->data)
		return 0;

	/* Extract number */
	for (i = 0; i < field->size && i < sizeof(buffer)-1; i++) {
		if (isdigit(data[i]))
			buffer[i] = data[i];
		else if (data[i] == '-')
			sign = -1;
		else if (data[i] == ' ')
			buffer[i] = '0';
		else
			buffer[i] = '0';
	}
	buffer[i] = '\0';

	val = atoll(buffer);
	return val * sign;
}

double
__cobol_get_double(cobol_field_t *field)
{
	char buffer[128];
	char *data = (char *)field->data;
	double val = 0.0;
	size_t i, j = 0;
	int has_decimal = 0;

	if (!field || !field->data)
		return 0.0;

	/* Extract number with decimal point */
	for (i = 0; i < field->size && j < sizeof(buffer)-2; i++) {
		if (isdigit(data[i])) {
			buffer[j++] = data[i];
		} else if (data[i] == '.' || data[i] == ',') {
			if (!has_decimal) {
				buffer[j++] = '.';
				has_decimal = 1;
			}
		} else if (data[i] == '-' && j == 0) {
			buffer[j++] = '-';
		}
	}

	/* Add implicit decimal point based on scale */
	if (!has_decimal && field->scale > 0 && j > field->scale) {
		memmove(buffer + j - field->scale + 1, buffer + j - field->scale, field->scale);
		buffer[j - field->scale] = '.';
		j++;
	}

	buffer[j] = '\0';
	val = atof(buffer);

	return val;
}

void
__cobol_set_int(cobol_field_t *field, int value)
{
	char buffer[32];
	int len;

	if (!field || !field->data)
		return;

	snprintf(buffer, sizeof(buffer), "%d", value);
	len = strlen(buffer);

	/* Right-justify in field */
	if (len < field->size) {
		memset(field->data, '0', field->size - len);
		memcpy((char *)field->data + field->size - len, buffer, len);
	} else {
		memcpy(field->data, buffer + len - field->size, field->size);
	}
}

void
__cobol_set_long(cobol_field_t *field, long long value)
{
	char buffer[64];
	int len;

	if (!field || !field->data)
		return;

	snprintf(buffer, sizeof(buffer), "%lld", value);
	len = strlen(buffer);

	/* Right-justify in field */
	if (len < field->size) {
		memset(field->data, '0', field->size - len);
		memcpy((char *)field->data + field->size - len, buffer, len);
	} else {
		memcpy(field->data, buffer + len - field->size, field->size);
	}
}

void
__cobol_set_double(cobol_field_t *field, double value)
{
	char buffer[128];
	char *data = (char *)field->data;
	int len, integer_part, decimal_part;
	int i, j;

	if (!field || !field->data)
		return;

	/* Format based on scale */
	if (field->scale > 0) {
		snprintf(buffer, sizeof(buffer), "%.*f", field->scale, value);
		/* Remove decimal point */
		len = strlen(buffer);
		for (i = 0, j = 0; i < len && j < field->size; i++) {
			if (buffer[i] != '.')
				buffer[j++] = buffer[i];
		}
		len = j;
	} else {
		snprintf(buffer, sizeof(buffer), "%.0f", value);
		len = strlen(buffer);
	}

	/* Right-justify */
	if (len < field->size) {
		memset(data, '0', field->size - len);
		memcpy(data + field->size - len, buffer, len);
	} else {
		memcpy(data, buffer + len - field->size, field->size);
	}
}
