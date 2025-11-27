/*
 * COBOL Runtime Library - Packed Decimal Operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "cobolrt.h"

void
__cobol_pack(cobol_decimal_t *dec, const char *str)
{
	int i, len, digit_idx = 0;
	int sign = 0;

	if (!dec || !str)
		return;

	memset(dec, 0, sizeof(cobol_decimal_t));
	len = strlen(str);

	/* Parse sign */
	if (str[0] == '-') {
		sign = 1;
		str++;
		len--;
	} else if (str[0] == '+') {
		str++;
		len--;
	}

	/* Pack digits */
	for (i = 0; i < len && digit_idx < 31; i++) {
		if (isdigit(str[i])) {
			dec->digits[digit_idx++] = str[i] - '0';
		} else if (str[i] == '.') {
			dec->scale = len - i - 1;
		}
	}

	dec->length = digit_idx;
	dec->sign = sign;
}

void
__cobol_unpack(const cobol_decimal_t *dec, char *str)
{
	int i;

	if (!dec || !str)
		return;

	/* Add sign */
	if (dec->sign)
		*str++ = '-';

	/* Add digits */
	for (i = 0; i < dec->length; i++) {
		if (i == dec->length - dec->scale && dec->scale > 0)
			*str++ = '.';
		*str++ = dec->digits[i] + '0';
	}

	*str = '\0';
}

void
__cobol_decimal_add(cobol_decimal_t *result, const cobol_decimal_t *op1, const cobol_decimal_t *op2)
{
	/* Simplified implementation - convert to double, compute, pack back */
	char buf1[64], buf2[64], buf_result[64];
	double val1, val2, val_result;

	__cobol_unpack(op1, buf1);
	__cobol_unpack(op2, buf2);

	val1 = atof(buf1);
	val2 = atof(buf2);
	val_result = val1 + val2;

	snprintf(buf_result, sizeof(buf_result), "%.10f", val_result);
	__cobol_pack(result, buf_result);
}

void
__cobol_decimal_subtract(cobol_decimal_t *result, const cobol_decimal_t *op1, const cobol_decimal_t *op2)
{
	char buf1[64], buf2[64], buf_result[64];
	double val1, val2, val_result;

	__cobol_unpack(op1, buf1);
	__cobol_unpack(op2, buf2);

	val1 = atof(buf1);
	val2 = atof(buf2);
	val_result = val1 - val2;

	snprintf(buf_result, sizeof(buf_result), "%.10f", val_result);
	__cobol_pack(result, buf_result);
}

void
__cobol_decimal_multiply(cobol_decimal_t *result, const cobol_decimal_t *op1, const cobol_decimal_t *op2)
{
	char buf1[64], buf2[64], buf_result[64];
	double val1, val2, val_result;

	__cobol_unpack(op1, buf1);
	__cobol_unpack(op2, buf2);

	val1 = atof(buf1);
	val2 = atof(buf2);
	val_result = val1 * val2;

	snprintf(buf_result, sizeof(buf_result), "%.10f", val_result);
	__cobol_pack(result, buf_result);
}

void
__cobol_decimal_divide(cobol_decimal_t *result, const cobol_decimal_t *op1, const cobol_decimal_t *op2)
{
	char buf1[64], buf2[64], buf_result[64];
	double val1, val2, val_result;

	__cobol_unpack(op1, buf1);
	__cobol_unpack(op2, buf2);

	val1 = atof(buf1);
	val2 = atof(buf2);

	if (val2 == 0.0) {
		__cobol_set_exception(1, "Division by zero");
		memset(result, 0, sizeof(cobol_decimal_t));
		return;
	}

	val_result = val1 / val2;

	snprintf(buf_result, sizeof(buf_result), "%.10f", val_result);
	__cobol_pack(result, buf_result);
}
