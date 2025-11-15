/*
 * Copyright (c) 2025 PCC Project
 * Extended Mathematics Library - Core Implementation
 */

#include "xmath.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

/*
 * Format Information Table
 */
static const xmath_format_info_t format_info_table[] = {
	[XMATH_IEEE754_BINARY16] = {
		.name = "IEEE 754 binary16",
		.total_bits = 16,
		.sign_bits = 1,
		.exponent_bits = 5,
		.mantissa_bits = 10,
		.exponent_bias = 15,
		.has_implicit_bit = 1
	},
	[XMATH_IEEE754_BINARY32] = {
		.name = "IEEE 754 binary32",
		.total_bits = 32,
		.sign_bits = 1,
		.exponent_bits = 8,
		.mantissa_bits = 23,
		.exponent_bias = 127,
		.has_implicit_bit = 1
	},
	[XMATH_IEEE754_BINARY64] = {
		.name = "IEEE 754 binary64",
		.total_bits = 64,
		.sign_bits = 1,
		.exponent_bits = 11,
		.mantissa_bits = 52,
		.exponent_bias = 1023,
		.has_implicit_bit = 1
	},
	[XMATH_BFLOAT16] = {
		.name = "bfloat16",
		.total_bits = 16,
		.sign_bits = 1,
		.exponent_bits = 8,
		.mantissa_bits = 7,
		.exponent_bias = 127,
		.has_implicit_bit = 1
	},
	[XMATH_FP8_E4M3] = {
		.name = "FP8 E4M3",
		.total_bits = 8,
		.sign_bits = 1,
		.exponent_bits = 4,
		.mantissa_bits = 3,
		.exponent_bias = 7,
		.has_implicit_bit = 1
	},
	[XMATH_FP8_E5M2] = {
		.name = "FP8 E5M2",
		.total_bits = 8,
		.sign_bits = 1,
		.exponent_bits = 5,
		.mantissa_bits = 2,
		.exponent_bias = 15,
		.has_implicit_bit = 1
	}
};

const xmath_format_info_t *xmath_get_format_info(xmath_format_t format) {
	if (format < sizeof(format_info_table) / sizeof(format_info_table[0])) {
		return &format_info_table[format];
	}
	return NULL;
}

/*
 * IEEE 754 Binary16 (Half Precision)
 */
xmath_binary16_t xmath_binary16_from_float(float value) {
	uint32_t bits;
	memcpy(&bits, &value, sizeof(float));

	uint32_t sign = (bits >> 31) & 0x1;
	uint32_t exp = (bits >> 23) & 0xFF;
	uint32_t mant = bits & 0x7FFFFF;

	uint16_t result;

	if (exp == 0xFF) {
		/* Infinity or NaN */
		result = (sign << 15) | 0x7C00 | (mant ? 1 : 0);
	} else if (exp == 0) {
		/* Zero or subnormal */
		result = (sign << 15);
	} else {
		/* Normalized number */
		int exp16 = (int)exp - 127 + 15;

		if (exp16 <= 0) {
			/* Underflow to zero */
			result = (sign << 15);
		} else if (exp16 >= 31) {
			/* Overflow to infinity */
			result = (sign << 15) | 0x7C00;
		} else {
			/* Normal conversion */
			uint16_t mant16 = (mant >> 13) & 0x3FF;
			result = (sign << 15) | (exp16 << 10) | mant16;
		}
	}

	return result;
}

float xmath_binary16_to_float(xmath_binary16_t value) {
	uint32_t sign = (value >> 15) & 0x1;
	uint32_t exp = (value >> 10) & 0x1F;
	uint32_t mant = value & 0x3FF;

	uint32_t bits;

	if (exp == 0x1F) {
		/* Infinity or NaN */
		bits = (sign << 31) | 0x7F800000 | (mant << 13);
	} else if (exp == 0) {
		/* Zero or subnormal */
		if (mant == 0) {
			bits = (sign << 31);
		} else {
			/* Subnormal - convert to normalized float */
			int shift = 0;
			while ((mant & 0x400) == 0) {
				mant <<= 1;
				shift++;
			}
			mant &= 0x3FF;
			int exp32 = 127 - 15 + 1 - shift;
			bits = (sign << 31) | (exp32 << 23) | (mant << 13);
		}
	} else {
		/* Normalized number */
		int exp32 = (int)exp - 15 + 127;
		bits = (sign << 31) | (exp32 << 23) | (mant << 13);
	}

	float result;
	memcpy(&result, &bits, sizeof(float));
	return result;
}

/*
 * bfloat16 (Google Brain Float)
 */
xmath_bfloat16_t xmath_bfloat16_from_float(float value) {
	uint32_t bits;
	memcpy(&bits, &value, sizeof(float));

	/* bfloat16 is just the upper 16 bits of float32 */
	/* Add rounding */
	uint32_t rounding = 0x7FFF + ((bits >> 16) & 0x1);
	bits += rounding;

	return (xmath_bfloat16_t)(bits >> 16);
}

float xmath_bfloat16_to_float(xmath_bfloat16_t value) {
	/* Extend to 32 bits by adding zeros */
	uint32_t bits = ((uint32_t)value) << 16;

	float result;
	memcpy(&result, &bits, sizeof(float));
	return result;
}

/*
 * FP8 E4M3 Format
 */
xmath_fp8_t xmath_fp8_e4m3_from_float(float value) {
	uint32_t bits;
	memcpy(&bits, &value, sizeof(float));

	uint32_t sign = (bits >> 31) & 0x1;
	int32_t exp = ((bits >> 23) & 0xFF) - 127;
	uint32_t mant = bits & 0x7FFFFF;

	/* E4M3: 1 sign, 4 exponent (bias 7), 3 mantissa */
	int exp8 = exp + 7;

	if (exp8 <= 0) {
		/* Underflow */
		return (sign << 7);
	} else if (exp8 >= 15) {
		/* Overflow to max value (no infinity in E4M3) */
		return (sign << 7) | 0x7F;
	} else {
		uint8_t mant8 = (mant >> 20) & 0x7;
		return (sign << 7) | (exp8 << 3) | mant8;
	}
}

float xmath_fp8_e4m3_to_float(xmath_fp8_t value) {
	uint32_t sign = (value >> 7) & 0x1;
	uint32_t exp = (value >> 3) & 0xF;
	uint32_t mant = value & 0x7;

	if (exp == 0) {
		/* Zero or subnormal */
		if (mant == 0) {
			uint32_t bits = (sign << 31);
			float result;
			memcpy(&result, &bits, sizeof(float));
			return result;
		}
		/* Subnormal conversion */
		exp = 1;
	}

	int exp32 = (int)exp - 7 + 127;
	uint32_t mant32 = mant << 20;
	uint32_t bits = (sign << 31) | (exp32 << 23) | mant32;

	float result;
	memcpy(&result, &bits, sizeof(float));
	return result;
}

/*
 * FP8 E5M2 Format
 */
xmath_fp8_t xmath_fp8_e5m2_from_float(float value) {
	uint32_t bits;
	memcpy(&bits, &value, sizeof(float));

	uint32_t sign = (bits >> 31) & 0x1;
	int32_t exp = ((bits >> 23) & 0xFF) - 127;
	uint32_t mant = bits & 0x7FFFFF;

	/* E5M2: 1 sign, 5 exponent (bias 15), 2 mantissa */
	int exp8 = exp + 15;

	if (exp8 <= 0) {
		return (sign << 7);
	} else if (exp8 >= 31) {
		/* Infinity */
		return (sign << 7) | 0x7C;
	} else {
		uint8_t mant8 = (mant >> 21) & 0x3;
		return (sign << 7) | (exp8 << 2) | mant8;
	}
}

float xmath_fp8_e5m2_to_float(xmath_fp8_t value) {
	uint32_t sign = (value >> 7) & 0x1;
	uint32_t exp = (value >> 2) & 0x1F;
	uint32_t mant = value & 0x3;

	if (exp == 0x1F) {
		/* Infinity or NaN */
		uint32_t bits = (sign << 31) | 0x7F800000 | (mant << 21);
		float result;
		memcpy(&result, &bits, sizeof(float));
		return result;
	} else if (exp == 0) {
		/* Zero */
		uint32_t bits = (sign << 31);
		float result;
		memcpy(&result, &bits, sizeof(float));
		return result;
	} else {
		int exp32 = (int)exp - 15 + 127;
		uint32_t mant32 = mant << 21;
		uint32_t bits = (sign << 31) | (exp32 << 23) | mant32;

		float result;
		memcpy(&result, &bits, sizeof(float));
		return result;
	}
}

/*
 * Universal Float Conversion
 */
xmath_float_t xmath_from_double(double value, xmath_format_t format) {
	xmath_float_t result;
	result.format = format;
	memset(&result.data, 0, sizeof(result.data));

	float fval = (float)value;

	switch (format) {
	case XMATH_IEEE754_BINARY16:
		result.data.u16 = xmath_binary16_from_float(fval);
		break;
	case XMATH_IEEE754_BINARY32:
		memcpy(&result.data.u32, &fval, sizeof(float));
		break;
	case XMATH_IEEE754_BINARY64:
		memcpy(&result.data.u64, &value, sizeof(double));
		break;
	case XMATH_BFLOAT16:
		result.data.u16 = xmath_bfloat16_from_float(fval);
		break;
	case XMATH_FP8_E4M3:
		result.data.bytes[0] = xmath_fp8_e4m3_from_float(fval);
		break;
	case XMATH_FP8_E5M2:
		result.data.bytes[0] = xmath_fp8_e5m2_from_float(fval);
		break;
	default:
		/* Not yet implemented */
		break;
	}

	return result;
}

double xmath_to_double(const xmath_float_t *value) {
	if (!value) return 0.0;

	float fval;
	double dval;

	switch (value->format) {
	case XMATH_IEEE754_BINARY16:
		fval = xmath_binary16_to_float(value->data.u16);
		return (double)fval;
	case XMATH_IEEE754_BINARY32:
		memcpy(&fval, &value->data.u32, sizeof(float));
		return (double)fval;
	case XMATH_IEEE754_BINARY64:
		memcpy(&dval, &value->data.u64, sizeof(double));
		return dval;
	case XMATH_BFLOAT16:
		fval = xmath_bfloat16_to_float(value->data.u16);
		return (double)fval;
	case XMATH_FP8_E4M3:
		fval = xmath_fp8_e4m3_to_float(value->data.bytes[0]);
		return (double)fval;
	case XMATH_FP8_E5M2:
		fval = xmath_fp8_e5m2_to_float(value->data.bytes[0]);
		return (double)fval;
	default:
		return 0.0;
	}
}

xmath_float_t xmath_convert(const xmath_float_t *value, xmath_format_t target_format) {
	if (!value) {
		xmath_float_t result = { 0 };
		return result;
	}

	double dval = xmath_to_double(value);
	return xmath_from_double(dval, target_format);
}

/*
 * Special Value Checks
 */
int xmath_is_nan(const xmath_float_t *value) {
	if (!value) return 0;
	double d = xmath_to_double(value);
	return isnan(d);
}

int xmath_is_inf(const xmath_float_t *value) {
	if (!value) return 0;
	double d = xmath_to_double(value);
	return isinf(d);
}

int xmath_is_zero(const xmath_float_t *value) {
	if (!value) return 1;
	double d = xmath_to_double(value);
	return d == 0.0;
}

/*
 * Arithmetic Operations
 */
xmath_float_t xmath_add(const xmath_float_t *a, const xmath_float_t *b) {
	if (!a || !b) {
		xmath_float_t result = { 0 };
		return result;
	}

	double da = xmath_to_double(a);
	double db = xmath_to_double(b);
	return xmath_from_double(da + db, a->format);
}

xmath_float_t xmath_sub(const xmath_float_t *a, const xmath_float_t *b) {
	if (!a || !b) {
		xmath_float_t result = { 0 };
		return result;
	}

	double da = xmath_to_double(a);
	double db = xmath_to_double(b);
	return xmath_from_double(da - db, a->format);
}

xmath_float_t xmath_mul(const xmath_float_t *a, const xmath_float_t *b) {
	if (!a || !b) {
		xmath_float_t result = { 0 };
		return result;
	}

	double da = xmath_to_double(a);
	double db = xmath_to_double(b);
	return xmath_from_double(da * db, a->format);
}

xmath_float_t xmath_div(const xmath_float_t *a, const xmath_float_t *b) {
	if (!a || !b) {
		xmath_float_t result = { 0 };
		return result;
	}

	double da = xmath_to_double(a);
	double db = xmath_to_double(b);
	return xmath_from_double(da / db, a->format);
}

xmath_float_t xmath_sqrt(const xmath_float_t *value) {
	if (!value) {
		xmath_float_t result = { 0 };
		return result;
	}

	double d = xmath_to_double(value);
	return xmath_from_double(sqrt(d), value->format);
}

/*
 * Comparison
 */
int xmath_compare(const xmath_float_t *a, const xmath_float_t *b) {
	if (!a || !b) return 0;

	double da = xmath_to_double(a);
	double db = xmath_to_double(b);

	if (da < db) return -1;
	if (da > db) return 1;
	return 0;
}

int xmath_equal(const xmath_float_t *a, const xmath_float_t *b) {
	return xmath_compare(a, b) == 0;
}

/*
 * String Conversion
 */
char *xmath_to_string(const xmath_float_t *value, int precision) {
	if (!value) return NULL;

	double d = xmath_to_double(value);

	char *result = (char *)malloc(64);
	if (!result) return NULL;

	snprintf(result, 64, "%.*g", precision, d);
	return result;
}

/*
 * Endianness
 */
int xmath_is_big_endian(void) {
	uint16_t test = 0x0102;
	uint8_t *bytes = (uint8_t *)&test;
	return bytes[0] == 0x01;
}

void xmath_swap_endian(xmath_float_t *value) {
	if (!value) return;

	switch (value->format) {
	case XMATH_IEEE754_BINARY16:
	case XMATH_BFLOAT16:
	case XMATH_FP16: {
		uint16_t v = value->data.u16;
		value->data.u16 = ((v & 0xFF) << 8) | ((v >> 8) & 0xFF);
		break;
	}
	case XMATH_IEEE754_BINARY32: {
		uint32_t v = value->data.u32;
		value->data.u32 = ((v & 0xFF) << 24) | ((v & 0xFF00) << 8) |
		                  ((v >> 8) & 0xFF00) | ((v >> 24) & 0xFF);
		break;
	}
	case XMATH_IEEE754_BINARY64: {
		uint64_t v = value->data.u64;
		value->data.u64 = ((v & 0xFF) << 56) | ((v & 0xFF00) << 40) |
		                  ((v & 0xFF0000) << 24) | ((v & 0xFF000000) << 8) |
		                  ((v >> 8) & 0xFF000000) | ((v >> 24) & 0xFF0000) |
		                  ((v >> 40) & 0xFF00) | ((v >> 56) & 0xFF);
		break;
	}
	default:
		break;
	}
}
