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

/*
 * VAX Floating-Point Conversions
 */

float xmath_vax_f_to_float(uint32_t vax_f) {
	if (vax_f == 0) return 0.0f;
	
	/* VAX F: 1 sign, 8 exponent (bias 128), 23 mantissa (no implicit bit) */
	uint32_t sign = (vax_f >> 15) & 0x1;
	uint32_t exp = (vax_f >> 7) & 0xFF;
	uint32_t mant = ((vax_f & 0x7F) << 16) | ((vax_f >> 16) & 0xFFFF);
	
	if (exp == 0) return 0.0f;  /* Reserved */
	
	/* Convert to IEEE 754 */
	int ieee_exp = exp - 128 + 127;  /* VAX bias 128, IEEE bias 127 */
	if (ieee_exp <= 0) return 0.0f;
	if (ieee_exp >= 255) ieee_exp = 254;
	
	uint32_t ieee = (sign << 31) | (ieee_exp << 23) | (mant >> 1);
	
	float result;
	memcpy(&result, &ieee, sizeof(float));
	return result;
}

uint32_t xmath_vax_f_from_float(float value) {
	uint32_t ieee;
	memcpy(&ieee, &value, sizeof(float));
	
	if (value == 0.0f) return 0;
	
	uint32_t sign = (ieee >> 31) & 0x1;
	int exp = ((ieee >> 23) & 0xFF) - 127;
	uint32_t mant = (ieee & 0x7FFFFF) << 1;
	
	int vax_exp = exp + 128;
	if (vax_exp <= 0) return 0;
	if (vax_exp >= 255) vax_exp = 255;
	
	uint32_t vax_f = (sign << 15) | (vax_exp << 7) | ((mant >> 16) & 0x7F);
	vax_f |= (mant & 0xFFFF) << 16;
	
	return vax_f;
}

/*
 * Cray Floating-Point Conversions
 */

double xmath_cray_to_double(uint64_t cray_val) {
	if (cray_val == 0) return 0.0;
	
	/* Cray: 1 sign, 15 exponent (bias 0x4000), 48 mantissa */
	uint64_t sign = (cray_val >> 63) & 0x1;
	int exp = ((cray_val >> 48) & 0x7FFF) - 0x4000;
	uint64_t mant = cray_val & 0xFFFFFFFFFFFFULL;
	
	/* Convert to IEEE 754 double */
	int ieee_exp = exp + 1023;
	if (ieee_exp <= 0) return 0.0;
	if (ieee_exp >= 2047) ieee_exp = 2046;
	
	uint64_t ieee = (sign << 63) | ((uint64_t)ieee_exp << 52) | (mant >> 4);
	
	double result;
	memcpy(&result, &ieee, sizeof(double));
	return result;
}

uint64_t xmath_cray_from_double(double value) {
	uint64_t ieee;
	memcpy(&ieee, &value, sizeof(double));
	
	if (value == 0.0) return 0;
	
	uint64_t sign = (ieee >> 63) & 0x1;
	int exp = ((ieee >> 52) & 0x7FF) - 1023;
	uint64_t mant = (ieee & 0xFFFFFFFFFFFFFULL) << 4;
	
	int cray_exp = exp + 0x4000;
	if (cray_exp < 0) return 0;
	if (cray_exp > 0x7FFF) cray_exp = 0x7FFF;
	
	return (sign << 63) | ((uint64_t)cray_exp << 48) | mant;
}

/*
 * IBM Hexadecimal Floating-Point Conversions
 */

double xmath_ibm_to_double(uint64_t ibm_val) {
	if (ibm_val == 0) return 0.0;
	
	/* IBM: 1 sign, 7 exponent (base 16, bias 64), 56 mantissa */
	uint64_t sign = (ibm_val >> 63) & 0x1;
	int exp = ((ibm_val >> 56) & 0x7F) - 64;  /* Base 16 exponent */
	uint64_t mant = ibm_val & 0xFFFFFFFFFFFFFFULL;
	
	/* Normalize mantissa and convert to base 2 */
	while (mant && !(mant & 0xF00000000000000ULL)) {
		mant <<= 4;
		exp--;
	}
	
	/* Convert to IEEE 754 */
	int ieee_exp = exp * 4 + 1023;  /* Base 16 to base 2, add IEEE bias */
	if (ieee_exp <= 0) return 0.0;
	if (ieee_exp >= 2047) ieee_exp = 2046;
	
	uint64_t ieee = (sign << 63) | ((uint64_t)ieee_exp << 52) | (mant >> 8);
	
	double result;
	memcpy(&result, &ieee, sizeof(double));
	return result;
}

uint64_t xmath_ibm_from_double(double value) {
	uint64_t ieee;
	memcpy(&ieee, &value, sizeof(double));
	
	if (value == 0.0) return 0;
	
	uint64_t sign = (ieee >> 63) & 0x1;
	int exp = ((ieee >> 52) & 0x7FF) - 1023;
	uint64_t mant = (ieee & 0xFFFFFFFFFFFFFULL) << 8;
	
	/* Convert to base 16 exponent */
	int ibm_exp = (exp / 4) + 64;
	int shift = exp % 4;
	if (shift < 0) { shift += 4; ibm_exp--; }
	mant >>= shift;
	
	if (ibm_exp < 0) return 0;
	if (ibm_exp > 127) ibm_exp = 127;
	
	return (sign << 63) | ((uint64_t)ibm_exp << 56) | mant;
}

/*
 * Universal Float Operations
 */

xmath_float_t xmath_float_from_native(double value, xmath_format_t format) {
	xmath_float_t result;
	result.format = format;
	memset(&result.data, 0, sizeof(result.data));
	
	switch (format) {
	case XMATH_IEEE754_BINARY32: {
		float f = (float)value;
		memcpy(&result.data.u32, &f, sizeof(float));
		break;
	}
	case XMATH_IEEE754_BINARY64:
		memcpy(&result.data.u64, &value, sizeof(double));
		break;
	case XMATH_BFLOAT16:
		result.data.u16 = xmath_bfloat16_from_float((float)value);
		break;
	case XMATH_VAX_F_FLOAT:
		result.data.u32 = xmath_vax_f_from_float((float)value);
		break;
	case XMATH_CRAY_FLOAT:
		result.data.u64 = xmath_cray_from_double(value);
		break;
	case XMATH_IBM_FLOAT:
		result.data.u64 = xmath_ibm_from_double(value);
		break;
	default:
		break;
	}
	
	return result;
}

double xmath_float_to_native(const xmath_float_t *value) {
	if (!value) return 0.0;
	
	switch (value->format) {
	case XMATH_IEEE754_BINARY32: {
		float f;
		memcpy(&f, &value->data.u32, sizeof(float));
		return (double)f;
	}
	case XMATH_IEEE754_BINARY64: {
		double d;
		memcpy(&d, &value->data.u64, sizeof(double));
		return d;
	}
	case XMATH_BFLOAT16:
		return (double)xmath_bfloat16_to_float(value->data.u16);
	case XMATH_VAX_F_FLOAT:
		return (double)xmath_vax_f_to_float(value->data.u32);
	case XMATH_CRAY_FLOAT:
		return xmath_cray_to_double(value->data.u64);
	case XMATH_IBM_FLOAT:
		return xmath_ibm_to_double(value->data.u64);
	default:
		return 0.0;
	}
}

xmath_float_t xmath_float_convert(const xmath_float_t *value, xmath_format_t target_format) {
	if (!value) {
		xmath_float_t result = {0};
		return result;
	}
	
	double native = xmath_float_to_native(value);
	return xmath_float_from_native(native, target_format);
}
