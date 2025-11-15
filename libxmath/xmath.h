/*
 * Copyright (c) 2025 PCC Project
 *
 * Extended Mathematics Library
 *
 * Supports multiple floating-point formats:
 * - IEEE 754-2008: binary16, binary32, binary64, binary128
 * - IEEE 754-2008: decimal64, decimal128
 * - bfloat16 (Google Brain Float)
 * - FP16 (half precision), FP8 (quarter precision)
 * - VAX floating-point (F, D, G, H)
 * - Cray floating-point
 * - IBM floating-point (all sizes)
 * - MBF (Microsoft Binary Format)
 */

#ifndef _PCC_XMATH_H_
#define _PCC_XMATH_H_

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Float Format Types
 */
typedef enum {
	XMATH_IEEE754_BINARY16,
	XMATH_IEEE754_BINARY32,
	XMATH_IEEE754_BINARY64,
	XMATH_IEEE754_BINARY128,
	XMATH_IEEE754_DECIMAL64,
	XMATH_IEEE754_DECIMAL128,
	XMATH_BFLOAT16,
	XMATH_FP16,
	XMATH_FP8_E4M3,         /* 1 sign, 4 exp, 3 mantissa */
	XMATH_FP8_E5M2,         /* 1 sign, 5 exp, 2 mantissa */
	XMATH_VAX_F,            /* 32-bit VAX F-float */
	XMATH_VAX_D,            /* 64-bit VAX D-float */
	XMATH_VAX_G,            /* 64-bit VAX G-float */
	XMATH_VAX_H,            /* 128-bit VAX H-float */
	XMATH_CRAY,             /* Cray 64-bit float */
	XMATH_IBM_SHORT,        /* IBM 32-bit float */
	XMATH_IBM_LONG,         /* IBM 64-bit float */
	XMATH_IBM_EXTENDED,     /* IBM 128-bit extended */
	XMATH_MBF_SINGLE,       /* Microsoft Binary Format 32-bit */
	XMATH_MBF_DOUBLE        /* Microsoft Binary Format 64-bit */
} xmath_format_t;

/*
 * Universal Float Container
 */
typedef struct {
	xmath_format_t format;
	union {
		uint16_t u16;
		uint32_t u32;
		uint64_t u64;
		struct {
			uint64_t low;
			uint64_t high;
		} u128;
		uint8_t bytes[16];
	} data;
} xmath_float_t;

/*
 * Format Conversion
 */

/* Convert from native double to any format */
xmath_float_t xmath_from_double(double value, xmath_format_t format);

/* Convert from any format to native double */
double xmath_to_double(const xmath_float_t *value);

/* Convert between formats */
xmath_float_t xmath_convert(const xmath_float_t *value, xmath_format_t target_format);

/*
 * IEEE 754 Binary16 (Half Precision)
 */
typedef uint16_t xmath_binary16_t;

xmath_binary16_t xmath_binary16_from_float(float value);
float xmath_binary16_to_float(xmath_binary16_t value);

/*
 * IEEE 754 Binary32 (Single Precision)
 */
typedef uint32_t xmath_binary32_t;

xmath_binary32_t xmath_binary32_from_float(float value);
float xmath_binary32_to_float(xmath_binary32_t value);

/*
 * IEEE 754 Binary64 (Double Precision)
 */
typedef uint64_t xmath_binary64_t;

xmath_binary64_t xmath_binary64_from_double(double value);
double xmath_binary64_to_double(xmath_binary64_t value);

/*
 * IEEE 754 Binary128 (Quadruple Precision)
 */
typedef struct {
	uint64_t low;
	uint64_t high;
} xmath_binary128_t;

xmath_binary128_t xmath_binary128_from_double(double value);
double xmath_binary128_to_double(const xmath_binary128_t *value);

/*
 * bfloat16 (Google Brain Float)
 * 1 sign bit, 8 exponent bits, 7 mantissa bits
 */
typedef uint16_t xmath_bfloat16_t;

xmath_bfloat16_t xmath_bfloat16_from_float(float value);
float xmath_bfloat16_to_float(xmath_bfloat16_t value);

/*
 * FP8 Formats
 */
typedef uint8_t xmath_fp8_t;

/* E4M3: 1 sign, 4 exponent, 3 mantissa */
xmath_fp8_t xmath_fp8_e4m3_from_float(float value);
float xmath_fp8_e4m3_to_float(xmath_fp8_t value);

/* E5M2: 1 sign, 5 exponent, 2 mantissa */
xmath_fp8_t xmath_fp8_e5m2_from_float(float value);
float xmath_fp8_e5m2_to_float(xmath_fp8_t value);

/*
 * VAX Floating-Point Formats
 */

/* VAX F-float (32-bit) */
typedef uint32_t xmath_vax_f_t;

xmath_vax_f_t xmath_vax_f_from_float(float value);
float xmath_vax_f_to_float(xmath_vax_f_t value);

/* VAX D-float (64-bit) */
typedef uint64_t xmath_vax_d_t;

xmath_vax_d_t xmath_vax_d_from_double(double value);
double xmath_vax_d_to_double(xmath_vax_d_t value);

/* VAX G-float (64-bit, different from D) */
typedef uint64_t xmath_vax_g_t;

xmath_vax_g_t xmath_vax_g_from_double(double value);
double xmath_vax_g_to_double(xmath_vax_g_t value);

/* VAX H-float (128-bit) */
typedef struct {
	uint64_t low;
	uint64_t high;
} xmath_vax_h_t;

xmath_vax_h_t xmath_vax_h_from_double(double value);
double xmath_vax_h_to_double(const xmath_vax_h_t *value);

/*
 * Cray Floating-Point (64-bit)
 */
typedef uint64_t xmath_cray_t;

xmath_cray_t xmath_cray_from_double(double value);
double xmath_cray_to_double(xmath_cray_t value);

/*
 * IBM Floating-Point Formats
 */

/* IBM 32-bit */
typedef uint32_t xmath_ibm_short_t;

xmath_ibm_short_t xmath_ibm_short_from_float(float value);
float xmath_ibm_short_to_float(xmath_ibm_short_t value);

/* IBM 64-bit */
typedef uint64_t xmath_ibm_long_t;

xmath_ibm_long_t xmath_ibm_long_from_double(double value);
double xmath_ibm_long_to_double(xmath_ibm_long_t value);

/* IBM 128-bit extended */
typedef struct {
	uint64_t high;
	uint64_t low;
} xmath_ibm_extended_t;

xmath_ibm_extended_t xmath_ibm_extended_from_double(double value);
double xmath_ibm_extended_to_double(const xmath_ibm_extended_t *value);

/*
 * Microsoft Binary Format (MBF)
 */

/* MBF 32-bit */
typedef uint32_t xmath_mbf_single_t;

xmath_mbf_single_t xmath_mbf_single_from_float(float value);
float xmath_mbf_single_to_float(xmath_mbf_single_t value);

/* MBF 64-bit */
typedef uint64_t xmath_mbf_double_t;

xmath_mbf_double_t xmath_mbf_double_from_double(double value);
double xmath_mbf_double_to_double(xmath_mbf_double_t value);

/*
 * Format Information
 */

typedef struct {
	const char *name;
	int total_bits;
	int sign_bits;
	int exponent_bits;
	int mantissa_bits;
	int exponent_bias;
	int has_implicit_bit;
} xmath_format_info_t;

const xmath_format_info_t *xmath_get_format_info(xmath_format_t format);

/*
 * Special Value Checks
 */

int xmath_is_nan(const xmath_float_t *value);
int xmath_is_inf(const xmath_float_t *value);
int xmath_is_zero(const xmath_float_t *value);
int xmath_is_subnormal(const xmath_float_t *value);

/*
 * Arithmetic Operations (on universal float)
 */

xmath_float_t xmath_add(const xmath_float_t *a, const xmath_float_t *b);
xmath_float_t xmath_sub(const xmath_float_t *a, const xmath_float_t *b);
xmath_float_t xmath_mul(const xmath_float_t *a, const xmath_float_t *b);
xmath_float_t xmath_div(const xmath_float_t *a, const xmath_float_t *b);
xmath_float_t xmath_sqrt(const xmath_float_t *value);
xmath_float_t xmath_fma(const xmath_float_t *a, const xmath_float_t *b, const xmath_float_t *c);

/*
 * Comparison
 */

int xmath_compare(const xmath_float_t *a, const xmath_float_t *b);
int xmath_equal(const xmath_float_t *a, const xmath_float_t *b);

/*
 * Rounding Modes
 */

typedef enum {
	XMATH_ROUND_NEAREST,
	XMATH_ROUND_TOWARD_ZERO,
	XMATH_ROUND_TOWARD_POS_INF,
	XMATH_ROUND_TOWARD_NEG_INF
} xmath_round_mode_t;

void xmath_set_round_mode(xmath_round_mode_t mode);
xmath_round_mode_t xmath_get_round_mode(void);

/*
 * Exception Flags
 */

typedef enum {
	XMATH_EXC_INVALID = 0x01,
	XMATH_EXC_DIVZERO = 0x02,
	XMATH_EXC_OVERFLOW = 0x04,
	XMATH_EXC_UNDERFLOW = 0x08,
	XMATH_EXC_INEXACT = 0x10
} xmath_exception_t;

int xmath_get_exceptions(void);
void xmath_clear_exceptions(void);
void xmath_set_exception_handler(void (*handler)(xmath_exception_t));

/*
 * String Conversion
 */

char *xmath_to_string(const xmath_float_t *value, int precision);
xmath_float_t xmath_from_string(const char *str, xmath_format_t format);

/*
 * Hex Float Conversion (for exact representation)
 */

char *xmath_to_hexfloat(const xmath_float_t *value);
xmath_float_t xmath_from_hexfloat(const char *hexstr, xmath_format_t format);

/*
 * Endianness Conversion
 */

void xmath_swap_endian(xmath_float_t *value);
int xmath_is_big_endian(void);

#ifdef __cplusplus
}
#endif

#endif /* _PCC_XMATH_H_ */
