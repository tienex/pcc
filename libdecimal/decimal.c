/*
 * IEEE 754-2008 Decimal Floating Point Implementation
 * Portable software implementation for PCC
 *
 * This is a simplified implementation using BID (Binary Integer Decimal) encoding.
 * For production use, consider integrating GCC's libdecnumber for full IEEE 754-2008 compliance.
 *
 * NOTE: This library is only compiled on systems without native decimal FP support.
 *       If you're seeing errors about _Decimal types, your system has built-in support
 *       and this library should not be used.
 */

#include "decimal.h"

/* Only compile this library if we don't have built-in decimal types */
#ifndef HAVE_BUILTIN_DECIMAL

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* ================================================================
 * Internal Helper Functions
 * ================================================================ */

/*
 * BID (Binary Integer Decimal) encoding helpers
 * Format: sign + exponent + coefficient
 */

/* Extract sign from _Decimal32 */
static inline int dec32_get_sign(_Decimal32 d) {
    return (d.value >> 31) & 1;
}

/* Extract exponent from _Decimal32 (simplified) */
static inline int dec32_get_exp(_Decimal32 d) {
    return (int)((d.value >> 23) & 0xFF) - DEC32_BIAS;
}

/* Extract coefficient from _Decimal32 (simplified) */
static inline uint32_t dec32_get_coef(_Decimal32 d) {
    return d.value & 0x7FFFFF;
}

/* Pack sign, exponent, coefficient into _Decimal32 */
static inline _Decimal32 dec32_pack(int sign, int exp, uint32_t coef) {
    _Decimal32 result;
    uint32_t biased_exp = (uint32_t)(exp + DEC32_BIAS) & 0xFF;
    result.value = ((uint32_t)sign << 31) | (biased_exp << 23) | (coef & 0x7FFFFF);
    return result;
}

/* Similar helpers for _Decimal64 */
static inline int dec64_get_sign(_Decimal64 d) {
    return (d.value >> 63) & 1;
}

static inline int dec64_get_exp(_Decimal64 d) {
    return (int)((d.value >> 53) & 0x3FF) - DEC64_BIAS;
}

static inline uint64_t dec64_get_coef(_Decimal64 d) {
    return d.value & 0x1FFFFFFFFFFFFFULL;
}

static inline _Decimal64 dec64_pack(int sign, int exp, uint64_t coef) {
    _Decimal64 result;
    uint64_t biased_exp = (uint64_t)(exp + DEC64_BIAS) & 0x3FF;
    result.value = ((uint64_t)sign << 63) | (biased_exp << 53) | (coef & 0x1FFFFFFFFFFFFFULL);
    return result;
}

/* ================================================================
 * Special Values
 * ================================================================ */

_Decimal32 dec32_zero(void) {
    _Decimal32 d;
    d.value = 0;
    return d;
}

_Decimal32 dec32_one(void) {
    return dec32_pack(0, 0, 1);
}

_Decimal32 dec32_inf(void) {
    _Decimal32 d;
    d.value = 0x78000000; /* Infinity encoding */
    return d;
}

_Decimal32 dec32_nan(void) {
    _Decimal32 d;
    d.value = 0x7C000000; /* NaN encoding */
    return d;
}

_Decimal64 dec64_zero(void) {
    _Decimal64 d;
    d.value = 0;
    return d;
}

_Decimal64 dec64_one(void) {
    return dec64_pack(0, 0, 1);
}

_Decimal64 dec64_inf(void) {
    _Decimal64 d;
    d.value = 0x7800000000000000ULL;
    return d;
}

_Decimal64 dec64_nan(void) {
    _Decimal64 d;
    d.value = 0x7C00000000000000ULL;
    return d;
}

_Decimal128 dec128_zero(void) {
    _Decimal128 d;
    d.high = 0;
    d.low = 0;
    return d;
}

_Decimal128 dec128_one(void) {
    _Decimal128 d;
    d.high = 0x3040000000000000ULL; /* 1.0 in DPD encoding */
    d.low = 0;
    return d;
}

_Decimal128 dec128_inf(void) {
    _Decimal128 d;
    d.high = 0x7800000000000000ULL;
    d.low = 0;
    return d;
}

_Decimal128 dec128_nan(void) {
    _Decimal128 d;
    d.high = 0x7C00000000000000ULL;
    d.low = 0;
    return d;
}

/* ================================================================
 * Classification Functions
 * ================================================================ */

bool dec32_is_zero(_Decimal32 d) {
    return dec32_get_coef(d) == 0;
}

bool dec32_is_inf(_Decimal32 d) {
    return (d.value & 0x7C000000) == 0x78000000;
}

bool dec32_is_nan(_Decimal32 d) {
    return (d.value & 0x7C000000) == 0x7C000000;
}

bool dec32_is_finite(_Decimal32 d) {
    return !dec32_is_inf(d) && !dec32_is_nan(d);
}

bool dec64_is_zero(_Decimal64 d) {
    return dec64_get_coef(d) == 0;
}

bool dec64_is_inf(_Decimal64 d) {
    return (d.value & 0x7C00000000000000ULL) == 0x7800000000000000ULL;
}

bool dec64_is_nan(_Decimal64 d) {
    return (d.value & 0x7C00000000000000ULL) == 0x7C00000000000000ULL;
}

bool dec64_is_finite(_Decimal64 d) {
    return !dec64_is_inf(d) && !dec64_is_nan(d);
}

bool dec128_is_zero(_Decimal128 d) {
    return d.high == 0 && d.low == 0;
}

bool dec128_is_inf(_Decimal128 d) {
    return (d.high & 0x7C00000000000000ULL) == 0x7800000000000000ULL;
}

bool dec128_is_nan(_Decimal128 d) {
    return (d.high & 0x7C00000000000000ULL) == 0x7C00000000000000ULL;
}

bool dec128_is_finite(_Decimal128 d) {
    return !dec128_is_inf(d) && !dec128_is_nan(d);
}

/* ================================================================
 * Integer Conversions
 * ================================================================ */

_Decimal32 dec32_from_int32(int32_t i) {
    if (i == 0) return dec32_zero();
    int sign = i < 0 ? 1 : 0;
    uint32_t abs_i = sign ? -(uint32_t)i : (uint32_t)i;
    return dec32_pack(sign, 0, abs_i & 0x7FFFFF);
}

_Decimal64 dec64_from_int32(int32_t i) {
    if (i == 0) return dec64_zero();
    int sign = i < 0 ? 1 : 0;
    uint64_t abs_i = sign ? -(uint64_t)i : (uint64_t)i;
    return dec64_pack(sign, 0, abs_i);
}

_Decimal64 dec64_from_int64(int64_t i) {
    if (i == 0) return dec64_zero();
    int sign = i < 0 ? 1 : 0;
    uint64_t abs_i = sign ? -(uint64_t)i : (uint64_t)i;
    return dec64_pack(sign, 0, abs_i & 0x1FFFFFFFFFFFFFULL);
}

_Decimal128 dec128_from_int32(int32_t i) {
    _Decimal128 d;
    if (i == 0) return dec128_zero();
    int sign = i < 0 ? 1 : 0;
    uint32_t abs_i = sign ? -(uint32_t)i : (uint32_t)i;
    d.high = ((uint64_t)sign << 63) | (((uint64_t)DEC128_BIAS & 0x3FFF) << 49);
    d.low = abs_i;
    return d;
}

_Decimal128 dec128_from_int64(int64_t i) {
    _Decimal128 d;
    if (i == 0) return dec128_zero();
    int sign = i < 0 ? 1 : 0;
    uint64_t abs_i = sign ? -(uint64_t)i : (uint64_t)i;
    d.high = ((uint64_t)sign << 63) | (((uint64_t)DEC128_BIAS & 0x3FFF) << 49);
    d.low = abs_i;
    return d;
}

int32_t dec32_to_int32(_Decimal32 d) {
    if (dec32_is_zero(d)) return 0;
    int sign = dec32_get_sign(d);
    uint32_t coef = dec32_get_coef(d);
    int exp = dec32_get_exp(d);

    /* Scale by exponent */
    while (exp > 0 && coef < INT32_MAX / 10) {
        coef *= 10;
        exp--;
    }

    return sign ? -(int32_t)coef : (int32_t)coef;
}

int64_t dec64_to_int64(_Decimal64 d) {
    if (dec64_is_zero(d)) return 0;
    int sign = dec64_get_sign(d);
    uint64_t coef = dec64_get_coef(d);
    int exp = dec64_get_exp(d);

    /* Scale by exponent */
    while (exp > 0 && coef < INT64_MAX / 10) {
        coef *= 10;
        exp--;
    }

    return sign ? -(int64_t)coef : (int64_t)coef;
}

int64_t dec128_to_int64(_Decimal128 d) {
    if (dec128_is_zero(d)) return 0;
    /* Simplified: extract from low bits */
    return (int64_t)d.low;
}

/* ================================================================
 * Cross-type Conversions
 * ================================================================ */

_Decimal64 dec32_to_dec64(_Decimal32 d) {
    if (dec32_is_zero(d)) return dec64_zero();
    if (dec32_is_inf(d)) return dec64_inf();
    if (dec32_is_nan(d)) return dec64_nan();

    return dec64_pack(dec32_get_sign(d), dec32_get_exp(d), dec32_get_coef(d));
}

_Decimal128 dec32_to_dec128(_Decimal32 d) {
    _Decimal128 result;
    if (dec32_is_zero(d)) return dec128_zero();
    if (dec32_is_inf(d)) return dec128_inf();
    if (dec32_is_nan(d)) return dec128_nan();

    result.high = ((uint64_t)dec32_get_sign(d) << 63) |
                  (((uint64_t)(dec32_get_exp(d) + DEC128_BIAS) & 0x3FFF) << 49);
    result.low = dec32_get_coef(d);
    return result;
}

_Decimal128 dec64_to_dec128(_Decimal64 d) {
    _Decimal128 result;
    if (dec64_is_zero(d)) return dec128_zero();
    if (dec64_is_inf(d)) return dec128_inf();
    if (dec64_is_nan(d)) return dec128_nan();

    result.high = ((uint64_t)dec64_get_sign(d) << 63) |
                  (((uint64_t)(dec64_get_exp(d) + DEC128_BIAS) & 0x3FFF) << 49);
    result.low = dec64_get_coef(d);
    return result;
}

_Decimal32 dec64_to_dec32(_Decimal64 d) {
    if (dec64_is_zero(d)) return dec32_zero();
    if (dec64_is_inf(d)) return dec32_inf();
    if (dec64_is_nan(d)) return dec32_nan();

    /* Truncate coefficient if needed */
    uint64_t coef = dec64_get_coef(d);
    while (coef > 0x7FFFFF) {
        coef /= 10;
    }

    return dec32_pack(dec64_get_sign(d), dec64_get_exp(d), (uint32_t)coef);
}

_Decimal32 dec128_to_dec32(_Decimal128 d) {
    if (dec128_is_zero(d)) return dec32_zero();
    if (dec128_is_inf(d)) return dec32_inf();
    if (dec128_is_nan(d)) return dec32_nan();

    /* Simplified conversion */
    uint64_t coef = d.low;
    while (coef > 0x7FFFFF) {
        coef /= 10;
    }

    return dec32_pack((d.high >> 63) & 1, 0, (uint32_t)coef);
}

_Decimal64 dec128_to_dec64(_Decimal128 d) {
    if (dec128_is_zero(d)) return dec64_zero();
    if (dec128_is_inf(d)) return dec64_inf();
    if (dec128_is_nan(d)) return dec64_nan();

    /* Simplified conversion */
    uint64_t coef = d.low;
    while (coef > 0x1FFFFFFFFFFFFFULL) {
        coef /= 10;
    }

    return dec64_pack((d.high >> 63) & 1, 0, coef);
}

/* ================================================================
 * String Conversions
 * ================================================================ */

_Decimal32 dec32_from_string(const char *str) {
    /* Simple parsing - production code would use proper decimal parser */
    double val = atof(str);
    int sign = val < 0 ? 1 : 0;
    val = fabs(val);

    /* Extract mantissa and exponent */
    int exp = 0;
    while (val >= 10.0 && val < 10000000.0) {
        val /= 10.0;
        exp++;
    }

    uint32_t coef = (uint32_t)val;
    return dec32_pack(sign, exp, coef);
}

_Decimal64 dec64_from_string(const char *str) {
    double val = atof(str);
    int sign = val < 0 ? 1 : 0;
    val = fabs(val);

    int exp = 0;
    while (val >= 10.0 && exp < DEC64_EMAX) {
        val /= 10.0;
        exp++;
    }

    uint64_t coef = (uint64_t)val;
    return dec64_pack(sign, exp, coef);
}

_Decimal128 dec128_from_string(const char *str) {
    /* Simplified: convert via _Decimal64 */
    return dec64_to_dec128(dec64_from_string(str));
}

void dec32_to_string(_Decimal32 d, char *buf, size_t bufsize) {
    if (dec32_is_nan(d)) {
        snprintf(buf, bufsize, "NaN");
        return;
    }
    if (dec32_is_inf(d)) {
        snprintf(buf, bufsize, "%sInf", dec32_get_sign(d) ? "-" : "+");
        return;
    }
    if (dec32_is_zero(d)) {
        snprintf(buf, bufsize, "0.0");
        return;
    }

    int sign = dec32_get_sign(d);
    uint32_t coef = dec32_get_coef(d);
    int exp = dec32_get_exp(d);

    /* Simple formatting */
    snprintf(buf, bufsize, "%s%ue%d", sign ? "-" : "", coef, exp);
}

void dec64_to_string(_Decimal64 d, char *buf, size_t bufsize) {
    if (dec64_is_nan(d)) {
        snprintf(buf, bufsize, "NaN");
        return;
    }
    if (dec64_is_inf(d)) {
        snprintf(buf, bufsize, "%sInf", dec64_get_sign(d) ? "-" : "+");
        return;
    }
    if (dec64_is_zero(d)) {
        snprintf(buf, bufsize, "0.0");
        return;
    }

    int sign = dec64_get_sign(d);
    uint64_t coef = dec64_get_coef(d);
    int exp = dec64_get_exp(d);

    snprintf(buf, bufsize, "%s%llue%d", sign ? "-" : "", (unsigned long long)coef, exp);
}

void dec128_to_string(_Decimal128 d, char *buf, size_t bufsize) {
    if (dec128_is_nan(d)) {
        snprintf(buf, bufsize, "NaN");
        return;
    }
    if (dec128_is_inf(d)) {
        snprintf(buf, bufsize, "%sInf", (d.high >> 63) ? "-" : "+");
        return;
    }
    if (dec128_is_zero(d)) {
        snprintf(buf, bufsize, "0.0");
        return;
    }

    /* Simplified: show low bits */
    snprintf(buf, bufsize, "%llue0", (unsigned long long)d.low);
}

/* ================================================================
 * Arithmetic Operations - _Decimal32
 * ================================================================ */

_Decimal32 dec32_add(_Decimal32 a, _Decimal32 b) {
    /* Simplified addition - align exponents and add coefficients */
    if (dec32_is_zero(a)) return b;
    if (dec32_is_zero(b)) return a;

    int sign_a = dec32_get_sign(a);
    int sign_b = dec32_get_sign(b);
    int exp_a = dec32_get_exp(a);
    int exp_b = dec32_get_exp(b);
    uint32_t coef_a = dec32_get_coef(a);
    uint32_t coef_b = dec32_get_coef(b);

    /* Align exponents */
    while (exp_a < exp_b && coef_a < 0x7FFFFF / 10) {
        coef_a *= 10;
        exp_a++;
    }
    while (exp_b < exp_a && coef_b < 0x7FFFFF / 10) {
        coef_b *= 10;
        exp_b++;
    }

    /* Add or subtract based on signs */
    uint32_t result_coef;
    int result_sign = sign_a;

    if (sign_a == sign_b) {
        result_coef = coef_a + coef_b;
    } else {
        if (coef_a >= coef_b) {
            result_coef = coef_a - coef_b;
        } else {
            result_coef = coef_b - coef_a;
            result_sign = sign_b;
        }
    }

    return dec32_pack(result_sign, exp_a, result_coef);
}

_Decimal32 dec32_sub(_Decimal32 a, _Decimal32 b) {
    /* Negate b and add */
    b.value ^= 0x80000000; /* Flip sign bit */
    return dec32_add(a, b);
}

_Decimal32 dec32_mul(_Decimal32 a, _Decimal32 b) {
    if (dec32_is_zero(a) || dec32_is_zero(b)) return dec32_zero();

    int sign = dec32_get_sign(a) ^ dec32_get_sign(b);
    int exp = dec32_get_exp(a) + dec32_get_exp(b);
    uint64_t coef = (uint64_t)dec32_get_coef(a) * dec32_get_coef(b);

    /* Normalize */
    while (coef > 0x7FFFFF) {
        coef /= 10;
        exp++;
    }

    return dec32_pack(sign, exp, (uint32_t)coef);
}

_Decimal32 dec32_div(_Decimal32 a, _Decimal32 b) {
    if (dec32_is_zero(b)) return dec32_inf();
    if (dec32_is_zero(a)) return dec32_zero();

    int sign = dec32_get_sign(a) ^ dec32_get_sign(b);
    int exp = dec32_get_exp(a) - dec32_get_exp(b);
    uint64_t coef_a = dec32_get_coef(a);
    uint32_t coef_b = dec32_get_coef(b);

    /* Scale dividend for precision */
    while (coef_a < 0x7FFFFF) {
        coef_a *= 10;
        exp--;
    }

    uint32_t coef = (uint32_t)(coef_a / coef_b);
    return dec32_pack(sign, exp, coef);
}

_Decimal32 dec32_neg(_Decimal32 a) {
    a.value ^= 0x80000000; /* Flip sign bit */
    return a;
}

_Decimal32 dec32_abs(_Decimal32 a) {
    a.value &= 0x7FFFFFFF; /* Clear sign bit */
    return a;
}

_Decimal32 dec32_sqrt(_Decimal32 a) {
    if (dec32_is_zero(a)) return dec32_zero();
    if (dec32_get_sign(a)) return dec32_nan(); /* sqrt of negative */

    /* Simplified: convert to double, sqrt, convert back */
    uint32_t coef = dec32_get_coef(a);
    int exp = dec32_get_exp(a);
    double val = (double)coef * pow(10.0, exp);
    double result = sqrt(val);

    exp = 0;
    while (result >= 10.0) {
        result /= 10.0;
        exp++;
    }

    return dec32_pack(0, exp, (uint32_t)result);
}

/* ================================================================
 * Arithmetic Operations - _Decimal64
 * ================================================================ */

_Decimal64 dec64_add(_Decimal64 a, _Decimal64 b) {
    /* Similar to dec32_add but with 64-bit arithmetic */
    if (dec64_is_zero(a)) return b;
    if (dec64_is_zero(b)) return a;

    int sign_a = dec64_get_sign(a);
    int sign_b = dec64_get_sign(b);
    int exp_a = dec64_get_exp(a);
    int exp_b = dec64_get_exp(b);
    uint64_t coef_a = dec64_get_coef(a);
    uint64_t coef_b = dec64_get_coef(b);

    /* Align exponents */
    while (exp_a < exp_b && coef_a < 0x1FFFFFFFFFFFFFULL / 10) {
        coef_a *= 10;
        exp_a++;
    }
    while (exp_b < exp_a && coef_b < 0x1FFFFFFFFFFFFFULL / 10) {
        coef_b *= 10;
        exp_b++;
    }

    uint64_t result_coef;
    int result_sign = sign_a;

    if (sign_a == sign_b) {
        result_coef = coef_a + coef_b;
    } else {
        if (coef_a >= coef_b) {
            result_coef = coef_a - coef_b;
        } else {
            result_coef = coef_b - coef_a;
            result_sign = sign_b;
        }
    }

    return dec64_pack(result_sign, exp_a, result_coef);
}

_Decimal64 dec64_sub(_Decimal64 a, _Decimal64 b) {
    b.value ^= 0x8000000000000000ULL;
    return dec64_add(a, b);
}

_Decimal64 dec64_mul(_Decimal64 a, _Decimal64 b) {
    if (dec64_is_zero(a) || dec64_is_zero(b)) return dec64_zero();

    int sign = dec64_get_sign(a) ^ dec64_get_sign(b);
    int exp = dec64_get_exp(a) + dec64_get_exp(b);

    /* Use 128-bit arithmetic for multiplication */
    uint64_t coef_a = dec64_get_coef(a);
    uint64_t coef_b = dec64_get_coef(b);

    /* Simplified: may overflow */
    uint64_t coef = coef_a * coef_b;

    /* Normalize */
    while (coef > 0x1FFFFFFFFFFFFFULL) {
        coef /= 10;
        exp++;
    }

    return dec64_pack(sign, exp, coef);
}

_Decimal64 dec64_div(_Decimal64 a, _Decimal64 b) {
    if (dec64_is_zero(b)) return dec64_inf();
    if (dec64_is_zero(a)) return dec64_zero();

    int sign = dec64_get_sign(a) ^ dec64_get_sign(b);
    int exp = dec64_get_exp(a) - dec64_get_exp(b);
    uint64_t coef_a = dec64_get_coef(a);
    uint64_t coef_b = dec64_get_coef(b);

    /* Scale for precision */
    while (coef_a < 0x1FFFFFFFFFFFFFULL / 10) {
        coef_a *= 10;
        exp--;
    }

    uint64_t coef = coef_a / coef_b;
    return dec64_pack(sign, exp, coef);
}

_Decimal64 dec64_neg(_Decimal64 a) {
    a.value ^= 0x8000000000000000ULL;
    return a;
}

_Decimal64 dec64_abs(_Decimal64 a) {
    a.value &= 0x7FFFFFFFFFFFFFFFULL;
    return a;
}

_Decimal64 dec64_sqrt(_Decimal64 a) {
    if (dec64_is_zero(a)) return dec64_zero();
    if (dec64_get_sign(a)) return dec64_nan();

    uint64_t coef = dec64_get_coef(a);
    int exp = dec64_get_exp(a);
    double val = (double)coef * pow(10.0, exp);
    double result = sqrt(val);

    exp = 0;
    while (result >= 10.0) {
        result /= 10.0;
        exp++;
    }

    return dec64_pack(0, exp, (uint64_t)result);
}

/* ================================================================
 * Arithmetic Operations - _Decimal128
 * ================================================================ */

_Decimal128 dec128_add(_Decimal128 a, _Decimal128 b) {
    /* Simplified: for full implementation, use proper 128-bit decimal arithmetic */
    if (dec128_is_zero(a)) return b;
    if (dec128_is_zero(b)) return a;

    /* Very simplified - just add low parts */
    _Decimal128 result;
    result.high = a.high;
    result.low = a.low + b.low;
    if (result.low < a.low) { /* Overflow */
        result.high++;
    }
    return result;
}

_Decimal128 dec128_sub(_Decimal128 a, _Decimal128 b) {
    b.high ^= 0x8000000000000000ULL;
    return dec128_add(a, b);
}

_Decimal128 dec128_mul(_Decimal128 a, _Decimal128 b) {
    /* Simplified multiplication */
    if (dec128_is_zero(a) || dec128_is_zero(b)) return dec128_zero();

    _Decimal128 result;
    result.high = 0;
    result.low = a.low * b.low; /* Simplified */
    return result;
}

_Decimal128 dec128_div(_Decimal128 a, _Decimal128 b) {
    if (dec128_is_zero(b)) return dec128_inf();
    if (dec128_is_zero(a)) return dec128_zero();

    _Decimal128 result;
    result.high = 0;
    result.low = a.low / b.low; /* Simplified */
    return result;
}

_Decimal128 dec128_neg(_Decimal128 a) {
    a.high ^= 0x8000000000000000ULL;
    return a;
}

_Decimal128 dec128_abs(_Decimal128 a) {
    a.high &= 0x7FFFFFFFFFFFFFFFULL;
    return a;
}

_Decimal128 dec128_sqrt(_Decimal128 a) {
    /* Very simplified */
    _Decimal128 result;
    result.high = 0;
    result.low = (uint64_t)sqrt((double)a.low);
    return result;
}

/* ================================================================
 * Comparison Operations
 * ================================================================ */

bool dec32_eq(_Decimal32 a, _Decimal32 b) {
    return a.value == b.value;
}

bool dec32_ne(_Decimal32 a, _Decimal32 b) {
    return a.value != b.value;
}

bool dec32_lt(_Decimal32 a, _Decimal32 b) {
    /* Simplified comparison */
    int32_t a_val = dec32_to_int32(a);
    int32_t b_val = dec32_to_int32(b);
    return a_val < b_val;
}

bool dec32_le(_Decimal32 a, _Decimal32 b) {
    return dec32_lt(a, b) || dec32_eq(a, b);
}

bool dec32_gt(_Decimal32 a, _Decimal32 b) {
    return !dec32_le(a, b);
}

bool dec32_ge(_Decimal32 a, _Decimal32 b) {
    return !dec32_lt(a, b);
}

bool dec64_eq(_Decimal64 a, _Decimal64 b) {
    return a.value == b.value;
}

bool dec64_ne(_Decimal64 a, _Decimal64 b) {
    return a.value != b.value;
}

bool dec64_lt(_Decimal64 a, _Decimal64 b) {
    int64_t a_val = dec64_to_int64(a);
    int64_t b_val = dec64_to_int64(b);
    return a_val < b_val;
}

bool dec64_le(_Decimal64 a, _Decimal64 b) {
    return dec64_lt(a, b) || dec64_eq(a, b);
}

bool dec64_gt(_Decimal64 a, _Decimal64 b) {
    return !dec64_le(a, b);
}

bool dec64_ge(_Decimal64 a, _Decimal64 b) {
    return !dec64_lt(a, b);
}

bool dec128_eq(_Decimal128 a, _Decimal128 b) {
    return a.high == b.high && a.low == b.low;
}

bool dec128_ne(_Decimal128 a, _Decimal128 b) {
    return !dec128_eq(a, b);
}

bool dec128_lt(_Decimal128 a, _Decimal128 b) {
    if (a.high != b.high) return a.high < b.high;
    return a.low < b.low;
}

bool dec128_le(_Decimal128 a, _Decimal128 b) {
    return dec128_lt(a, b) || dec128_eq(a, b);
}

bool dec128_gt(_Decimal128 a, _Decimal128 b) {
    return !dec128_le(a, b);
}

bool dec128_ge(_Decimal128 a, _Decimal128 b) {
    return !dec128_lt(a, b);
}

#endif /* !HAVE_BUILTIN_DECIMAL */
