/*
 * IEEE 754-2008 Decimal Floating Point Support
 * Portable implementation for PCC
 *
 * This header provides _Decimal32, _Decimal64, and _Decimal128 types
 * as specified in ISO/IEC TR 24732 (Decimal Floating-Point arithmetic)
 */

#ifndef _DECIMAL_H
#define _DECIMAL_H

#include <stdint.h>
#include <stddef.h>
#include <limits.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Check if compiler has built-in decimal floating point support */
#if defined(__STDC_DEC_FP__) || (defined(__GNUC__) && __GNUC__ >= 4)
#define HAVE_BUILTIN_DECIMAL 1
#endif

/* ================================================================
 * Type Definitions
 * ================================================================ */

/*
 * If the compiler doesn't have built-in decimal types,
 * we define them as structures containing encoded values.
 * If the compiler has built-in types, we use those instead.
 */

#ifndef HAVE_BUILTIN_DECIMAL
/*
 * _Decimal32: 7 decimal digits precision
 * Range: 10^-95 to 10^96
 * Storage: 4 bytes
 */
typedef struct {
    uint32_t value;  /* DPD or BID encoded */
} _Decimal32;

/*
 * _Decimal64: 16 decimal digits precision
 * Range: 10^-383 to 10^384
 * Storage: 8 bytes
 */
typedef struct {
    uint64_t value;  /* DPD or BID encoded */
} _Decimal64;

/*
 * _Decimal128: 34 decimal digits precision
 * Range: 10^-6143 to 10^6144
 * Storage: 16 bytes
 */
typedef struct {
    uint64_t high;   /* High 64 bits */
    uint64_t low;    /* Low 64 bits */
} _Decimal128;
#endif /* !HAVE_BUILTIN_DECIMAL */

/* ================================================================
 * Constants
 * ================================================================ */

/* _Decimal32 parameters */
#define DEC32_BYTES     4
#define DEC32_PMAX      7       /* Maximum precision */
#define DEC32_EMAX      96      /* Maximum exponent */
#define DEC32_EMIN      (-95)   /* Minimum exponent */
#define DEC32_BIAS      101     /* Exponent bias */

/* _Decimal64 parameters */
#define DEC64_BYTES     8
#define DEC64_PMAX      16
#define DEC64_EMAX      384
#define DEC64_EMIN      (-383)
#define DEC64_BIAS      398

/* _Decimal128 parameters */
#define DEC128_BYTES    16
#define DEC128_PMAX     34
#define DEC128_EMAX     6144
#define DEC128_EMIN     (-6143)
#define DEC128_BIAS     6176

/* ================================================================
 * Conversion Functions
 * ================================================================ */

/* String conversions */
_Decimal32  dec32_from_string(const char *str);
_Decimal64  dec64_from_string(const char *str);
_Decimal128 dec128_from_string(const char *str);

void dec32_to_string(_Decimal32 d, char *buf, size_t bufsize);
void dec64_to_string(_Decimal64 d, char *buf, size_t bufsize);
void dec128_to_string(_Decimal128 d, char *buf, size_t bufsize);

/* Integer conversions */
_Decimal32  dec32_from_int32(int32_t i);
_Decimal64  dec64_from_int32(int32_t i);
_Decimal64  dec64_from_int64(int64_t i);
_Decimal128 dec128_from_int32(int32_t i);
_Decimal128 dec128_from_int64(int64_t i);

int32_t dec32_to_int32(_Decimal32 d);
int64_t dec64_to_int64(_Decimal64 d);
int64_t dec128_to_int64(_Decimal128 d);

/* Cross-type conversions */
_Decimal64  dec32_to_dec64(_Decimal32 d);
_Decimal128 dec32_to_dec128(_Decimal32 d);
_Decimal128 dec64_to_dec128(_Decimal64 d);

_Decimal32  dec64_to_dec32(_Decimal64 d);
_Decimal32  dec128_to_dec32(_Decimal128 d);
_Decimal64  dec128_to_dec64(_Decimal128 d);

/* ================================================================
 * Arithmetic Operations - _Decimal32
 * ================================================================ */

_Decimal32 dec32_add(_Decimal32 a, _Decimal32 b);
_Decimal32 dec32_sub(_Decimal32 a, _Decimal32 b);
_Decimal32 dec32_mul(_Decimal32 a, _Decimal32 b);
_Decimal32 dec32_div(_Decimal32 a, _Decimal32 b);
_Decimal32 dec32_neg(_Decimal32 a);
_Decimal32 dec32_abs(_Decimal32 a);
_Decimal32 dec32_sqrt(_Decimal32 a);

/* ================================================================
 * Arithmetic Operations - _Decimal64
 * ================================================================ */

_Decimal64 dec64_add(_Decimal64 a, _Decimal64 b);
_Decimal64 dec64_sub(_Decimal64 a, _Decimal64 b);
_Decimal64 dec64_mul(_Decimal64 a, _Decimal64 b);
_Decimal64 dec64_div(_Decimal64 a, _Decimal64 b);
_Decimal64 dec64_neg(_Decimal64 a);
_Decimal64 dec64_abs(_Decimal64 a);
_Decimal64 dec64_sqrt(_Decimal64 a);

/* ================================================================
 * Arithmetic Operations - _Decimal128
 * ================================================================ */

_Decimal128 dec128_add(_Decimal128 a, _Decimal128 b);
_Decimal128 dec128_sub(_Decimal128 a, _Decimal128 b);
_Decimal128 dec128_mul(_Decimal128 a, _Decimal128 b);
_Decimal128 dec128_div(_Decimal128 a, _Decimal128 b);
_Decimal128 dec128_neg(_Decimal128 a);
_Decimal128 dec128_abs(_Decimal128 a);
_Decimal128 dec128_sqrt(_Decimal128 a);

/* ================================================================
 * Comparison Operations
 * ================================================================ */

/* _Decimal32 comparisons */
bool dec32_eq(_Decimal32 a, _Decimal32 b);
bool dec32_ne(_Decimal32 a, _Decimal32 b);
bool dec32_lt(_Decimal32 a, _Decimal32 b);
bool dec32_le(_Decimal32 a, _Decimal32 b);
bool dec32_gt(_Decimal32 a, _Decimal32 b);
bool dec32_ge(_Decimal32 a, _Decimal32 b);

/* _Decimal64 comparisons */
bool dec64_eq(_Decimal64 a, _Decimal64 b);
bool dec64_ne(_Decimal64 a, _Decimal64 b);
bool dec64_lt(_Decimal64 a, _Decimal64 b);
bool dec64_le(_Decimal64 a, _Decimal64 b);
bool dec64_gt(_Decimal64 a, _Decimal64 b);
bool dec64_ge(_Decimal64 a, _Decimal64 b);

/* _Decimal128 comparisons */
bool dec128_eq(_Decimal128 a, _Decimal128 b);
bool dec128_ne(_Decimal128 a, _Decimal128 b);
bool dec128_lt(_Decimal128 a, _Decimal128 b);
bool dec128_le(_Decimal128 a, _Decimal128 b);
bool dec128_gt(_Decimal128 a, _Decimal128 b);
bool dec128_ge(_Decimal128 a, _Decimal128 b);

/* ================================================================
 * Classification and Special Values
 * ================================================================ */

/* Classification */
bool dec32_is_zero(_Decimal32 d);
bool dec32_is_inf(_Decimal32 d);
bool dec32_is_nan(_Decimal32 d);
bool dec32_is_finite(_Decimal32 d);

bool dec64_is_zero(_Decimal64 d);
bool dec64_is_inf(_Decimal64 d);
bool dec64_is_nan(_Decimal64 d);
bool dec64_is_finite(_Decimal64 d);

bool dec128_is_zero(_Decimal128 d);
bool dec128_is_inf(_Decimal128 d);
bool dec128_is_nan(_Decimal128 d);
bool dec128_is_finite(_Decimal128 d);

/* Special values */
_Decimal32  dec32_zero(void);
_Decimal32  dec32_one(void);
_Decimal32  dec32_inf(void);
_Decimal32  dec32_nan(void);

_Decimal64  dec64_zero(void);
_Decimal64  dec64_one(void);
_Decimal64  dec64_inf(void);
_Decimal64  dec64_nan(void);

_Decimal128 dec128_zero(void);
_Decimal128 dec128_one(void);
_Decimal128 dec128_inf(void);
_Decimal128 dec128_nan(void);

#ifdef __cplusplus
}
#endif

#endif /* _DECIMAL_H */
