/*
 * C23 stdckdint.h implementation
 * Checked integer arithmetic with overflow detection
 */

#include "stdckdint.h"
#include <limits.h>

/* Use compiler built-ins if available */
#if defined(__GNUC__) && __GNUC__ >= 5
#define HAS_BUILTIN_OVERFLOW
#endif

/* Signed int operations */
bool ckd_add_int(int *result, int a, int b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_add_overflow(a, b, result);
#else
    *result = a + b;
    if (b > 0 && a > INT_MAX - b) return true;
    if (b < 0 && a < INT_MIN - b) return true;
    return false;
#endif
}

bool ckd_sub_int(int *result, int a, int b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_sub_overflow(a, b, result);
#else
    *result = a - b;
    if (b < 0 && a > INT_MAX + b) return true;
    if (b > 0 && a < INT_MIN + b) return true;
    return false;
#endif
}

bool ckd_mul_int(int *result, int a, int b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_mul_overflow(a, b, result);
#else
    *result = a * b;
    if (a > 0) {
        if (b > 0) {
            if (a > INT_MAX / b) return true;
        } else {
            if (b < INT_MIN / a) return true;
        }
    } else {
        if (b > 0) {
            if (a < INT_MIN / b) return true;
        } else {
            if (a != 0 && b < INT_MAX / a) return true;
        }
    }
    return false;
#endif
}

/* Signed long operations */
bool ckd_add_long(long *result, long a, long b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_add_overflow(a, b, result);
#else
    *result = a + b;
    if (b > 0 && a > LONG_MAX - b) return true;
    if (b < 0 && a < LONG_MIN - b) return true;
    return false;
#endif
}

bool ckd_sub_long(long *result, long a, long b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_sub_overflow(a, b, result);
#else
    *result = a - b;
    if (b < 0 && a > LONG_MAX + b) return true;
    if (b > 0 && a < LONG_MIN + b) return true;
    return false;
#endif
}

bool ckd_mul_long(long *result, long a, long b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_mul_overflow(a, b, result);
#else
    *result = a * b;
    if (a > 0) {
        if (b > 0) {
            if (a > LONG_MAX / b) return true;
        } else {
            if (b < LONG_MIN / a) return true;
        }
    } else {
        if (b > 0) {
            if (a < LONG_MIN / b) return true;
        } else {
            if (a != 0 && b < LONG_MAX / a) return true;
        }
    }
    return false;
#endif
}

/* Signed long long operations */
bool ckd_add_llong(long long *result, long long a, long long b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_add_overflow(a, b, result);
#else
    *result = a + b;
    if (b > 0 && a > LLONG_MAX - b) return true;
    if (b < 0 && a < LLONG_MIN - b) return true;
    return false;
#endif
}

bool ckd_sub_llong(long long *result, long long a, long long b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_sub_overflow(a, b, result);
#else
    *result = a - b;
    if (b < 0 && a > LLONG_MAX + b) return true;
    if (b > 0 && a < LLONG_MIN + b) return true;
    return false;
#endif
}

bool ckd_mul_llong(long long *result, long long a, long long b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_mul_overflow(a, b, result);
#else
    *result = a * b;
    if (a > 0) {
        if (b > 0) {
            if (a > LLONG_MAX / b) return true;
        } else {
            if (b < LLONG_MIN / a) return true;
        }
    } else {
        if (b > 0) {
            if (a < LLONG_MIN / b) return true;
        } else {
            if (a != 0 && b < LLONG_MAX / a) return true;
        }
    }
    return false;
#endif
}

/* Unsigned int operations */
bool ckd_add_uint(unsigned int *result, unsigned int a, unsigned int b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_add_overflow(a, b, result);
#else
    *result = a + b;
    return *result < a;
#endif
}

bool ckd_sub_uint(unsigned int *result, unsigned int a, unsigned int b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_sub_overflow(a, b, result);
#else
    *result = a - b;
    return b > a;
#endif
}

bool ckd_mul_uint(unsigned int *result, unsigned int a, unsigned int b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_mul_overflow(a, b, result);
#else
    *result = a * b;
    if (a == 0 || b == 0) return false;
    return a > UINT_MAX / b;
#endif
}

/* Unsigned long operations */
bool ckd_add_ulong(unsigned long *result, unsigned long a, unsigned long b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_add_overflow(a, b, result);
#else
    *result = a + b;
    return *result < a;
#endif
}

bool ckd_sub_ulong(unsigned long *result, unsigned long a, unsigned long b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_sub_overflow(a, b, result);
#else
    *result = a - b;
    return b > a;
#endif
}

bool ckd_mul_ulong(unsigned long *result, unsigned long a, unsigned long b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_mul_overflow(a, b, result);
#else
    *result = a * b;
    if (a == 0 || b == 0) return false;
    return a > ULONG_MAX / b;
#endif
}

/* Unsigned long long operations */
bool ckd_add_ullong(unsigned long long *result, unsigned long long a, unsigned long long b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_add_overflow(a, b, result);
#else
    *result = a + b;
    return *result < a;
#endif
}

bool ckd_sub_ullong(unsigned long long *result, unsigned long long a, unsigned long long b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_sub_overflow(a, b, result);
#else
    *result = a - b;
    return b > a;
#endif
}

bool ckd_mul_ullong(unsigned long long *result, unsigned long long a, unsigned long long b) {
#ifdef HAS_BUILTIN_OVERFLOW
    return __builtin_mul_overflow(a, b, result);
#else
    *result = a * b;
    if (a == 0 || b == 0) return false;
    return a > ULLONG_MAX / b;
#endif
}
