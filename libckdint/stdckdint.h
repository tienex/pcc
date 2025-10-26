/*
 * C23 stdckdint.h - Checked integer arithmetic
 * Portable implementation for systems lacking C23 support
 */

#ifndef _STDCKDINT_H_
#define _STDCKDINT_H_

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Checked integer arithmetic operations
 * Returns true if overflow occurred, false otherwise
 * Result is stored in *result even if overflow occurs (wraps around)
 */

/* Addition with overflow checking */
#define ckd_add(result, a, b) _Generic((result), \
    int *: ckd_add_int, \
    long *: ckd_add_long, \
    long long *: ckd_add_llong, \
    unsigned int *: ckd_add_uint, \
    unsigned long *: ckd_add_ulong, \
    unsigned long long *: ckd_add_ullong \
)(result, a, b)

/* Subtraction with overflow checking */
#define ckd_sub(result, a, b) _Generic((result), \
    int *: ckd_sub_int, \
    long *: ckd_sub_long, \
    long long *: ckd_sub_llong, \
    unsigned int *: ckd_sub_uint, \
    unsigned long *: ckd_sub_ulong, \
    unsigned long long *: ckd_sub_ullong \
)(result, a, b)

/* Multiplication with overflow checking */
#define ckd_mul(result, a, b) _Generic((result), \
    int *: ckd_mul_int, \
    long *: ckd_mul_long, \
    long long *: ckd_mul_llong, \
    unsigned int *: ckd_mul_uint, \
    unsigned long *: ckd_mul_ulong, \
    unsigned long long *: ckd_mul_ullong \
)(result, a, b)

/* Signed int operations */
bool ckd_add_int(int *result, int a, int b);
bool ckd_sub_int(int *result, int a, int b);
bool ckd_mul_int(int *result, int a, int b);

/* Signed long operations */
bool ckd_add_long(long *result, long a, long b);
bool ckd_sub_long(long *result, long a, long b);
bool ckd_mul_long(long *result, long a, long b);

/* Signed long long operations */
bool ckd_add_llong(long long *result, long long a, long long b);
bool ckd_sub_llong(long long *result, long long a, long long b);
bool ckd_mul_llong(long long *result, long long a, long long b);

/* Unsigned int operations */
bool ckd_add_uint(unsigned int *result, unsigned int a, unsigned int b);
bool ckd_sub_uint(unsigned int *result, unsigned int a, unsigned int b);
bool ckd_mul_uint(unsigned int *result, unsigned int a, unsigned int b);

/* Unsigned long operations */
bool ckd_add_ulong(unsigned long *result, unsigned long a, unsigned long b);
bool ckd_sub_ulong(unsigned long *result, unsigned long a, unsigned long b);
bool ckd_mul_ulong(unsigned long *result, unsigned long a, unsigned long b);

/* Unsigned long long operations */
bool ckd_add_ullong(unsigned long long *result, unsigned long long a, unsigned long long b);
bool ckd_sub_ullong(unsigned long long *result, unsigned long long a, unsigned long long b);
bool ckd_mul_ullong(unsigned long long *result, unsigned long long a, unsigned long long b);

#ifdef __cplusplus
}
#endif

#endif /* _STDCKDINT_H_ */
