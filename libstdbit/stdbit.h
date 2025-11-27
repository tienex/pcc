/*
 * C23 stdbit.h - Bit and byte utilities
 * Portable implementation for systems lacking C23 support
 */

#ifndef _STDBIT_H_
#define _STDBIT_H_

#include <stdint.h>
#include <limits.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Endianness macros */
#define __STDC_ENDIAN_LITTLE__ 1234
#define __STDC_ENDIAN_BIG__    4321
#define __STDC_ENDIAN_NATIVE__ __BYTE_ORDER__

/* Leading zeros - count leading zero bits */
unsigned int stdc_leading_zeros_uc(unsigned char value);
unsigned int stdc_leading_zeros_us(unsigned short value);
unsigned int stdc_leading_zeros_ui(unsigned int value);
unsigned int stdc_leading_zeros_ul(unsigned long value);
unsigned int stdc_leading_zeros_ull(unsigned long long value);

/* Leading ones - count leading one bits */
unsigned int stdc_leading_ones_uc(unsigned char value);
unsigned int stdc_leading_ones_us(unsigned short value);
unsigned int stdc_leading_ones_ui(unsigned int value);
unsigned int stdc_leading_ones_ul(unsigned long value);
unsigned int stdc_leading_ones_ull(unsigned long long value);

/* Trailing zeros - count trailing zero bits */
unsigned int stdc_trailing_zeros_uc(unsigned char value);
unsigned int stdc_trailing_zeros_us(unsigned short value);
unsigned int stdc_trailing_zeros_ui(unsigned int value);
unsigned int stdc_trailing_zeros_ul(unsigned long value);
unsigned int stdc_trailing_zeros_ull(unsigned long long value);

/* Trailing ones - count trailing one bits */
unsigned int stdc_trailing_ones_uc(unsigned char value);
unsigned int stdc_trailing_ones_us(unsigned short value);
unsigned int stdc_trailing_ones_ui(unsigned int value);
unsigned int stdc_trailing_ones_ul(unsigned long value);
unsigned int stdc_trailing_ones_ull(unsigned long long value);

/* First leading zero - position of first leading zero bit */
unsigned int stdc_first_leading_zero_uc(unsigned char value);
unsigned int stdc_first_leading_zero_us(unsigned short value);
unsigned int stdc_first_leading_zero_ui(unsigned int value);
unsigned int stdc_first_leading_zero_ul(unsigned long value);
unsigned int stdc_first_leading_zero_ull(unsigned long long value);

/* First leading one - position of first leading one bit */
unsigned int stdc_first_leading_one_uc(unsigned char value);
unsigned int stdc_first_leading_one_us(unsigned short value);
unsigned int stdc_first_leading_one_ui(unsigned int value);
unsigned int stdc_first_leading_one_ul(unsigned long value);
unsigned int stdc_first_leading_one_ull(unsigned long long value);

/* First trailing zero - position of first trailing zero bit */
unsigned int stdc_first_trailing_zero_uc(unsigned char value);
unsigned int stdc_first_trailing_zero_us(unsigned short value);
unsigned int stdc_first_trailing_zero_ui(unsigned int value);
unsigned int stdc_first_trailing_zero_ul(unsigned long value);
unsigned int stdc_first_trailing_zero_ull(unsigned long long value);

/* First trailing one - position of first trailing one bit */
unsigned int stdc_first_trailing_one_uc(unsigned char value);
unsigned int stdc_first_trailing_one_us(unsigned short value);
unsigned int stdc_first_trailing_one_ui(unsigned int value);
unsigned int stdc_first_trailing_one_ul(unsigned long value);
unsigned int stdc_first_trailing_one_ull(unsigned long long value);

/* Count zeros - count zero bits */
unsigned int stdc_count_zeros_uc(unsigned char value);
unsigned int stdc_count_zeros_us(unsigned short value);
unsigned int stdc_count_zeros_ui(unsigned int value);
unsigned int stdc_count_zeros_ul(unsigned long value);
unsigned int stdc_count_zeros_ull(unsigned long long value);

/* Count ones - count one bits (popcount) */
unsigned int stdc_count_ones_uc(unsigned char value);
unsigned int stdc_count_ones_us(unsigned short value);
unsigned int stdc_count_ones_ui(unsigned int value);
unsigned int stdc_count_ones_ul(unsigned long value);
unsigned int stdc_count_ones_ull(unsigned long long value);

/* Has single bit - check if value is power of 2 */
_Bool stdc_has_single_bit_uc(unsigned char value);
_Bool stdc_has_single_bit_us(unsigned short value);
_Bool stdc_has_single_bit_ui(unsigned int value);
_Bool stdc_has_single_bit_ul(unsigned long value);
_Bool stdc_has_single_bit_ull(unsigned long long value);

/* Bit width - number of bits needed to represent value */
unsigned int stdc_bit_width_uc(unsigned char value);
unsigned int stdc_bit_width_us(unsigned short value);
unsigned int stdc_bit_width_ui(unsigned int value);
unsigned int stdc_bit_width_ul(unsigned long value);
unsigned int stdc_bit_width_ull(unsigned long long value);

/* Bit floor - largest power of 2 not greater than value */
unsigned char stdc_bit_floor_uc(unsigned char value);
unsigned short stdc_bit_floor_us(unsigned short value);
unsigned int stdc_bit_floor_ui(unsigned int value);
unsigned long stdc_bit_floor_ul(unsigned long value);
unsigned long long stdc_bit_floor_ull(unsigned long long value);

/* Bit ceil - smallest power of 2 not less than value */
unsigned char stdc_bit_ceil_uc(unsigned char value);
unsigned short stdc_bit_ceil_us(unsigned short value);
unsigned int stdc_bit_ceil_ui(unsigned int value);
unsigned long stdc_bit_ceil_ul(unsigned long value);
unsigned long long stdc_bit_ceil_ull(unsigned long long value);

/* Type-generic macros */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
#define stdc_leading_zeros(x) _Generic((x), \
    unsigned char: stdc_leading_zeros_uc, \
    unsigned short: stdc_leading_zeros_us, \
    unsigned int: stdc_leading_zeros_ui, \
    unsigned long: stdc_leading_zeros_ul, \
    unsigned long long: stdc_leading_zeros_ull)(x)

#define stdc_leading_ones(x) _Generic((x), \
    unsigned char: stdc_leading_ones_uc, \
    unsigned short: stdc_leading_ones_us, \
    unsigned int: stdc_leading_ones_ui, \
    unsigned long: stdc_leading_ones_ul, \
    unsigned long long: stdc_leading_ones_ull)(x)

#define stdc_trailing_zeros(x) _Generic((x), \
    unsigned char: stdc_trailing_zeros_uc, \
    unsigned short: stdc_trailing_zeros_us, \
    unsigned int: stdc_trailing_zeros_ui, \
    unsigned long: stdc_trailing_zeros_ul, \
    unsigned long long: stdc_trailing_zeros_ull)(x)

#define stdc_trailing_ones(x) _Generic((x), \
    unsigned char: stdc_trailing_ones_uc, \
    unsigned short: stdc_trailing_ones_us, \
    unsigned int: stdc_trailing_ones_ui, \
    unsigned long: stdc_trailing_ones_ul, \
    unsigned long long: stdc_trailing_ones_ull)(x)

#define stdc_count_zeros(x) _Generic((x), \
    unsigned char: stdc_count_zeros_uc, \
    unsigned short: stdc_count_zeros_us, \
    unsigned int: stdc_count_zeros_ui, \
    unsigned long: stdc_count_zeros_ul, \
    unsigned long long: stdc_count_zeros_ull)(x)

#define stdc_count_ones(x) _Generic((x), \
    unsigned char: stdc_count_ones_uc, \
    unsigned short: stdc_count_ones_us, \
    unsigned int: stdc_count_ones_ui, \
    unsigned long: stdc_count_ones_ul, \
    unsigned long long: stdc_count_ones_ull)(x)

#define stdc_has_single_bit(x) _Generic((x), \
    unsigned char: stdc_has_single_bit_uc, \
    unsigned short: stdc_has_single_bit_us, \
    unsigned int: stdc_has_single_bit_ui, \
    unsigned long: stdc_has_single_bit_ul, \
    unsigned long long: stdc_has_single_bit_ull)(x)

#define stdc_bit_width(x) _Generic((x), \
    unsigned char: stdc_bit_width_uc, \
    unsigned short: stdc_bit_width_us, \
    unsigned int: stdc_bit_width_ui, \
    unsigned long: stdc_bit_width_ul, \
    unsigned long long: stdc_bit_width_ull)(x)

#define stdc_bit_floor(x) _Generic((x), \
    unsigned char: stdc_bit_floor_uc, \
    unsigned short: stdc_bit_floor_us, \
    unsigned int: stdc_bit_floor_ui, \
    unsigned long: stdc_bit_floor_ul, \
    unsigned long long: stdc_bit_floor_ull)(x)

#define stdc_bit_ceil(x) _Generic((x), \
    unsigned char: stdc_bit_ceil_uc, \
    unsigned short: stdc_bit_ceil_us, \
    unsigned int: stdc_bit_ceil_ui, \
    unsigned long: stdc_bit_ceil_ul, \
    unsigned long long: stdc_bit_ceil_ull)(x)
#endif /* C11 generics */

#ifdef __cplusplus
}
#endif

#endif /* _STDBIT_H_ */
