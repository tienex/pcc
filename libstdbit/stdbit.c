/*
 * C23 stdbit.h implementation
 * Portable bit manipulation functions
 */

#include "stdbit.h"

/* Use compiler built-ins if available, otherwise use portable implementations */
#if defined(__GNUC__) || defined(__clang__)
#define HAS_BUILTIN_CLZ
#define HAS_BUILTIN_CTZ
#define HAS_BUILTIN_POPCOUNT
#endif

/* Leading zeros */
unsigned int stdc_leading_zeros_uc(unsigned char value) {
    if (value == 0) return CHAR_BIT;
#ifdef HAS_BUILTIN_CLZ
    return __builtin_clz((unsigned int)value) - (sizeof(unsigned int) * CHAR_BIT - CHAR_BIT);
#else
    unsigned int count = 0;
    unsigned char mask = (unsigned char)(1U << (CHAR_BIT - 1));
    while (mask && !(value & mask)) {
        count++;
        mask >>= 1;
    }
    return count;
#endif
}

unsigned int stdc_leading_zeros_us(unsigned short value) {
    if (value == 0) return sizeof(unsigned short) * CHAR_BIT;
#ifdef HAS_BUILTIN_CLZ
    return __builtin_clz((unsigned int)value) - (sizeof(unsigned int) * CHAR_BIT - sizeof(unsigned short) * CHAR_BIT);
#else
    unsigned int count = 0;
    unsigned short mask = (unsigned short)(1U << (sizeof(unsigned short) * CHAR_BIT - 1));
    while (mask && !(value & mask)) {
        count++;
        mask >>= 1;
    }
    return count;
#endif
}

unsigned int stdc_leading_zeros_ui(unsigned int value) {
    if (value == 0) return sizeof(unsigned int) * CHAR_BIT;
#ifdef HAS_BUILTIN_CLZ
    return __builtin_clz(value);
#else
    unsigned int count = 0;
    unsigned int mask = 1U << (sizeof(unsigned int) * CHAR_BIT - 1);
    while (mask && !(value & mask)) {
        count++;
        mask >>= 1;
    }
    return count;
#endif
}

unsigned int stdc_leading_zeros_ul(unsigned long value) {
    if (value == 0) return sizeof(unsigned long) * CHAR_BIT;
#ifdef HAS_BUILTIN_CLZ
    return __builtin_clzl(value);
#else
    unsigned int count = 0;
    unsigned long mask = 1UL << (sizeof(unsigned long) * CHAR_BIT - 1);
    while (mask && !(value & mask)) {
        count++;
        mask >>= 1;
    }
    return count;
#endif
}

unsigned int stdc_leading_zeros_ull(unsigned long long value) {
    if (value == 0) return sizeof(unsigned long long) * CHAR_BIT;
#ifdef HAS_BUILTIN_CLZ
    return __builtin_clzll(value);
#else
    unsigned int count = 0;
    unsigned long long mask = 1ULL << (sizeof(unsigned long long) * CHAR_BIT - 1);
    while (mask && !(value & mask)) {
        count++;
        mask >>= 1;
    }
    return count;
#endif
}

/* Leading ones */
unsigned int stdc_leading_ones_uc(unsigned char value) {
    return stdc_leading_zeros_uc(~value);
}

unsigned int stdc_leading_ones_us(unsigned short value) {
    return stdc_leading_zeros_us((unsigned short)~value);
}

unsigned int stdc_leading_ones_ui(unsigned int value) {
    return stdc_leading_zeros_ui(~value);
}

unsigned int stdc_leading_ones_ul(unsigned long value) {
    return stdc_leading_zeros_ul(~value);
}

unsigned int stdc_leading_ones_ull(unsigned long long value) {
    return stdc_leading_zeros_ull(~value);
}

/* Trailing zeros */
unsigned int stdc_trailing_zeros_uc(unsigned char value) {
    if (value == 0) return CHAR_BIT;
#ifdef HAS_BUILTIN_CTZ
    return __builtin_ctz((unsigned int)value);
#else
    unsigned int count = 0;
    while (value && !(value & 1)) {
        count++;
        value >>= 1;
    }
    return count;
#endif
}

unsigned int stdc_trailing_zeros_us(unsigned short value) {
    if (value == 0) return sizeof(unsigned short) * CHAR_BIT;
#ifdef HAS_BUILTIN_CTZ
    return __builtin_ctz((unsigned int)value);
#else
    unsigned int count = 0;
    while (value && !(value & 1)) {
        count++;
        value >>= 1;
    }
    return count;
#endif
}

unsigned int stdc_trailing_zeros_ui(unsigned int value) {
    if (value == 0) return sizeof(unsigned int) * CHAR_BIT;
#ifdef HAS_BUILTIN_CTZ
    return __builtin_ctz(value);
#else
    unsigned int count = 0;
    while (value && !(value & 1)) {
        count++;
        value >>= 1;
    }
    return count;
#endif
}

unsigned int stdc_trailing_zeros_ul(unsigned long value) {
    if (value == 0) return sizeof(unsigned long) * CHAR_BIT;
#ifdef HAS_BUILTIN_CTZ
    return __builtin_ctzl(value);
#else
    unsigned int count = 0;
    while (value && !(value & 1)) {
        count++;
        value >>= 1;
    }
    return count;
#endif
}

unsigned int stdc_trailing_zeros_ull(unsigned long long value) {
    if (value == 0) return sizeof(unsigned long long) * CHAR_BIT;
#ifdef HAS_BUILTIN_CTZ
    return __builtin_ctzll(value);
#else
    unsigned int count = 0;
    while (value && !(value & 1)) {
        count++;
        value >>= 1;
    }
    return count;
#endif
}

/* Trailing ones */
unsigned int stdc_trailing_ones_uc(unsigned char value) {
    return stdc_trailing_zeros_uc((unsigned char)~value);
}

unsigned int stdc_trailing_ones_us(unsigned short value) {
    return stdc_trailing_zeros_us((unsigned short)~value);
}

unsigned int stdc_trailing_ones_ui(unsigned int value) {
    return stdc_trailing_zeros_ui(~value);
}

unsigned int stdc_trailing_ones_ul(unsigned long value) {
    return stdc_trailing_zeros_ul(~value);
}

unsigned int stdc_trailing_ones_ull(unsigned long long value) {
    return stdc_trailing_zeros_ull(~value);
}

/* First leading zero */
unsigned int stdc_first_leading_zero_uc(unsigned char value) {
    unsigned int lz = stdc_leading_ones_uc(value);
    return (lz < CHAR_BIT) ? lz : 0;
}

unsigned int stdc_first_leading_zero_us(unsigned short value) {
    unsigned int lz = stdc_leading_ones_us(value);
    return (lz < sizeof(unsigned short) * CHAR_BIT) ? lz : 0;
}

unsigned int stdc_first_leading_zero_ui(unsigned int value) {
    unsigned int lz = stdc_leading_ones_ui(value);
    return (lz < sizeof(unsigned int) * CHAR_BIT) ? lz : 0;
}

unsigned int stdc_first_leading_zero_ul(unsigned long value) {
    unsigned int lz = stdc_leading_ones_ul(value);
    return (lz < sizeof(unsigned long) * CHAR_BIT) ? lz : 0;
}

unsigned int stdc_first_leading_zero_ull(unsigned long long value) {
    unsigned int lz = stdc_leading_ones_ull(value);
    return (lz < sizeof(unsigned long long) * CHAR_BIT) ? lz : 0;
}

/* First leading one */
unsigned int stdc_first_leading_one_uc(unsigned char value) {
    unsigned int lz = stdc_leading_zeros_uc(value);
    return (lz < CHAR_BIT) ? lz : 0;
}

unsigned int stdc_first_leading_one_us(unsigned short value) {
    unsigned int lz = stdc_leading_zeros_us(value);
    return (lz < sizeof(unsigned short) * CHAR_BIT) ? lz : 0;
}

unsigned int stdc_first_leading_one_ui(unsigned int value) {
    unsigned int lz = stdc_leading_zeros_ui(value);
    return (lz < sizeof(unsigned int) * CHAR_BIT) ? lz : 0;
}

unsigned int stdc_first_leading_one_ul(unsigned long value) {
    unsigned int lz = stdc_leading_zeros_ul(value);
    return (lz < sizeof(unsigned long) * CHAR_BIT) ? lz : 0;
}

unsigned int stdc_first_leading_one_ull(unsigned long long value) {
    unsigned int lz = stdc_leading_zeros_ull(value);
    return (lz < sizeof(unsigned long long) * CHAR_BIT) ? lz : 0;
}

/* First trailing zero */
unsigned int stdc_first_trailing_zero_uc(unsigned char value) {
    unsigned int tz = stdc_trailing_ones_uc(value);
    return (tz < CHAR_BIT) ? tz : 0;
}

unsigned int stdc_first_trailing_zero_us(unsigned short value) {
    unsigned int tz = stdc_trailing_ones_us(value);
    return (tz < sizeof(unsigned short) * CHAR_BIT) ? tz : 0;
}

unsigned int stdc_first_trailing_zero_ui(unsigned int value) {
    unsigned int tz = stdc_trailing_ones_ui(value);
    return (tz < sizeof(unsigned int) * CHAR_BIT) ? tz : 0;
}

unsigned int stdc_first_trailing_zero_ul(unsigned long value) {
    unsigned int tz = stdc_trailing_ones_ul(value);
    return (tz < sizeof(unsigned long) * CHAR_BIT) ? tz : 0;
}

unsigned int stdc_first_trailing_zero_ull(unsigned long long value) {
    unsigned int tz = stdc_trailing_ones_ull(value);
    return (tz < sizeof(unsigned long long) * CHAR_BIT) ? tz : 0;
}

/* First trailing one */
unsigned int stdc_first_trailing_one_uc(unsigned char value) {
    unsigned int tz = stdc_trailing_zeros_uc(value);
    return (tz < CHAR_BIT) ? tz : 0;
}

unsigned int stdc_first_trailing_one_us(unsigned short value) {
    unsigned int tz = stdc_trailing_zeros_us(value);
    return (tz < sizeof(unsigned short) * CHAR_BIT) ? tz : 0;
}

unsigned int stdc_first_trailing_one_ui(unsigned int value) {
    unsigned int tz = stdc_trailing_zeros_ui(value);
    return (tz < sizeof(unsigned int) * CHAR_BIT) ? tz : 0;
}

unsigned int stdc_first_trailing_one_ul(unsigned long value) {
    unsigned int tz = stdc_trailing_zeros_ul(value);
    return (tz < sizeof(unsigned long) * CHAR_BIT) ? tz : 0;
}

unsigned int stdc_first_trailing_one_ull(unsigned long long value) {
    unsigned int tz = stdc_trailing_zeros_ull(value);
    return (tz < sizeof(unsigned long long) * CHAR_BIT) ? tz : 0;
}

/* Count zeros */
unsigned int stdc_count_zeros_uc(unsigned char value) {
    return CHAR_BIT - stdc_count_ones_uc(value);
}

unsigned int stdc_count_zeros_us(unsigned short value) {
    return sizeof(unsigned short) * CHAR_BIT - stdc_count_ones_us(value);
}

unsigned int stdc_count_zeros_ui(unsigned int value) {
    return sizeof(unsigned int) * CHAR_BIT - stdc_count_ones_ui(value);
}

unsigned int stdc_count_zeros_ul(unsigned long value) {
    return sizeof(unsigned long) * CHAR_BIT - stdc_count_ones_ul(value);
}

unsigned int stdc_count_zeros_ull(unsigned long long value) {
    return sizeof(unsigned long long) * CHAR_BIT - stdc_count_ones_ull(value);
}

/* Count ones (popcount) */
unsigned int stdc_count_ones_uc(unsigned char value) {
#ifdef HAS_BUILTIN_POPCOUNT
    return __builtin_popcount((unsigned int)value);
#else
    unsigned int count = 0;
    while (value) {
        count += value & 1;
        value >>= 1;
    }
    return count;
#endif
}

unsigned int stdc_count_ones_us(unsigned short value) {
#ifdef HAS_BUILTIN_POPCOUNT
    return __builtin_popcount((unsigned int)value);
#else
    unsigned int count = 0;
    while (value) {
        count += value & 1;
        value >>= 1;
    }
    return count;
#endif
}

unsigned int stdc_count_ones_ui(unsigned int value) {
#ifdef HAS_BUILTIN_POPCOUNT
    return __builtin_popcount(value);
#else
    unsigned int count = 0;
    while (value) {
        count += value & 1;
        value >>= 1;
    }
    return count;
#endif
}

unsigned int stdc_count_ones_ul(unsigned long value) {
#ifdef HAS_BUILTIN_POPCOUNT
    return __builtin_popcountl(value);
#else
    unsigned int count = 0;
    while (value) {
        count += value & 1;
        value >>= 1;
    }
    return count;
#endif
}

unsigned int stdc_count_ones_ull(unsigned long long value) {
#ifdef HAS_BUILTIN_POPCOUNT
    return __builtin_popcountll(value);
#else
    unsigned int count = 0;
    while (value) {
        count += value & 1;
        value >>= 1;
    }
    return count;
#endif
}

/* Has single bit (power of 2 check) */
_Bool stdc_has_single_bit_uc(unsigned char value) {
    return value != 0 && (value & (value - 1)) == 0;
}

_Bool stdc_has_single_bit_us(unsigned short value) {
    return value != 0 && (value & (value - 1)) == 0;
}

_Bool stdc_has_single_bit_ui(unsigned int value) {
    return value != 0 && (value & (value - 1)) == 0;
}

_Bool stdc_has_single_bit_ul(unsigned long value) {
    return value != 0 && (value & (value - 1)) == 0;
}

_Bool stdc_has_single_bit_ull(unsigned long long value) {
    return value != 0 && (value & (value - 1)) == 0;
}

/* Bit width - number of bits to represent value */
unsigned int stdc_bit_width_uc(unsigned char value) {
    if (value == 0) return 0;
    return CHAR_BIT - stdc_leading_zeros_uc(value);
}

unsigned int stdc_bit_width_us(unsigned short value) {
    if (value == 0) return 0;
    return sizeof(unsigned short) * CHAR_BIT - stdc_leading_zeros_us(value);
}

unsigned int stdc_bit_width_ui(unsigned int value) {
    if (value == 0) return 0;
    return sizeof(unsigned int) * CHAR_BIT - stdc_leading_zeros_ui(value);
}

unsigned int stdc_bit_width_ul(unsigned long value) {
    if (value == 0) return 0;
    return sizeof(unsigned long) * CHAR_BIT - stdc_leading_zeros_ul(value);
}

unsigned int stdc_bit_width_ull(unsigned long long value) {
    if (value == 0) return 0;
    return sizeof(unsigned long long) * CHAR_BIT - stdc_leading_zeros_ull(value);
}

/* Bit floor - largest power of 2 not greater than value */
unsigned char stdc_bit_floor_uc(unsigned char value) {
    if (value == 0) return 0;
    return (unsigned char)(1U << (stdc_bit_width_uc(value) - 1));
}

unsigned short stdc_bit_floor_us(unsigned short value) {
    if (value == 0) return 0;
    return (unsigned short)(1U << (stdc_bit_width_us(value) - 1));
}

unsigned int stdc_bit_floor_ui(unsigned int value) {
    if (value == 0) return 0;
    return 1U << (stdc_bit_width_ui(value) - 1);
}

unsigned long stdc_bit_floor_ul(unsigned long value) {
    if (value == 0) return 0;
    return 1UL << (stdc_bit_width_ul(value) - 1);
}

unsigned long long stdc_bit_floor_ull(unsigned long long value) {
    if (value == 0) return 0;
    return 1ULL << (stdc_bit_width_ull(value) - 1);
}

/* Bit ceil - smallest power of 2 not less than value */
unsigned char stdc_bit_ceil_uc(unsigned char value) {
    if (value <= 1) return 1;
    if (stdc_has_single_bit_uc(value)) return value;
    unsigned int width = stdc_bit_width_uc(value);
    if (width >= CHAR_BIT) return 0; /* Overflow */
    return (unsigned char)(1U << width);
}

unsigned short stdc_bit_ceil_us(unsigned short value) {
    if (value <= 1) return 1;
    if (stdc_has_single_bit_us(value)) return value;
    unsigned int width = stdc_bit_width_us(value);
    if (width >= sizeof(unsigned short) * CHAR_BIT) return 0; /* Overflow */
    return (unsigned short)(1U << width);
}

unsigned int stdc_bit_ceil_ui(unsigned int value) {
    if (value <= 1) return 1;
    if (stdc_has_single_bit_ui(value)) return value;
    unsigned int width = stdc_bit_width_ui(value);
    if (width >= sizeof(unsigned int) * CHAR_BIT) return 0; /* Overflow */
    return 1U << width;
}

unsigned long stdc_bit_ceil_ul(unsigned long value) {
    if (value <= 1) return 1;
    if (stdc_has_single_bit_ul(value)) return value;
    unsigned int width = stdc_bit_width_ul(value);
    if (width >= sizeof(unsigned long) * CHAR_BIT) return 0; /* Overflow */
    return 1UL << width;
}

unsigned long long stdc_bit_ceil_ull(unsigned long long value) {
    if (value <= 1) return 1;
    if (stdc_has_single_bit_ull(value)) return value;
    unsigned int width = stdc_bit_width_ull(value);
    if (width >= sizeof(unsigned long long) * CHAR_BIT) return 0; /* Overflow */
    return 1ULL << width;
}
