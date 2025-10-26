/*
 * C11 Unicode character utilities - uchar.h
 * Portable implementation for systems lacking C11 Unicode support
 */

#ifndef _UCHAR_H_
#define _UCHAR_H_

#include <stddef.h>
#include <wchar.h>

#ifdef __cplusplus
extern "C" {
#endif

/* C11 Unicode character types */
#if !defined(__cplusplus) || __cplusplus < 201103L
typedef uint_least16_t char16_t;
typedef uint_least32_t char32_t;
#endif

#ifndef __STDC_UTF_16__
#define __STDC_UTF_16__ 1
#endif

#ifndef __STDC_UTF_32__
#define __STDC_UTF_32__ 1
#endif

/* C11 Unicode conversion functions */

/*
 * Convert a multibyte character to a 16-bit wide character (UTF-16)
 */
size_t mbrtoc16(char16_t *restrict pc16, const char *restrict s, size_t n,
                mbstate_t *restrict ps);

/*
 * Convert a 16-bit wide character (UTF-16) to a multibyte character
 */
size_t c16rtomb(char *restrict s, char16_t c16, mbstate_t *restrict ps);

/*
 * Convert a multibyte character to a 32-bit wide character (UTF-32)
 */
size_t mbrtoc32(char32_t *restrict pc32, const char *restrict s, size_t n,
                mbstate_t *restrict ps);

/*
 * Convert a 32-bit wide character (UTF-32) to a multibyte character
 */
size_t c32rtomb(char *restrict s, char32_t c32, mbstate_t *restrict ps);

#ifdef __cplusplus
}
#endif

#endif /* _UCHAR_H_ */
