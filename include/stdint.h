/*	$Id$	*/
/*
 * Copyright (c) 2004 Anders Magnusson (ragge@ludd.ltu.se).
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * stdint.h - Integer types (C99)
 * Compiler-provided header for PCC
 */

#ifndef _STDINT_H
#define _STDINT_H

/* Exact-width integer types */
typedef signed char             int8_t;
typedef short                   int16_t;
typedef int                     int32_t;
typedef long long               int64_t;

typedef unsigned char           uint8_t;
typedef unsigned short          uint16_t;
typedef unsigned int            uint32_t;
typedef unsigned long long      uint64_t;

/* Minimum-width integer types */
typedef int8_t                  int_least8_t;
typedef int16_t                 int_least16_t;
typedef int32_t                 int_least32_t;
typedef int64_t                 int_least64_t;

typedef uint8_t                 uint_least8_t;
typedef uint16_t                uint_least16_t;
typedef uint32_t                uint_least32_t;
typedef uint64_t                uint_least64_t;

/* Fastest minimum-width integer types */
typedef int8_t                  int_fast8_t;
typedef int16_t                 int_fast16_t;
typedef int32_t                 int_fast32_t;
typedef int64_t                 int_fast64_t;

typedef uint8_t                 uint_fast8_t;
typedef uint16_t                uint_fast16_t;
typedef uint32_t                uint_fast32_t;
typedef uint64_t                uint_fast64_t;

/* Integer types capable of holding object pointers */
#if __SIZEOF_POINTER__ == 8
typedef long                    intptr_t;
typedef unsigned long           uintptr_t;
#else
typedef int                     intptr_t;
typedef unsigned int            uintptr_t;
#endif

/* Greatest-width integer types */
typedef long long               intmax_t;
typedef unsigned long long      uintmax_t;

/* Limits of exact-width integer types */
#define INT8_MIN                (-128)
#define INT16_MIN               (-32767-1)
#define INT32_MIN               (-2147483647-1)
#define INT64_MIN               (-9223372036854775807LL-1)

#define INT8_MAX                (127)
#define INT16_MAX               (32767)
#define INT32_MAX               (2147483647)
#define INT64_MAX               (9223372036854775807LL)

#define UINT8_MAX               (255)
#define UINT16_MAX              (65535)
#define UINT32_MAX              (4294967295U)
#define UINT64_MAX              (18446744073709551615ULL)

/* Limits of minimum-width integer types */
#define INT_LEAST8_MIN          INT8_MIN
#define INT_LEAST16_MIN         INT16_MIN
#define INT_LEAST32_MIN         INT32_MIN
#define INT_LEAST64_MIN         INT64_MIN

#define INT_LEAST8_MAX          INT8_MAX
#define INT_LEAST16_MAX         INT16_MAX
#define INT_LEAST32_MAX         INT32_MAX
#define INT_LEAST64_MAX         INT64_MAX

#define UINT_LEAST8_MAX         UINT8_MAX
#define UINT_LEAST16_MAX        UINT16_MAX
#define UINT_LEAST32_MAX        UINT32_MAX
#define UINT_LEAST64_MAX        UINT64_MAX

/* Limits of fastest minimum-width integer types */
#define INT_FAST8_MIN           INT8_MIN
#define INT_FAST16_MIN          INT16_MIN
#define INT_FAST32_MIN          INT32_MIN
#define INT_FAST64_MIN          INT64_MIN

#define INT_FAST8_MAX           INT8_MAX
#define INT_FAST16_MAX          INT16_MAX
#define INT_FAST32_MAX          INT32_MAX
#define INT_FAST64_MAX          INT64_MAX

#define UINT_FAST8_MAX          UINT8_MAX
#define UINT_FAST16_MAX         UINT16_MAX
#define UINT_FAST32_MAX         UINT32_MAX
#define UINT_FAST64_MAX         UINT64_MAX

/* Limits of integer types capable of holding object pointers */
#if __SIZEOF_POINTER__ == 8
#define INTPTR_MIN              INT64_MIN
#define INTPTR_MAX              INT64_MAX
#define UINTPTR_MAX             UINT64_MAX
#else
#define INTPTR_MIN              INT32_MIN
#define INTPTR_MAX              INT32_MAX
#define UINTPTR_MAX             UINT32_MAX
#endif

/* Limits of greatest-width integer types */
#define INTMAX_MIN              INT64_MIN
#define INTMAX_MAX              INT64_MAX
#define UINTMAX_MAX             UINT64_MAX

/* Limits of other integer types */
#if __SIZEOF_POINTER__ == 8
#define PTRDIFF_MIN             INT64_MIN
#define PTRDIFF_MAX             INT64_MAX
#define SIZE_MAX                UINT64_MAX
#else
#define PTRDIFF_MIN             INT32_MIN
#define PTRDIFF_MAX             INT32_MAX
#define SIZE_MAX                UINT32_MAX
#endif

#define SIG_ATOMIC_MIN          INT32_MIN
#define SIG_ATOMIC_MAX          INT32_MAX

#define WCHAR_MIN               __WCHAR_MIN__
#define WCHAR_MAX               __WCHAR_MAX__

#define WINT_MIN                0
#define WINT_MAX                __WINT_MAX__

/* Macros for integer constants */
#define INT8_C(c)               c
#define INT16_C(c)              c
#define INT32_C(c)              c
#define INT64_C(c)              c ## LL

#define UINT8_C(c)              c ## U
#define UINT16_C(c)             c ## U
#define UINT32_C(c)             c ## U
#define UINT64_C(c)             c ## ULL

#define INTMAX_C(c)             c ## LL
#define UINTMAX_C(c)            c ## ULL

#endif /* _STDINT_H */
