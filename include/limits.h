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
 * limits.h - Implementation limits
 * Compiler-provided header for PCC
 */

#ifndef _LIMITS_H
#define _LIMITS_H

/* Number of bits in a `char' */
#define CHAR_BIT        8

/* Minimum and maximum values a `signed char' can hold */
#define SCHAR_MIN       (-128)
#define SCHAR_MAX       127

/* Maximum value an `unsigned char' can hold */
#define UCHAR_MAX       255

/* Minimum and maximum values a `char' can hold */
#ifdef __CHAR_UNSIGNED__
#define CHAR_MIN        0
#define CHAR_MAX        UCHAR_MAX
#else
#define CHAR_MIN        SCHAR_MIN
#define CHAR_MAX        SCHAR_MAX
#endif

/* Minimum and maximum values a `signed short int' can hold */
#define SHRT_MIN        __SHRT_MAX__
#define SHRT_MAX        __SHRT_MAX__

/* Maximum value an `unsigned short int' can hold */
#define USHRT_MAX       (SHRT_MAX * 2 + 1)

/* Minimum and maximum values a `signed int' can hold */
#define INT_MIN         (-INT_MAX - 1)
#define INT_MAX         __INT_MAX__

/* Maximum value an `unsigned int' can hold */
#define UINT_MAX        (INT_MAX * 2U + 1U)

/* Minimum and maximum values a `signed long int' can hold */
#define LONG_MIN        (-LONG_MAX - 1L)
#define LONG_MAX        __LONG_MAX__

/* Maximum value an `unsigned long int' can hold */
#define ULONG_MAX       (LONG_MAX * 2UL + 1UL)

/* Minimum and maximum values a `signed long long int' can hold */
#define LLONG_MIN       (-LLONG_MAX - 1LL)
#define LLONG_MAX       __LONG_LONG_MAX__

/* Maximum value an `unsigned long long int' can hold */
#define ULLONG_MAX      (LLONG_MAX * 2ULL + 1ULL)

/* Maximum number of bytes in a multibyte character */
#define MB_LEN_MAX      16

#endif /* _LIMITS_H */
