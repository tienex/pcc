/*	$Id$	*/
/*
 * Copyright (c) 2024 PCC Contributors.
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
 * bits/floatn.h - GCC compatibility header for extended floating point types
 * Compiler-provided compatibility header for PCC
 *
 * This header provides compatibility definitions for GCC's extended floating
 * point types that PCC doesn't natively support. We map them to standard types.
 */

#ifndef _BITS_FLOATN_H
#define _BITS_FLOATN_H

/* PCC doesn't support __float128, so we don't define _Float128 */
#define __HAVE_FLOAT128 0
#define __HAVE_DISTINCT_FLOAT128 0

#define __HAVE_FLOAT64X 0
#define __HAVE_FLOAT64X_LONG_DOUBLE 0

#define __HAVE_FLOAT16 0
#define __HAVE_FLOAT32 0
#define __HAVE_FLOAT64 0
#define __HAVE_FLOAT32X 0
#define __HAVE_FLOAT128X 0

/* Include system floatn.h if it exists and defines additional macros */
#if defined(__linux__) && !defined(_PCC_FLOATN_GUARD)
#define _PCC_FLOATN_GUARD
/* We've provided the essential macros, system header can add more */
#endif

#endif /* _BITS_FLOATN_H */
