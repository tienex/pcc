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
 * float.h - Floating-point limits
 * Compiler-provided header for PCC
 */

#ifndef _FLOAT_H
#define _FLOAT_H

/* Radix of exponent representation */
#define FLT_RADIX		__FLT_RADIX__

/* Number of base-FLT_RADIX digits in the significand */
#define FLT_MANT_DIG		__FLT_MANT_DIG__
#define DBL_MANT_DIG		__DBL_MANT_DIG__
#define LDBL_MANT_DIG		__LDBL_MANT_DIG__

/* Number of decimal digits of precision */
#define FLT_DIG			__FLT_DIG__
#define DBL_DIG			__DBL_DIG__
#define LDBL_DIG		__LDBL_DIG__

/* Minimum negative integer such that FLT_RADIX raised to that power minus 1 is normalized */
#define FLT_MIN_EXP		__FLT_MIN_EXP__
#define DBL_MIN_EXP		__DBL_MIN_EXP__
#define LDBL_MIN_EXP		__LDBL_MIN_EXP__

/* Minimum negative integer such that 10 raised to that power is normalized */
#define FLT_MIN_10_EXP		__FLT_MIN_10_EXP__
#define DBL_MIN_10_EXP		__DBL_MIN_10_EXP__
#define LDBL_MIN_10_EXP		__LDBL_MIN_10_EXP__

/* Maximum integer such that FLT_RADIX raised to that power minus 1 is representable */
#define FLT_MAX_EXP		__FLT_MAX_EXP__
#define DBL_MAX_EXP		__DBL_MAX_EXP__
#define LDBL_MAX_EXP		__LDBL_MAX_EXP__

/* Maximum integer such that 10 raised to that power is representable */
#define FLT_MAX_10_EXP		__FLT_MAX_10_EXP__
#define DBL_MAX_10_EXP		__DBL_MAX_10_EXP__
#define LDBL_MAX_10_EXP		__LDBL_MAX_10_EXP__

/* Maximum representable finite floating-point number */
#define FLT_MAX			__FLT_MAX__
#define DBL_MAX			__DBL_MAX__
#define LDBL_MAX		__LDBL_MAX__

/* Difference between 1.0 and the minimum float greater than 1.0 */
#define FLT_EPSILON		__FLT_EPSILON__
#define DBL_EPSILON		__DBL_EPSILON__
#define LDBL_EPSILON		__LDBL_EPSILON__

/* Minimum normalized positive floating-point number */
#define FLT_MIN			__FLT_MIN__
#define DBL_MIN			__DBL_MIN__
#define LDBL_MIN		__LDBL_MIN__

/* Floating-point evaluation method */
#define FLT_EVAL_METHOD		__FLT_EVAL_METHOD__

#endif /* _FLOAT_H */
