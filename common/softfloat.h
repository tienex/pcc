/*	$Id$	*/

/*
 * Copyright (c) 2015 Anders Magnusson. All rights reserved.
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
 * Definitions for softfloat routines.
 *
 * Floating point numbers will always be stored as the largest supported
 * float type (long double).  This in turn will be stuffed bitwise into
 * an array of short for addressing.
 */
#ifndef CROSS_COMPILING
#define	DEBUGFP	/* compare everything with native fp */
#endif

typedef struct softfloat {
	union {
		unsigned short int fp[(SZLDOUBLE+15)/16];
#ifdef DEBUGFP
		long double debugfp;
#endif
	};
	unsigned long long significand;
	short exponent;
	short kind;
} SF;
typedef SF *SFP;

#define C(x,y) C2(x,y)
#define C2(x,y) x##y

#ifdef USE_IEEEFP_32
#define	IEEEFP_32_RADIX 2
#define IEEEFP_32_DIG 6
#define IEEEFP_32_EPSILON 1.19209290E-07F
#define IEEEFP_32_MAX_10_EXP +38
#define IEEEFP_32_MAX_EXP +128
#define IEEEFP_32_MAX 3.40282347E+38F
#define IEEEFP_32_MIN_10_EXP -37
#define IEEEFP_32_MIN_EXP -125
#define IEEEFP_32_MIN 1.17549435E-38F
#define IEEEFP_32_MANT_DIG 24
#define IEEEFP_32_HAS_SUBNORM 1
#endif
#ifdef USE_IEEEFP_64
#define IEEEFP_64_DIG 15
#define IEEEFP_64_EPSILON 2.22044604925031308085e-16
#define IEEEFP_64_MAX_10_EXP 308
#define IEEEFP_64_MAX_EXP 1024
#define IEEEFP_64_MAX 1.79769313486231570815e+308
#define IEEEFP_64_MIN_10_EXP (-307)
#define IEEEFP_64_MIN_EXP (-1021)
#define IEEEFP_64_MIN 2.22507385850720138309e-308
#define IEEEFP_64_MANT_DIG 53
#endif
#ifdef USE_IEEEFP_X80
#define IEEEFP_X80_DIG 18
#define IEEEFP_X80_EPSILON 1.08420217248550443401e-19L
#define IEEEFP_X80_MAX_10_EXP 4932
#define IEEEFP_X80_MAX_EXP 16384
#define IEEEFP_X80_MAX 1.18973149535723176502e+4932L
#define IEEEFP_X80_MIN_10_EXP (-4931)
#define IEEEFP_X80_MIN_EXP (-16381)
#define IEEEFP_X80_MIN 3.36210314311209350626e-4932L
#define IEEEFP_X80_MANT_DIG 64
#endif
#ifdef IEEEFP_128
#endif

/* Microsoft Binary Format (MBF) constants */
#ifdef USE_MSBFP_32
#define MSBFP_32_RADIX 2
#define MSBFP_32_DIG 6
#define MSBFP_32_MAX_10_EXP +38
#define MSBFP_32_MAX_EXP +127
#define MSBFP_32_MIN_10_EXP -38
#define MSBFP_32_MIN_EXP -127
#define MSBFP_32_MANT_DIG 24
#define MSBFP_32_HAS_SUBNORM 0
#endif
#ifdef USE_MSBFP_64
#define MSBFP_64_RADIX 2
#define MSBFP_64_DIG 16
#define MSBFP_64_MAX_10_EXP +38
#define MSBFP_64_MAX_EXP +127
#define MSBFP_64_MIN_10_EXP -38
#define MSBFP_64_MIN_EXP -127
#define MSBFP_64_MANT_DIG 56
#define MSBFP_64_HAS_SUBNORM 0
#endif

/* IBM Hexadecimal Floating Point constants */
#ifdef USE_IBMFP_32
#define IBMFP_32_RADIX 16
#define IBMFP_32_DIG 6
#define IBMFP_32_MAX_10_EXP +75
#define IBMFP_32_MAX_EXP +63
#define IBMFP_32_MIN_10_EXP -78
#define IBMFP_32_MIN_EXP -64
#define IBMFP_32_MANT_DIG 24
#define IBMFP_32_HAS_SUBNORM 0
#endif
#ifdef USE_IBMFP_64
#define IBMFP_64_RADIX 16
#define IBMFP_64_DIG 16
#define IBMFP_64_MAX_10_EXP +75
#define IBMFP_64_MAX_EXP +63
#define IBMFP_64_MIN_10_EXP -78
#define IBMFP_64_MIN_EXP -64
#define IBMFP_64_MANT_DIG 56
#define IBMFP_64_HAS_SUBNORM 0
#endif
#ifdef USE_IBMFP_128
#define IBMFP_128_RADIX 16
#define IBMFP_128_DIG 33
#define IBMFP_128_MAX_10_EXP +75
#define IBMFP_128_MAX_EXP +63
#define IBMFP_128_MIN_10_EXP -78
#define IBMFP_128_MIN_EXP -64
#define IBMFP_128_MANT_DIG 112
#define IBMFP_128_HAS_SUBNORM 0
#endif

/* Cray Floating Point constants */
#ifdef USE_CRAYFP_64
#define CRAYFP_64_RADIX 2
#define CRAYFP_64_DIG 14
#define CRAYFP_64_MAX_10_EXP +2465
#define CRAYFP_64_MAX_EXP +8189
#define CRAYFP_64_MIN_10_EXP -2465
#define CRAYFP_64_MIN_EXP -8189
#define CRAYFP_64_MANT_DIG 48
#define CRAYFP_64_HAS_SUBNORM 0
#endif

/* IEEE 754-2008 binary16 (half precision) constants */
#ifdef USE_IEEEFP_16
#define IEEEFP_16_RADIX 2
#define IEEEFP_16_DIG 3
#define IEEEFP_16_EPSILON 9.76562500E-04F
#define IEEEFP_16_MAX_10_EXP +4
#define IEEEFP_16_MAX_EXP +16
#define IEEEFP_16_MAX 6.55040000E+04F
#define IEEEFP_16_MIN_10_EXP -4
#define IEEEFP_16_MIN_EXP -13
#define IEEEFP_16_MIN 6.10351562E-05F
#define IEEEFP_16_MANT_DIG 11
#define IEEEFP_16_HAS_SUBNORM 1
#endif

/* IEEE 754-2008 binary128 (quad precision) constants */
#ifdef USE_IEEEFP_128
#define IEEEFP_128_RADIX 2
#define IEEEFP_128_DIG 33
#define IEEEFP_128_EPSILON 1.92592994045684017991e-34L
#define IEEEFP_128_MAX_10_EXP +4932
#define IEEEFP_128_MAX_EXP +16384
#define IEEEFP_128_MAX 1.18973149535723176502e+4932L
#define IEEEFP_128_MIN_10_EXP -4931
#define IEEEFP_128_MIN_EXP -16381
#define IEEEFP_128_MIN 3.36210314311209350626e-4932L
#define IEEEFP_128_MANT_DIG 113
#define IEEEFP_128_HAS_SUBNORM 1
#endif

/* bfloat16 (Brain Float) constants */
#ifdef USE_BFLOAT16
#define BFLOAT16_RADIX 2
#define BFLOAT16_DIG 2
#define BFLOAT16_MAX_10_EXP +38
#define BFLOAT16_MAX_EXP +128
#define BFLOAT16_MIN_10_EXP -37
#define BFLOAT16_MIN_EXP -125
#define BFLOAT16_MANT_DIG 8
#define BFLOAT16_HAS_SUBNORM 1
#endif

/* TensorFloat-32 (TF32) constants */
#ifdef USE_TENSORFLOAT32
#define TF32_RADIX 2
#define TF32_DIG 3
#define TF32_MAX_10_EXP +38
#define TF32_MAX_EXP +128
#define TF32_MIN_10_EXP -37
#define TF32_MIN_EXP -125
#define TF32_MANT_DIG 11
#define TF32_HAS_SUBNORM 1
#endif

/* FP8 E4M3 constants */
#ifdef USE_FP8_E4M3
#define FP8_E4M3_RADIX 2
#define FP8_E4M3_DIG 1
#define FP8_E4M3_MAX_10_EXP +1
#define FP8_E4M3_MAX_EXP +7
#define FP8_E4M3_MIN_10_EXP -2
#define FP8_E4M3_MIN_EXP -5
#define FP8_E4M3_MANT_DIG 4
#define FP8_E4M3_HAS_SUBNORM 0
#endif

/* FP8 E5M2 constants */
#ifdef USE_FP8_E5M2
#define FP8_E5M2_RADIX 2
#define FP8_E5M2_DIG 1
#define FP8_E5M2_MAX_10_EXP +4
#define FP8_E5M2_MAX_EXP +15
#define FP8_E5M2_MIN_10_EXP -4
#define FP8_E5M2_MIN_EXP -14
#define FP8_E5M2_MANT_DIG 3
#define FP8_E5M2_HAS_SUBNORM 1
#endif

/* DEC VAX F_floating constants */
#ifdef USE_VAXFP_F
#define VAXFP_F_RADIX 2
#define VAXFP_F_DIG 6
#define VAXFP_F_MAX_10_EXP +38
#define VAXFP_F_MAX_EXP +127
#define VAXFP_F_MIN_10_EXP -38
#define VAXFP_F_MIN_EXP -127
#define VAXFP_F_MANT_DIG 24
#define VAXFP_F_HAS_SUBNORM 0
#endif

/* DEC VAX D_floating constants */
#ifdef USE_VAXFP_D
#define VAXFP_D_RADIX 2
#define VAXFP_D_DIG 16
#define VAXFP_D_MAX_10_EXP +38
#define VAXFP_D_MAX_EXP +127
#define VAXFP_D_MIN_10_EXP -38
#define VAXFP_D_MIN_EXP -127
#define VAXFP_D_MANT_DIG 56
#define VAXFP_D_HAS_SUBNORM 0
#endif

/* DEC VAX G_floating constants */
#ifdef USE_VAXFP_G
#define VAXFP_G_RADIX 2
#define VAXFP_G_DIG 15
#define VAXFP_G_MAX_10_EXP +307
#define VAXFP_G_MAX_EXP +1023
#define VAXFP_G_MIN_10_EXP -308
#define VAXFP_G_MIN_EXP -1023
#define VAXFP_G_MANT_DIG 53
#define VAXFP_G_HAS_SUBNORM 0
#endif

/* DEC VAX H_floating constants */
#ifdef USE_VAXFP_H
#define VAXFP_H_RADIX 2
#define VAXFP_H_DIG 33
#define VAXFP_H_MAX_10_EXP +4932
#define VAXFP_H_MAX_EXP +16383
#define VAXFP_H_MIN_10_EXP -4932
#define VAXFP_H_MIN_EXP -16382
#define VAXFP_H_MANT_DIG 113
#define VAXFP_H_HAS_SUBNORM 0
#endif

/* ARM Alternative Half Precision constants */
#ifdef USE_ARM_ALT_HALF
#define ARM_ALT_HALF_RADIX 2
#define ARM_ALT_HALF_DIG 3
#define ARM_ALT_HALF_MAX_10_EXP +4
#define ARM_ALT_HALF_MAX_EXP +16
#define ARM_ALT_HALF_MIN_10_EXP -4
#define ARM_ALT_HALF_MIN_EXP -14
#define ARM_ALT_HALF_MANT_DIG 11
#define ARM_ALT_HALF_HAS_SUBNORM 1
#endif

/* Posit8 constants */
#ifdef USE_POSIT8
#define POSIT8_DIG 2
#define POSIT8_MAX_10_EXP +7
#define POSIT8_MIN_10_EXP -7
#define POSIT8_MANT_DIG 6
#define POSIT8_HAS_SUBNORM 0
#endif

/* Posit16 constants */
#ifdef USE_POSIT16
#define POSIT16_DIG 4
#define POSIT16_MAX_10_EXP +36
#define POSIT16_MIN_10_EXP -36
#define POSIT16_MANT_DIG 13
#define POSIT16_HAS_SUBNORM 0
#endif

/* Posit32 constants */
#ifdef USE_POSIT32
#define POSIT32_DIG 8
#define POSIT32_MAX_10_EXP +151
#define POSIT32_MIN_10_EXP -151
#define POSIT32_MANT_DIG 28
#define POSIT32_HAS_SUBNORM 0
#endif

/* Posit64 constants */
#ifdef USE_POSIT64
#define POSIT64_DIG 17
#define POSIT64_MAX_10_EXP +614
#define POSIT64_MIN_10_EXP -614
#define POSIT64_MANT_DIG 59
#define POSIT64_HAS_SUBNORM 0
#endif

/* Pixar PXR24 constants */
#ifdef USE_PXR24
#define PXR24_RADIX 2
#define PXR24_DIG 4
#define PXR24_MAX_10_EXP +38
#define PXR24_MAX_EXP +128
#define PXR24_MIN_10_EXP -37
#define PXR24_MIN_EXP -125
#define PXR24_MANT_DIG 16
#define PXR24_HAS_SUBNORM 1
#endif

/* AMD 24-bit float constants */
#ifdef USE_AMD24
#define AMD24_RADIX 2
#define AMD24_DIG 4
#define AMD24_MAX_10_EXP +38
#define AMD24_MAX_EXP +128
#define AMD24_MIN_10_EXP -37
#define AMD24_MIN_EXP -125
#define AMD24_MANT_DIG 16
#define AMD24_HAS_SUBNORM 1
#endif

/* Minifloat E3M2 constants */
#ifdef USE_MINIFLOAT_E3M2
#define MINI_E3M2_RADIX 2
#define MINI_E3M2_DIG 1
#define MINI_E3M2_MAX_10_EXP 0
#define MINI_E3M2_MAX_EXP +3
#define MINI_E3M2_MIN_10_EXP -1
#define MINI_E3M2_MIN_EXP -1
#define MINI_E3M2_MANT_DIG 3
#define MINI_E3M2_HAS_SUBNORM 0
#endif

/* Minifloat E2M3 constants */
#ifdef USE_MINIFLOAT_E2M3
#define MINI_E2M3_RADIX 2
#define MINI_E2M3_DIG 1
#define MINI_E2M3_MAX_10_EXP 0
#define MINI_E2M3_MAX_EXP +1
#define MINI_E2M3_MIN_10_EXP -1
#define MINI_E2M3_MIN_EXP 0
#define MINI_E2M3_MANT_DIG 4
#define MINI_E2M3_HAS_SUBNORM 0
#endif

#define	TARGET_FLT_RADIX	C(FLT_FP,_RADIX)

/*
 * Description of a floating point format, based what is in gdtoa package.
 * The first members are the same as in gdtoa,  the rest are pcc specific.
 */
typedef struct FPI {
	int nbits;
	int emin;
	int emax;
	int rounding;

	int sudden_underflow:1;
	int explicit_one:1; /* if MSB is explicitely stored */
	int has_inf_nan:1;  /* highest exponent means INF and NaN */
	int has_neg_zero:1;
	int has_radix_16:1;
	int storage;
	int exp_bias;
} FPI;

/* SF.kind values; same as STRTODG_* values */
enum {
	SF_Zero		= 0,
	SF_Normal	= 1,
	SF_Denormal	= 2,
	SF_Infinite	= 3,
	SF_NaN		= 4, /* default quiet NaN */
	SF_NaNbits	= 5, /* (not used) */
	SF_NoNumber	= 6, /* signaling NaN */
	SF_kmask	= 7,

	/* The following may be or-ed into one of the above values. */
	SF_Neg		= 0x80, /* does not affect SFEXCP_Inex(lo|hi) */
	SFEXCP_Inexlo	= 0x100, /* returned result rounded toward zero */
	SFEXCP_Inexhi	= 0x200, /* returned result rounded away from zero */
	SFEXCP_Inexact	= 0x300,
	SFEXCP_Underflow= 0x400,
	SFEXCP_Overflow = 0x800,
	SFEXCP_DivByZero= 0x1000,
	SFEXCP_Invalid	= 0x2000,

	SFEXCP_Aborted	= 0x8000, /* Not IEEE; operation not performed */
	SFEXCP_ALLmask	= 0xFF00 /* All exceptions (mask) */
};

/* FPI.rounding values: same as FLT_ROUNDS */
enum {
	FPI_Round_zero = 0,	/* same meaning as FE_TOWARDZERO */
	FPI_Round_near = 1,	/* same meaning as FE_TONEAREST */
	FPI_Round_up = 2,	/* same meaning as FE_UPWARD */
	FPI_Round_down = 3,	/* same meaning as FE_DOWNWARD */
/* Warning: if adding new modes, keep same meaning for 2 low bits. */
	FPI_Round_near_from0 = 5, /* to nearest but ties up (Vax) */

	FPI_RoundNotSet = -4,	/* to implement dynamic rounding */
};

extern FPI * fpis[3]; /* FLOAT, DOUBLE, LDOUBLE, respectively */

#ifndef CC_DRIVER
SF soft_neg(SF);
SF soft_int2fp(CONSZ p, TWORD f, TWORD v);
CONSZ soft_fp2int(SF p, TWORD v);
SF soft_fp2fp(SF p, TWORD v);
SFP soft_cast(CONSZ v, TWORD);
SF soft_plus(SF, SF, TWORD);
SF soft_minus(SF, SF, TWORD);
SF soft_mul(SF, SF, TWORD);
SF soft_div(SF, SF, TWORD);
int soft_cmp(SF, SF, int);
int soft_isz(SF);
SF strtosf(char *, TWORD);
SF hugesf(int, TWORD);
SF nansf(int);
SF zerosf(int kind);
SF infsf(int kind);
CONSZ soft_signbit(SF sf);
SF soft_copysign(SF, SF);
void soft_cxmul(SF r1, SF i1, SF r2, SF i2, SF *rrv, SF *irv, TWORD t);
void soft_cxdiv(SF r1, SF i1, SF r2, SF i2, SF *rrv, SF *irv, TWORD t);
int soft_isnan(SF sf);
int soft_fpclassify(SF sf, TWORD t);
SF soft_huge_val(void);
SF soft_nan(char *);
SF soft_zero(void);
#ifdef DEBUGFP
void fpwarn(char *s, long double soft, long double hard);
#endif
#endif
