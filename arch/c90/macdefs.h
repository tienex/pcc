/*	$Id$	*/
/*
 * Copyright (c) 2025 C90 Backend Generator
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
 * Machine-dependent definitions for C90 code generator backend.
 * This backend generates portable C90-compliant C code instead of assembly.
 */

#define makecc(val,i)	lastcon = (lastcon<<8)|((val<<24)>>24);

/* C90 standard type sizes (configurable based on memory model) */
#define ARGINIT		0	/* arguments passed via C function calls */
#define AUTOINIT	0	/* automatic variables in C */

/*
 * Memory Model Support (following i386/i86 patterns)
 * Allows C90 backend to generate code targeting different architectures
 */
#define MCC90_16BIT	0001	/* 16-bit pointers (embedded/8/16-bit CPUs) */
#define MCC90_32BIT	0002	/* 32-bit pointers (default, most common) */
#define MCC90_64BIT	0004	/* 64-bit pointers (modern 64-bit systems) */
#define MCC90_SEGMENTED	0010	/* Segmented addressing (16:16 or 16:32) */

#ifndef MCC90MODEL
extern int mcc90model;		/* Runtime memory model selection */
#else
#define mcc90model MCC90MODEL	/* Compile-time model */
#endif

/*
 * Storage space requirements (C90 standard)
 * Configurable for different target architectures
 */
#define SZCHAR		8
#define SZBOOL		8
#define SZINT		((mcc90model & MCC90_16BIT) ? 16 : 32)
#define SZFLOAT		32
#define SZDOUBLE	64
#define SZLDOUBLE	((mcc90model & MCC90_16BIT) ? 64 : 80)
#define SZLONG		((mcc90model & MCC90_64BIT) ? 64 : 32)
#define SZSHORT		16
#define SZLONGLONG	64

/*
 * Pointer size depends on memory model
 * - 16-bit mode: 16-bit or 32-bit (segmented)
 * - 32-bit mode: 32-bit (default)
 * - 64-bit mode: 64-bit
 */
#define SZPOINT(t)	((mcc90model & MCC90_64BIT) ? 64 : \
			 (mcc90model & MCC90_SEGMENTED) ? 32 : \
			 (mcc90model & MCC90_16BIT) ? 16 : 32)

/*
 * Alignment constraints (C90 standard, model-aware)
 */
#define ALCHAR		8
#define ALBOOL		8
#define ALINT		((mcc90model & MCC90_16BIT) ? 16 : 32)
#define ALFLOAT		32
#define ALDOUBLE	((mcc90model & MCC90_16BIT) ? 16 : 32)
#define ALLDOUBLE	((mcc90model & MCC90_16BIT) ? 16 : 32)
#define ALLONG		((mcc90model & MCC90_64BIT) ? 64 : 32)
#define ALLONGLONG	((mcc90model & MCC90_16BIT) ? 16 : 32)
#define ALSHORT		16
#define ALPOINT		((mcc90model & MCC90_64BIT) ? 64 : \
			 (mcc90model & MCC90_16BIT) ? 16 : 32)
#define ALSTRUCT	8
#define ALSTACK		((mcc90model & MCC90_64BIT) ? 64 : \
			 (mcc90model & MCC90_16BIT) ? 16 : 32)

/*
 * Min/max values
 */
#define MIN_CHAR	-128
#define MAX_CHAR	127
#define MAX_UCHAR	255
#define MIN_SHORT	-32768
#define MAX_SHORT	32767
#define MAX_USHORT	65535
#define MIN_INT		(-0x7fffffff-1)
#define MAX_INT		0x7fffffff
#define MAX_UNSIGNED	0xffffffff
#define MIN_LONG	MIN_INT
#define MAX_LONG	MAX_INT
#define MAX_ULONG	MAX_UNSIGNED
#define MIN_LONGLONG	0x8000000000000000LL
#define MAX_LONGLONG	0x7fffffffffffffffLL
#define MAX_ULONGLONG	0xffffffffffffffffULL

/* Default char is signed */
#undef	CHAR_UNSIGNED
#define	BOOL_TYPE	UCHAR

/*
 * Use large-enough types
 */
typedef	long long CONSZ;
typedef	unsigned long long U_CONSZ;
typedef long long OFFSZ;

#define CONFMT	"%lld"
#define LABFMT	"L%d"
#define	STABLBL	"LL%d"

#undef	BACKAUTO
#undef	BACKTEMP

#undef	FIELDOPS
#define TARGET_ENDIAN TARGET_LE

/*
 * Floating-point format support
 * C90 backend can target different float representations
 */
#define USE_IEEEFP_32		/* IEEE 754 single precision (default) */
#define USE_IEEEFP_64		/* IEEE 754 double precision */
#define FLT_PREFIX IEEEFP_32
#define DBL_PREFIX IEEEFP_64

/*
 * C90 standard library support macros
 */
#define C90_NEED_STDDEF		/* Include <stddef.h> for size_t, ptrdiff_t */
#define C90_NEED_LIMITS		/* Include <limits.h> for INT_MAX, etc. */
#define C90_NEED_FLOAT		/* Include <float.h> for FLT_MAX, etc. */

/*
 * C90 backend uses virtual registers that map to C variables
 * We define a simple register set for the allocator
 */
#define R0	0
#define R1	1
#define R2	2
#define R3	3
#define R4	4
#define R5	5
#define R6	6
#define R7	7
#define R8	8
#define R9	9
#define R10	10
#define R11	11
#define R12	12
#define R13	13
#define R14	14
#define R15	15

/* Floating point registers */
#define F0	16
#define F1	17
#define F2	18
#define F3	19
#define F4	20
#define F5	21
#define F6	22
#define F7	23

#define MAXREGS	24

#define RSTATUS \
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, \
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, \
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, \
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, \
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, \
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG,

#define ROVERLAP \
	{ -1 }, { -1 }, { -1 }, { -1 }, \
	{ -1 }, { -1 }, { -1 }, { -1 }, \
	{ -1 }, { -1 }, { -1 }, { -1 }, \
	{ -1 }, { -1 }, { -1 }, { -1 }, \
	{ -1 }, { -1 }, { -1 }, { -1 }, \
	{ -1 }, { -1 }, { -1 }, { -1 },

/* C90 register names (map to C variable names) */
#define MINRVAR	R0
#define MAXRVAR	R15
#define MINFVAR	F0
#define MAXFVAR	F7

#define	szty(t)	(((t) == DOUBLE || (t) == FLOAT || \
	(t) == LONGLONG || (t) == ULONGLONG) ? 2 : 1)

#define STOARG(p)
#define STOFARG(p)
#define STOSTARG(p)
#define genfcall(a,b)	gencall(a,b)

#define MYALIGN
