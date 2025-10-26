/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC CLR Backend Contributors.
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
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission
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
 * Machine-dependent defines for Microsoft CLR (Common Language Runtime) backend.
 * This backend generates ILASM (CIL/MSIL) code for the .NET Framework.
 *
 * The CLR is a stack-based virtual machine, unlike traditional register-based
 * architectures. This backend maps C/Fortran constructs to CLR IL instructions.
 */

/*
 * Convert (multi-)character constant to integer.
 * CLR uses little-endian encoding for multi-byte values.
 */
#define makecc(val,i)	lastcon = (lastcon<<8)|((val<<24)>>24);

/*
 * Argument and automatic variable initialization offsets.
 * CLR uses zero-based indexing for both arguments and locals.
 */
#define ARGINIT		0	/* Arguments start at index 0 */
#define AUTOINIT	0	/* Automatic variables (locals) start at index 0 */

/*
 * Storage space requirements (in bits)
 * These match the CLR type system as defined in ECMA-335.
 */
#define SZCHAR		8	/* char: maps to int8 in CLR */
#define SZBOOL		32	/* _Bool: CLR bool is 32-bit on stack */
#define SZINT		32	/* int: maps to int32 in CLR */
#define SZFLOAT		32	/* float: maps to float32 in CLR */
#define SZDOUBLE	64	/* double: maps to float64 in CLR */
#define SZLDOUBLE	64	/* long double: CLR doesn't have extended precision */
#define SZLONG		32	/* long: 32-bit (use SZLONGLONG for 64-bit) */
#define SZSHORT		16	/* short: maps to int16 in CLR */
#define SZLONGLONG	64	/* long long: maps to int64 in CLR */

/*
 * Pointer size - depends on target CLR platform.
 * 32-bit for CLR on x86, 64-bit for CLR on x64.
 * This can be overridden by defining CLR64 for 64-bit target.
 */
#ifdef CLR64
#define SZPOINT(t)	64	/* native int/object references are 64-bit */
#else
#define SZPOINT(t)	32	/* native int/object references are 32-bit */
#endif

/*
 * Alignment constraints (in bits)
 * CLR handles alignment automatically; these values reflect
 * natural alignment for interoperability with native code.
 */
#define ALCHAR		8
#define ALBOOL		32
#define ALINT		32
#define ALFLOAT		32
#define ALDOUBLE	64
#define ALLDOUBLE	64
#define ALLONG		32
#define ALLONGLONG	64
#define ALSHORT		16
#define ALPOINT		32
#define ALSTACK		32
#define	ALMAX		64

/*
 * Min/max values for integral types.
 * These match ECMA-335 specification for CLR types.
 */
#define	MIN_CHAR	-128
#define	MAX_CHAR	127
#define	MAX_UCHAR	255
#define	MIN_SHORT	-32768
#define	MAX_SHORT	32767
#define	MAX_USHORT	65535
#define	MIN_INT		(-0x7fffffff-1)
#define	MAX_INT		0x7fffffff
#define	MAX_UNSIGNED	0xffffffffU
#define	MIN_LONG	MIN_INT
#define	MAX_LONG	MAX_INT
#define	MAX_ULONG	MAX_UNSIGNED
#define	MIN_LONGLONG	0x8000000000000000LL
#define	MAX_LONGLONG	0x7fffffffffffffffLL
#define	MAX_ULONGLONG	0xffffffffffffffffULL

/* Default char is signed (matches CLR int8) */
#undef	CHAR_UNSIGNED
#define	BOOL_TYPE	UCHAR	/* _Bool storage type */

/*
 * Use large-enough types for constants and offsets.
 */
typedef	long long CONSZ;
typedef	unsigned long long U_CONSZ;
typedef long long OFFSZ;

#define CONFMT	"%lld"		/* format for printing constants */
#define LABFMT	"IL_%d"		/* format for printing IL labels */
#define	STABLBL	"IL_DBG_%d"	/* format for debugging labels */

/*
 * CLR-specific: we're targeting ILASM output format
 */
#define TARGET_ILASM		/* Generate ILASM (CIL) output */

/*
 * Stack grows downward for locals (conceptual for stack machine)
 */
#define BACKAUTO
#define BACKTEMP

/*
 * No bit-field instructions in CLR
 */
#undef	FIELDOPS

/*
 * CLR is little-endian (matches x86/x64)
 */
#define TARGET_ENDIAN TARGET_LE

/*
 * Division by zero is checked by CLR runtime
 */
#define	CC_DIV_0

/*
 * Register definitions for CLR "virtual stack"
 * CLR is stack-based, but we model the evaluation stack as virtual registers.
 * This simplifies the code generator and allows reuse of PCC's register allocator.
 */

/* Evaluation stack positions (virtual registers) */
#define	EVAL_0		000	/* Top of evaluation stack */
#define	EVAL_1		001	/* Stack depth 1 */
#define	EVAL_2		002	/* Stack depth 2 */
#define	EVAL_3		003	/* Stack depth 3 */
#define	EVAL_4		004	/* Stack depth 4 */
#define	EVAL_5		005	/* Stack depth 5 */
#define	EVAL_6		006	/* Stack depth 6 */
#define	EVAL_7		007	/* Stack depth 7 */

/* Long long (int64) pseudo-register pairs */
#define	EVAL_01		010	/* EVAL_0:EVAL_1 pair for int64 */
#define	EVAL_23		011	/* EVAL_2:EVAL_3 pair for int64 */
#define	EVAL_45		012	/* EVAL_4:EVAL_5 pair for int64 */
#define	EVAL_67		013	/* EVAL_6:EVAL_7 pair for int64 */

/* Floating-point "registers" (separate float stack in FPU model) */
#define	FP_0		014	/* FP stack top */
#define	FP_1		015	/* FP stack depth 1 */
#define	FP_2		016	/* FP stack depth 2 */
#define	FP_3		017	/* FP stack depth 3 */

#define	MAXREGS		020	/* 16 virtual registers total */

/*
 * Register status flags
 * SAREG - integer/pointer registers (evaluation stack)
 * SBREG - byte registers (not distinct in CLR, maps to SAREG)
 * SCREG - long long registers (64-bit pairs)
 * SDREG - floating-point registers
 */
#define	RSTATUS	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SCREG|TEMPREG, SCREG|TEMPREG, SCREG|TEMPREG, SCREG|TEMPREG,	\
	SDREG|TEMPREG, SDREG|TEMPREG, SDREG|TEMPREG, SDREG|TEMPREG,

/*
 * Register overlap definitions
 * Since CLR is stack-based, there's minimal overlap.
 * Long long registers "overlap" with their component registers.
 */
#define	ROVERLAP \
	/* EVAL_0-7: no overlaps */ \
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, \
	/* EVAL_01: overlaps with EVAL_0, EVAL_1 */ \
	{ EVAL_0, EVAL_1, -1 }, \
	/* EVAL_23: overlaps with EVAL_2, EVAL_3 */ \
	{ EVAL_2, EVAL_3, -1 }, \
	/* EVAL_45: overlaps with EVAL_4, EVAL_5 */ \
	{ EVAL_4, EVAL_5, -1 }, \
	/* EVAL_67: overlaps with EVAL_6, EVAL_7 */ \
	{ EVAL_6, EVAL_7, -1 }, \
	/* FP_0-3: no overlaps */ \
	{ -1 }, { -1 }, { -1 }, { -1 },

/*
 * Return a register class based on the type of the node
 */
#define PCLASS(p) \
	((p)->n_type == LONGLONG || (p)->n_type == ULONGLONG ? SCREG : \
	 ((p)->n_type >= FLOAT && (p)->n_type <= LDOUBLE ? SDREG : SAREG))

#define	NUMCLASS	3	/* Number of register classes: A, C, D */

int COLORMAP(int c, int *r);
#define	GCLASS(x) (x < 8 ? CLASSA : x < 14 ? CLASSC : CLASSD)
#define DECRA(x,y)	(((x) >> (y*6)) & 63)	/* decode encoded regs */
#define	ENCRD(x)	(x)			/* Encode dest reg in n_reg */
#define ENCRA1(x)	((x) << 6)		/* A1 */
#define ENCRA2(x)	((x) << 12)		/* A2 */
#define ENCRA(x,y)	((x) << (6+y*6))	/* encode regs in int */

/*
 * Return register for function return value based on type
 */
#define	RETREG(x) \
	((x) == LONGLONG || (x) == ULONGLONG ? EVAL_01 : \
	 (x) == FLOAT || (x) == DOUBLE || (x) == LDOUBLE ? FP_0 : EVAL_0)

/*
 * Special opcodes for CLR-specific operations
 */
#define	CLR_BOX		(MAXSPECIAL+1)	/* Box value type to object */
#define	CLR_UNBOX	(MAXSPECIAL+2)	/* Unbox object to value type */
#define	CLR_NEWARR	(MAXSPECIAL+3)	/* Create new array */
#define	CLR_NEWOBJ	(MAXSPECIAL+4)	/* Create new object */
#define	CLR_CALLVIRT	(MAXSPECIAL+5)	/* Virtual method call */
#define	CLR_ISINST	(MAXSPECIAL+6)	/* Instance type check */
#define	CLR_CASTCLASS	(MAXSPECIAL+7)	/* Cast to class type */
#define	CLR_THROW	(MAXSPECIAL+8)	/* Throw exception */
#define	CLR_LEAVE	(MAXSPECIAL+9)	/* Leave protected region */

/*
 * CLR-specific interpass data
 * Track information needed between compilation passes.
 */
#define TARGET_IPP_MEMBERS			\
	int ipp_argstacksize;			\
	int ipp_maxstack;			\
	int ipp_localcount;			\
	int ipp_is_pinvoke;

#define	target_members_print_prolog(ipp) \
	printf("%d %d %d %d", ipp->ipp_argstacksize, ipp->ipp_maxstack, \
	       ipp->ipp_localcount, ipp->ipp_is_pinvoke)

#define	target_members_print_epilog(ipp) \
	printf("%d %d %d %d", ipp->ipp_argstacksize, ipp->ipp_maxstack, \
	       ipp->ipp_localcount, ipp->ipp_is_pinvoke)

#define target_members_read_prolog(ipp) \
	ipp->ipp_argstacksize = rdint(&p); \
	ipp->ipp_maxstack = rdint(&p); \
	ipp->ipp_localcount = rdint(&p); \
	ipp->ipp_is_pinvoke = rdint(&p)

#define target_members_read_epilog(ipp) \
	ipp->ipp_argstacksize = rdint(&p); \
	ipp->ipp_maxstack = rdint(&p); \
	ipp->ipp_localcount = rdint(&p); \
	ipp->ipp_is_pinvoke = rdint(&p)

/*
 * CLR supports weak references via WeakReference class
 */
#define	HAVE_WEAKREF

/*
 * Floating-point evaluation method
 * CLR evaluates all FP as float64 (double precision)
 */
#define	TARGET_FLT_EVAL_METHOD	1

/*
 * Size calculation for types (in register units)
 */
#define	szty(t)	\
	(((t) == DOUBLE || (t) == FLOAT || \
	  (t) == LONGLONG || (t) == ULONGLONG || (t) == LDOUBLE) ? 2 : 1)

/*
 * CLR-specific attribute flags
 */
#define	ATTR_CLR_MARSHAL	(ATTR_MI_MAX+1)	/* P/Invoke marshalling */
#define	ATTR_CLR_VALUETYPE	(ATTR_MI_MAX+2)	/* Value type (struct) */
#define	ATTR_CLR_REFTYPE	(ATTR_MI_MAX+3)	/* Reference type (class) */
#define	ATTR_CLR_PINNED		(ATTR_MI_MAX+4)	/* Pinned for GC */
#define	ATTR_MI_TARGET		ATTR_CLR_MARSHAL, ATTR_CLR_VALUETYPE, \
				ATTR_CLR_REFTYPE, ATTR_CLR_PINNED

/*
 * Floating-point format definitions
 * CLR uses IEEE 754 formats
 */
#define	USE_IEEEFP_32
#define	FLT_PREFIX	IEEEFP_32
#define	USE_IEEEFP_64
#define	DBL_PREFIX	IEEEFP_64
#define	LDBL_PREFIX	IEEEFP_64	/* No extended precision in CLR */

/*
 * Fortran-specific definitions
 */
#ifdef LANG_F77
#define BLANKCOMMON "_BLNK_"
#define MSKIREG  (M(TYSHORT)|M(TYLONG))
#define TYIREG TYLONG
#define FSZLENG  FSZLONG
#define	AUTOREG	EVAL_0
#define	ARGREG	EVAL_0
#define ARGOFFSET 0
#endif
