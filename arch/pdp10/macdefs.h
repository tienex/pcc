/*	$Id$	*/
/*
 * Copyright (c) 2003 Anders Magnusson (ragge@ludd.luth.se).
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
 * Machine-dependent defines for both passes.
 */

/*
 * PDP-10 Configuration Options:
 *
 * Type Size Mode (compile-time):
 *   PDP10_POW2     - Use power-of-2 types (8/16/32/64 bit) for portability
 *   (default)      - Use native PDP-10 types (9/18/36/72 bit)
 *
 * Assembly Syntax and ABI (runtime-selectable):
 *   See pdp10_asmfmt and pdp10_abi global variables
 */

/* Assembly syntax formats */
#define PDP10_ASM_GNU    0  /* GNU assembler syntax (default) */
#define PDP10_ASM_MIDAS  1  /* MIDAS assembler syntax (ITS/MIT) */

/* ABI/Object formats */
#define PDP10_ABI_ELF    0  /* ELF object format (Linux/BSD) */
#define PDP10_ABI_MACHO  1  /* Mach-O object format (Darwin/macOS) */
#define PDP10_ABI_PECOFF 2  /* PE/COFF object format (Windows) */
#define PDP10_ABI_NONE   3  /* Generic/MIDAS format */

#ifdef PDP10_POW2
/*
 * Power-of-2 mode: Standard 8/16/32/64 bit types for portable code
 * NOTE: These are COMPILE-TIME settings. The -mpow2 runtime flag provides
 * experimental runtime mode switching but has significant limitations:
 * - Type sizes here (SZCHAR, SZINT, etc.) remain fixed at compile time
 * - makecc() macro cannot be changed at runtime
 * - Struct layouts use compile-time sizes
 * - Only code generation attempts to respect runtime mode
 */
#define makecc(val,i)	lastcon = (lastcon<<8)|((val<<24)>>24)
#define ARGINIT		32	/* # bits below fp where arguments start */
#define AUTOINIT	32	/* # bits above fp where automatics start */
#define SZCHAR		8
#define SZBOOL		8
#define SZINT		32
#define SZFLOAT		32
#define SZDOUBLE	64
#define SZLDOUBLE	64
#define SZLONG		64
#define SZSHORT		16
#define SZPOINT(x)	64
#define SZLONGLONG	64
#define ALCHAR		8
#define ALBOOL		8
#define ALINT		32
#define ALFLOAT		32
#define ALDOUBLE	64
#define ALLDOUBLE	64
#define ALLONG		64
#define ALLONGLONG	64
#define ALSHORT		16
#define ALPOINT		64
#define ALSTRUCT	32
#define ALSTACK		64
#undef WORD_ADDRESSED
#else
/* Native PDP-10 mode: Traditional 9/18/36/72 bit types */
#define makecc(val,i) {			\
	if (i == 0) { lastcon = val;	\
	} else if (i == 1) { lastcon = (lastcon << 9) | val; lastcon <<= 18; \
	} else { lastcon |= (val << (27 - (i * 9))); } }
#define ARGINIT		36	/* # bits below fp where arguments start */
#define AUTOINIT	36	/* # bits above fp where automatics start */
#define SZCHAR		9
#define SZBOOL		36
#define SZINT		36
#define SZFLOAT		36
#define SZDOUBLE	72
#define SZLDOUBLE	72
#define SZLONG		36
#define SZSHORT		18
#define SZPOINT(x)	36
#define SZLONGLONG	72
#define ALCHAR		9
#define ALBOOL		36
#define ALINT		36
#define ALFLOAT		36
#define ALDOUBLE	36
#define ALLDOUBLE	36
#define ALLONG		36
#define ALLONGLONG	36
#define ALSHORT		18
#define ALPOINT		36
#define ALSTRUCT	36
#define ALSTACK		36
#define	WORD_ADDRESSED
#endif /* PDP10_POW2 */ 

/*
 * Min/Max values.
 */
#ifdef PDP10_POW2
/* Power-of-2 mode values */
#define	MIN_CHAR	-128
#define	MAX_CHAR	127
#define	MAX_UCHAR	255
#define	MIN_SHORT	-32768
#define	MAX_SHORT	32767
#define	MAX_USHORT	65535
#define	MIN_INT		(-0x7fffffff-1)
#define	MAX_INT		0x7fffffff
#define	MAX_UNSIGNED	0xffffffffU
#define	MIN_LONG	0x8000000000000000LL
#define	MAX_LONG	0x7fffffffffffffffLL
#define	MAX_ULONG	0xffffffffffffffffULL
#define	MIN_LONGLONG	0x8000000000000000LL
#define	MAX_LONGLONG	0x7fffffffffffffffLL
#define	MAX_ULONGLONG	0xffffffffffffffffULL
#undef	CHAR_UNSIGNED
#define	BOOL_TYPE	UCHAR
#else
/* Native PDP-10 mode values */
#define	MIN_CHAR	-256
#define	MAX_CHAR	255
#define	MAX_UCHAR	511
#define	MIN_SHORT	-131072
#define	MAX_SHORT	131071
#define	MAX_USHORT	262143
#define	MIN_INT		(-0377777777777LL-1)
#define	MAX_INT		0377777777777LL
#define	MAX_UNSIGNED	0777777777777ULL
#define	MIN_LONG	(-0377777777777LL-1)
#define	MAX_LONG	0377777777777LL
#define	MAX_ULONG	0777777777777ULL
#define	MIN_LONGLONG	(000777777777777777777777LL-1)
#define	MAX_LONGLONG	000777777777777777777777LL
#define	MAX_ULONGLONG	001777777777777777777777ULL
#define	CHAR_UNSIGNED
#define	BOOL_TYPE	INT
#endif

/* Common defines */
#define TARGET_STDARGS

/*
 * Use large-enough types.
 */
typedef	long long CONSZ;
typedef	unsigned long long U_CONSZ;
typedef long long OFFSZ;

#define CONFMT	"0%llo"		/* format for printing constants */
#define LABFMT	".L%d"		/* format for printing labels */
#define STABLBL ".LL%d"		/* format for stab (debugging) labels */

#undef BACKAUTO 		/* stack grows negatively for automatics */
#undef BACKTEMP 		/* stack grows negatively for temporaries */

#undef	FIELDOPS		/* no bit-field instructions */
#define TARGET_ENDIAN TARGET_BE

/*
 * Floating-point format definitions for softfloat library.
 *
 * Native PDP-10 mode: PDP-10 proprietary 36/72-bit format
 *   Single precision (36-bit): sign(1) + exponent(8, excess-128) + fraction(27)
 *   Double precision (72-bit): sign(1) + exponent(8, excess-1024) + fraction(62)
 *
 * Power-of-2 mode: VAX F/D floating-point format
 *   F-floating (32-bit): Compatible with 32-bit float
 *   D-floating (64-bit): Compatible with 64-bit double
 *
 * VAX format is preferred over IEEE 754 for POW2 mode because:
 * - Similar architecture philosophy to PDP-10
 * - Better match for cross-compilation scenarios
 * - Already implemented in PCC softfloat
 */
#ifdef PDP10_POW2
#define FDFLOAT		/* Use VAX F/D floating-point format for POW2 mode */
#else
#define PDP10FLOAT	/* Use native PDP-10 floating-point format */
#endif

/*
 * Variadic argument support
 * PDP-10 always passes variadic arguments on the stack, making implementation simple.
 */
#define TARGET_VALIST
#define TARGET_STDARGS
#define TARGET_BUILTINS							\
	{ "__builtin_stdarg_start", pdp10_builtin_stdarg_start,	\
						0, 2, 0, VOID },	\
	{ "__builtin_va_start", pdp10_builtin_stdarg_start,		\
						0, 2, 0, VOID },	\
	{ "__builtin_va_arg", pdp10_builtin_va_arg, BTNORVAL|BTNOPROTO, \
							2, 0, 0 },	\
	{ "__builtin_va_end", pdp10_builtin_va_end, 0, 1, 0, VOID },	\
	{ "__builtin_va_copy", pdp10_builtin_va_copy, 0, 2, 0, VOID },

#ifdef LANG_CXX
#define P1ND struct node
#else
#define P1ND struct p1node
#endif
struct node;
struct bitable;
P1ND *pdp10_builtin_stdarg_start(const struct bitable *, P1ND *a);
P1ND *pdp10_builtin_va_arg(const struct bitable *, P1ND *a);
P1ND *pdp10_builtin_va_end(const struct bitable *, P1ND *a);
P1ND *pdp10_builtin_va_copy(const struct bitable *, P1ND *a);
/* MI builtins that each architecture must implement */
P1ND *builtin_return_address(const struct bitable *, P1ND *a);
P1ND *builtin_frame_address(const struct bitable *, P1ND *a);
P1ND *builtin_cfa(const struct bitable *, P1ND *a);
#undef P1ND

/*
 * Extended assembler support.
 */
int xasmconstregs(char *);
#define XASMCONSTREGS(x)	xasmconstregs(x)

/*
 * Runtime configuration globals for assembly format, ABI, and type sizes.
 * Can be set via mflags() or other configuration mechanisms.
 *
 * EXPERIMENTAL: pdp10_pow2 mode
 * When enabled, attempts to use power-of-2 type sizes (8/16/32/64 bit)
 * instead of native PDP-10 sizes (9/18/36/72 bit) for code generation.
 *
 * WHAT WORKS:
 * - Register allocation now uses runtime pdp10_szty()
 * - Argument register assignment uses runtime sizes
 * - Register class selection (PCLASS/RETREG) uses runtime sizes
 *
 * CRITICAL LIMITATIONS:
 * - Struct layouts use compile-time type sizes (INCORRECT in POW2 mode!)
 * - Floating-point format is COMPILE-TIME ONLY
 *   * If compiler built with -DPDP10_POW2: uses FDFLOAT (VAX, correct)
 *   * If compiler built without: uses PDP10FLOAT (36/72-bit, WRONG!)
 *   The -mpow2 runtime flag cannot change FP format!
 * - Array indexing uses compile-time element sizes
 * - Multi-character constants use compile-time byte size
 * - Frontend type calculations use compile-time sizes
 *
 * RECOMMENDATION: Recompile PCC with -DPDP10_POW2 for correct POW2 support.
 */
extern int pdp10_asmfmt;  /* Assembly syntax: PDP10_ASM_* */
extern int pdp10_abi;     /* ABI/Object format: PDP10_ABI_* */
extern int pdp10_pow2;    /* EXPERIMENTAL: Use power-of-2 type sizes */

/* Runtime type size helpers (respect pdp10_pow2 flag) */
int pdp10_szchar(void);
int pdp10_szshort(void);
int pdp10_szint(void);
int pdp10_szlong(void);
int pdp10_szfloat(void);
int pdp10_szdouble(void);

/*
 * Runtime pointer size function.
 * Defining this function enables runtime pointer size in core PCC (pftn.c).
 */
int pdp10_szpointer(void);

int pdp10_is_word_addressed(void);
int pdp10_byteoff(OFFSZ offset);
int pdp10_wdal(OFFSZ offset);
int pdp10_szty(TWORD t);

/* Runtime stack frame initialization */
int pdp10_autoinit(void);
int pdp10_arginit(void);

/* Runtime type system initialization */
void pdp10_init_runtime_types(void);

/* Definitions mostly used in pass2 */

#define BYTEOFF(x)	((x)&03)
#define wdal(k)		(BYTEOFF(k)==0)

#define STOARG(p)
#define STOFARG(p)
#define STOSTARG(p)
#define genfcall(a,b)	gencall(a,b)

/*
 * WARNING: This macro uses compile-time type sizes.
 * For runtime POW2-aware code, use pdp10_szty() instead.
 */
#define	szty(t)	(((t) == DOUBLE || (t) == FLOAT || \
	(t) == LONGLONG || (t) == ULONGLONG) ? 2 : 1)

#define	shltype(o, p) \
	((o) == REG || (o) == NAME || (o) == ICON || \
	 (o) == OREG || ((o) == UMUL && shumul((p)->n_left, SOREG)))

#undef	SPECIAL_INTEGERS

/*
 * Special shapes used in code generation.
 */
#define	SUSHCON	(SPECIAL|6)	/* unsigned short constant */
#define	SNSHCON	(SPECIAL|7)	/* negative short constant */
#define	SILDB	(SPECIAL|8)	/* use ildb here */

/*
 * Register allocator definitions.
 *
 * The pdp10 has 16 general-purpose registers, but the two
 * highest are used as sp and fp.  Register 0 has special 
 * constraints in its possible use as index register.
 * All regs can be used as pairs, named by the lowest number.
 * In here we call the registers Rn and the pairs XRn, in assembler
 * just its number prefixed with %.
 * 
 * R1/XR1 are return registers.
 *
 * R0 is currently not used.
 */

#define	MAXREGS		29 /* 16 + 13 regs */
#define	NUMCLASS	2

#define R0	00
#define R1	01
#define R2	02
#define R3	03
#define R4	04
#define R5	05
#define R6	06
#define R7	07
#define R10	010
#define R11	011
#define R12	012
#define R13	013
#define R14	014
#define R15	015
#define R16	016
#define R17	017
#define FPREG	R16		/* frame pointer */
#define STKREG	R17		/* stack pointer */


#define XR0	020
#define XR1	021
#define XR2	022
#define XR3	023
#define XR4	024
#define XR5	025
#define XR6	026
#define XR7	027
#define XR10	030
#define XR11	031
#define XR12	032
#define XR13	033
#define XR14	034


#define RSTATUS \
	0, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,			\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|PERMREG, SAREG|PERMREG, SAREG|PERMREG, SAREG|PERMREG,	\
	SAREG|PERMREG, SAREG|PERMREG, 0, 0,				\
	SBREG, SBREG, SBREG, SBREG, SBREG, SBREG, SBREG, SBREG,		\
	SBREG, SBREG, SBREG, SBREG, SBREG,

#define ROVERLAP \
        { XR0, -1 },			\
        { XR0, XR1, -1 },		\
        { XR1, XR2, -1 },		\
        { XR2, XR3, -1 },		\
        { XR3, XR4, -1 },		\
        { XR4, XR5, -1 },		\
        { XR5, XR6, -1 },		\
        { XR6, XR7, -1 },		\
        { XR7, XR10, -1 },		\
        { XR10, XR11, -1 },		\
        { XR11, XR12, -1 },		\
        { XR12, XR13, -1 },		\
        { XR13, XR14, -1 },		\
        { XR14, -1 },			\
        { -1 },				\
        { -1 },				\
        { R0, R1, XR1, -1 },		\
        { R1, R2, XR0, XR2, -1 },	\
        { R2, R3, XR1, XR3, -1 },	\
        { R3, R4, XR2, XR4, -1 },	\
        { R4, R5, XR3, XR5, -1 },	\
        { R5, R6, XR4, XR6, -1 },	\
        { R6, R7, XR5, XR7, -1 },	\
        { R7, R10, XR6, XR10, -1 },	\
        { R10, R11, XR7, XR11, -1 },	\
        { R11, R12, XR10, XR12, -1 },	\
        { R12, R13, XR11, XR13, -1 },	\
        { R13, R14, XR12, XR14, -1 },	\
        { R14, R15, XR13, -1 },

/* Return a register class based on the type of the node */
#define PCLASS(p) (pdp10_szty((p)->n_type) == 2 ? SBREG : SAREG)
#define RETREG(x) (pdp10_szty(x) == 2 ? XR1 : R1)
#define DECRA(x,y)      (((x) >> (y*6)) & 63)   /* decode encoded regs */
#define ENCRD(x)        (x)             /* Encode dest reg in n_reg */
#define ENCRA1(x)       ((x) << 6)      /* A1 */
#define ENCRA2(x)       ((x) << 12)     /* A2 */
#define ENCRA(x,y)      ((x) << (6+y*6))        /* encode regs in int */
#define GCLASS(x)	(x < 16 ? CLASSA : CLASSB)
int COLORMAP(int c, int *r);
