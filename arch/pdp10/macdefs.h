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

/*
 * Runtime-aware type size macros.
 * These macros check the pdp10_pow2 flag at runtime and return the
 * appropriate value for either native PDP-10 or power-of-2 mode.
 *
 * This is MUCH simpler than modifying sztable[] and allows all existing
 * PCC code to automatically use the correct sizes!
 */

/* Forward declarations for runtime FP size functions */
int pdp10_szfloat(void);
int pdp10_szdouble(void);
int pdp10_szldouble(void);

/* Type sizes - runtime selection based on pdp10_pow2 flag and FP format */
#define SZCHAR		(pdp10_pow2 ? 8 : 9)
#define SZBOOL		(pdp10_pow2 ? 8 : 36)
#define SZINT		(pdp10_pow2 ? 32 : 36)
#define SZFLOAT		(pdp10_szfloat())
#define SZDOUBLE	(pdp10_szdouble())
#define SZLDOUBLE	(pdp10_szldouble())
#define SZLONG		(pdp10_pow2 ? 64 : 36)
#define SZSHORT		(pdp10_pow2 ? 16 : 18)

/*
 * Pointer size depends on both type mode and addressing mode:
 * - Native mode: 18-bit (PDP-6), 30-bit (extended), or 36-bit (full word)
 * - POW2 mode: 32-bit or 64-bit
 * The pdp10_ptrsize variable is set by -mxva/-m18/-m32 flags.
 */
#define SZPOINT(x)	(pdp10_ptrsize)
#define SZLONGLONG	(pdp10_pow2 ? 64 : 72)

/* Alignment - runtime selection (typically matches size for most types) */
#define ALCHAR		(pdp10_pow2 ? 8 : 9)
#define ALBOOL		(pdp10_pow2 ? 8 : 36)
#define ALINT		(pdp10_pow2 ? 32 : 36)
#define ALFLOAT		(pdp10_szfloat())   /* Alignment matches FP size */
#define ALDOUBLE	(pdp10_szdouble())  /* Alignment matches FP size */
#define ALLDOUBLE	(pdp10_szldouble()) /* Alignment matches FP size */
#define ALLONG		(pdp10_pow2 ? 64 : 36)
#define ALLONGLONG	(pdp10_pow2 ? 64 : 36)
#define ALSHORT		(pdp10_pow2 ? 16 : 18)
#define ALPOINT		(pdp10_ptrsize)
#define ALSTRUCT	(pdp10_pow2 ? 32 : 36)
#define ALSTACK		(pdp10_pow2 ? 64 : 36)

/* Stack frame initialization - runtime selection */
#define ARGINIT		(pdp10_pow2 ? 32 : 36)
#define AUTOINIT	(pdp10_pow2 ? 32 : 36)

/* Word addressing - runtime selection */
#ifdef WORD_ADDRESSED
#undef WORD_ADDRESSED
#endif
#define WORD_ADDRESSED	(!pdp10_pow2)

/* Multi-character constant handling - runtime selection */
#define makecc(val,i)	do { \
	if (pdp10_pow2) { \
		lastcon = (lastcon<<8)|((val<<24)>>24); \
	} else { \
		if (i == 0) { lastcon = val; \
		} else if (i == 1) { lastcon = (lastcon << 9) | val; lastcon <<= 18; \
		} else { lastcon |= (val << (27 - (i * 9))); } \
	} \
} while(0)

/* Floating-point format - still compile-time! */
#ifdef PDP10_POW2
#define FDFLOAT		/* Use VAX F/D floating-point format for POW2 mode */
#else
#define PDP10FLOAT	/* Use native PDP-10 floating-point format */
#endif 

/*
 * Min/Max values - runtime selection.
 */
#define	MIN_CHAR	(pdp10_pow2 ? -128 : -256)
#define	MAX_CHAR	(pdp10_pow2 ? 127 : 255)
#define	MAX_UCHAR	(pdp10_pow2 ? 255 : 511)
#define	MIN_SHORT	(pdp10_pow2 ? -32768 : -131072)
#define	MAX_SHORT	(pdp10_pow2 ? 32767 : 131071)
#define	MAX_USHORT	(pdp10_pow2 ? 65535 : 262143)
#define	MIN_INT		(pdp10_pow2 ? (-0x7fffffff-1) : (-0377777777777LL-1))
#define	MAX_INT		(pdp10_pow2 ? 0x7fffffff : 0377777777777LL)
#define	MAX_UNSIGNED	(pdp10_pow2 ? 0xffffffffU : 0777777777777ULL)
#define	MIN_LONG	(pdp10_pow2 ? 0x8000000000000000LL : (-0377777777777LL-1))
#define	MAX_LONG	(pdp10_pow2 ? 0x7fffffffffffffffLL : 0377777777777LL)
#define	MAX_ULONG	(pdp10_pow2 ? 0xffffffffffffffffULL : 0777777777777ULL)
#define	MIN_LONGLONG	(pdp10_pow2 ? 0x8000000000000000LL : (000777777777777777777777LL-1))
#define	MAX_LONGLONG	(pdp10_pow2 ? 0x7fffffffffffffffLL : 000777777777777777777777LL)
#define	MAX_ULONGLONG	(pdp10_pow2 ? 0xffffffffffffffffULL : 001777777777777777777777ULL)

/*
 * CHAR signedness - POW2 uses signed char, native uses unsigned char.
 * NOTE: This affects #ifdef CHAR_UNSIGNED checks, so we use compile-time PDP10_POW2.
 */
#ifdef PDP10_POW2
#undef	CHAR_UNSIGNED
#define	BOOL_TYPE	UCHAR
#else
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
 * WHAT WORKS AT RUNTIME (with -m64/-m36 flags):
 * ✓ All type size macros (SZCHAR, SZINT, SZLONG, SZPOINT, etc.) - runtime-aware!
 * ✓ Struct layouts - calculated using runtime type sizes
 * ✓ Array indexing - uses runtime element sizes
 * ✓ Multi-character constants - makecc() macro is runtime-aware
 * ✓ Stack frame layout - ARGINIT/AUTOINIT macros are runtime-aware
 * ✓ Register allocation - szty() macro is runtime-aware
 * ✓ All integer type operations
 *
 * SINGLE LIMITATION:
 * ✗ Floating-point format is COMPILE-TIME ONLY
 *   - If compiler built with -DPDP10_POW2: uses FDFLOAT (VAX)
 *   - If compiler built without: uses PDP10FLOAT (36/72-bit)
 *   - Runtime -m64/-m36 flags do NOT change FP format
 *
 * RECOMMENDATION: For integer-only code, one PCC build handles both modes!
 *                 For floating-point, build separate PCC binaries with/without -DPDP10_POW2.
 */
extern int pdp10_asmfmt;  /* Assembly syntax: PDP10_ASM_* */
extern int pdp10_abi;     /* ABI/Object format: PDP10_ABI_* */
extern int pdp10_pow2;    /* Runtime flag: 0=native (36-bit), 1=power-of-2 (64-bit) */
extern int pdp10_ptrsize; /* Runtime pointer size: 18/30/32/36/64 bits */

/*
 * Runtime type system initialization.
 * Updates the global sztable[] array when -m64/-m36 flags are used.
 * This is needed because sztable[] is initialized when PCC itself is compiled,
 * so it contains compile-time values. We update it at runtime based on the mode.
 */
void pdp10_init_runtime_types(void);

/*
 * Runtime endianness support (for future dual-endian architectures).
 * PDP-10 is always big-endian, so this is not applicable here.
 * Other architectures could implement:
 *   extern int target_endian; // TARGET_BE or TARGET_LE
 *   int pdp10_target_endian(void); // Returns runtime endianness
 * And use #ifdef pdp10_target_endian to enable runtime selection.
 */

/*
 * Runtime floating-point format selection.
 * Allows single PCC binary to generate code for multiple FP formats.
 * Essential for Alpha (IEEE + VAX G-float), ML workloads (FP8/FP16), etc.
 */

/* Floating-point format identifiers */
#define PDP10_FP_PDP10     0  /* PDP-10 native format (36/72-bit) */
#define PDP10_FP_VAX_FD    1  /* VAX F-float (32-bit) and D-float (64-bit) */
#define PDP10_FP_VAX_G     2  /* VAX G-float (64-bit, extended range) */
#define PDP10_FP_VAX_H     3  /* VAX H-float (128-bit, quad precision) */
#define PDP10_FP_IEEE32    4  /* IEEE 754 binary32 (32-bit float) */
#define PDP10_FP_IEEE64    5  /* IEEE 754 binary64 (64-bit double) */
#define PDP10_FP_IEEE80    6  /* IEEE 754 binary80 (80-bit long double, x87) */
#define PDP10_FP_IEEE16    7  /* IEEE 754 binary16 (16-bit half precision) */
#define PDP10_FP_IEEE128   8  /* IEEE 754 binary128 (128-bit quad precision) */
#define PDP10_FP_FP8_E4M3  9  /* FP8 E4M3 (8-bit, ML training) */
#define PDP10_FP_FP8_E5M2  10 /* FP8 E5M2 (8-bit, ML inference) */

/* Runtime FP format selection variables (set by command-line flags) */
extern int pdp10_fpfmt_float;   /* Format for FLOAT type */
extern int pdp10_fpfmt_double;  /* Format for DOUBLE type */
extern int pdp10_fpfmt_ldouble; /* Format for LDOUBLE type */

/*
 * Runtime floating-point format initialization.
 * Updates the global fpis[] array to point to selected FPI structures.
 * Must be called after setting pdp10_fpfmt_* variables.
 */
void pdp10_init_fp_formats(void);

/* Definitions mostly used in pass2 */

#define BYTEOFF(x)	((x)&03)
#define wdal(k)		(BYTEOFF(k)==0)

#define STOARG(p)
#define STOFARG(p)
#define STOSTARG(p)
#define genfcall(a,b)	gencall(a,b)

/*
 * Runtime-aware register size determination.
 * Returns how many registers needed for a type.
 *
 * Native mode: FLOAT(36), DOUBLE(72), LONGLONG(72) need 2 registers
 * POW2 mode: DOUBLE(64), LONG(64), LONGLONG(64) need 2 registers
 * Pointers: need 2 registers if > 36 bits (depends on addressing mode)
 */
#define	szty(t)	(pdp10_pow2 ? \
	(((t) == DOUBLE || (t) == LONG || (t) == ULONG || \
	  (t) == LONGLONG || (t) == ULONGLONG || (ISPTR(t) && pdp10_ptrsize > 36)) ? 2 : 1) : \
	(((t) == DOUBLE || (t) == FLOAT || \
	  (t) == LONGLONG || (t) == ULONGLONG || (ISPTR(t) && pdp10_ptrsize > 36)) ? 2 : 1))

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
#define PCLASS(p) (szty((p)->n_type) == 2 ? SBREG : SAREG)
#define RETREG(x) (szty(x) == 2 ? XR1 : R1)
#define DECRA(x,y)      (((x) >> (y*6)) & 63)   /* decode encoded regs */
#define ENCRD(x)        (x)             /* Encode dest reg in n_reg */
#define ENCRA1(x)       ((x) << 6)      /* A1 */
#define ENCRA2(x)       ((x) << 12)     /* A2 */
#define ENCRA(x,y)      ((x) << (6+y*6))        /* encode regs in int */
#define GCLASS(x)	(x < 16 ? CLASSA : CLASSB)
int COLORMAP(int c, int *r);
