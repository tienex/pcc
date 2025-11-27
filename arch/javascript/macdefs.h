/*	$Id$	*/
/*
 * Copyright (c) 2025 JavaScript Backend
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
 * Machine-dependent definitions for JavaScript ES2025 code generator backend.
 * This backend generates JavaScript code with support for ES2025 and prior versions,
 * plus extensions from ActionScript, TypeScript, CoffeeScript, and LiveScript.
 */

#define makecc(val,i)	lastcon = (lastcon<<8)|((val<<24)>>24);

/*
 * JavaScript ES Version Support Flags
 */
#define MJES_ES3	0001	/* ECMAScript 3 (1999) */
#define MJES_ES5	0002	/* ECMAScript 5 (2009) */
#define MJES_ES6	0004	/* ECMAScript 2015 (ES6) */
#define MJES_ES2016	0010	/* ECMAScript 2016 */
#define MJES_ES2017	0020	/* ECMAScript 2017 */
#define MJES_ES2018	0040	/* ECMAScript 2018 */
#define MJES_ES2019	0100	/* ECMAScript 2019 */
#define MJES_ES2020	0200	/* ECMAScript 2020 */
#define MJES_ES2021	0400	/* ECMAScript 2021 */
#define MJES_ES2022	01000	/* ECMAScript 2022 */
#define MJES_ES2023	02000	/* ECMAScript 2023 */
#define MJES_ES2024	04000	/* ECMAScript 2024 */
#define MJES_ES2025	010000	/* ECMAScript 2025 */

/*
 * Extension Language Features
 */
#define MJES_TYPESCRIPT	020000	/* TypeScript extensions (types, interfaces, decorators) */
#define MJES_ACTIONSCRIPT 040000 /* ActionScript extensions (packages, namespaces) */
#define MJES_COFFEESCRIPT 0100000 /* CoffeeScript extensions (comprehensions, destructuring) */
#define MJES_LIVESCRIPT 0200000	/* LiveScript extensions (operators, function composition) */

#ifndef MJESMODEL
extern int mjesmodel;		/* Runtime ES version/feature selection */
#else
#define mjesmodel MJESMODEL	/* Compile-time ES version/features */
#endif

/*
 * JavaScript Module System Support
 */
#define MJES_MODULE_NONE	0	/* No module system (global scope) */
#define MJES_MODULE_CJS		1	/* CommonJS (require/module.exports) */
#define MJES_MODULE_AMD		2	/* AMD (define/require) */
#define MJES_MODULE_UMD		3	/* UMD (Universal Module Definition) */
#define MJES_MODULE_ESM		4	/* ES6 Modules (import/export) */

#ifndef MJESMODULE
extern int mjesmodule;		/* Runtime module system selection */
#else
#define mjesmodule MJESMODULE	/* Compile-time module system */
#endif

/*
 * Storage space requirements (JavaScript runtime model)
 * JavaScript uses 64-bit floats for numbers, BigInt for arbitrary precision integers
 */
#define SZCHAR		8	/* UTF-16 code unit */
#define SZBOOL		8	/* boolean (true/false) */
#define SZINT		32	/* JavaScript Number (32-bit int range safe) */
#define SZFLOAT		64	/* JavaScript Number (IEEE 754 double) */
#define SZDOUBLE	64	/* JavaScript Number (IEEE 754 double) */
#define SZLDOUBLE	64	/* JavaScript Number (no extended precision) */
#define SZLONG		64	/* BigInt or Number */
#define SZSHORT		16	/* 16-bit integer */
#define SZLONGLONG	64	/* BigInt */
#define SZPOINT(t)	64	/* pointers are references (64-bit internally) */

/*
 * Alignment constraints (JavaScript has no alignment requirements)
 */
#define ALCHAR		8
#define ALBOOL		8
#define ALINT		32
#define ALFLOAT		64
#define ALDOUBLE	64
#define ALLDOUBLE	64
#define ALLONG		64
#define ALLONGLONG	64
#define ALSHORT		16
#define ALPOINT		64
#define ALSTRUCT	8
#define ALSTACK		64
#define ALMAX		64

/*
 * Min/max values (JavaScript Number safe integer range)
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

/* JavaScript Number.MAX_SAFE_INTEGER = 2^53 - 1 */
#define JS_MAX_SAFE_INTEGER	9007199254740991LL
#define JS_MIN_SAFE_INTEGER	(-9007199254740991LL)

/* Default char is signed */
#undef	CHAR_UNSIGNED
#define	BOOL_TYPE	UCHAR

/*
 * Use large-enough types
 */
typedef	long long CONSZ;
typedef	unsigned long long U_CONSZ;
typedef long long OFFSZ;

#define CONFMT	"%lld"		/* format for printing constants */
#define LABFMT	"_L%d"		/* format for printing labels */
#define	STABLBL	"_LL%d"		/* format for stab (debugging) labels */

#undef	BACKAUTO
#undef	BACKTEMP

#define ARGINIT		0	/* arguments passed via JavaScript function parameters */
#define AUTOINIT	0	/* automatic variables as JavaScript locals */

#undef	FIELDOPS		/* no bit-field instructions */
#define TARGET_ENDIAN TARGET_LE	/* JavaScript is implementation-defined, assume LE */

/* Definitions mostly used in pass2 */

#define BYTEOFF(x)	((x)&3)
#define wdal(k)		(BYTEOFF(k)==0)

#define	szty(t)	(((t) == DOUBLE || (t) == FLOAT || \
	(t) == LONGLONG || (t) == ULONGLONG) ? 2 : (t) == LDOUBLE ? 2 : 1)

/*
 * JavaScript backend uses virtual registers that map to JavaScript variables
 * We define a generous register set for the allocator
 */

/* General purpose registers (map to JavaScript let/var/const variables) */
#define	R0	0
#define	R1	1
#define	R2	2
#define	R3	3
#define	R4	4
#define	R5	5
#define	R6	6
#define	R7	7
#define	R8	8
#define	R9	9
#define	R10	10
#define	R11	11
#define	R12	12
#define	R13	13
#define	R14	14
#define	R15	15
#define	R16	16
#define	R17	17
#define	R18	18
#define	R19	19
#define	R20	20
#define	R21	21
#define	R22	22
#define	R23	23
#define	R24	24
#define	R25	25
#define	R26	26
#define	R27	27
#define	R28	28
#define	R29	29
#define	R30	30
#define	R31	31

/* Floating point registers (JavaScript Numbers) */
#define	F0	32
#define	F1	33
#define	F2	34
#define	F3	35
#define	F4	36
#define	F5	37
#define	F6	38
#define	F7	39
#define	F8	40
#define	F9	41
#define	F10	42
#define	F11	43
#define	F12	44
#define	F13	45
#define	F14	46
#define	F15	47

#define	MAXREGS	48	/* 48 total virtual registers (32 int + 16 fp) */

#define	RSTATUS	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG, SAREG|TEMPREG,	\
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG,	\
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG,	\
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG,	\
	SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG, SBREG|TEMPREG,

/* No register overlap in JavaScript (separate int/fp virtual register sets) */
#define	ROVERLAP \
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 },	\
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 },	\
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 },	\
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 },	\
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 },	\
	{ -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 }, { -1 },

/* Return a register class based on the type of the node */
#define PCLASS(p) \
	((p)->n_type >= FLOAT && (p)->n_type <= LDOUBLE ? SBREG : SAREG)

#define	NUMCLASS 	2	/* highest number of reg classes used */

int COLORMAP(int c, int *r);
#define	GCLASS(x) ((x) < 32 ? CLASSA : CLASSB)
#define DECRA(x,y)	(((x) >> (y*6)) & 63)	/* decode encoded regs */
#define	ENCRD(x)	(x)		/* Encode dest reg in n_reg */
#define ENCRA1(x)	((x) << 6)	/* A1 */
#define ENCRA2(x)	((x) << 12)	/* A2 */
#define ENCRA(x,y)	((x) << (6+y*6))	/* encode regs in int */
#define	RETREG(x)	((x) == FLOAT || (x) == DOUBLE || (x) == LDOUBLE ? F0 : R0)

/* Frame/stack registers (virtual in JavaScript) */
#define FPREG	R30	/* frame pointer (virtual) */
#define STKREG	R31	/* stack pointer (virtual) */

/* JavaScript-specific output format */
#define JS_COMMENT_PREFIX	"//"
#define JS_BLOCK_COMMENT_START	"/*"
#define JS_BLOCK_COMMENT_END	"*/"

/*
 * JavaScript variable naming conventions
 */
#define JS_VAR_PREFIX	"_r"	/* register variable prefix: _r0, _r1, etc. */
#define JS_FVAR_PREFIX	"_f"	/* floating point variable prefix: _f0, _f1, etc. */
#define JS_TEMP_PREFIX	"_t"	/* temporary variable prefix */
#define JS_LABEL_PREFIX	"_L"	/* label prefix */

/*
 * JavaScript type annotation support (TypeScript extension)
 */
#define JS_TYPE_ANY	0
#define JS_TYPE_NUMBER	1
#define JS_TYPE_STRING	2
#define JS_TYPE_BOOLEAN	3
#define JS_TYPE_OBJECT	4
#define JS_TYPE_ARRAY	5
#define JS_TYPE_FUNCTION 6
#define JS_TYPE_BIGINT	7
#define JS_TYPE_SYMBOL	8

/*
 * JavaScript class/prototype system support
 */
#define JS_CLASS_ES5	0	/* ES5 prototype-based */
#define JS_CLASS_ES6	1	/* ES6 class syntax */
#define JS_CLASS_TS	2	/* TypeScript class with types */

/*
 * Operator overloading support (ActionScript/TypeScript extensions)
 */
#define JS_OPERATOR_OVERLOAD	/* Enable operator overloading */

/*
 * Function composition operators (LiveScript extension)
 */
#define JS_FUNCTION_COMPOSITION	/* Enable >> and << for function composition */

/*
 * Comprehension syntax (CoffeeScript extension)
 */
#define JS_COMPREHENSIONS	/* Enable list/array comprehensions */

/*
 * Destructuring and pattern matching (ES6/CoffeeScript/LiveScript)
 */
#define JS_DESTRUCTURING	/* Enable destructuring assignments */
#define JS_PATTERN_MATCHING	/* Enable pattern matching (LiveScript) */

/*
 * Async/await support (ES2017+)
 */
#define JS_ASYNC_AWAIT		/* Enable async/await syntax */

/*
 * BigInt support (ES2020+)
 */
#define JS_BIGINT		/* Enable BigInt for 64-bit integers */

/*
 * Optional chaining and nullish coalescing (ES2020)
 */
#define JS_OPTIONAL_CHAINING	/* Enable ?. operator */
#define JS_NULLISH_COALESCING	/* Enable ?? operator */

/*
 * Private class fields (ES2022)
 */
#define JS_PRIVATE_FIELDS	/* Enable # prefix for private fields */

/*
 * Top-level await (ES2022)
 */
#define JS_TOP_LEVEL_AWAIT	/* Enable await at module top level */

/*
 * Type guards and assertions (TypeScript)
 */
#define JS_TYPE_GUARDS		/* Enable is, as operators */

/*
 * Decorators (TypeScript/ES2025)
 */
#define JS_DECORATORS		/* Enable @decorator syntax */

/*
 * Pipeline operator (ES2025 proposal)
 */
#define JS_PIPELINE		/* Enable |> operator */

/*
 * Record and Tuple (ES2025 proposal)
 */
#define JS_RECORDS_TUPLES	/* Enable #{} and #[] syntax */
