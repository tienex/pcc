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
 * JavaScript ES2025 instruction table
 * Maps intermediate representation (IR) operations to JavaScript code
 */

#include "pass2.h"

#define TLL TLONGLONG|TULONGLONG
#define ANYSIGNED TINT|TLONG|TSHORT|TCHAR
#define ANYUSIGNED TUNSIGNED|TULONG|TUSHORT|TUCHAR
#define ANYFIXED ANYSIGNED|ANYUSIGNED
#define TUWORD TUNSIGNED|TULONG
#define TSWORD TINT|TLONG
#define TWORD TUWORD|TSWORD
#define SHINT SAREG		/* shape for integers in A registers */
#define ININT INAREG	/* result is integer in A register */
#define SHLL SAREG		/* shape for long long in A registers */
#define INLL INAREG		/* result is long long in A register */
#define SHFL SBREG		/* shape for float in B registers */
#define INFL INBREG		/* result is float in B register */

/*
 * JavaScript operator table
 * Each entry maps an IR operation to JavaScript code
 *
 * Format:
 * { op, cookie, left_shape, left_type, right_shape, right_type, needs, rewrite, template }
 *
 * op: IR operation (PLUS, MUL, ASSIGN, etc.)
 * cookie: where result goes (INAREG, INBREG, FOREFF, etc.)
 * left_shape: required shape of left operand (SAREG, SCON, SNAME, etc.)
 * left_type: required type of left operand (TINT, TWORD, TFLOAT, etc.)
 * right_shape: required shape of right operand
 * right_type: required type of right operand
 * needs: registers needed for computation (NAREG, NBREG, etc.)
 * rewrite: rewrite action (RLEFT, RRIGHT, RESC1, RDEST, etc.)
 * template: JavaScript code template
 */

struct optab table[] = {
/* First entry must be an empty entry */
{ -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },

/*
 * Pointer conversions - no-ops in JavaScript
 */
{ PCONV,	INAREG,
	SAREG,	TPOINT|TWORD,
	SANY,	TANY,
		0,	RLEFT,
		"", },

{ PCONV,	INAREG,
	SCON,	TPOINT|TWORD,
	SANY,	TANY,
		NAREG,	RESC1,
		"", },

/*
 * Type conversions
 * JavaScript has automatic type coercion, but we emit explicit conversions
 * for clarity and correctness
 */

/* No-op conversions (same type or compatible types) */
{ SCONV,	INAREG,
	SAREG,	TCHAR|TUCHAR,
	SAREG,	TCHAR|TUCHAR,
		0,	RLEFT,
		"", },

{ SCONV,	INAREG,
	SAREG,	TSHORT|TUSHORT,
	SAREG,	TSHORT|TUSHORT,
		0,	RLEFT,
		"", },

{ SCONV,	INAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RLEFT,
		"", },

{ SCONV,	INAREG,
	SAREG,	TLL,
	SAREG,	TLL,
		0,	RLEFT,
		"", },

{ SCONV,	INBREG,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"", },

/* Sign/zero extension for integers */
{ SCONV,	INAREG,
	SAREG,	TCHAR,
	SAREG,	TWORD|TSHORT,
		0,	RLEFT,
		"((AL << 24) >> 24)", }, /* Sign extend 8-bit */

{ SCONV,	INAREG,
	SAREG,	TUCHAR,
	SAREG,	TWORD|TSHORT,
		0,	RLEFT,
		"(AL & 0xff)", }, /* Zero extend 8-bit */

{ SCONV,	INAREG,
	SAREG,	TSHORT,
	SAREG,	TWORD,
		0,	RLEFT,
		"((AL << 16) >> 16)", }, /* Sign extend 16-bit */

{ SCONV,	INAREG,
	SAREG,	TUSHORT,
	SAREG,	TWORD,
		0,	RLEFT,
		"(AL & 0xffff)", }, /* Zero extend 16-bit */

/* Integer to BigInt (ES2020+) */
{ SCONV,	INAREG,
	SAREG,	TWORD,
	SAREG,	TLL,
		0,	RLEFT,
		"BigInt(AL)", },

/* BigInt to integer */
{ SCONV,	INAREG,
	SAREG,	TLL,
	SAREG,	TWORD,
		0,	RLEFT,
		"Number(AL)", },

/* Integer to float/double */
{ SCONV,	INBREG,
	SAREG,	TWORD|TSHORT|TCHAR,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"Number(AL)", },

/* Float/double to integer */
{ SCONV,	INAREG,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SAREG,	TWORD|TSHORT|TCHAR,
		NAREG,	RESC1,
		"Math.trunc(AL)", }, /* ES6+ */

/* Float/double conversions are no-ops in JavaScript (all are 64-bit floats) */
{ SCONV,	INBREG,
	SBREG,	TFLOAT,
	SBREG,	TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"", },

{ SCONV,	INBREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT,
		0,	RLEFT,
		"", },

/*
 * Load operations (OPLTYPE)
 * Load values into registers
 */

/* Load register */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SAREG,	TWORD|TPOINT|TLL,
		0,	RRIGHT,
		"", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RRIGHT,
		"", },

/* Load constant */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SCON,	TWORD|TPOINT,
		NAREG,	RESC1,
		"AR", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SCON,	TLL,
		NAREG,	RESC1,
		"ZB", }, /* Use 'B' code for BigInt literal */

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SCON,	TFLOAT|TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"AR", },

/* Load from memory (NAME or OREG) */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SNAME,	TWORD|TPOINT|TLL,
		NAREG,	RESC1,
		"AR", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SNAME,	TFLOAT|TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"AR", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SOREG,	TWORD|TPOINT|TLL,
		NAREG,	RESC1,
		"AR", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SOREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"AR", },

/*
 * Assignment operations
 */

/* reg = reg */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TWORD|TPOINT|TLL,
	SAREG,	TWORD|TPOINT|TLL,
		0,	RDEST,
		"AL = AR;\n", },

{ ASSIGN,	FOREFF|INBREG,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"AL = AR;\n", },

/* reg = const */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TWORD|TPOINT,
	SCON,	TWORD|TPOINT,
		0,	RDEST,
		"AL = AR;\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TLL,
	SCON,	TLL,
		0,	RDEST,
		"AL = ZB;\n", }, /* BigInt literal */

{ ASSIGN,	FOREFF|INBREG,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SCON,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"AL = AR;\n", },

/* reg = mem */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TWORD|TPOINT|TLL,
	SNAME|SOREG,	TWORD|TPOINT|TLL,
		0,	RDEST,
		"AL = AR;\n", },

{ ASSIGN,	FOREFF|INBREG,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SNAME|SOREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"AL = AR;\n", },

/* mem = reg */
{ ASSIGN,	FOREFF,
	SNAME|SOREG,	TWORD|TPOINT|TLL,
	SAREG,	TWORD|TPOINT|TLL,
		0,	RDEST,
		"AL = AR;\n", },

{ ASSIGN,	FOREFF,
	SNAME|SOREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"AL = AR;\n", },

/* mem = const */
{ ASSIGN,	FOREFF,
	SNAME|SOREG,	TWORD|TPOINT,
	SCON,	TWORD|TPOINT,
		0,	RDEST,
		"AL = AR;\n", },

{ ASSIGN,	FOREFF,
	SNAME|SOREG,	TLL,
	SCON,	TLL,
		0,	RDEST,
		"AL = ZB;\n", },

{ ASSIGN,	FOREFF,
	SNAME|SOREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SCON,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"AL = AR;\n", },

/*
 * Arithmetic operations: PLUS, MINUS, MUL, DIV, MOD
 */

/* Addition: reg + reg */
{ PLUS,		INAREG|FOREFF,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RLEFT,
		"AL = AL + AR;\n", },

{ PLUS,		INAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TLL,
		0,	RLEFT,
		"AL = AL + AR;\n", },

{ PLUS,		INBREG|FOREFF,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"AL = AL + AR;\n", },

/* Addition: reg + const */
{ PLUS,		INAREG|FOREFF,
	SAREG,	TWORD|TPOINT,
	SCON,	TWORD|TPOINT,
		0,	RLEFT,
		"AL = AL + AR;\n", },

{ PLUS,		INAREG|FOREFF,
	SAREG,	TLL,
	SCON,	TLL,
		0,	RLEFT,
		"AL = AL + ZB;\n", },

/* Subtraction: reg - reg */
{ MINUS,	INAREG|FOREFF,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RLEFT,
		"AL = AL - AR;\n", },

{ MINUS,	INAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TLL,
		0,	RLEFT,
		"AL = AL - AR;\n", },

{ MINUS,	INBREG|FOREFF,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"AL = AL - AR;\n", },

/* Subtraction: reg - const */
{ MINUS,	INAREG|FOREFF,
	SAREG,	TWORD|TPOINT,
	SCON,	TWORD|TPOINT,
		0,	RLEFT,
		"AL = AL - AR;\n", },

{ MINUS,	INAREG|FOREFF,
	SAREG,	TLL,
	SCON,	TLL,
		0,	RLEFT,
		"AL = AL - ZB;\n", },

/* Multiplication: reg * reg */
{ MUL,		INAREG|FOREFF,
	SAREG,	TWORD,
	SAREG,	TWORD,
		0,	RLEFT,
		"AL = AL * AR;\n", },

{ MUL,		INAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TLL,
		0,	RLEFT,
		"AL = AL * AR;\n", },

{ MUL,		INBREG|FOREFF,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"AL = AL * AR;\n", },

/* Division: reg / reg */
/* Note: JavaScript / is float division, need Math.trunc for integer division */
{ DIV,		INAREG|FOREFF,
	SAREG,	TWORD,
	SAREG,	TWORD,
		0,	RLEFT,
		"AL = ZC;\n", }, /* Use 'C' code for integer division */

{ DIV,		INAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TLL,
		0,	RLEFT,
		"AL = BigInt(Math.trunc(Number(AL) / Number(AR)));\n", },

{ DIV,		INBREG|FOREFF,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"AL = AL / AR;\n", },

/* Modulo: reg % reg */
{ MOD,		INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SAREG,	TWORD|TLL,
		0,	RLEFT,
		"AL = AL % AR;\n", },

/*
 * Unary operations: UMINUS, COMPL, NOT
 */

/* Unary minus */
{ UMINUS,	INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SANY,	TANY,
		0,	RLEFT,
		"AL = -AL;\n", },

{ UMINUS,	INBREG|FOREFF,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SANY,	TANY,
		0,	RLEFT,
		"AL = -AL;\n", },

/* Bitwise NOT */
{ COMPL,	INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SANY,	TANY,
		0,	RLEFT,
		"AL = ~AL;\n", },

/* Logical NOT */
{ NOT,		INAREG|FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	RLEFT,
		"AL = !AL;\n", },

{ NOT,		INBREG|FOREFF,
	SBREG,	TANY,
	SANY,	TANY,
		0,	RLEFT,
		"AL = !AL;\n", },

/*
 * Bitwise operations: AND, OR, ER (XOR)
 */

/* Bitwise AND: reg & reg */
{ AND,		INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SAREG,	TWORD|TLL,
		0,	RLEFT,
		"AL = AL & AR;\n", },

/* Bitwise AND: reg & const */
{ AND,		INAREG|FOREFF,
	SAREG,	TWORD,
	SCON,	TWORD,
		0,	RLEFT,
		"AL = AL & AR;\n", },

/* Bitwise OR: reg | reg */
{ OR,		INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SAREG,	TWORD|TLL,
		0,	RLEFT,
		"AL = AL | AR;\n", },

/* Bitwise OR: reg | const */
{ OR,		INAREG|FOREFF,
	SAREG,	TWORD,
	SCON,	TWORD,
		0,	RLEFT,
		"AL = AL | AR;\n", },

/* Bitwise XOR: reg ^ reg */
{ ER,		INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SAREG,	TWORD|TLL,
		0,	RLEFT,
		"AL = AL ^ AR;\n", },

/* Bitwise XOR: reg ^ const */
{ ER,		INAREG|FOREFF,
	SAREG,	TWORD,
	SCON,	TWORD,
		0,	RLEFT,
		"AL = AL ^ AR;\n", },

/*
 * Shift operations: LS (left shift), RS (right shift)
 */

/* Left shift: reg << reg */
{ LS,		INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SAREG,	TWORD,
		0,	RLEFT,
		"AL = AL << AR;\n", },

/* Left shift: reg << const */
{ LS,		INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SCON,	TWORD,
		0,	RLEFT,
		"AL = AL << AR;\n", },

/* Right shift: reg >> reg (signed) */
{ RS,		INAREG|FOREFF,
	SAREG,	TSWORD,
	SAREG,	TWORD,
		0,	RLEFT,
		"AL = AL >> AR;\n", },

/* Right shift: reg >>> reg (unsigned) */
{ RS,		INAREG|FOREFF,
	SAREG,	TUWORD,
	SAREG,	TWORD,
		0,	RLEFT,
		"AL = AL >>> AR;\n", },

/* Right shift: reg >> const */
{ RS,		INAREG|FOREFF,
	SAREG,	TSWORD,
	SCON,	TWORD,
		0,	RLEFT,
		"AL = AL >> AR;\n", },

{ RS,		INAREG|FOREFF,
	SAREG,	TUWORD,
	SCON,	TWORD,
		0,	RLEFT,
		"AL = AL >>> AR;\n", },

/*
 * Comparison operations: EQ, NE, LT, LE, GT, GE
 */

/* Equality: == (or === for ES5+) */
{ EQ,		INAREG|FORCC,
	SAREG,	TWORD|TPOINT|TLL,
	SAREG,	TWORD|TPOINT|TLL,
		0,	RLEFT,
		"AL = (AL ZG AR);\n", }, /* Use 'G' code for === */

{ EQ,		INBREG|FORCC,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"AL = (AL ZG AR);\n", },

/* Inequality: != (or !== for ES5+) */
{ NE,		INAREG|FORCC,
	SAREG,	TWORD|TPOINT|TLL,
	SAREG,	TWORD|TPOINT|TLL,
		0,	RLEFT,
		"AL = (AL ZH AR);\n", }, /* Use 'H' code for !== */

{ NE,		INBREG|FORCC,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"AL = (AL ZH AR);\n", },

/* Less than: < */
{ LT,		INAREG|FORCC,
	SAREG,	TWORD|TPOINT|TLL,
	SAREG,	TWORD|TPOINT|TLL,
		0,	RLEFT,
		"AL = (AL < AR);\n", },

{ LT,		INBREG|FORCC,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"AL = (AL < AR);\n", },

/* Less than or equal: <= */
{ LE,		INAREG|FORCC,
	SAREG,	TWORD|TPOINT|TLL,
	SAREG,	TWORD|TPOINT|TLL,
		0,	RLEFT,
		"AL = (AL <= AR);\n", },

{ LE,		INBREG|FORCC,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"AL = (AL <= AR);\n", },

/* Greater than: > */
{ GT,		INAREG|FORCC,
	SAREG,	TWORD|TPOINT|TLL,
	SAREG,	TWORD|TPOINT|TLL,
		0,	RLEFT,
		"AL = (AL > AR);\n", },

{ GT,		INBREG|FORCC,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"AL = (AL > AR);\n", },

/* Greater than or equal: >= */
{ GE,		INAREG|FORCC,
	SAREG,	TWORD|TPOINT|TLL,
	SAREG,	TWORD|TPOINT|TLL,
		0,	RLEFT,
		"AL = (AL >= AR);\n", },

{ GE,		INBREG|FORCC,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"AL = (AL >= AR);\n", },

/*
 * Logical operations: ANDAND (&&), OROR (||)
 */

/* Logical AND: && */
{ ANDAND,	INAREG|FORCC,
	SAREG,	TANY,
	SAREG,	TANY,
		0,	RLEFT,
		"AL = (AL && AR);\n", },

/* Logical OR: || */
{ OROR,		INAREG|FORCC,
	SAREG,	TANY,
	SAREG,	TANY,
		0,	RLEFT,
		"AL = (AL || AR);\n", },

/*
 * Control flow: GOTO, RETURN, CBRANCH
 */

/* Unconditional branch */
{ GOTO,		FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"goto ZL;\n", },

/* Return statement */
{ RETURN,	FOREFF,
	SAREG,	TWORD|TPOINT|TLL,
	SANY,	TANY,
		0,	RNOP,
		"ZR AL;\n", },

{ RETURN,	FOREFF,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SANY,	TANY,
		0,	RNOP,
		"ZR AL;\n", },

{ RETURN,	FOREFF,
	SANY,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"ZR;\n", },

/* Conditional branch */
{ CBRANCH,	FOREFF,
	SAREG,	TANY,
	SCON,	TANY,
		0,	RNOP,
		"if (AL) goto ZL;\n", },

{ CBRANCH,	FOREFF,
	SBREG,	TANY,
	SCON,	TANY,
		0,	RNOP,
		"if (AL) goto ZL;\n", },

/*
 * Function calls: CALL, UCALL, STCALL
 */

/* Function call with arguments: result = func(args) */
{ CALL,		INAREG,
	SCON|SNAME,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"ZA", }, /* Use 'A' code for function call */

{ CALL,		INBREG,
	SCON|SNAME,	TANY,
	SANY,	TANY,
		NBREG|NASL,	RESC1,
		"ZA", },

{ CALL,		FOREFF,
	SCON|SNAME,	TANY,
	SANY,	TANY,
		0,	0,
		"ZA;\n", },

/* Indirect function call: result = (*func)(args) */
{ UCALL,	INAREG,
	SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"AL", },

{ UCALL,	INBREG,
	SAREG,	TANY,
	SANY,	TANY,
		NBREG|NASL,	RESC1,
		"AL", },

{ UCALL,	FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	0,
		"AL;\n", },

/* Structure-returning call */
{ STCALL,	INAREG,
	SCON|SNAME,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"ZA", },

{ STCALL,	FOREFF,
	SCON|SNAME,	TANY,
	SANY,	TANY,
		0,	0,
		"ZA;\n", },

/*
 * Increment/Decrement: INCR, DECR (++, --)
 */

/* Pre-increment: ++reg */
{ INCR,		INAREG|FOREFF,
	SAREG,	TWORD|TPOINT|TLL,
	SCON,	TANY,
		0,	RLEFT,
		"AL = ++AL;\n", },

/* Post-increment: reg++ */
{ INCR,		INAREG,
	SAREG,	TWORD|TPOINT|TLL,
	SCON,	TANY,
		NAREG,	RESC1,
		"A1 = AL++;\n", },

/* Pre-decrement: --reg */
{ DECR,		INAREG|FOREFF,
	SAREG,	TWORD|TPOINT|TLL,
	SCON,	TANY,
		0,	RLEFT,
		"AL = --AL;\n", },

/* Post-decrement: reg-- */
{ DECR,		INAREG,
	SAREG,	TWORD|TPOINT|TLL,
	SCON,	TANY,
		NAREG,	RESC1,
		"A1 = AL--;\n", },

/*
 * Ternary operator: QUEST (?:)
 */

/* cond ? true_expr : false_expr */
{ QUEST,	INAREG,
	SAREG,	TANY,
	SANY,	TANY,
		NAREG,	RESC1,
		"A1 = (AL ? AR1 : AR2);\n", },

{ QUEST,	INBREG,
	SAREG,	TANY,
	SANY,	TANY,
		NBREG,	RESC1,
		"A1 = (AL ? AR1 : AR2);\n", },

/*
 * Memory operations: UMUL (dereference), STASG (struct assignment)
 */

/* Dereference: *ptr */
{ UMUL,		INAREG,
	SAREG,	TPOINT,
	SANY,	TWORD|TPOINT|TLL,
		NAREG,	RESC1,
		"A1 = *AL;\n", },

{ UMUL,		INBREG,
	SAREG,	TPOINT,
	SANY,	TFLOAT|TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"A1 = *AL;\n", },

/* Structure assignment: struct1 = struct2 */
{ STASG,	FOREFF|INAREG,
	SOREG|SNAME,	TANY,
	SAREG|SOREG|SNAME,	TANY,
		0,	RDEST,
		"Object.assign(AL, AR);\n", }, /* ES6+ */

/*
 * Compound assignment operators: +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=
 */

/* Add and assign: += */
{ ASG PLUS,	INAREG|FOREFF,
	SAREG,	TWORD|TPOINT|TLL,
	SAREG,	TWORD|TPOINT|TLL,
		0,	RDEST,
		"AL += AR;\n", },

{ ASG PLUS,	INBREG|FOREFF,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"AL += AR;\n", },

/* Subtract and assign: -= */
{ ASG MINUS,	INAREG|FOREFF,
	SAREG,	TWORD|TPOINT|TLL,
	SAREG,	TWORD|TPOINT|TLL,
		0,	RDEST,
		"AL -= AR;\n", },

{ ASG MINUS,	INBREG|FOREFF,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"AL -= AR;\n", },

/* Multiply and assign: *= */
{ ASG MUL,	INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SAREG,	TWORD|TLL,
		0,	RDEST,
		"AL *= AR;\n", },

{ ASG MUL,	INBREG|FOREFF,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"AL *= AR;\n", },

/* Divide and assign: /= */
{ ASG DIV,	INAREG|FOREFF,
	SAREG,	TWORD,
	SAREG,	TWORD,
		0,	RDEST,
		"AL = Math.trunc(AL / AR);\n", }, /* Integer division */

{ ASG DIV,	INBREG|FOREFF,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"AL /= AR;\n", }, /* Float division */

/* Modulo and assign: %= */
{ ASG MOD,	INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SAREG,	TWORD|TLL,
		0,	RDEST,
		"AL %= AR;\n", },

/* Bitwise AND and assign: &= */
{ ASG AND,	INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SAREG,	TWORD|TLL,
		0,	RDEST,
		"AL &= AR;\n", },

/* Bitwise OR and assign: |= */
{ ASG OR,	INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SAREG,	TWORD|TLL,
		0,	RDEST,
		"AL |= AR;\n", },

/* Bitwise XOR and assign: ^= */
{ ASG ER,	INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SAREG,	TWORD|TLL,
		0,	RDEST,
		"AL ^= AR;\n", },

/* Left shift and assign: <<= */
{ ASG LS,	INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SAREG,	TWORD,
		0,	RDEST,
		"AL <<= AR;\n", },

/* Right shift and assign: >>= or >>>= */
{ ASG RS,	INAREG|FOREFF,
	SAREG,	TSWORD,
	SAREG,	TWORD,
		0,	RDEST,
		"AL >>= AR;\n", },

{ ASG RS,	INAREG|FOREFF,
	SAREG,	TUWORD,
	SAREG,	TWORD,
		0,	RDEST,
		"AL >>>= AR;\n", },

/*
 * Array/object access: OREG
 */

/* Array element access: array[index] */
{ OREG,		INAREG,
	SANY,	TANY,
	SOREG,	TWORD|TPOINT|TLL,
		NAREG,	RESC1,
		"AR", },

{ OREG,		INBREG,
	SANY,	TANY,
	SOREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"AR", },

/* Catch-all entry for unmatched operations */
{ FREE,		FREE,
	FREE,	FREE,
	FREE,	FREE,
		FREE,	FREE,
		FREE, },
};

/*
 * Color mapping for register allocation
 */
int
COLORMAP(int c, int *r)
{
	int num = c - 1;

	/* Class A (general purpose registers) */
	if (num < 32) {
		r[0] = num;
		return 1;
	}
	/* Class B (floating point registers) */
	else if (num < 48) {
		r[0] = num;
		return 1;
	}

	return 0;
}
