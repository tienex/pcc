/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
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

#include "pass2.h"

#define TLL TLONGLONG|TULONGLONG
#define ANYSIGNED TINT|TLONG|TSHORT|TCHAR
#define ANYUSIGNED TUNSIGNED|TULONG|TUSHORT|TUCHAR
#define ANYFIXED ANYSIGNED|ANYUSIGNED
#define TUWORD TUNSIGNED|TULONG
#define TSWORD TINT|TLONG
#define TWORD TUWORD|TSWORD
#define SHINT SAREG
#define ININT INAREG
#define SHLL SAREG  /* WebAssembly uses same regs for i64 */
#define INLL INAREG
#define SHFL SBREG
#define INFL INBREG

struct optab table[] = {
/* First entry must be an empty entry */
{ -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },

/*
 * Pointer conversions - no-ops in WebAssembly
 */
{ PCONV,	INAREG,
	SAREG,	TPOINT|TWORD,
	SAREG,	TPOINT|TWORD,
		0,	RLEFT,
		"", },

/*
 * Type conversions
 */
/* Convert char to char */
{ SCONV,	INAREG,
	SAREG,	TCHAR|TUCHAR,
	SAREG,	TCHAR|TUCHAR,
		0,	RLEFT,
		"", },

/* Convert int/pointer to int/pointer */
{ SCONV,	ININT,
	SHINT,	TPOINT|TWORD,
	SANY,	TWORD,
		0,	RLEFT,
		"", },

/* Convert long long to long long */
{ SCONV,	INLL,
	SHLL,	TLL,
	SHLL,	TLL,
		0,	RLEFT,
		"", },

/* Convert float/double to float/double */
{ SCONV,	INFL,
	SHFL,	TFLOAT|TDOUBLE|TLDOUBLE,
	SHFL,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"", },

/* Convert char to int */
{ SCONV,	INAREG,
	SAREG,	TCHAR,
	SAREG,	TWORD,
		0,	RLEFT,
		"	;; sign extend i8 to i32\n"
		"	i32.const 24\n	i32.shl\n	i32.const 24\n	i32.shr_s\n", },

{ SCONV,	INAREG,
	SAREG,	TUCHAR,
	SAREG,	TWORD,
		0,	RLEFT,
		"	i32.const 255\n	i32.and\n", },

/* Convert short to int */
{ SCONV,	INAREG,
	SAREG,	TSHORT,
	SAREG,	TWORD,
		0,	RLEFT,
		"	;; sign extend i16 to i32\n"
		"	i32.const 16\n	i32.shl\n	i32.const 16\n	i32.shr_s\n", },

{ SCONV,	INAREG,
	SAREG,	TUSHORT,
	SAREG,	TWORD,
		0,	RLEFT,
		"	i32.const 65535\n	i32.and\n", },

/* Convert int to long long */
{ SCONV,	INAREG,
	SAREG,	TWORD,
	SAREG,	TLL,
		0,	RLEFT,
		"	i64.extend_i32_s\n", },

{ SCONV,	INAREG,
	SAREG,	TUWORD,
	SAREG,	TLL,
		0,	RLEFT,
		"	i64.extend_i32_u\n", },

/* Convert long long to int */
{ SCONV,	INAREG,
	SAREG,	TLL,
	SAREG,	TWORD,
		0,	RLEFT,
		"	i32.wrap_i64\n", },

/* Convert int to float */
{ SCONV,	INBREG,
	SAREG,	TSWORD,
	SBREG,	TFLOAT,
		NBREG,	RESC1,
		"	f32.convert_i32_s\n", },

{ SCONV,	INBREG,
	SAREG,	TUWORD,
	SBREG,	TFLOAT,
		NBREG,	RESC1,
		"	f32.convert_i32_u\n", },

/* Convert int to double */
{ SCONV,	INBREG,
	SAREG,	TSWORD,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"	f64.convert_i32_s\n", },

{ SCONV,	INBREG,
	SAREG,	TUWORD,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"	f64.convert_i32_u\n", },

/* Convert float to int */
{ SCONV,	INAREG,
	SBREG,	TFLOAT,
	SAREG,	TSWORD,
		NAREG,	RESC1,
		"	i32.trunc_f32_s\n", },

{ SCONV,	INAREG,
	SBREG,	TFLOAT,
	SAREG,	TUWORD,
		NAREG,	RESC1,
		"	i32.trunc_f32_u\n", },

/* Convert double to int */
{ SCONV,	INAREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SAREG,	TSWORD,
		NAREG,	RESC1,
		"	i32.trunc_f64_s\n", },

{ SCONV,	INAREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SAREG,	TUWORD,
		NAREG,	RESC1,
		"	i32.trunc_f64_u\n", },

/* Convert float to double */
{ SCONV,	INBREG,
	SBREG,	TFLOAT,
	SBREG,	TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"	f64.promote_f32\n", },

/* Convert double to float */
{ SCONV,	INBREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT,
		0,	RLEFT,
		"	f32.demote_f64\n", },

/*
 * Assignment operations
 */
/* reg = reg */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TWORD|TPOINT|TLL,
	SAREG,	TWORD|TPOINT|TLL,
		0,	RDEST,
		"	local.get AR\n	local.set AL\n", },

/* reg = const */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TWORD|TPOINT,
	SCON,	TWORD|TPOINT,
		0,	RDEST,
		"	i32.const AR\n	local.set AL\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TLL,
	SCON,	TLL,
		0,	RDEST,
		"	i64.const AR\n	local.set AL\n", },

/* reg = mem */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TWORD|TPOINT,
	SOREG|SNAME,	TWORD|TPOINT,
		0,	RDEST,
		"	local.get AR\n	i32.load\n	local.set AL\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TLL,
	SOREG|SNAME,	TLL,
		0,	RDEST,
		"	local.get AR\n	i64.load\n	local.set AL\n", },

/* mem = reg */
{ ASSIGN,	FOREFF,
	SOREG|SNAME,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RDEST,
		"	local.get AL\n	local.get AR\n	i32.store\n", },

{ ASSIGN,	FOREFF,
	SOREG|SNAME,	TLL,
	SAREG,	TLL,
		0,	RDEST,
		"	local.get AL\n	local.get AR\n	i64.store\n", },

/* mem = const */
{ ASSIGN,	FOREFF,
	SOREG|SNAME,	TWORD|TPOINT,
	SCON,	TWORD|TPOINT,
		0,	RDEST,
		"	local.get AL\n	i32.const AR\n	i32.store\n", },

{ ASSIGN,	FOREFF,
	SOREG|SNAME,	TLL,
	SCON,	TLL,
		0,	RDEST,
		"	local.get AL\n	i64.const AR\n	i64.store\n", },

/* Float/double assignments */
{ ASSIGN,	FOREFF|INBREG,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		0,	RDEST,
		"	local.get AR\n	local.set AL\n", },

{ ASSIGN,	FOREFF|INBREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"	local.get AR\n	local.set AL\n", },

{ ASSIGN,	FOREFF|INBREG,
	SOREG|SNAME,	TFLOAT,
	SBREG,	TFLOAT,
		0,	RDEST,
		"	local.get AL\n	local.get AR\n	f32.store\n", },

{ ASSIGN,	FOREFF|INBREG,
	SOREG|SNAME,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"	local.get AL\n	local.get AR\n	f64.store\n", },

{ ASSIGN,	FOREFF|INBREG,
	SBREG,	TFLOAT,
	SOREG|SNAME,	TFLOAT,
		0,	RDEST,
		"	local.get AR\n	f32.load\n	local.set AL\n", },

{ ASSIGN,	FOREFF|INBREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SOREG|SNAME,	TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"	local.get AR\n	f64.load\n	local.set AL\n", },

/*
 * Load operations (OPLTYPE)
 */
/* Load constant 0 */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SZERO,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	i32.const 0\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SZERO,	TLL,
		NAREG,	RESC1,
		"	i64.const 0\n", },

/* Load constant 1 */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SONE,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	i32.const 1\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SONE,	TLL,
		NAREG,	RESC1,
		"	i64.const 1\n", },

/* Load constant */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SCON,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	i32.const AR\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SCON,	TLL,
		NAREG,	RESC1,
		"	i64.const AR\n", },

/* Load from memory */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SOREG|SNAME,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	local.get AR\n	i32.load\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SOREG|SNAME,	TLL,
		NAREG,	RESC1,
		"	local.get AR\n	i64.load\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SOREG|SNAME,	TCHAR,
		NAREG,	RESC1,
		"	local.get AR\n	i32.load8_s\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SOREG|SNAME,	TUCHAR,
		NAREG,	RESC1,
		"	local.get AR\n	i32.load8_u\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SOREG|SNAME,	TSHORT,
		NAREG,	RESC1,
		"	local.get AR\n	i32.load16_s\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SOREG|SNAME,	TUSHORT,
		NAREG,	RESC1,
		"	local.get AR\n	i32.load16_u\n", },

/* Load register */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SAREG,	TWORD|TPOINT|TLL,
		NAREG,	RESC1,
		"	local.get AR\n", },

/* Float load operations */
{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SBREG,	TFLOAT,
		NBREG,	RESC1,
		"	local.get AR\n", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"	local.get AR\n", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SOREG|SNAME,	TFLOAT,
		NBREG,	RESC1,
		"	local.get AR\n	f32.load\n", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SOREG|SNAME,	TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"	local.get AR\n	f64.load\n", },

/*
 * Arithmetic operations
 */
/* Addition */
{ PLUS,		INAREG|FOREFF,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.add\n", },

{ PLUS,		INAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TLL,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i64.add\n", },

{ PLUS,		INAREG|FOREFF,
	SAREG,	TWORD|TPOINT,
	SONE,	TANY,
		0,	RLEFT,
		"	local.get AL\n	i32.const 1\n	i32.add\n	local.set AL\n", },

{ PLUS,		INAREG|FOREFF,
	SAREG,	TLL,
	SONE,	TANY,
		0,	RLEFT,
		"	local.get AL\n	i64.const 1\n	i64.add\n	local.set AL\n", },

{ PLUS,		INBREG|FOREFF,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		NBREG|NBSL,	RESC1,
		"	local.get AL\n	local.get AR\n	f32.add\n", },

{ PLUS,		INBREG|FOREFF,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG|NBSL,	RESC1,
		"	local.get AL\n	local.get AR\n	f64.add\n", },

/* Subtraction */
{ MINUS,	INAREG|FOREFF,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.sub\n", },

{ MINUS,	INAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TLL,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i64.sub\n", },

{ MINUS,	INAREG|FOREFF,
	SAREG,	TWORD|TPOINT,
	SONE,	TANY,
		0,	RLEFT,
		"	local.get AL\n	i32.const 1\n	i32.sub\n	local.set AL\n", },

{ MINUS,	INAREG|FOREFF,
	SAREG,	TLL,
	SONE,	TANY,
		0,	RLEFT,
		"	local.get AL\n	i64.const 1\n	i64.sub\n	local.set AL\n", },

{ MINUS,	INBREG|FOREFF,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		NBREG|NBSL,	RESC1,
		"	local.get AL\n	local.get AR\n	f32.sub\n", },

{ MINUS,	INBREG|FOREFF,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG|NBSL,	RESC1,
		"	local.get AL\n	local.get AR\n	f64.sub\n", },

/* Multiplication */
{ MUL,		INAREG|FOREFF,
	SAREG,	TWORD,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.mul\n", },

{ MUL,		INAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TLL,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i64.mul\n", },

{ MUL,		INBREG|FOREFF,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		NBREG|NBSL,	RESC1,
		"	local.get AL\n	local.get AR\n	f32.mul\n", },

{ MUL,		INBREG|FOREFF,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG|NBSL,	RESC1,
		"	local.get AL\n	local.get AR\n	f64.mul\n", },

/* Division */
{ DIV,		INAREG|FOREFF,
	SAREG,	TSWORD,
	SAREG,	TSWORD,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.div_s\n", },

{ DIV,		INAREG|FOREFF,
	SAREG,	TUWORD,
	SAREG,	TUWORD,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.div_u\n", },

{ DIV,		INAREG|FOREFF,
	SAREG,	TLONGLONG,
	SAREG,	TLONGLONG,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i64.div_s\n", },

{ DIV,		INAREG|FOREFF,
	SAREG,	TULONGLONG,
	SAREG,	TULONGLONG,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i64.div_u\n", },

{ DIV,		INBREG|FOREFF,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		NBREG|NBSL,	RESC1,
		"	local.get AL\n	local.get AR\n	f32.div\n", },

{ DIV,		INBREG|FOREFF,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG|NBSL,	RESC1,
		"	local.get AL\n	local.get AR\n	f64.div\n", },

/* Modulo */
{ MOD,		INAREG|FOREFF,
	SAREG,	TSWORD,
	SAREG,	TSWORD,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.rem_s\n", },

{ MOD,		INAREG|FOREFF,
	SAREG,	TUWORD,
	SAREG,	TUWORD,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.rem_u\n", },

{ MOD,		INAREG|FOREFF,
	SAREG,	TLONGLONG,
	SAREG,	TLONGLONG,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i64.rem_s\n", },

{ MOD,		INAREG|FOREFF,
	SAREG,	TULONGLONG,
	SAREG,	TULONGLONG,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i64.rem_u\n", },

/*
 * Logical operations
 */
{ AND,		INAREG|FOREFF,
	SAREG,	TWORD,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.and\n", },

{ AND,		INAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TLL,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i64.and\n", },

{ OR,		INAREG|FOREFF,
	SAREG,	TWORD,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.or\n", },

{ OR,		INAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TLL,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i64.or\n", },

{ ER,		INAREG|FOREFF,
	SAREG,	TWORD,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.xor\n", },

{ ER,		INAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TLL,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i64.xor\n", },

/*
 * Shift operations
 */
{ LS,		INAREG|FOREFF,
	SAREG,	TWORD,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.shl\n", },

{ LS,		INAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TLL,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i64.shl\n", },

{ RS,		INAREG|FOREFF,
	SAREG,	TSWORD,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.shr_s\n", },

{ RS,		INAREG|FOREFF,
	SAREG,	TUWORD,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.shr_u\n", },

{ RS,		INAREG|FOREFF,
	SAREG,	TLONGLONG,
	SAREG,	TLL,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i64.shr_s\n", },

{ RS,		INAREG|FOREFF,
	SAREG,	TULONGLONG,
	SAREG,	TLL,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	local.get AR\n	i64.shr_u\n", },

/*
 * Unary operations
 */
{ UMINUS,	INAREG|FOREFF,
	SAREG,	TWORD,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	i32.const 0\n	local.get AL\n	i32.sub\n", },

{ UMINUS,	INAREG|FOREFF,
	SAREG,	TLL,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	i64.const 0\n	local.get AL\n	i64.sub\n", },

{ UMINUS,	INBREG|FOREFF,
	SBREG,	TFLOAT,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,
		"	local.get AL\n	f32.neg\n", },

{ UMINUS,	INBREG|FOREFF,
	SBREG,	TDOUBLE|TLDOUBLE,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,
		"	local.get AL\n	f64.neg\n", },

{ COMPL,	INAREG|FOREFF,
	SAREG,	TWORD,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	i32.const -1\n	i32.xor\n", },

{ COMPL,	INAREG|FOREFF,
	SAREG,	TLL,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	i64.const -1\n	i64.xor\n", },

/*
 * Indirection (load from pointer)
 */
{ UMUL,		INAREG,
	SANY,	TPOINT|TWORD,
	SOREG,	TWORD|TPOINT,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	i32.load\n", },

{ UMUL,		INAREG,
	SANY,	TPOINT|TWORD,
	SOREG,	TLL,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	i64.load\n", },

{ UMUL,		INAREG,
	SANY,	TPOINT|TWORD,
	SOREG,	TCHAR,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	i32.load8_s\n", },

{ UMUL,		INAREG,
	SANY,	TPOINT|TWORD,
	SOREG,	TUCHAR,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	i32.load8_u\n", },

{ UMUL,		INAREG,
	SANY,	TPOINT|TWORD,
	SOREG,	TSHORT,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	i32.load16_s\n", },

{ UMUL,		INAREG,
	SANY,	TPOINT|TWORD,
	SOREG,	TUSHORT,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	i32.load16_u\n", },

{ UMUL,		INBREG,
	SANY,	TPOINT|TWORD,
	SOREG,	TFLOAT,
		NBREG|NBSL,	RESC1,
		"	local.get AL\n	f32.load\n", },

{ UMUL,		INBREG,
	SANY,	TPOINT|TWORD,
	SOREG,	TDOUBLE|TLDOUBLE,
		NBREG|NBSL,	RESC1,
		"	local.get AL\n	f64.load\n", },

/*
 * Function calls
 */
{ CALL,		FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	0,
		"	call $AL\n", },

{ UCALL,	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	0,
		"	call $AL\n", },

{ CALL,		INAREG,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	call $AL\n", },

{ UCALL,	INAREG,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	call $AL\n", },

{ CALL,		INBREG,
	SCON,	TANY,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,
		"	call $AL\n", },

{ UCALL,	INBREG,
	SCON,	TANY,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,
		"	call $AL\n", },

/* Indirect calls */
{ CALL,		FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	0,
		"	local.get AL\n	call_indirect\n", },

{ UCALL,	FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	0,
		"	local.get AL\n	call_indirect\n", },

{ CALL,		INAREG,
	SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	call_indirect\n", },

{ UCALL,	INAREG,
	SAREG,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	local.get AL\n	call_indirect\n", },

{ CALL,		INBREG,
	SAREG,	TANY,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,
		"	local.get AL\n	call_indirect\n", },

{ UCALL,	INBREG,
	SAREG,	TANY,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,
		"	local.get AL\n	call_indirect\n", },

/* Function arguments */
{ FUNARG,	FOREFF,
	SAREG,	TWORD|TPOINT|TLL,
	SANY,	TWORD|TPOINT|TLL,
		0,	RNULL,
		"	local.get AL\n", },

{ FUNARG,	FOREFF,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SANY,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RNULL,
		"	local.get AL\n", },

/*
 * Comparison operations
 */
{ EQ,		FORCC,
	SAREG,	TWORD,
	SAREG,	TWORD,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i32.eq\n	br_if LC\n", },

{ EQ,		FORCC,
	SAREG,	TLL,
	SAREG,	TLL,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i64.eq\n	br_if LC\n", },

{ NE,		FORCC,
	SAREG,	TWORD,
	SAREG,	TWORD,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i32.ne\n	br_if LC\n", },

{ NE,		FORCC,
	SAREG,	TLL,
	SAREG,	TLL,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i64.ne\n	br_if LC\n", },

{ LT,		FORCC,
	SAREG,	TSWORD,
	SAREG,	TSWORD,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i32.lt_s\n	br_if LC\n", },

{ LT,		FORCC,
	SAREG,	TUWORD,
	SAREG,	TUWORD,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i32.lt_u\n	br_if LC\n", },

{ LT,		FORCC,
	SAREG,	TLONGLONG,
	SAREG,	TLONGLONG,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i64.lt_s\n	br_if LC\n", },

{ LT,		FORCC,
	SAREG,	TULONGLONG,
	SAREG,	TULONGLONG,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i64.lt_u\n	br_if LC\n", },

{ LE,		FORCC,
	SAREG,	TSWORD,
	SAREG,	TSWORD,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i32.le_s\n	br_if LC\n", },

{ LE,		FORCC,
	SAREG,	TUWORD,
	SAREG,	TUWORD,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i32.le_u\n	br_if LC\n", },

{ LE,		FORCC,
	SAREG,	TLONGLONG,
	SAREG,	TLONGLONG,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i64.le_s\n	br_if LC\n", },

{ LE,		FORCC,
	SAREG,	TULONGLONG,
	SAREG,	TULONGLONG,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i64.le_u\n	br_if LC\n", },

{ GT,		FORCC,
	SAREG,	TSWORD,
	SAREG,	TSWORD,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i32.gt_s\n	br_if LC\n", },

{ GT,		FORCC,
	SAREG,	TUWORD,
	SAREG,	TUWORD,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i32.gt_u\n	br_if LC\n", },

{ GT,		FORCC,
	SAREG,	TLONGLONG,
	SAREG,	TLONGLONG,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i64.gt_s\n	br_if LC\n", },

{ GT,		FORCC,
	SAREG,	TULONGLONG,
	SAREG,	TULONGLONG,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i64.gt_u\n	br_if LC\n", },

{ GE,		FORCC,
	SAREG,	TSWORD,
	SAREG,	TSWORD,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i32.ge_s\n	br_if LC\n", },

{ GE,		FORCC,
	SAREG,	TUWORD,
	SAREG,	TUWORD,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i32.ge_u\n	br_if LC\n", },

{ GE,		FORCC,
	SAREG,	TLONGLONG,
	SAREG,	TLONGLONG,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i64.ge_s\n	br_if LC\n", },

{ GE,		FORCC,
	SAREG,	TULONGLONG,
	SAREG,	TULONGLONG,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i64.ge_u\n	br_if LC\n", },

/* Floating point comparisons */
{ EQ,		FORCC,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	f32.eq\n	br_if LC\n", },

{ EQ,		FORCC,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	f64.eq\n	br_if LC\n", },

{ NE,		FORCC,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	f32.ne\n	br_if LC\n", },

{ NE,		FORCC,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	f64.ne\n	br_if LC\n", },

{ LT,		FORCC,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	f32.lt\n	br_if LC\n", },

{ LT,		FORCC,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	f64.lt\n	br_if LC\n", },

{ LE,		FORCC,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	f32.le\n	br_if LC\n", },

{ LE,		FORCC,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	f64.le\n	br_if LC\n", },

{ GT,		FORCC,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	f32.gt\n	br_if LC\n", },

{ GT,		FORCC,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	f64.gt\n	br_if LC\n", },

{ GE,		FORCC,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	f32.ge\n	br_if LC\n", },

{ GE,		FORCC,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	f64.ge\n	br_if LC\n", },

/* Must be at end */
{ FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	"help; I'm in trouble\n" },
};

int tablesize = sizeof(table)/sizeof(table[0]);
