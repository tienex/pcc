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

#define ANYSIGNED TINT|TLONG|TSHORT|TCHAR
#define ANYUSIGNED TUNSIGNED|TULONG|TUSHORT|TUCHAR
#define ANYFIXED ANYSIGNED|ANYUSIGNED
#define TUWORD TUNSIGNED|TULONG
#define TSWORD TINT|TLONG
#define TWORD TUWORD|TSWORD

struct optab table[] = {
/* First entry must be an empty entry */
{ -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },

/*
 * Conversions
 */
/* Same type - no conversion needed */
{ SCONV,	INAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RLEFT,
		"", },

{ SCONV,	INBREG,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"", },

/*
 * Assignment operations
 */
/* reg = reg */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RDEST,
		"	local.set AR\n", },

/* reg = const */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TWORD|TPOINT,
	SCON,	TWORD|TPOINT,
		0,	RDEST,
		"	i32.const AR\n	local.set AL\n", },

/* reg = mem */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TWORD|TPOINT,
	SOREG|SNAME,	TWORD|TPOINT,
		0,	RDEST,
		"	local.get AR\n	i32.load\n	local.set AL\n", },

/* mem = reg */
{ ASSIGN,	FOREFF|INAREG,
	SOREG|SNAME,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RDEST,
		"	local.get AL\n	local.get AR\n	i32.store\n", },

/* Float/double assignments */
{ ASSIGN,	FOREFF|INBREG,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		0,	RDEST,
		"	local.set AR\n", },

{ ASSIGN,	FOREFF|INBREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"	local.set AR\n", },

/*
 * Load operations (OPLTYPE)
 */
/* Load constant 0 */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SZERO,	TWORD,
		NAREG,	RESC1,
		"	i32.const 0\n", },

/* Load constant 1 */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SONE,	TWORD,
		NAREG,	RESC1,
		"	i32.const 1\n", },

/* Load constant */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SCON,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	i32.const AR\n", },

/* Load from memory */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SOREG|SNAME,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	local.get AR\n	i32.load\n", },

/* Load register */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SAREG,	TWORD|TPOINT,
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

/*
 * Arithmetic operations
 */
/* Addition */
{ PLUS,		INAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.add\n", },

{ PLUS,		INAREG,
	SAREG,	TWORD|TPOINT,
	SONE,	TANY,
		0,	RLEFT,
		"	local.get AL\n	i32.const 1\n	i32.add\n", },

{ PLUS,		INBREG,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		NBREG,	RESC1,
		"	local.get AL\n	local.get AR\n	f32.add\n", },

{ PLUS,		INBREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"	local.get AL\n	local.get AR\n	f64.add\n", },

/* Subtraction */
{ MINUS,	INAREG,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.sub\n", },

{ MINUS,	INBREG,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		NBREG,	RESC1,
		"	local.get AL\n	local.get AR\n	f32.sub\n", },

{ MINUS,	INBREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"	local.get AL\n	local.get AR\n	f64.sub\n", },

/* Multiplication */
{ MUL,		INAREG,
	SAREG,	TWORD,
	SAREG,	TWORD,
		NAREG,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.mul\n", },

{ MUL,		INBREG,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		NBREG,	RESC1,
		"	local.get AL\n	local.get AR\n	f32.mul\n", },

{ MUL,		INBREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"	local.get AL\n	local.get AR\n	f64.mul\n", },

/* Division */
{ DIV,		INAREG,
	SAREG,	TSWORD,
	SAREG,	TSWORD,
		NAREG,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.div_s\n", },

{ DIV,		INAREG,
	SAREG,	TUWORD,
	SAREG,	TUWORD,
		NAREG,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.div_u\n", },

{ DIV,		INBREG,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		NBREG,	RESC1,
		"	local.get AL\n	local.get AR\n	f32.div\n", },

{ DIV,		INBREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"	local.get AL\n	local.get AR\n	f64.div\n", },

/* Modulo */
{ MOD,		INAREG,
	SAREG,	TSWORD,
	SAREG,	TSWORD,
		NAREG,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.rem_s\n", },

{ MOD,		INAREG,
	SAREG,	TUWORD,
	SAREG,	TUWORD,
		NAREG,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.rem_u\n", },

/*
 * Logical operations
 */
{ AND,		INAREG,
	SAREG,	TWORD,
	SAREG,	TWORD,
		NAREG,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.and\n", },

{ OR,		INAREG,
	SAREG,	TWORD,
	SAREG,	TWORD,
		NAREG,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.or\n", },

{ ER,		INAREG,
	SAREG,	TWORD,
	SAREG,	TWORD,
		NAREG,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.xor\n", },

/*
 * Shift operations
 */
{ LS,		INAREG,
	SAREG,	TWORD,
	SAREG,	TWORD,
		NAREG,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.shl\n", },

{ RS,		INAREG,
	SAREG,	TSWORD,
	SAREG,	TWORD,
		NAREG,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.shr_s\n", },

{ RS,		INAREG,
	SAREG,	TUWORD,
	SAREG,	TWORD,
		NAREG,	RESC1,
		"	local.get AL\n	local.get AR\n	i32.shr_u\n", },

/*
 * Unary operations
 */
{ UMINUS,	INAREG,
	SAREG,	TWORD,
	SANY,	TANY,
		NAREG,	RESC1,
		"	i32.const 0\n	local.get AL\n	i32.sub\n", },

{ UMINUS,	INBREG,
	SBREG,	TFLOAT,
	SANY,	TANY,
		NBREG,	RESC1,
		"	local.get AL\n	f32.neg\n", },

{ UMINUS,	INBREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SANY,	TANY,
		NBREG,	RESC1,
		"	local.get AL\n	f64.neg\n", },

{ COMPL,	INAREG,
	SAREG,	TWORD,
	SANY,	TANY,
		NAREG,	RESC1,
		"	local.get AL\n	i32.const -1\n	i32.xor\n", },

/*
 * Indirection (load from pointer)
 */
{ UMUL,		INAREG,
	SANY,	TPOINT|TWORD,
	SOREG,	TWORD,
		NAREG,	RESC1,
		"	local.get AL\n	i32.load\n", },

{ UMUL,		INBREG,
	SANY,	TPOINT|TWORD,
	SOREG,	TFLOAT,
		NBREG,	RESC1,
		"	local.get AL\n	f32.load\n", },

{ UMUL,		INBREG,
	SANY,	TPOINT|TWORD,
	SOREG,	TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"	local.get AL\n	f64.load\n", },

/*
 * Comparison operations
 */
{ EQ,		FORCC,
	SAREG,	TWORD,
	SAREG,	TWORD,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i32.eq\n	br_if LC\n", },

{ NE,		FORCC,
	SAREG,	TWORD,
	SAREG,	TWORD,
		0,	RNOP,
		"	local.get AL\n	local.get AR\n	i32.ne\n	br_if LC\n", },

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

/*
 * Return statement
 */
{ ASSIGN,	FOREFF,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RDEST,
		"	local.get AR\n	return\n", },

/* Must be at end */
{ FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	"help; I'm in trouble\n" },
};

int tablesize = sizeof(table)/sizeof(table[0]);
