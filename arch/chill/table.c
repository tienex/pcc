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
#define SHLL SAREG
#define INLL INAREG
#define SHFL SBREG
#define INFL INBREG

struct optab table[] = {
/* First entry must be an empty entry */
{ -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },

/*
 * Pointer conversions
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
		"	sext.b	A1,A1\n", },

{ SCONV,	INAREG,
	SAREG,	TUCHAR,
	SAREG,	TWORD,
		0,	RLEFT,
		"	andi	A1,A1,255\n", },

/* Convert short to int */
{ SCONV,	INAREG,
	SAREG,	TSHORT,
	SAREG,	TWORD,
		0,	RLEFT,
		"	sext.h	A1,A1\n", },

{ SCONV,	INAREG,
	SAREG,	TUSHORT,
	SAREG,	TWORD,
		0,	RLEFT,
		"	andi	A1,A1,65535\n", },

/* Convert int to long long */
{ SCONV,	INAREG,
	SAREG,	TWORD,
	SAREG,	TLL,
		0,	RLEFT,
		"	sext.w	A1,A1\n", },

{ SCONV,	INAREG,
	SAREG,	TUWORD,
	SAREG,	TLL,
		0,	RLEFT,
		"	zext.w	A1,A1\n", },

/* Convert long long to int */
{ SCONV,	INAREG,
	SAREG,	TLL,
	SAREG,	TWORD,
		0,	RLEFT,
		"", },

/* Convert int to float */
{ SCONV,	INBREG,
	SAREG,	TSWORD,
	SBREG,	TFLOAT,
		NBREG,	RESC1,
		"	fcvt.s.w	A1,AL\n", },

{ SCONV,	INBREG,
	SAREG,	TUWORD,
	SBREG,	TFLOAT,
		NBREG,	RESC1,
		"	fcvt.s.wu	A1,AL\n", },

/* Convert int to double */
{ SCONV,	INBREG,
	SAREG,	TSWORD,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"	fcvt.d.w	A1,AL\n", },

{ SCONV,	INBREG,
	SAREG,	TUWORD,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"	fcvt.d.wu	A1,AL\n", },

/* Convert float to int */
{ SCONV,	INAREG,
	SBREG,	TFLOAT,
	SAREG,	TSWORD,
		NAREG,	RESC1,
		"	fcvt.w.s	A1,AL\n", },

{ SCONV,	INAREG,
	SBREG,	TFLOAT,
	SAREG,	TUWORD,
		NAREG,	RESC1,
		"	fcvt.wu.s	A1,AL\n", },

/* Convert double to int */
{ SCONV,	INAREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SAREG,	TSWORD,
		NAREG,	RESC1,
		"	fcvt.w.d	A1,AL\n", },

{ SCONV,	INAREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SAREG,	TUWORD,
		NAREG,	RESC1,
		"	fcvt.wu.d	A1,AL\n", },

/* Convert float to double */
{ SCONV,	INBREG,
	SBREG,	TFLOAT,
	SBREG,	TDOUBLE|TLDOUBLE,
		0,	RLEFT,
		"	fcvt.d.s	A1,A1\n", },

/* Convert double to float */
{ SCONV,	INBREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TFLOAT,
		0,	RLEFT,
		"	fcvt.s.d	A1,A1\n", },

/*
 * Assignment operations
 */
/* reg = reg */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TWORD|TPOINT|TLL,
	SAREG,	TWORD|TPOINT|TLL,
		0,	RDEST,
		"	mv	AL,AR\n", },

/* reg = const */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TWORD|TPOINT,
	SCON,	TWORD|TPOINT,
		0,	RDEST,
		"	li	AL,AR\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TLL,
	SCON,	TLL,
		0,	RDEST,
		"	li	AL,AR\n", },

/* reg = mem */
{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TWORD|TPOINT,
	SOREG|SNAME,	TWORD|TPOINT,
		0,	RDEST,
		"	lw	AL,AR\n", },

{ ASSIGN,	FOREFF|INAREG,
	SAREG,	TLL,
	SOREG|SNAME,	TLL,
		0,	RDEST,
		"	ld	AL,AR\n", },

/* mem = reg */
{ ASSIGN,	FOREFF,
	SOREG|SNAME,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RDEST,
		"	sw	AR,AL\n", },

{ ASSIGN,	FOREFF,
	SOREG|SNAME,	TLL,
	SAREG,	TLL,
		0,	RDEST,
		"	sd	AR,AL\n", },

/* mem = const */
{ ASSIGN,	FOREFF,
	SOREG|SNAME,	TWORD|TPOINT,
	SCON,	TWORD|TPOINT,
		NAREG,	0,
		"	li	A1,AR\n	sw	A1,AL\n", },

{ ASSIGN,	FOREFF,
	SOREG|SNAME,	TLL,
	SCON,	TLL,
		NAREG,	0,
		"	li	A1,AR\n	sd	A1,AL\n", },

/* Float/double assignments */
{ ASSIGN,	FOREFF|INBREG,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		0,	RDEST,
		"	fmv.s	AL,AR\n", },

{ ASSIGN,	FOREFF|INBREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"	fmv.d	AL,AR\n", },

{ ASSIGN,	FOREFF|INBREG,
	SOREG|SNAME,	TFLOAT,
	SBREG,	TFLOAT,
		0,	RDEST,
		"	fsw	AR,AL\n", },

{ ASSIGN,	FOREFF|INBREG,
	SOREG|SNAME,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"	fsd	AR,AL\n", },

{ ASSIGN,	FOREFF|INBREG,
	SBREG,	TFLOAT,
	SOREG|SNAME,	TFLOAT,
		0,	RDEST,
		"	flw	AL,AR\n", },

{ ASSIGN,	FOREFF|INBREG,
	SBREG,	TDOUBLE|TLDOUBLE,
	SOREG|SNAME,	TDOUBLE|TLDOUBLE,
		0,	RDEST,
		"	fld	AL,AR\n", },

/*
 * Load operations (OPLTYPE)
 */
/* Load constant 0 */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SZERO,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	li	A1,0\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SZERO,	TLL,
		NAREG,	RESC1,
		"	li	A1,0\n", },

/* Load constant 1 */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SONE,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	li	A1,1\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SONE,	TLL,
		NAREG,	RESC1,
		"	li	A1,1\n", },

/* Load constant */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SCON,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	li	A1,AR\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SCON,	TLL,
		NAREG,	RESC1,
		"	li	A1,AR\n", },

/* Load from memory */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SOREG|SNAME,	TWORD|TPOINT,
		NAREG,	RESC1,
		"	lw	A1,AR\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SOREG|SNAME,	TLL,
		NAREG,	RESC1,
		"	ld	A1,AR\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SOREG|SNAME,	TCHAR,
		NAREG,	RESC1,
		"	lb	A1,AR\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SOREG|SNAME,	TUCHAR,
		NAREG,	RESC1,
		"	lbu	A1,AR\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SOREG|SNAME,	TSHORT,
		NAREG,	RESC1,
		"	lh	A1,AR\n", },

{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SOREG|SNAME,	TUSHORT,
		NAREG,	RESC1,
		"	lhu	A1,AR\n", },

/* Load register */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SAREG,	TWORD|TPOINT|TLL,
		NAREG,	RESC1,
		"	mv	A1,AR\n", },

/* Float load operations */
{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SBREG,	TFLOAT,
		NBREG,	RESC1,
		"	fmv.s	A1,AR\n", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"	fmv.d	A1,AR\n", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SOREG|SNAME,	TFLOAT,
		NBREG,	RESC1,
		"	flw	A1,AR\n", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SOREG|SNAME,	TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"	fld	A1,AR\n", },

/*
 * Arithmetic operations
 */
/* Addition */
{ PLUS,		INAREG|FOREFF,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL,	RESC1,
		"	add	A1,AL,AR\n", },

{ PLUS,		INAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TLL,
		NAREG|NASL,	RESC1,
		"	add	A1,AL,AR\n", },

{ PLUS,		INAREG|FOREFF,
	SAREG,	TWORD|TPOINT,
	SONE,	TANY,
		0,	RLEFT,
		"	addi	AL,AL,1\n", },

{ PLUS,		INAREG|FOREFF,
	SAREG,	TLL,
	SONE,	TANY,
		0,	RLEFT,
		"	addi	AL,AL,1\n", },

{ PLUS,		INBREG|FOREFF,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		NBREG|NBSL,	RESC1,
		"	fadd.s	A1,AL,AR\n", },

{ PLUS,		INBREG|FOREFF,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG|NBSL,	RESC1,
		"	fadd.d	A1,AL,AR\n", },

/* Subtraction */
{ MINUS,	INAREG|FOREFF,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		NAREG|NASL,	RESC1,
		"	sub	A1,AL,AR\n", },

{ MINUS,	INAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TLL,
		NAREG|NASL,	RESC1,
		"	sub	A1,AL,AR\n", },

{ MINUS,	INAREG|FOREFF,
	SAREG,	TWORD|TPOINT,
	SONE,	TANY,
		0,	RLEFT,
		"	addi	AL,AL,-1\n", },

{ MINUS,	INAREG|FOREFF,
	SAREG,	TLL,
	SONE,	TANY,
		0,	RLEFT,
		"	addi	AL,AL,-1\n", },

{ MINUS,	INBREG|FOREFF,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		NBREG|NBSL,	RESC1,
		"	fsub.s	A1,AL,AR\n", },

{ MINUS,	INBREG|FOREFF,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG|NBSL,	RESC1,
		"	fsub.d	A1,AL,AR\n", },

/* Multiplication */
{ MUL,		INAREG|FOREFF,
	SAREG,	TWORD,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	mul	A1,AL,AR\n", },

{ MUL,		INAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TLL,
		NAREG|NASL,	RESC1,
		"	mul	A1,AL,AR\n", },

{ MUL,		INBREG|FOREFF,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		NBREG|NBSL,	RESC1,
		"	fmul.s	A1,AL,AR\n", },

{ MUL,		INBREG|FOREFF,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG|NBSL,	RESC1,
		"	fmul.d	A1,AL,AR\n", },

/* Division */
{ DIV,		INAREG|FOREFF,
	SAREG,	TSWORD,
	SAREG,	TSWORD,
		NAREG|NASL,	RESC1,
		"	div	A1,AL,AR\n", },

{ DIV,		INAREG|FOREFF,
	SAREG,	TUWORD,
	SAREG,	TUWORD,
		NAREG|NASL,	RESC1,
		"	divu	A1,AL,AR\n", },

{ DIV,		INAREG|FOREFF,
	SAREG,	TLONGLONG,
	SAREG,	TLONGLONG,
		NAREG|NASL,	RESC1,
		"	div	A1,AL,AR\n", },

{ DIV,		INAREG|FOREFF,
	SAREG,	TULONGLONG,
	SAREG,	TULONGLONG,
		NAREG|NASL,	RESC1,
		"	divu	A1,AL,AR\n", },

{ DIV,		INBREG|FOREFF,
	SBREG,	TFLOAT,
	SBREG,	TFLOAT,
		NBREG|NBSL,	RESC1,
		"	fdiv.s	A1,AL,AR\n", },

{ DIV,		INBREG|FOREFF,
	SBREG,	TDOUBLE|TLDOUBLE,
	SBREG,	TDOUBLE|TLDOUBLE,
		NBREG|NBSL,	RESC1,
		"	fdiv.d	A1,AL,AR\n", },

/* Modulo */
{ MOD,		INAREG|FOREFF,
	SAREG,	TSWORD,
	SAREG,	TSWORD,
		NAREG|NASL,	RESC1,
		"	rem	A1,AL,AR\n", },

{ MOD,		INAREG|FOREFF,
	SAREG,	TUWORD,
	SAREG,	TUWORD,
		NAREG|NASL,	RESC1,
		"	remu	A1,AL,AR\n", },

{ MOD,		INAREG|FOREFF,
	SAREG,	TLONGLONG,
	SAREG,	TLONGLONG,
		NAREG|NASL,	RESC1,
		"	rem	A1,AL,AR\n", },

{ MOD,		INAREG|FOREFF,
	SAREG,	TULONGLONG,
	SAREG,	TULONGLONG,
		NAREG|NASL,	RESC1,
		"	remu	A1,AL,AR\n", },

/*
 * Logical operations
 */
{ AND,		INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SAREG,	TWORD|TLL,
		NAREG|NASL,	RESC1,
		"	and	A1,AL,AR\n", },

{ OR,		INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SAREG,	TWORD|TLL,
		NAREG|NASL,	RESC1,
		"	or	A1,AL,AR\n", },

{ ER,		INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SAREG,	TWORD|TLL,
		NAREG|NASL,	RESC1,
		"	xor	A1,AL,AR\n", },

/*
 * Shift operations
 */
{ LS,		INAREG|FOREFF,
	SAREG,	TWORD,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	sll	A1,AL,AR\n", },

{ LS,		INAREG|FOREFF,
	SAREG,	TLL,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	sll	A1,AL,AR\n", },

{ RS,		INAREG|FOREFF,
	SAREG,	TSWORD,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	sra	A1,AL,AR\n", },

{ RS,		INAREG|FOREFF,
	SAREG,	TUWORD,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	srl	A1,AL,AR\n", },

{ RS,		INAREG|FOREFF,
	SAREG,	TLONGLONG,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	sra	A1,AL,AR\n", },

{ RS,		INAREG|FOREFF,
	SAREG,	TULONGLONG,
	SAREG,	TWORD,
		NAREG|NASL,	RESC1,
		"	srl	A1,AL,AR\n", },

/*
 * Unary operations
 */
{ UMINUS,	INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	neg	A1,AL\n", },

{ COMPL,	INAREG|FOREFF,
	SAREG,	TWORD|TLL,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	not	A1,AL\n", },

{ UMINUS,	INBREG|FOREFF,
	SBREG,	TFLOAT,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,
		"	fneg.s	A1,AL\n", },

{ UMINUS,	INBREG|FOREFF,
	SBREG,	TDOUBLE|TLDOUBLE,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,
		"	fneg.d	A1,AL\n", },

/*
 * Memory indirection (dereference)
 */
{ UMUL,		INAREG,
	SANY,	TANY,
	SOREG,	TPOINT|TWORD,
		NAREG,	RESC1,
		"	lw	A1,AL\n", },

{ UMUL,		INAREG,
	SANY,	TANY,
	SOREG,	TLL,
		NAREG,	RESC1,
		"	ld	A1,AL\n", },

{ UMUL,		INBREG,
	SANY,	TANY,
	SOREG,	TFLOAT,
		NBREG,	RESC1,
		"	flw	A1,AL\n", },

{ UMUL,		INBREG,
	SANY,	TANY,
	SOREG,	TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"	fld	A1,AL\n", },

/*
 * Function calls
 */
{ CALL,		FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	0,
		"	call	CL\nZC", },

{ CALL,		INAREG,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	call	CL\nZC", },

{ CALL,		INBREG,
	SCON,	TANY,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,
		"	call	CL\nZC", },

{ UCALL,	FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	0,
		"	call	CL\n", },

{ UCALL,	INAREG,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	call	CL\n", },

{ UCALL,	INBREG,
	SCON,	TANY,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,
		"	call	CL\n", },

/*
 * Comparisons - set to 1 if true, 0 if false
 */
{ EQ,		FORCC,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RESCC,
		"	sub	ZN,AL,AR\n", },

{ NE,		FORCC,
	SAREG,	TWORD|TPOINT,
	SAREG,	TWORD|TPOINT,
		0,	RESCC,
		"	sub	ZN,AL,AR\n", },

{ LT,		FORCC,
	SAREG,	TSWORD,
	SAREG,	TSWORD,
		0,	RESCC,
		"	slt	ZN,AL,AR\n", },

{ LT,		FORCC,
	SAREG,	TUWORD|TPOINT,
	SAREG,	TUWORD|TPOINT,
		0,	RESCC,
		"	sltu	ZN,AL,AR\n", },

{ LE,		FORCC,
	SAREG,	TSWORD,
	SAREG,	TSWORD,
		0,	RESCC,
		"	slt	ZN,AR,AL\nxori	ZN,ZN,1\n", },

{ LE,		FORCC,
	SAREG,	TUWORD|TPOINT,
	SAREG,	TUWORD|TPOINT,
		0,	RESCC,
		"	sltu	ZN,AR,AL\nxori	ZN,ZN,1\n", },

{ GT,		FORCC,
	SAREG,	TSWORD,
	SAREG,	TSWORD,
		0,	RESCC,
		"	slt	ZN,AR,AL\n", },

{ GT,		FORCC,
	SAREG,	TUWORD|TPOINT,
	SAREG,	TUWORD|TPOINT,
		0,	RESCC,
		"	sltu	ZN,AR,AL\n", },

{ GE,		FORCC,
	SAREG,	TSWORD,
	SAREG,	TSWORD,
		0,	RESCC,
		"	slt	ZN,AL,AR\nxori	ZN,ZN,1\n", },

{ GE,		FORCC,
	SAREG,	TUWORD|TPOINT,
	SAREG,	TUWORD|TPOINT,
		0,	RESCC,
		"	sltu	ZN,AL,AR\nxori	ZN,ZN,1\n", },

/*
 * Branches
 */
{ GOTO,		FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	0,
		"	j	LL\n", },

/*
 * Register moves (for register allocator)
 */
{ REG,		INAREG,
	SANY,	TANY,
	SAREG,	TWORD|TPOINT|TLL,
		NAREG,	RESC1,
		"	mv	A1,AL\n", },

{ REG,		INBREG,
	SANY,	TANY,
	SBREG,	TFLOAT|TDOUBLE|TLDOUBLE,
		NBREG,	RESC1,
		"	fmv.d	A1,AL\n", },

/*
 * End of table
 */
{ FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	FREE,	"help; I'm in trouble\n" },
};

int tablesize = sizeof(table)/sizeof(table[0]);
