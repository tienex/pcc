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

#include "pass2.h"

#define TWORD TINT|TUNSIGNED|TPOINT
#define TAREG TINT|TUNSIGNED|TPOINT|TSHORT|TUSHORT|TCHAR|TUCHAR
#define TBREG TLONGLONG|TULONGLONG|TDOUBLE
#define ANYSIGNED TINT|TSHORT|TCHAR
#define ANYUSIGNED TUNSIGNED|TUSHORT|TUCHAR|TPOINT
#define AWD SNAME|SOREG|SCON
#define SAWM SNAME|SOREG|STARNM|STARREG

/*
 * Instruction table for C90 backend
 * Maps IR operations to C code patterns
 */
struct optab table[] = {
/* First entry must be an empty entry */
{ -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },

/* Pointer conversions */
{ PCONV,	INAREG,
	SAREG,	TWORD,
	SAREG,	TWORD,
		0,	RLEFT,
		"", },

/* Sign conversions */
{ SCONV,	INAREG,
	SAREG,	TWORD,
	SAREG,	TWORD,
		0,	RLEFT,
		"", },

/* Load operations */
{ OPLTYPE,	INAREG,
	SANY,	TANY,
	SAREG,	TAREG,
		NAREG,	RESC1,
		"\tA1 = AL;\n", },

{ OPLTYPE,	INBREG,
	SANY,	TANY,
	SBREG,	TBREG,
		NBREG,	RESC1,
		"\tA1 = AL;\n", },

/* Assignments */
{ ASSIGN,	FOREFF|INAREG,
	SAREG|SNAME|SOREG,	TAREG,
	SAREG,			TAREG,
		0,	RDEST,
		"\tAL = AR;\n", },

{ ASSIGN,	FOREFF|INBREG,
	SBREG|SNAME|SOREG,	TBREG,
	SBREG,			TBREG,
		0,	RDEST,
		"\tAL = AR;\n", },

/* Arithmetic operations - PLUS */
{ PLUS,		INAREG,
	SAREG,	TAREG,
	SAREG,	TAREG,
		NAREG|NASL|NASR,	RESC1,
		"\tA1 = AL + AR;\n", },

{ PLUS,		INBREG,
	SBREG,	TBREG,
	SBREG,	TBREG,
		NBREG|NBSL|NBSR,	RESC1,
		"\tA1 = AL + AR;\n", },

/* MINUS */
{ MINUS,	INAREG,
	SAREG,	TAREG,
	SAREG,	TAREG,
		NAREG|NASL|NASR,	RESC1,
		"\tA1 = AL - AR;\n", },

{ MINUS,	INBREG,
	SBREG,	TBREG,
	SBREG,	TBREG,
		NBREG|NBSL|NBSR,	RESC1,
		"\tA1 = AL - AR;\n", },

/* MUL */
{ MUL,		INAREG,
	SAREG,	TAREG,
	SAREG,	TAREG,
		NAREG|NASL|NASR,	RESC1,
		"\tA1 = AL * AR;\n", },

{ MUL,		INBREG,
	SBREG,	TBREG,
	SBREG,	TBREG,
		NBREG|NBSL|NBSR,	RESC1,
		"\tA1 = AL * AR;\n", },

/* DIV */
{ DIV,		INAREG,
	SAREG,	TAREG,
	SAREG,	TAREG,
		NAREG|NASL|NASR,	RESC1,
		"\tA1 = AL / AR;\n", },

{ DIV,		INBREG,
	SBREG,	TBREG,
	SBREG,	TBREG,
		NBREG|NBSL|NBSR,	RESC1,
		"\tA1 = AL / AR;\n", },

/* MOD */
{ MOD,		INAREG,
	SAREG,	TAREG,
	SAREG,	TAREG,
		NAREG|NASL|NASR,	RESC1,
		"\tA1 = AL % AR;\n", },

/* Bitwise operations - AND */
{ AND,		INAREG,
	SAREG,	TAREG,
	SAREG,	TAREG,
		NAREG|NASL|NASR,	RESC1,
		"\tA1 = AL & AR;\n", },

/* OR */
{ OR,		INAREG,
	SAREG,	TAREG,
	SAREG,	TAREG,
		NAREG|NASL|NASR,	RESC1,
		"\tA1 = AL | AR;\n", },

/* ER (XOR) */
{ ER,		INAREG,
	SAREG,	TAREG,
	SAREG,	TAREG,
		NAREG|NASL|NASR,	RESC1,
		"\tA1 = AL ^ AR;\n", },

/* Shift operations - LS (left shift) */
{ LS,		INAREG,
	SAREG,	TAREG,
	SAREG,	TAREG,
		NAREG|NASL|NASR,	RESC1,
		"\tA1 = AL << AR;\n", },

/* RS (right shift) */
{ RS,		INAREG,
	SAREG,	TAREG,
	SAREG,	TAREG,
		NAREG|NASL|NASR,	RESC1,
		"\tA1 = AL >> AR;\n", },

/* Unary operations - UMINUS */
{ UMINUS,	INAREG,
	SAREG,	TAREG,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"\tA1 = -AL;\n", },

{ UMINUS,	INBREG,
	SBREG,	TBREG,
	SANY,	TANY,
		NBREG|NBSL,	RESC1,
		"\tA1 = -AL;\n", },

/* COMPL (bitwise NOT) */
{ COMPL,	INAREG,
	SAREG,	TAREG,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"\tA1 = ~AL;\n", },

/* Comparison operations - EQ */
{ EQ,		FORCC,
	SAREG,	TAREG,
	SAREG,	TAREG,
		0,	RESCC,
		"\tCC = (AL == AR);\n", },

{ NE,		FORCC,
	SAREG,	TAREG,
	SAREG,	TAREG,
		0,	RESCC,
		"\tCC = (AL != AR);\n", },

{ LT,		FORCC,
	SAREG,	TAREG,
	SAREG,	TAREG,
		0,	RESCC,
		"\tCC = (AL < AR);\n", },

{ LE,		FORCC,
	SAREG,	TAREG,
	SAREG,	TAREG,
		0,	RESCC,
		"\tCC = (AL <= AR);\n", },

{ GT,		FORCC,
	SAREG,	TAREG,
	SAREG,	TAREG,
		0,	RESCC,
		"\tCC = (AL > AR);\n", },

{ GE,		FORCC,
	SAREG,	TAREG,
	SAREG,	TAREG,
		0,	RESCC,
		"\tCC = (AL >= AR);\n", },

/* Indirect (UMUL - dereference) */
{ UMUL,		INAREG,
	SANY,	TPOINT|TWORD,
	SAREG,	TAREG,
		NAREG,	RESC1,
		"\tA1 = *AL;\n", },

{ UMUL,		INBREG,
	SANY,	TPOINT,
	SBREG,	TBREG,
		NBREG,	RESC1,
		"\tA1 = *AL;\n", },

/* Address of */
{ ADDROF,	INAREG,
	SNAME|SOREG,	TANY,
	SAREG,		TPOINT,
		NAREG,	RESC1,
		"\tA1 = &AL;\n", },

/* Function call */
{ CALL,		FOREFF,
	SCON|SNAME,	TANY,
	SANY,		TANY,
		0,	0,
		"\tAL();\n", },

{ CALL,		INAREG,
	SCON|SNAME,	TANY,
	SAREG,		TAREG,
		NAREG,	RESC1,
		"\tA1 = AL();\n", },

{ CALL,		INBREG,
	SCON|SNAME,	TANY,
	SBREG,		TBREG,
		NBREG,	RESC1,
		"\tA1 = AL();\n", },

/* Indirect function call */
{ UCALL,	FOREFF,
	SAREG,	TANY,
	SANY,	TANY,
		0,	0,
		"\t(*AL)();\n", },

{ UCALL,	INAREG,
	SAREG,	TANY,
	SAREG,	TAREG,
		NAREG,	RESC1,
		"\tA1 = (*AL)();\n", },

/* GOTO */
{ GOTO,		FOREFF,
	SCON,	TANY,
	SANY,	TANY,
		0,	RNOP,
		"\tgoto LL;\n", },

/* Return */
{ RETURN,	FOREFF,
	SANY,	TANY,
	SANY,	TANY,
		0,	0,
		"\treturn;\n", },

/* Free operations (expression evaluation) */
{ FREE,		FOREFF,
	SANY,	TANY,
	SANY,	TANY,
		0,	0,
		"", },

/* Default fallback */
{ -1,	FOREFF,
	SANY,	TANY,
	SANY,	TANY,
		0,	0,
		"", },

};

int tablesize = sizeof(table)/sizeof(table[0]);
