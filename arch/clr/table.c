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
 * CLR backend - instruction selection table
 *
 * This table maps PCC's intermediate representation to CLR IL instructions.
 * Each entry specifies how to generate IL code for an operation.
 */

#include "pass2.h"

/* Type groupings for CLR */
#define TLL		TLONGLONG|TULONGLONG
#define ANYSIGNED	TINT|TLONG|TSHORT|TCHAR
#define ANYUSIGNED	TUNSIGNED|TULONG|TUSHORT|TUCHAR
#define ANYFIXED	ANYSIGNED|ANYUSIGNED
#define TUWORD		TUNSIGNED|TULONG
#define TSWORD		TINT|TLONG
#define TWORD		TUWORD|TSWORD
#define SHINT		SAREG	/* short and int on eval stack */
#define ININT		INAREG
#define SHLL		SCREG	/* long long */
#define INLL		INCREG
#define SHFL		SDREG	/* float/double */
#define INFL		INDREG

/*
 * Instruction selection table for CLR IL.
 *
 * Format:
 * { operation, cookie, left_shape, left_type, right_shape, right_type,
 *   needs, rewrite, "IL instruction template" }
 *
 * Template codes:
 * - AL/AR: left/right address
 * - A1/A2: allocated register numbers
 * - ZI: special handling
 */

struct optab table[] = {

/* ========== FIRST ENTRY MUST BE EMPTY ========== */
{ -1, FOREFF,
	SANY, TANY,
	SANY, TANY,
		0, 0,
		"", },

/* ========== POINTER CONVERSIONS ========== */

/* Pointer conversion (usually no-op in CLR) */
{ PCONV, INAREG,
	SAREG, TWORD|TPOINT,
	SAREG, TWORD|TPOINT,
		0, RLEFT,
		"", },

/* ========== TYPE CONVERSIONS ========== */

/* Convert int to int (no-op) */
{ SCONV, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RLEFT,
		"", },

/* Convert char to int */
{ SCONV, INAREG,
	SAREG, TCHAR,
	SAREG, TINT|TUNSIGNED,
		0, RLEFT,
		"\t\t\tconv.i4\n", },

/* Convert unsigned char to int */
{ SCONV, INAREG,
	SAREG, TUCHAR,
	SAREG, TINT|TUNSIGNED,
		0, RLEFT,
		"\t\t\tconv.u4\n", },

/* Convert short to int */
{ SCONV, INAREG,
	SAREG, TSHORT,
	SAREG, TINT|TUNSIGNED,
		0, RLEFT,
		"\t\t\tconv.i4\n", },

/* Convert unsigned short to int */
{ SCONV, INAREG,
	SAREG, TUSHORT,
	SAREG, TINT|TUNSIGNED,
		0, RLEFT,
		"\t\t\tconv.u4\n", },

/* Convert int to long long */
{ SCONV, INLL,
	SAREG, TINT,
	SCREG, TLL,
		0, RLEFT,
		"\t\t\tconv.i8\n", },

/* Convert unsigned int to long long */
{ SCONV, INLL,
	SAREG, TUNSIGNED,
	SCREG, TLL,
		0, RLEFT,
		"\t\t\tconv.u8\n", },

/* Convert long long to int */
{ SCONV, INAREG,
	SCREG, TLL,
	SAREG, TWORD,
		0, RLEFT,
		"\t\t\tconv.i4\n", },

/* Convert int to float */
{ SCONV, INFL,
	SAREG, TINT,
	SDREG, TFLOAT,
		0, RLEFT,
		"\t\t\tconv.r4\n", },

/* Convert int to double */
{ SCONV, INFL,
	SAREG, TINT,
	SDREG, TDOUBLE|TLDOUBLE,
		0, RLEFT,
		"\t\t\tconv.r8\n", },

/* Convert float to int */
{ SCONV, INAREG,
	SDREG, TFLOAT,
	SAREG, TINT,
		0, RLEFT,
		"\t\t\tconv.i4\n", },

/* Convert double to int */
{ SCONV, INAREG,
	SDREG, TDOUBLE|TLDOUBLE,
	SAREG, TINT,
		0, RLEFT,
		"\t\t\tconv.i4\n", },

/* Convert float to double */
{ SCONV, INFL,
	SDREG, TFLOAT,
	SDREG, TDOUBLE|TLDOUBLE,
		0, RLEFT,
		"\t\t\tconv.r8\n", },

/* Convert double to float */
{ SCONV, INFL,
	SDREG, TDOUBLE|TLDOUBLE,
	SDREG, TFLOAT,
		0, RLEFT,
		"\t\t\tconv.r4\n", },

/* ========== INTEGER ARITHMETIC ========== */

/* Integer addition: reg + reg */
{ PLUS, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RLEFT,
		"\t\t\tadd\n", },

/* Integer addition: reg + const */
{ PLUS, INAREG,
	SAREG, TWORD,
	SCON, TWORD,
		0, RLEFT,
		"\t\t\tldc.i4\tAR\n\t\t\tadd\n", },

/* Pointer + integer offset */
{ PLUS, INAREG,
	SAREG, TPOINT,
	SAREG, TWORD,
		0, RLEFT,
		"\t\t\tadd\n", },

/* Integer subtraction: reg - reg */
{ MINUS, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RLEFT,
		"\t\t\tsub\n", },

/* Integer subtraction: reg - const */
{ MINUS, INAREG,
	SAREG, TWORD,
	SCON, TWORD,
		0, RLEFT,
		"\t\t\tldc.i4\tAR\n\t\t\tsub\n", },

/* Integer multiplication */
{ MUL, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RLEFT,
		"\t\t\tmul\n", },

/* Integer division (signed) */
{ DIV, INAREG,
	SAREG, TSWORD,
	SAREG, TSWORD,
		0, RLEFT,
		"\t\t\tdiv\n", },

/* Integer division (unsigned) */
{ DIV, INAREG,
	SAREG, TUWORD,
	SAREG, TUWORD,
		0, RLEFT,
		"\t\t\tdiv.un\n", },

/* Integer modulo (signed) */
{ MOD, INAREG,
	SAREG, TSWORD,
	SAREG, TSWORD,
		0, RLEFT,
		"\t\t\trem\n", },

/* Integer modulo (unsigned) */
{ MOD, INAREG,
	SAREG, TUWORD,
	SAREG, TUWORD,
		0, RLEFT,
		"\t\t\trem.un\n", },

/* Unary minus */
{ UMINUS, INAREG,
	SAREG, TWORD,
	SANY, TANY,
		0, RLEFT,
		"\t\t\tneg\n", },

/* ========== LONG LONG ARITHMETIC ========== */

/* Long long addition */
{ PLUS, INLL,
	SCREG, TLL,
	SCREG, TLL,
		0, RLEFT,
		"\t\t\tadd\n", },

/* Long long subtraction */
{ MINUS, INLL,
	SCREG, TLL,
	SCREG, TLL,
		0, RLEFT,
		"\t\t\tsub\n", },

/* Long long multiplication */
{ MUL, INLL,
	SCREG, TLL,
	SCREG, TLL,
		0, RLEFT,
		"\t\t\tmul\n", },

/* Long long division */
{ DIV, INLL,
	SCREG, TLL,
	SCREG, TLL,
		0, RLEFT,
		"\t\t\tdiv\n", },

/* ========== FLOATING-POINT ARITHMETIC ========== */

/* Float addition */
{ PLUS, INFL,
	SDREG, TFLOAT,
	SDREG, TFLOAT,
		0, RLEFT,
		"\t\t\tadd\n", },

/* Double addition */
{ PLUS, INFL,
	SDREG, TDOUBLE|TLDOUBLE,
	SDREG, TDOUBLE|TLDOUBLE,
		0, RLEFT,
		"\t\t\tadd\n", },

/* Float subtraction */
{ MINUS, INFL,
	SDREG, TFLOAT,
	SDREG, TFLOAT,
		0, RLEFT,
		"\t\t\tsub\n", },

/* Double subtraction */
{ MINUS, INFL,
	SDREG, TDOUBLE|TLDOUBLE,
	SDREG, TDOUBLE|TLDOUBLE,
		0, RLEFT,
		"\t\t\tsub\n", },

/* Float multiplication */
{ MUL, INFL,
	SDREG, TFLOAT,
	SDREG, TFLOAT,
		0, RLEFT,
		"\t\t\tmul\n", },

/* Double multiplication */
{ MUL, INFL,
	SDREG, TDOUBLE|TLDOUBLE,
	SDREG, TDOUBLE|TLDOUBLE,
		0, RLEFT,
		"\t\t\tmul\n", },

/* Float division */
{ DIV, INFL,
	SDREG, TFLOAT,
	SDREG, TFLOAT,
		0, RLEFT,
		"\t\t\tdiv\n", },

/* Double division */
{ DIV, INFL,
	SDREG, TDOUBLE|TLDOUBLE,
	SDREG, TDOUBLE|TLDOUBLE,
		0, RLEFT,
		"\t\t\tdiv\n", },

/* Floating-point unary minus */
{ UMINUS, INFL,
	SDREG, TFLOAT|TDOUBLE|TLDOUBLE,
	SANY, TANY,
		0, RLEFT,
		"\t\t\tneg\n", },

/* ========== BITWISE OPERATIONS ========== */

/* Bitwise AND */
{ AND, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RLEFT,
		"\t\t\tand\n", },

/* Bitwise OR */
{ OR, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RLEFT,
		"\t\t\tor\n", },

/* Bitwise XOR */
{ ER, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RLEFT,
		"\t\t\txor\n", },

/* Bitwise NOT/complement */
{ COMPL, INAREG,
	SAREG, TWORD,
	SANY, TANY,
		0, RLEFT,
		"\t\t\tnot\n", },

/* Left shift */
{ LS, INAREG,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RLEFT,
		"\t\t\tshl\n", },

/* Right shift (signed) */
{ RS, INAREG,
	SAREG, TSWORD,
	SAREG, TWORD,
		0, RLEFT,
		"\t\t\tshr\n", },

/* Right shift (unsigned) */
{ RS, INAREG,
	SAREG, TUWORD,
	SAREG, TWORD,
		0, RLEFT,
		"\t\t\tshr.un\n", },

/* ========== COMPARISONS ========== */

/* Integer equality */
{ EQ, FORCC,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RLEFT,
		"\t\t\tceq\n", },

/* Integer inequality */
{ NE, FORCC,
	SAREG, TWORD,
	SAREG, TWORD,
		0, RLEFT,
		"\t\t\tceq\n\t\t\tldc.i4.0\n\t\t\tceq\n", },

/* Less than (signed) */
{ LT, FORCC,
	SAREG, TSWORD,
	SAREG, TSWORD,
		0, RLEFT,
		"\t\t\tclt\n", },

/* Less than (unsigned) */
{ ULT, FORCC,
	SAREG, TUWORD,
	SAREG, TUWORD,
		0, RLEFT,
		"\t\t\tclt.un\n", },

/* Less than or equal (signed) */
{ LE, FORCC,
	SAREG, TSWORD,
	SAREG, TSWORD,
		0, RLEFT,
		"\t\t\tcgt\n\t\t\tldc.i4.0\n\t\t\tceq\n", },

/* Greater than (signed) */
{ GT, FORCC,
	SAREG, TSWORD,
	SAREG, TSWORD,
		0, RLEFT,
		"\t\t\tcgt\n", },

/* Greater than (unsigned) */
{ UGT, FORCC,
	SAREG, TUWORD,
	SAREG, TUWORD,
		0, RLEFT,
		"\t\t\tcgt.un\n", },

/* Greater than or equal (signed) */
{ GE, FORCC,
	SAREG, TSWORD,
	SAREG, TSWORD,
		0, RLEFT,
		"\t\t\tclt\n\t\t\tldc.i4.0\n\t\t\tceq\n", },

/* ========== LOAD/STORE OPERATIONS ========== */

/* Load integer constant */
{ ICON, INAREG,
	SANY, TWORD,
	SANY, TWORD,
		0, RESC1,
		"\t\t\tldc.i4\tAL\n", },

/* Load long long constant */
{ ICON, INLL,
	SANY, TLL,
	SANY, TLL,
		0, RESC1,
		"\t\t\tldc.i8\tAL\n", },

/* Load float constant */
{ FCON, INFL,
	SANY, TFLOAT,
	SANY, TFLOAT,
		0, RESC1,
		"\t\t\tldc.r4\tAL\n", },

/* Load double constant */
{ FCON, INFL,
	SANY, TDOUBLE|TLDOUBLE,
	SANY, TDOUBLE|TLDOUBLE,
		0, RESC1,
		"\t\t\tldc.r8\tAL\n", },

/* Load from global variable (NAME) */
{ NAME, INAREG,
	SANY, TWORD,
	SANY, TWORD,
		0, RESC1,
		"\t\t\tldsfld\tint32 ZG\n", },

/* Load from local variable (OREG) */
{ OREG, INAREG,
	SANY, TWORD,
	SANY, TWORD,
		0, RESC1,
		"\t\t\tldloc.s\tAL\n", },

/* Register move */
{ REG, INAREG,
	SANY, TWORD,
	SANY, TWORD,
		0, RESC1,
		"", },

/* ========== ASSIGNMENT OPERATIONS ========== */

/* Assign to global variable */
{ ASSIGN, FOREFF,
	SNAME, TWORD,
	SAREG, TWORD,
		0, RDEST,
		"\t\t\tstsfld\tint32 ZG\n", },

/* Assign to local variable */
{ ASSIGN, FOREFF,
	SOREG, TWORD,
	SAREG, TWORD,
		0, RDEST,
		"\t\t\tstloc.s\tAL\n", },

/* Assign and return value */
{ ASSIGN, INAREG,
	SAREG|SOREG|SNAME, TWORD,
	SAREG, TWORD,
		0, RDEST,
		"\t\t\tZZ\t\t\tstloc.s\tAL\n", },

/* ========== INDIRECTION/DEREFERENCE ========== */

/* Dereference pointer to int */
{ UMUL, INAREG,
	SAREG, TPOINT,
	SANY, TINT|TUNSIGNED,
		0, RLEFT,
		"\t\t\tldind.i4\n", },

/* Dereference pointer to char */
{ UMUL, INAREG,
	SAREG, TPOINT,
	SANY, TCHAR,
		0, RLEFT,
		"\t\t\tldind.i1\n", },

/* Dereference pointer to unsigned char */
{ UMUL, INAREG,
	SAREG, TPOINT,
	SANY, TUCHAR,
		0, RLEFT,
		"\t\t\tldind.u1\n", },

/* Dereference pointer to short */
{ UMUL, INAREG,
	SAREG, TPOINT,
	SANY, TSHORT,
		0, RLEFT,
		"\t\t\tldind.i2\n", },

/* Dereference pointer to float */
{ UMUL, INFL,
	SAREG, TPOINT,
	SANY, TFLOAT,
		0, RLEFT,
		"\t\t\tldind.r4\n", },

/* Dereference pointer to double */
{ UMUL, INFL,
	SAREG, TPOINT,
	SANY, TDOUBLE|TLDOUBLE,
		0, RLEFT,
		"\t\t\tldind.r8\n", },

/* Address-of operator */
{ ADDROF, INAREG,
	SNAME, TANY,
	SANY, TPOINT,
		0, RESC1,
		"\t\t\tldsflda\tZG\n", },

/* Address of local */
{ ADDROF, INAREG,
	SOREG, TANY,
	SANY, TPOINT,
		0, RESC1,
		"\t\t\tldloca.s\tAL\n", },

/* ========== FUNCTION CALLS ========== */

/* Function call with return value */
{ CALL, INAREG,
	SCON, TANY,
	SANY, TWORD,
		0, RESC1,
		"\t\t\tZM\n", },

/* Function call for effect (void) */
{ CALL, FOREFF,
	SCON, TANY,
	SANY, TVOID,
		0, 0,
		"\t\t\tZM\n", },

/* Function call returning float/double */
{ CALL, INFL,
	SCON, TANY,
	SANY, TFLOAT|TDOUBLE|TLDOUBLE,
		0, RESC1,
		"\t\t\tZM\n", },

/* Indirect function call (via pointer) */
{ UCALL, INAREG,
	SAREG, TANY,
	SANY, TWORD,
		0, RESC1,
		"\t\t\tcalli\tZR\n", },

/* ========== CONTROL FLOW ========== */

/* Conditional branch (if condition) */
{ CBRANCH, FOREFF,
	SAREG, TWORD,
	SCON, TANY,
		0, 0,
		"\t\t\tbrtrue\tZL\n", },

/* Unconditional goto */
{ GOTO, FOREFF,
	SCON, TANY,
	SANY, TANY,
		0, 0,
		"\t\t\tbr\tZL\n", },

/* Return statement with value */
{ RETURN, FOREFF,
	SAREG, TWORD,
	SANY, TANY,
		0, 0,
		"\t\t\tret\n", },

/* Return void */
{ RETURN, FOREFF,
	SANY, TVOID,
	SANY, TANY,
		0, 0,
		"\t\t\tret\n", },

/* ========== ARRAY OPERATIONS ========== */

/* Array subscript (pointer arithmetic) */
{ LB, INAREG,
	SAREG, TPOINT,
	SAREG, TWORD,
		0, RLEFT,
		"\t\t\t// array subscript\n\t\t\tZE\n\t\t\tldelem.ZT\n", },

/* ========== LOGICAL OPERATIONS ========== */

/* Logical NOT */
{ NOT, INAREG,
	SAREG, TWORD,
	SANY, TANY,
		0, RLEFT,
		"\t\t\tldc.i4.0\n\t\t\tceq\n", },

/* ========== SPECIAL OPERATIONS ========== */

/* Comma operator (discard left, return right) */
{ COMOP, INAREG,
	SAREG, TANY,
	SAREG, TWORD,
		0, RRIGHT,
		"\t\t\tpop\n", },

/* No-op (for optimization) */
{ FREE, FOREFF,
	SANY, TANY,
	SANY, TANY,
		0, 0,
		"", },

}; /* end of table */

/*
 * Table size and entry count.
 */
int tablesize = sizeof(table)/sizeof(table[0]);
