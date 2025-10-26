/*	$Id$	*/

/*
 * Copyright (c) 2025 Claude AI Assistant.
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
 * CodeView Debug Format Support (CV4-CV8)
 *
 * Microsoft CodeView debugging information format:
 * - CV4 (CodeView 4.x) - Visual C++ 4.x, 16-bit & 32-bit
 * - CV5 (CodeView 5.x) - Visual C++ 5.0-6.0
 * - CV6 (CodeView 6.x) - Visual C++ 7.0 (.NET 2002)
 * - CV7 (CodeView 7.x) - Visual C++ 7.1 (.NET 2003)
 * - CV8 (CodeView 8.x) - Visual Studio 2005+, PDB 7.0 format
 */

#include "pass1.h"
#include "debugsym.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* CodeView signature magic numbers */
#define CV_SIGNATURE_C6		0x0  /* C6.00 */
#define CV_SIGNATURE_C7		0x1  /* C7.00 */
#define CV_SIGNATURE_C11	0x2  /* C11 (VC5) */
#define CV_SIGNATURE_C13	0x4  /* C13 (VC7) */

/* CodeView symbol types (leaf types) */
#define LF_MODIFIER_16t		0x0001
#define LF_POINTER_16t		0x0002
#define LF_ARRAY_16t		0x0003
#define LF_CLASS_16t		0x0004
#define LF_STRUCTURE_16t	0x0005
#define LF_UNION_16t		0x0006
#define LF_ENUM_16t		0x0007
#define LF_PROCEDURE_16t	0x0008
#define LF_MFUNCTION_16t	0x0009
#define LF_VTSHAPE		0x000a
#define LF_COBOL0_16t		0x000b
#define LF_COBOL1		0x000c
#define LF_BARRAY_16t		0x000d
#define LF_LABEL		0x000e
#define LF_NULL			0x000f
#define LF_NOTTRAN		0x0010
#define LF_DIMARRAY_16t		0x0011
#define LF_VFTPATH_16t		0x0012
#define LF_PRECOMP_16t		0x0013
#define LF_ENDPRECOMP		0x0014
#define LF_OEM_16t		0x0015
#define LF_TYPESERVER_ST	0x0016

/* 32-bit CodeView types */
#define LF_SKIP			0x0200
#define LF_ARGLIST		0x0201
#define LF_DEFARG		0x0202
#define LF_LIST			0x0203
#define LF_FIELDLIST		0x0204
#define LF_DERIVED		0x0205
#define LF_BITFIELD		0x0206
#define LF_METHODLIST		0x0207
#define LF_DIMCONU		0x0208
#define LF_DIMCONLU		0x0209
#define LF_DIMVARU		0x020a
#define LF_DIMVARLU		0x020b
#define LF_REFSYM		0x020c
#define LF_BCLASS		0x0400
#define LF_VBCLASS		0x0401
#define LF_IVBCLASS		0x0402
#define LF_ENUMERATE_ST		0x0403
#define LF_FRIENDFCN_ST		0x0404
#define LF_INDEX		0x0405
#define LF_MEMBER_ST		0x0406
#define LF_STMEMBER_ST		0x0407
#define LF_METHOD_ST		0x0408
#define LF_NESTTYPE_ST		0x0409
#define LF_VFUNCTAB		0x040a
#define LF_FRIENDCLS		0x040b
#define LF_ONEMETHOD_ST		0x040c
#define LF_VFUNCOFF		0x040d
#define LF_TI16_MAX		0x1000

/* Numeric leaf types */
#define LF_NUMERIC		0x8000
#define LF_CHAR			0x8000
#define LF_SHORT		0x8001
#define LF_USHORT		0x8002
#define LF_LONG			0x8003
#define LF_ULONG		0x8004
#define LF_REAL32		0x8005
#define LF_REAL64		0x8006
#define LF_REAL80		0x8007
#define LF_REAL128		0x8008
#define LF_QUADWORD		0x8009
#define LF_UQUADWORD		0x800a
#define LF_REAL48		0x800b
#define LF_COMPLEX32		0x800c
#define LF_COMPLEX64		0x800d
#define LF_COMPLEX80		0x800e
#define LF_COMPLEX128		0x800f
#define LF_VARSTRING		0x8010

/* CV5+ types */
#define LF_MODIFIER		0x1001
#define LF_POINTER		0x1002
#define LF_ARRAY_ST		0x1003
#define LF_CLASS_ST		0x1004
#define LF_STRUCTURE_ST		0x1005
#define LF_UNION_ST		0x1006
#define LF_ENUM_ST		0x1007
#define LF_PROCEDURE		0x1008
#define LF_MFUNCTION		0x1009
#define LF_COBOL0		0x100a
#define LF_BARRAY		0x100b
#define LF_DIMARRAY_ST		0x100c
#define LF_VFTPATH		0x100d
#define LF_PRECOMP_ST		0x100e
#define LF_OEM			0x100f
#define LF_ALIAS_ST		0x1010
#define LF_OEM2			0x1011

/* CV8 (modern) types */
#define LF_STRUCTURE		0x1505
#define LF_CLASS		0x1504
#define LF_UNION		0x1506
#define LF_ENUM			0x1507
#define LF_ARRAY		0x1503
#define LF_MEMBER		0x150d
#define LF_STMEMBER		0x150e
#define LF_METHOD		0x150f
#define LF_NESTTYPE		0x1510
#define LF_ONEMETHOD		0x1511
#define LF_ENUMERATE		0x1502
#define LF_VFUNCTAB_		0x151d
#define LF_FUNC_ID		0x1601
#define LF_MFUNC_ID		0x1602
#define LF_BUILDINFO		0x1603
#define LF_SUBSTR_LIST		0x1604
#define LF_STRING_ID		0x1605
#define LF_UDT_SRC_LINE		0x1606
#define LF_UDT_MOD_SRC_LINE	0x1607

/* Symbol record types */
#define S_COMPILE		0x0001
#define S_REGISTER_16t		0x0002
#define S_CONSTANT_16t		0x0003
#define S_UDT_16t		0x0004
#define S_SSEARCH		0x0005
#define S_END			0x0006
#define S_SKIP			0x0007
#define S_CVRESERVE		0x0008
#define S_OBJNAME_ST		0x0009
#define S_ENDARG		0x000a
#define S_COBOLUDT_16t		0x000b
#define S_MANYREG_16t		0x000c
#define S_RETURN		0x000d
#define S_ENTRYTHIS		0x000e

#define S_BPREL32_16t		0x0200
#define S_LDATA32_16t		0x0201
#define S_GDATA32_16t		0x0202
#define S_PUB32_16t		0x0203
#define S_LPROC32_16t		0x0204
#define S_GPROC32_16t		0x0205
#define S_THUNK32_ST		0x0206
#define S_BLOCK32_ST		0x0207
#define S_WITH32_ST		0x0208
#define S_LABEL32_ST		0x0209
#define S_REGISTER_ST		0x020a
#define S_CONSTANT_ST		0x020b
#define S_UDT_ST		0x020c
#define S_COBOLUDT_ST		0x020d
#define S_MANYREG_ST		0x020e
#define S_BPREL32_ST		0x020f
#define S_LDATA32_ST		0x0210
#define S_GDATA32_ST		0x0211
#define S_PUB32_ST		0x0212
#define S_LPROC32_ST		0x0213
#define S_GPROC32_ST		0x0214
#define S_VFTABLE32		0x0215
#define S_REGREL32_ST		0x0216
#define S_LTHREAD32_ST		0x0217
#define S_GTHREAD32_ST		0x0218

/* CV8 symbol types */
#define S_LPROC32		0x110f
#define S_GPROC32		0x1110
#define S_REGREL32		0x1111
#define S_LDATA32		0x110c
#define S_GDATA32		0x110d
#define S_PUB32			0x110e
#define S_LMANDATA		0x111c
#define S_GMANDATA		0x111d
#define S_MANFRAMEREL		0x111e
#define S_MANREGISTER		0x111f
#define S_MANSLOT		0x1120
#define S_MANMANYREG		0x1121
#define S_MANREGREL		0x1122
#define S_MANMANYREG2		0x1123
#define S_MANTYPREF		0x1124
#define S_UNAMESPACE		0x1124

/* CodeView state */
static struct {
	int version;		/* CodeView version (4-8) */
	int signature;		/* Signature type */
	FILE *symbols;		/* Symbol section */
	FILE *types;		/* Type section */
	int type_index;		/* Current type index */
	int symbol_offset;	/* Symbol offset */
} cv_state;

/*
 * Initialize CodeView output
 */
void
debugsym_codeview_init(int version)
{
	memset(&cv_state, 0, sizeof(cv_state));
	cv_state.version = version;
	cv_state.type_index = 0x1000;  /* Type indices start at 0x1000 */
	cv_state.symbol_offset = 0;

	/* Determine signature based on version */
	if (version <= 5)
		cv_state.signature = CV_SIGNATURE_C11;
	else if (version <= 7)
		cv_state.signature = CV_SIGNATURE_C13;
	else
		cv_state.signature = CV_SIGNATURE_C13;

	/* Emit section headers */
	printf("\t.section .debug$S,\"dr\"\n");
	printf("\t.long 0x%x\n", cv_state.signature);  /* CV signature */

	printf("\t.section .debug$T,\"dr\"\n");
	printf("\t.long 0x%x\n", cv_state.signature);  /* Type signature */
}

/*
 * Emit a length-prefixed string (Pascal-style for CV4-CV5)
 */
static void
emit_length_string(const char *str)
{
	int len = str ? strlen(str) : 0;

	if (len > 255)
		len = 255;

	printf("\t.byte %d\n", len);
	if (len > 0)
		printf("\t.ascii \"%.*s\"\n", len, str);
}

/*
 * Emit a null-terminated string (C-style for CV6+)
 */
static void
emit_null_string(const char *str)
{
	if (str == NULL)
		str = "";

	printf("\t.asciz \"%s\"\n", str);
}

/*
 * Emit a numeric leaf value
 */
static void
emit_numeric_leaf(long value)
{
	if (value >= 0 && value < 0x8000) {
		printf("\t.short %ld\n", value);
	} else if (value >= -128 && value < 128) {
		printf("\t.short 0x%x\n", LF_CHAR);
		printf("\t.byte %ld\n", value);
	} else if (value >= -32768 && value < 32768) {
		printf("\t.short 0x%x\n", LF_SHORT);
		printf("\t.short %ld\n", value);
	} else {
		printf("\t.short 0x%x\n", LF_LONG);
		printf("\t.long %ld\n", value);
	}
}

/*
 * Get CodeView type index for a debug type
 */
static int
get_cv_type_index(debug_type_t *type)
{
	int type_index;

	if (type == NULL)
		return 0x0003;  /* T_VOID */

	/* Map to CodeView primitive types */
	switch (type->encoding) {
	case DBGTYPE_VOID:
		return 0x0003;  /* T_VOID */
	case DBGTYPE_CHAR:
		return type->size == 1 ? 0x0010 : 0x0070;  /* T_CHAR / T_WCHAR */
	case DBGTYPE_INT8:
		return 0x0068;  /* T_INT1 */
	case DBGTYPE_UINT8:
		return 0x0069;  /* T_UINT1 */
	case DBGTYPE_INT16:
		return 0x0072;  /* T_SHORT */
	case DBGTYPE_UINT16:
		return 0x0073;  /* T_USHORT */
	case DBGTYPE_INT32:
		return 0x0074;  /* T_LONG */
	case DBGTYPE_UINT32:
		return 0x0075;  /* T_ULONG */
	case DBGTYPE_INT64:
		return 0x0076;  /* T_QUAD */
	case DBGTYPE_UINT64:
		return 0x0077;  /* T_UQUAD */
	case DBGTYPE_FLOAT32:
		return 0x0040;  /* T_REAL32 */
	case DBGTYPE_FLOAT64:
		return 0x0041;  /* T_REAL64 */
	case DBGTYPE_FLOAT80:
		return 0x0042;  /* T_REAL80 */
	case DBGTYPE_BOOL:
		return 0x0030;  /* T_BOOL08 */
	default:
		/* Custom type - assign new index */
		type_index = cv_state.type_index++;
		return type_index;
	}
}

/*
 * Emit a CodeView symbol record
 */
static void
emit_symbol_record(int sym_type, const char *name, int type_index, int offset)
{
	int record_len;
	const char *sym_name = name ? name : "";

	printf("\t.section .debug$S\n");

	/* Record length (placeholder - will be calculated) */
	printf(".Lsym%d:\n", cv_state.symbol_offset);
	printf("\t.short 0  # length placeholder\n");

	/* Symbol type */
	printf("\t.short 0x%x\n", sym_type);

	/* Symbol-specific data */
	if (sym_type == S_LDATA32 || sym_type == S_GDATA32) {
		printf("\t.long %d\n", type_index);
		printf("\t.long %d\n", offset);
		if (cv_state.version >= 6)
			emit_null_string(sym_name);
		else
			emit_length_string(sym_name);
	} else if (sym_type == S_LPROC32 || sym_type == S_GPROC32) {
		printf("\t.long 0\n");  /* pParent */
		printf("\t.long 0\n");  /* pEnd */
		printf("\t.long 0\n");  /* pNext */
		printf("\t.long 0\n");  /* proc_len */
		printf("\t.long 0\n");  /* debug_start */
		printf("\t.long 0\n");  /* debug_end */
		printf("\t.long %d\n", type_index);
		printf("\t.long %d\n", offset);
		printf("\t.byte 0\n");  /* flags */
		if (cv_state.version >= 6)
			emit_null_string(sym_name);
		else
			emit_length_string(sym_name);
	}

	cv_state.symbol_offset++;
}

/*
 * Emit a symbol as CodeView
 */
void
debugsym_codeview_emit(debug_symbol_t *sym)
{
	int sym_type = 0;
	int type_index;

	if (sym == NULL)
		return;

	/* Get type index */
	type_index = get_cv_type_index(sym->type);

	/* Determine CodeView symbol type */
	switch (sym->kind) {
	case DBGSYM_VARIABLE:
		if (sym->storage_class == STATIC)
			sym_type = (cv_state.version >= 8) ? S_LDATA32 : S_LDATA32_ST;
		else
			sym_type = (cv_state.version >= 8) ? S_GDATA32 : S_GDATA32_ST;
		break;

	case DBGSYM_FUNCTION:
		if (sym->is_static)
			sym_type = (cv_state.version >= 8) ? S_LPROC32 : S_LPROC32_ST;
		else
			sym_type = (cv_state.version >= 8) ? S_GPROC32 : S_GPROC32_ST;
		break;

	case DBGSYM_PARAMETER:
		sym_type = (cv_state.version >= 8) ? S_REGREL32 : S_REGREL32_ST;
		break;

	case DBGSYM_CONSTANT:
		sym_type = (cv_state.version >= 8) ? S_CONSTANT_ST : S_CONSTANT_16t;
		break;

	default:
		return;
	}

	emit_symbol_record(sym_type, sym->name, type_index, sym->offset);
}

/*
 * Emit a type record
 */
static void
emit_type_record(int leaf_type, debug_symbol_t *sym)
{
	printf("\t.section .debug$T\n");

	/* Record length */
	printf("\t.short 0  # length placeholder\n");

	/* Leaf type */
	printf("\t.short 0x%x\n", leaf_type);

	/* Type-specific data would follow */
}

/*
 * Parse CodeView debug information
 */
debug_symbol_t *
debugsym_codeview_parse(void *data, size_t len)
{
	unsigned char *p = (unsigned char *)data;
	unsigned int signature;

	if (len < 4)
		return NULL;

	/* Read signature */
	signature = *(unsigned int *)p;

	/* Verify CodeView signature */
	if (signature != CV_SIGNATURE_C11 &&
	    signature != CV_SIGNATURE_C13 &&
	    signature != CV_SIGNATURE_C6 &&
	    signature != CV_SIGNATURE_C7)
		return NULL;

	/* Full parser would be implemented here */
	return NULL;
}

/*
 * Finish CodeView output
 */
void
debugsym_codeview_finish(void)
{
	/* Emit any pending records */
	printf("\t.section .debug$S\n");
	printf("\t# End of symbols\n");

	printf("\t.section .debug$T\n");
	printf("\t# End of types\n");
}
