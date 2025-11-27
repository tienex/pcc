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
 * IBM HLL (High Level Language) Debug Format Support
 *
 * Used by IBM compilers:
 * - VisualAge C++ (OS/2, AIX, Windows)
 * - XL C/C++ (AIX)
 * - IBM C/C++ Compilers
 * - OS/400 compilers
 *
 * HLL debug format features:
 * - Symbol table with type information
 * - Line number tables
 * - Source file information
 * - Block structure information
 * - Register allocation tracking
 */

#include "pass1.h"
#include "debugsym.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* IBM HLL record types */
#define HLL_BEGIN_MODULE	0x01	/* Begin module */
#define HLL_END_MODULE		0x02	/* End module */
#define HLL_BEGIN_BLOCK		0x03	/* Begin block */
#define HLL_END_BLOCK		0x04	/* End block */
#define HLL_DEF_SYMBOL		0x05	/* Define symbol */
#define HLL_DEF_TYPE		0x06	/* Define type */
#define HLL_LINE_NUM		0x07	/* Line number */
#define HLL_SOURCE_FILE		0x08	/* Source file */
#define HLL_BEGIN_FUNCTION	0x09	/* Begin function */
#define HLL_END_FUNCTION	0x0A	/* End function */
#define HLL_AUTO_VAR		0x0B	/* Automatic variable */
#define HLL_STATIC_VAR		0x0C	/* Static variable */
#define HLL_GLOBAL_VAR		0x0D	/* Global variable */
#define HLL_PARAMETER		0x0E	/* Function parameter */
#define HLL_TYPEDEF		0x0F	/* Type definition */
#define HLL_STRUCT_DEF		0x10	/* Structure definition */
#define HLL_UNION_DEF		0x11	/* Union definition */
#define HLL_ENUM_DEF		0x12	/* Enumeration definition */
#define HLL_MEMBER_DEF		0x13	/* Member definition */
#define HLL_ARRAY_DEF		0x14	/* Array definition */
#define HLL_POINTER_DEF		0x15	/* Pointer definition */
#define HLL_FUNCTION_DEF	0x16	/* Function definition */
#define HLL_REG_VAR		0x17	/* Register variable */
#define HLL_CONST_DEF		0x18	/* Constant definition */

/* IBM HLL type codes */
#define HLL_TYPE_VOID		0x00
#define HLL_TYPE_CHAR		0x01
#define HLL_TYPE_SHORT		0x02
#define HLL_TYPE_INT		0x03
#define HLL_TYPE_LONG		0x04
#define HLL_TYPE_LONGLONG	0x05
#define HLL_TYPE_FLOAT		0x06
#define HLL_TYPE_DOUBLE		0x07
#define HLL_TYPE_LDOUBLE	0x08
#define HLL_TYPE_UCHAR		0x09
#define HLL_TYPE_USHORT		0x0A
#define HLL_TYPE_UINT		0x0B
#define HLL_TYPE_ULONG		0x0C
#define HLL_TYPE_ULONGLONG	0x0D
#define HLL_TYPE_POINTER	0x10
#define HLL_TYPE_ARRAY		0x11
#define HLL_TYPE_STRUCT		0x12
#define HLL_TYPE_UNION		0x13
#define HLL_TYPE_ENUM		0x14
#define HLL_TYPE_FUNCTION	0x15
#define HLL_TYPE_TYPEDEF	0x16
#define HLL_TYPE_CONST		0x20
#define HLL_TYPE_VOLATILE	0x21

/* IBM HLL storage classes */
#define HLL_SC_AUTO		0x01
#define HLL_SC_STATIC		0x02
#define HLL_SC_EXTERN		0x03
#define HLL_SC_REGISTER		0x04
#define HLL_SC_TYPEDEF		0x05
#define HLL_SC_MEMBER		0x06
#define HLL_SC_PARAMETER	0x07

/* HLL state */
static struct {
	int initialized;
	int module_level;
	int block_level;
	int type_index;
	int symbol_count;
	int line_count;
	char *module_name;
} hll_state;

/*
 * Initialize IBM HLL output
 */
void
debugsym_hll_init(void)
{
	memset(&hll_state, 0, sizeof(hll_state));
	hll_state.initialized = 1;
	hll_state.type_index = 0x1000;  /* Start type indices */

	printf("\t# IBM HLL Debug Information\n");
	printf("\t.section .debug$H,\"dr\"\n");

	/* Emit HLL signature */
	printf("\t.ascii \"HLL1\"\n");  /* HLL version 1 */
	printf("\t.long 1\n");  /* Format version */

	/* Begin module */
	printf("\t# Begin module\n");
	printf("\t.byte 0x%02x\n", HLL_BEGIN_MODULE);
	printf("\t.short 0  # record length\n");
}

/*
 * Emit a length-prefixed string (IBM style)
 */
static void
emit_hll_string(const char *str)
{
	int len = str ? strlen(str) : 0;

	if (len > 255)
		len = 255;

	printf("\t.byte %d\n", len);
	if (len > 0)
		printf("\t.ascii \"%.*s\"\n", len, str);
}

/*
 * Emit a 32-bit value
 */
static void
emit_hll_long(unsigned long value)
{
	printf("\t.long 0x%08lx\n", value);
}

/*
 * Emit a 16-bit value
 */
static void
emit_hll_short(unsigned short value)
{
	printf("\t.short 0x%04x\n", value);
}

/*
 * Get HLL type code from debug type
 */
static int
get_hll_type_code(debug_type_t *type)
{
	if (type == NULL)
		return HLL_TYPE_VOID;

	switch (type->encoding) {
	case DBGTYPE_VOID:	return HLL_TYPE_VOID;
	case DBGTYPE_CHAR:	return HLL_TYPE_CHAR;
	case DBGTYPE_INT8:	return HLL_TYPE_CHAR;
	case DBGTYPE_UINT8:	return HLL_TYPE_UCHAR;
	case DBGTYPE_INT16:	return HLL_TYPE_SHORT;
	case DBGTYPE_UINT16:	return HLL_TYPE_USHORT;
	case DBGTYPE_INT32:	return HLL_TYPE_INT;
	case DBGTYPE_UINT32:	return HLL_TYPE_UINT;
	case DBGTYPE_INT64:	return HLL_TYPE_LONGLONG;
	case DBGTYPE_UINT64:	return HLL_TYPE_ULONGLONG;
	case DBGTYPE_FLOAT32:	return HLL_TYPE_FLOAT;
	case DBGTYPE_FLOAT64:	return HLL_TYPE_DOUBLE;
	case DBGTYPE_FLOAT80:	return HLL_TYPE_LDOUBLE;
	case DBGTYPE_POINTER:	return HLL_TYPE_POINTER;
	case DBGTYPE_ARRAY:	return HLL_TYPE_ARRAY;
	case DBGTYPE_STRUCT:	return HLL_TYPE_STRUCT;
	case DBGTYPE_UNION:	return HLL_TYPE_UNION;
	case DBGTYPE_ENUM:	return HLL_TYPE_ENUM;
	case DBGTYPE_FUNCTION:	return HLL_TYPE_FUNCTION;
	default:		return HLL_TYPE_INT;
	}
}

/*
 * Get HLL storage class from symbol
 */
static int
get_hll_storage_class(debug_symbol_t *sym)
{
	switch (sym->kind) {
	case DBGSYM_PARAMETER:
		return HLL_SC_PARAMETER;

	case DBGSYM_VARIABLE:
		if (sym->is_register)
			return HLL_SC_REGISTER;
		if (sym->storage_class == AUTO)
			return HLL_SC_AUTO;
		if (sym->storage_class == STATIC)
			return HLL_SC_STATIC;
		return HLL_SC_EXTERN;

	case DBGSYM_TYPEDEF:
		return HLL_SC_TYPEDEF;

	case DBGSYM_STRUCT:
	case DBGSYM_UNION:
	case DBGSYM_ENUM:
		return HLL_SC_TYPEDEF;

	default:
		return HLL_SC_EXTERN;
	}
}

/*
 * Emit a symbol definition
 */
static void
emit_hll_symbol(debug_symbol_t *sym)
{
	int record_type = HLL_DEF_SYMBOL;
	int storage_class, type_code;

	if (sym == NULL)
		return;

	storage_class = get_hll_storage_class(sym);
	type_code = get_hll_type_code(sym->type);

	/* Determine record type */
	if (sym->kind == DBGSYM_FUNCTION)
		record_type = HLL_BEGIN_FUNCTION;
	else if (sym->kind == DBGSYM_PARAMETER)
		record_type = HLL_PARAMETER;
	else if (storage_class == HLL_SC_AUTO)
		record_type = HLL_AUTO_VAR;
	else if (storage_class == HLL_SC_STATIC)
		record_type = HLL_STATIC_VAR;
	else if (storage_class == HLL_SC_EXTERN)
		record_type = HLL_GLOBAL_VAR;
	else if (storage_class == HLL_SC_REGISTER)
		record_type = HLL_REG_VAR;

	printf("\t# HLL Symbol: %s\n", sym->name ? sym->name : "(null)");
	printf("\t.byte 0x%02x\n", record_type);
	printf("\t.short 0  # record length\n");

	/* Storage class */
	printf("\t.byte 0x%02x\n", storage_class);

	/* Type code */
	printf("\t.byte 0x%02x\n", type_code);

	/* Type size */
	emit_hll_short(sym->type ? sym->type->size : 0);

	/* Offset or address */
	emit_hll_long(sym->offset);

	/* Symbol name */
	emit_hll_string(sym->name);

	/* Line number */
	emit_hll_short(sym->location.line);

	hll_state.symbol_count++;
}

/*
 * Emit a type definition
 */
static void
emit_hll_type(debug_symbol_t *sym)
{
	int type_code;

	if (sym == NULL || sym->type == NULL)
		return;

	type_code = get_hll_type_code(sym->type);

	printf("\t# HLL Type: %s\n", sym->name ? sym->name : "(null)");
	printf("\t.byte 0x%02x\n", HLL_DEF_TYPE);
	printf("\t.short 0  # record length\n");

	/* Type index */
	emit_hll_short(hll_state.type_index++);

	/* Type code */
	printf("\t.byte 0x%02x\n", type_code);

	/* Type size */
	emit_hll_long(sym->type->size);

	/* Type name */
	emit_hll_string(sym->name);

	/* For composite types, emit members */
	if (sym->type->encoding == DBGTYPE_STRUCT ||
	    sym->type->encoding == DBGTYPE_UNION) {
		int i;

		for (i = 0; i < sym->type->member_count; i++) {
			printf("\t# Member: %s\n", sym->type->members[i].name);
			printf("\t.byte 0x%02x\n", HLL_MEMBER_DEF);
			printf("\t.short 0  # record length\n");
			emit_hll_string(sym->type->members[i].name);
			emit_hll_long(sym->type->members[i].offset);
		}
	}

	/* For enums, emit enumerators */
	if (sym->type->encoding == DBGTYPE_ENUM) {
		int i;

		for (i = 0; i < sym->type->enum_value_count; i++) {
			printf("\t# Enumerator: %s\n", sym->type->enum_values[i].name);
			printf("\t.byte 0x%02x\n", HLL_CONST_DEF);
			printf("\t.short 0  # record length\n");
			emit_hll_string(sym->type->enum_values[i].name);
			emit_hll_long(sym->type->enum_values[i].value);
		}
	}
}

/*
 * Emit line number information
 */
static void
emit_hll_line(int line)
{
	printf("\t# Line %d\n", line);
	printf("\t.byte 0x%02x\n", HLL_LINE_NUM);
	printf("\t.short 4  # record length\n");
	emit_hll_short(line);
	emit_hll_short(0);  /* Column (unused) */

	hll_state.line_count++;
}

/*
 * Emit a symbol as IBM HLL
 */
void
debugsym_hll_emit(debug_symbol_t *sym)
{
	if (sym == NULL)
		return;

	/* Emit type information if needed */
	if (sym->kind == DBGSYM_TYPEDEF ||
	    sym->kind == DBGSYM_STRUCT ||
	    sym->kind == DBGSYM_UNION ||
	    sym->kind == DBGSYM_ENUM) {
		emit_hll_type(sym);
	} else {
		emit_hll_symbol(sym);
	}

	/* Emit line number */
	if (sym->location.line > 0)
		emit_hll_line(sym->location.line);
}

/*
 * Parse IBM HLL debug information
 */
debug_symbol_t *
debugsym_hll_parse(void *data, size_t len)
{
	unsigned char *p = (unsigned char *)data;

	if (len < 4)
		return NULL;

	/* Check for HLL signature */
	if (p[0] != 'H' || p[1] != 'L' || p[2] != 'L')
		return NULL;

	/* Full parser would be implemented here */
	return NULL;
}

/*
 * Finish IBM HLL output
 */
void
debugsym_hll_finish(void)
{
	/* End module */
	printf("\t# End module\n");
	printf("\t.byte 0x%02x\n", HLL_END_MODULE);
	printf("\t.short 0\n");

	printf("\t# IBM HLL: %d symbols, %d lines\n",
	    hll_state.symbol_count, hll_state.line_count);
}
