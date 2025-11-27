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
 * HP SOM (System Object Model) Debug Format Support
 *
 * Used by HP-UX systems:
 * - HP-UX on PA-RISC (HP 9000)
 * - HP-UX on Itanium (HP Integrity)
 * - HP C/C++ compilers
 * - HP Fortran compilers
 *
 * SOM debug features:
 * - Extension records in SOM files
 * - Debug symbol table (DNTT - Debug Name and Type Table)
 * - Source line table (SLT)
 * - Value table (VT)
 * - Local symbol table (LNTT)
 * - Global symbol table (GNTT)
 */

#include "pass1.h"
#include "debugsym.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* HP SOM magic number */
#define SOM_MAGIC		0x0210	/* PA-RISC 1.x */
#define SOM_MAGIC_2		0x020b	/* PA-RISC 2.0 */
#define SOM_MAGIC_IA64		0x0214	/* Itanium */

/* SOM auxiliary header types */
#define AUX_ID_EXEC		0x0004	/* Executable */
#define AUX_ID_DEBUG		0x000c	/* Debug info */
#define AUX_ID_LNTT		0x000d	/* Local name/type table */
#define AUX_ID_GNTT		0x000e	/* Global name/type table */
#define AUX_ID_SLT		0x000f	/* Source line table */
#define AUX_ID_VT		0x0010	/* Value table */

/* DNTT (Debug Name and Type Table) record types */
#define DNTT_TYPE_NIL		0x00	/* Null entry */
#define DNTT_TYPE_SRCFILE	0x01	/* Source file */
#define DNTT_TYPE_MODULE	0x02	/* Module */
#define DNTT_TYPE_FUNCTION	0x03	/* Function */
#define DNTT_TYPE_ENTRY		0x04	/* Entry point */
#define DNTT_TYPE_BEGIN		0x05	/* Begin block */
#define DNTT_TYPE_END		0x06	/* End block */
#define DNTT_TYPE_FPARAM	0x07	/* Formal parameter */
#define DNTT_TYPE_SVAR		0x08	/* Static variable */
#define DNTT_TYPE_DVAR		0x09	/* Dynamic variable */
#define DNTT_TYPE_CONST		0x0a	/* Constant */
#define DNTT_TYPE_TYPEDEF	0x0b	/* Type definition */
#define DNTT_TYPE_TAGDEF	0x0c	/* Tag definition */
#define DNTT_TYPE_POINTER	0x0d	/* Pointer type */
#define DNTT_TYPE_ENUM		0x0e	/* Enumeration */
#define DNTT_TYPE_MEMENUM	0x0f	/* Enumeration member */
#define DNTT_TYPE_SET		0x10	/* Set type */
#define DNTT_TYPE_SUBRANGE	0x11	/* Subrange type */
#define DNTT_TYPE_ARRAY		0x12	/* Array type */
#define DNTT_TYPE_STRUCT	0x13	/* Structure type */
#define DNTT_TYPE_UNION		0x14	/* Union type */
#define DNTT_TYPE_FIELD		0x15	/* Field */
#define DNTT_TYPE_VARIANT	0x16	/* Variant record */
#define DNTT_TYPE_FILE		0x17	/* File type */
#define DNTT_TYPE_FUNCTYPE	0x18	/* Function type */
#define DNTT_TYPE_WITH		0x19	/* WITH statement */
#define DNTT_TYPE_COMMON	0x1a	/* Common block */
#define DNTT_TYPE_COBSTRUCT	0x1b	/* COBOL structure */
#define DNTT_TYPE_XREF		0x1c	/* Cross reference */
#define DNTT_TYPE_SA		0x1d	/* Static array */
#define DNTT_TYPE_MACRO		0x1e	/* Macro definition */
#define DNTT_TYPE_BLOCKDATA	0x1f	/* Block data */
#define DNTT_TYPE_CLASS_SCOPE	0x20	/* C++ class scope */
#define DNTT_TYPE_REFERENCE	0x21	/* C++ reference */
#define DNTT_TYPE_PTRMEM	0x22	/* Pointer to member */
#define DNTT_TYPE_PTRMEMFUNC	0x23	/* Pointer to member func */
#define DNTT_TYPE_CLASS		0x24	/* C++ class */
#define DNTT_TYPE_GENFIELD	0x25	/* Generic field */
#define DNTT_TYPE_VFUNC		0x26	/* Virtual function */
#define DNTT_TYPE_MEMFUNC	0x27	/* Member function */
#define DNTT_TYPE_INHERITANCE	0x28	/* Inheritance */
#define DNTT_TYPE_FRIEND_CLASS	0x29	/* Friend class */
#define DNTT_TYPE_FRIEND_FUNC	0x2a	/* Friend function */
#define DNTT_TYPE_MODIFIER	0x2b	/* Type modifier */
#define DNTT_TYPE_OBJECT_ID	0x2c	/* Object identifier */
#define DNTT_TYPE_TEMPLATE	0x2d	/* Template */
#define DNTT_TYPE_TEMPL_ARG	0x2e	/* Template argument */
#define DNTT_TYPE_FUNC_TEMPLATE	0x2f	/* Function template */
#define DNTT_TYPE_LINK		0x30	/* Link */

/* HP SOM type qualifiers */
#define SOM_TYPE_BASIC		0x00
#define SOM_TYPE_POINTER	0x01
#define SOM_TYPE_ARRAY		0x02
#define SOM_TYPE_FUNCTION	0x03
#define SOM_TYPE_STRUCT		0x04
#define SOM_TYPE_UNION		0x05
#define SOM_TYPE_ENUM		0x06

/* HP SOM basic types */
#define SOM_BT_VOID		0x00
#define SOM_BT_BOOLEAN		0x01
#define SOM_BT_CHAR		0x02
#define SOM_BT_INT		0x03
#define SOM_BT_UNS_INT		0x04
#define SOM_BT_LONG		0x05
#define SOM_BT_UNS_LONG		0x06
#define SOM_BT_FLOAT		0x07
#define SOM_BT_DOUBLE		0x08
#define SOM_BT_COMPLEX		0x09
#define SOM_BT_DCOMPLEX		0x0a
#define SOM_BT_STRING		0x0b
#define SOM_BT_LONGLONG		0x0c
#define SOM_BT_UNS_LONGLONG	0x0d
#define SOM_BT_TEXT		0x0e
#define SOM_BT_FLABEL		0x0f
#define SOM_BT_FTN_STRING	0x10
#define SOM_BT_LONG_DOUBLE	0x11

/* SOM state */
static struct {
	int initialized;
	int dntt_index;		/* DNTT index */
	int slt_index;		/* SLT index */
	int vt_offset;		/* VT offset */
	int symbol_count;
	int type_count;
	char *source_file;
} som_state;

/*
 * Initialize HP SOM output
 */
void
debugsym_hpsom_init(void)
{
	memset(&som_state, 0, sizeof(som_state));
	som_state.initialized = 1;
	som_state.dntt_index = 1;  /* Index 0 is NIL */

	printf("\t# HP SOM Debug Information\n");
	printf("\t.section .debug,\"dr\"\n");

	/* Emit SOM debug header */
	printf("\t# SOM Debug Header\n");
	printf("\t.long 0x%04x\n", SOM_MAGIC);  /* Magic number */
	printf("\t.long 0\n");  /* Version */

	/* DNTT header */
	printf("\t# DNTT (Debug Name and Type Table)\n");
	printf(".Ldntt_start:\n");

	/* Emit NIL entry */
	printf("\t.long 0x%08x\n", DNTT_TYPE_NIL);
	printf("\t.space 12\n");  /* Padding */
}

/*
 * Emit a DNTT entry
 */
static void
emit_dntt_entry(int dntt_type, int data1, int data2, int data3)
{
	printf("\t# DNTT[%d] type=%d\n", som_state.dntt_index, dntt_type);
	printf("\t.long 0x%08x\n", dntt_type);
	printf("\t.long 0x%08x\n", data1);
	printf("\t.long 0x%08x\n", data2);
	printf("\t.long 0x%08x\n", data3);

	som_state.dntt_index++;
}

/*
 * Emit a string to the value table
 */
static int
emit_vt_string(const char *str)
{
	int offset = som_state.vt_offset;
	int len = str ? strlen(str) : 0;

	printf("\t# VT[%d]: \"%s\"\n", offset, str ? str : "");
	printf("\t.asciz \"%s\"\n", str ? str : "");

	som_state.vt_offset += len + 1;
	return offset;
}

/*
 * Get SOM basic type code
 */
static int
get_som_basic_type(debug_type_t *type)
{
	if (type == NULL)
		return SOM_BT_VOID;

	switch (type->encoding) {
	case DBGTYPE_VOID:	return SOM_BT_VOID;
	case DBGTYPE_BOOL:	return SOM_BT_BOOLEAN;
	case DBGTYPE_CHAR:	return SOM_BT_CHAR;
	case DBGTYPE_INT8:
	case DBGTYPE_INT16:
	case DBGTYPE_INT32:	return SOM_BT_INT;
	case DBGTYPE_UINT8:
	case DBGTYPE_UINT16:
	case DBGTYPE_UINT32:	return SOM_BT_UNS_INT;
	case DBGTYPE_INT64:	return SOM_BT_LONGLONG;
	case DBGTYPE_UINT64:	return SOM_BT_UNS_LONGLONG;
	case DBGTYPE_FLOAT32:	return SOM_BT_FLOAT;
	case DBGTYPE_FLOAT64:	return SOM_BT_DOUBLE;
	case DBGTYPE_FLOAT80:
	case DBGTYPE_FLOAT128:	return SOM_BT_LONG_DOUBLE;
	case DBGTYPE_COMPLEX_FLOAT:	return SOM_BT_COMPLEX;
	case DBGTYPE_COMPLEX_DOUBLE:	return SOM_BT_DCOMPLEX;
	default:		return SOM_BT_INT;
	}
}

/*
 * Get DNTT type code from symbol
 */
static int
get_dntt_type_code(debug_symbol_t *sym)
{
	if (sym == NULL)
		return DNTT_TYPE_NIL;

	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		return DNTT_TYPE_FUNCTION;
	case DBGSYM_VARIABLE:
		if (sym->storage_class == STATIC)
			return DNTT_TYPE_SVAR;
		else
			return DNTT_TYPE_DVAR;
	case DBGSYM_PARAMETER:
		return DNTT_TYPE_FPARAM;
	case DBGSYM_TYPEDEF:
		return DNTT_TYPE_TYPEDEF;
	case DBGSYM_STRUCT:
		return DNTT_TYPE_STRUCT;
	case DBGSYM_UNION:
		return DNTT_TYPE_UNION;
	case DBGSYM_ENUM:
		return DNTT_TYPE_ENUM;
	case DBGSYM_CONSTANT:
		return DNTT_TYPE_CONST;
	default:
		return DNTT_TYPE_NIL;
	}
}

/*
 * Emit a symbol as HP SOM DNTT entry
 */
void
debugsym_hpsom_emit(debug_symbol_t *sym)
{
	int dntt_type, name_offset, type_index, value;

	if (sym == NULL)
		return;

	dntt_type = get_dntt_type_code(sym);
	if (dntt_type == DNTT_TYPE_NIL)
		return;

	/* Add symbol name to value table */
	name_offset = emit_vt_string(sym->name);

	/* Get type information */
	type_index = 0;
	if (sym->type != NULL) {
		type_index = get_som_basic_type(sym->type);
	}

	/* Value or offset */
	value = (int)sym->offset;

	/* Emit DNTT entry */
	emit_dntt_entry(dntt_type, name_offset, type_index, value);

	/* Emit SLT (Source Line Table) entry */
	if (sym->location.line > 0) {
		printf("\t# SLT[%d] line=%d\n", som_state.slt_index, sym->location.line);
		printf("\t.long 0x%08x\n", sym->location.line);
		printf("\t.long 0x%08x\n", som_state.dntt_index - 1);
		som_state.slt_index++;
	}

	som_state.symbol_count++;

	/* For composite types, emit type definition */
	if (sym->kind == DBGSYM_STRUCT || sym->kind == DBGSYM_UNION) {
		int i;

		/* Emit field entries */
		for (i = 0; i < sym->type->member_count; i++) {
			int field_name = emit_vt_string(sym->type->members[i].name);
			int field_offset = sym->type->members[i].offset;

			emit_dntt_entry(DNTT_TYPE_FIELD, field_name, 0, field_offset);
		}

		som_state.type_count++;
	}

	/* For enums, emit enumerator entries */
	if (sym->kind == DBGSYM_ENUM) {
		int i;

		for (i = 0; i < sym->type->enum_value_count; i++) {
			int enum_name = emit_vt_string(sym->type->enum_values[i].name);
			int enum_value = sym->type->enum_values[i].value;

			emit_dntt_entry(DNTT_TYPE_MEMENUM, enum_name, enum_value, 0);
		}

		som_state.type_count++;
	}
}

/*
 * Parse HP SOM debug information
 */
debug_symbol_t *
debugsym_hpsom_parse(void *data, size_t len)
{
	unsigned short *magic;

	if (len < 2)
		return NULL;

	magic = (unsigned short *)data;

	/* Check for SOM magic number */
	if (*magic != SOM_MAGIC && *magic != SOM_MAGIC_2 && *magic != SOM_MAGIC_IA64)
		return NULL;

	/* Full parser would be implemented here */
	return NULL;
}

/*
 * Finish HP SOM output
 */
void
debugsym_hpsom_finish(void)
{
	printf("\t# End DNTT\n");
	printf(".Ldntt_end:\n");

	printf("\t# HP SOM: %d symbols, %d types, %d DNTT entries\n",
	    som_state.symbol_count, som_state.type_count, som_state.dntt_index);
}
