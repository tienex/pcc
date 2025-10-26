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
 * VMS/OpenVMS Debug Symbol Table (DST) Format Support
 *
 * Used by Digital Equipment Corporation (DEC) and HP systems:
 * - VAX/VMS (VAX architecture)
 * - OpenVMS on VAX
 * - OpenVMS on Alpha (AXP)
 * - OpenVMS on Itanium (I64)
 *
 * VMS DST features:
 * - Debug Symbol Table (DST) records
 * - Module and routine information
 * - Type descriptors (comprehensive)
 * - Source correlation records
 * - PC (Program Counter) correlation
 * - Line number tables
 * - Stack frame information
 * - Register usage tracking
 */

#include "pass1.h"
#include "debugsym.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* VMS DST record types */
#define DST_K_SOURCE		155	/* Source file specification */
#define DST_K_PROLOG		176	/* Procedure prolog */
#define DST_K_BLKBEG		177	/* Block begin */
#define DST_K_BLKEND		178	/* Block end */
#define DST_K_ENTRY		179	/* Entry point */
#define DST_K_PSECT		180	/* Program section */
#define DST_K_LABEL		181	/* Label */
#define DST_K_VALKIND_LITERAL	182	/* Literal value */
#define DST_K_VALKIND_ADDR	183	/* Address value */
#define DST_K_VALKIND_DESC	184	/* Descriptor value */
#define DST_K_VALKIND_REG	185	/* Register value */
#define DST_K_TYPSPEC		186	/* Type specification */
#define DST_K_MODBEG		188	/* Module begin */
#define DST_K_MODEND		189	/* Module end */
#define DST_K_RTNBEG		190	/* Routine begin */
#define DST_K_RTNEND		191	/* Routine end */
#define DST_K_VARIABLE		192	/* Variable */
#define DST_K_PARAMETER		193	/* Formal parameter */
#define DST_K_LINE_NUM		185	/* Line number */
#define DST_K_DELTA_PC		186	/* PC delta */
#define DST_K_INCR_LINUM	187	/* Increment line number */
#define DST_K_SETREG		200	/* Set register contents */
#define DST_K_REGNUM		201	/* Register number */
#define DST_K_PCSTMT		202	/* PC for statement */
#define DST_K_PCSCOPE		203	/* PC scope */
#define DST_K_LABEL_NORM	204	/* Normal label */
#define DST_K_RECBEG		205	/* Record begin */
#define DST_K_RECEND		206	/* Record end */
#define DST_K_VFLDBEG		207	/* Variant field begin */
#define DST_K_VFLDEND		208	/* Variant field end */
#define DST_K_CONSTANT		209	/* Named constant */
#define DST_K_TYPEDEF		210	/* Type definition */

/* VMS data type codes */
#define DST_K_TS_ATOM		0x00	/* Atomic type */
#define DST_K_TS_DSC		0x01	/* Descriptor */
#define DST_K_TS_IND		0x02	/* Indirect */
#define DST_K_TS_TPTR		0x03	/* Type pointer */
#define DST_K_TS_PTR		0x04	/* Pointer */
#define DST_K_TS_ARRAY		0x05	/* Array */
#define DST_K_TS_NOV		0x06	/* No value (void) */
#define DST_K_TS_STRUCT		0x07	/* Structure */
#define DST_K_TS_UNION		0x08	/* Union */
#define DST_K_TS_ENUM		0x09	/* Enumeration */
#define DST_K_TS_FILE		0x0a	/* File */
#define DST_K_TS_SET		0x0b	/* Set */
#define DST_K_TS_SUBRANGE	0x0c	/* Subrange */
#define DST_K_TS_ADA_ARRAY	0x0d	/* Ada array */
#define DST_K_TS_ADA_RECORD	0x0e	/* Ada record */
#define DST_K_TS_ADA_FIXED	0x0f	/* Ada fixed */
#define DST_K_TS_COMPLEX	0x10	/* Complex */

/* VMS atomic data types */
#define DST_K_DT_BYTE		0x02	/* Byte integer */
#define DST_K_DT_WORD		0x03	/* Word integer */
#define DST_K_DT_LONG		0x04	/* Longword integer */
#define DST_K_DT_QUAD		0x05	/* Quadword integer */
#define DST_K_DT_OCTA		0x06	/* Octaword integer */
#define DST_K_DT_F_FLOAT	0x0a	/* F floating (32-bit) */
#define DST_K_DT_D_FLOAT	0x0b	/* D floating (64-bit) */
#define DST_K_DT_G_FLOAT	0x0c	/* G floating (64-bit) */
#define DST_K_DT_H_FLOAT	0x0d	/* H floating (128-bit) */
#define DST_K_DT_F_CMPLX	0x0e	/* F complex */
#define DST_K_DT_D_CMPLX	0x0f	/* D complex */
#define DST_K_DT_G_CMPLX	0x10	/* G complex */
#define DST_K_DT_H_CMPLX	0x11	/* H complex */
#define DST_K_DT_CHAR		0x14	/* Character */
#define DST_K_DT_DECIMAL	0x16	/* Packed decimal */
#define DST_K_DT_ADDR		0x17	/* Address (pointer) */
#define DST_K_DT_IEEE_S		0x34	/* IEEE S_float (32-bit) */
#define DST_K_DT_IEEE_T		0x35	/* IEEE T_float (64-bit) */
#define DST_K_DT_IEEE_X		0x36	/* IEEE X_float (128-bit) */

/* VMS storage classes */
#define DST_K_SC_AUTOMATIC	0x01	/* Automatic (stack) */
#define DST_K_SC_STATIC		0x02	/* Static */
#define DST_K_SC_GLOBAL		0x03	/* Global */
#define DST_K_SC_REGISTER	0x04	/* Register */
#define DST_K_SC_LITERAL	0x05	/* Literal/constant */
#define DST_K_SC_BASED		0x06	/* Based variable */
#define DST_K_SC_COMMON		0x07	/* Common block */

/* VMS register codes (VAX) */
#define DST_K_R0		0
#define DST_K_R1		1
#define DST_K_R2		2
#define DST_K_R3		3
#define DST_K_R4		4
#define DST_K_R5		5
#define DST_K_R6		6
#define DST_K_R7		7
#define DST_K_R8		8
#define DST_K_R9		9
#define DST_K_R10		10
#define DST_K_R11		11
#define DST_K_AP		12	/* Argument pointer */
#define DST_K_FP		13	/* Frame pointer */
#define DST_K_SP		14	/* Stack pointer */
#define DST_K_PC		15	/* Program counter */

/* VMS DST state */
static struct {
	int initialized;
	int module_level;
	int routine_level;
	int block_level;
	unsigned int pc_value;
	unsigned int line_number;
	int symbol_count;
	int type_count;
	int record_count;
	char *module_name;
	char *source_file;
} vms_state;

/*
 * Initialize VMS DST output
 */
void
debugsym_vms_init(void)
{
	memset(&vms_state, 0, sizeof(vms_state));
	vms_state.initialized = 1;

	printf("\t# VMS/OpenVMS Debug Symbol Table (DST)\n");
	printf("\t.section .debug,\"dr\"\n");

	/* Emit DST header */
	printf("\t# DST Header\n");
	printf("\t.ascii \"DST$\"\n");  /* DST signature */
	printf("\t.long 1\n");  /* DST version */
}

/*
 * Emit a DST record header
 */
static void
emit_dst_record_header(int record_type, int record_length)
{
	printf("\t# DST Record: type=%d len=%d\n", record_type, record_length);
	printf("\t.byte %d\n", record_type);
	printf("\t.word %d\n", record_length);

	vms_state.record_count++;
}

/*
 * Emit a counted string (VMS style)
 */
static void
emit_counted_string(const char *str)
{
	int len = str ? strlen(str) : 0;

	if (len > 255)
		len = 255;

	printf("\t.byte %d\n", len);
	if (len > 0)
		printf("\t.ascii \"%.*s\"\n", len, str);
}

/*
 * Emit a word (16-bit) value
 */
static void
emit_word(unsigned short value)
{
	printf("\t.word 0x%04x\n", value);
}

/*
 * Emit a longword (32-bit) value
 */
static void
emit_long(unsigned long value)
{
	printf("\t.long 0x%08lx\n", value);
}

/*
 * Get VMS data type code from debug type
 */
static int
get_vms_data_type(debug_type_t *type)
{
	if (type == NULL)
		return DST_K_TS_NOV;

	switch (type->encoding) {
	case DBGTYPE_VOID:
		return DST_K_TS_NOV;

	case DBGTYPE_CHAR:
	case DBGTYPE_INT8:
	case DBGTYPE_UINT8:
		return DST_K_DT_BYTE;

	case DBGTYPE_INT16:
	case DBGTYPE_UINT16:
		return DST_K_DT_WORD;

	case DBGTYPE_INT32:
	case DBGTYPE_UINT32:
		return DST_K_DT_LONG;

	case DBGTYPE_INT64:
	case DBGTYPE_UINT64:
		return DST_K_DT_QUAD;

	case DBGTYPE_FLOAT32:
		return DST_K_DT_IEEE_S;  /* IEEE S_float for modern systems */

	case DBGTYPE_FLOAT64:
		return DST_K_DT_IEEE_T;  /* IEEE T_float */

	case DBGTYPE_FLOAT80:
	case DBGTYPE_FLOAT128:
		return DST_K_DT_IEEE_X;  /* IEEE X_float */

	case DBGTYPE_COMPLEX_FLOAT:
		return DST_K_DT_F_CMPLX;

	case DBGTYPE_COMPLEX_DOUBLE:
		return DST_K_DT_D_CMPLX;

	case DBGTYPE_POINTER:
		return DST_K_TS_PTR;

	case DBGTYPE_ARRAY:
		return DST_K_TS_ARRAY;

	case DBGTYPE_STRUCT:
		return DST_K_TS_STRUCT;

	case DBGTYPE_UNION:
		return DST_K_TS_UNION;

	case DBGTYPE_ENUM:
		return DST_K_TS_ENUM;

	default:
		return DST_K_DT_LONG;
	}
}

/*
 * Get VMS storage class from symbol
 */
static int
get_vms_storage_class(debug_symbol_t *sym)
{
	if (sym == NULL)
		return DST_K_SC_STATIC;

	if (sym->is_register)
		return DST_K_SC_REGISTER;

	switch (sym->storage_class) {
	case AUTO:
		return DST_K_SC_AUTOMATIC;
	case STATIC:
		return DST_K_SC_STATIC;
	case EXTERN:
	case EXTDEF:
		return DST_K_SC_GLOBAL;
	default:
		return DST_K_SC_AUTOMATIC;
	}
}

/*
 * Emit module begin record
 */
static void
emit_module_begin(const char *module_name)
{
	if (vms_state.module_level > 0)
		return;  /* Already in a module */

	printf("\t# Module Begin: %s\n", module_name ? module_name : "(null)");
	emit_dst_record_header(DST_K_MODBEG, 0);
	emit_counted_string(module_name);

	vms_state.module_level = 1;
	if (vms_state.module_name)
		free(vms_state.module_name);
	vms_state.module_name = module_name ? strdup(module_name) : NULL;
}

/*
 * Emit module end record
 */
static void
emit_module_end(void)
{
	if (vms_state.module_level == 0)
		return;

	printf("\t# Module End\n");
	emit_dst_record_header(DST_K_MODEND, 0);

	vms_state.module_level = 0;
}

/*
 * Emit routine begin record
 */
static void
emit_routine_begin(debug_symbol_t *sym)
{
	printf("\t# Routine Begin: %s\n", sym->name ? sym->name : "(null)");
	emit_dst_record_header(DST_K_RTNBEG, 0);

	/* Routine name */
	emit_counted_string(sym->name);

	/* Entry point address */
	emit_long(sym->low_pc);

	/* Procedure descriptor */
	printf("\t.byte 0\n");  /* Flags */
	printf("\t.byte %d\n", DST_K_FP);  /* Frame register */

	vms_state.routine_level++;
}

/*
 * Emit routine end record
 */
static void
emit_routine_end(void)
{
	if (vms_state.routine_level == 0)
		return;

	printf("\t# Routine End\n");
	emit_dst_record_header(DST_K_RTNEND, 0);

	vms_state.routine_level--;
}

/*
 * Emit block begin record
 */
static void
emit_block_begin(void)
{
	printf("\t# Block Begin\n");
	emit_dst_record_header(DST_K_BLKBEG, 0);

	vms_state.block_level++;
}

/*
 * Emit block end record
 */
static void
emit_block_end(void)
{
	if (vms_state.block_level == 0)
		return;

	printf("\t# Block End\n");
	emit_dst_record_header(DST_K_BLKEND, 0);

	vms_state.block_level--;
}

/*
 * Emit source file record
 */
static void
emit_source_file(const char *filename)
{
	printf("\t# Source File: %s\n", filename ? filename : "(null)");
	emit_dst_record_header(DST_K_SOURCE, 0);
	emit_counted_string(filename);

	if (vms_state.source_file)
		free(vms_state.source_file);
	vms_state.source_file = filename ? strdup(filename) : NULL;
}

/*
 * Emit line number correlation record
 */
static void
emit_line_number(int line, unsigned int pc)
{
	printf("\t# Line %d at PC 0x%x\n", line, pc);
	emit_dst_record_header(DST_K_LINE_NUM, 6);
	emit_word(line);
	emit_long(pc);

	vms_state.line_number = line;
	vms_state.pc_value = pc;
}

/*
 * Emit type specification record
 */
static void
emit_type_spec(debug_type_t *type)
{
	int type_code;

	if (type == NULL)
		return;

	type_code = get_vms_data_type(type);

	printf("\t# Type Specification: code=%d size=%u\n", type_code, type->size);
	emit_dst_record_header(DST_K_TYPSPEC, 0);

	printf("\t.byte %d\n", type_code);  /* Type code */
	emit_long(type->size);  /* Size in bytes */

	/* For atomic types, emit basic type */
	if (type_code < DST_K_TS_PTR) {
		printf("\t.byte 0\n");  /* Sign/scale */
		printf("\t.byte 0\n");  /* Reserved */
	}

	/* For composite types, emit additional info */
	if (type->encoding == DBGTYPE_STRUCT || type->encoding == DBGTYPE_UNION) {
		int i;

		/* Emit field count */
		emit_word(type->member_count);

		/* Emit fields */
		for (i = 0; i < type->member_count; i++) {
			emit_counted_string(type->members[i].name);
			emit_long(type->members[i].offset);
		}
	}

	/* For arrays, emit bounds */
	if (type->encoding == DBGTYPE_ARRAY) {
		int i;

		emit_word(type->array_dimensions);

		for (i = 0; i < type->array_dimensions; i++) {
			emit_long(0);  /* Lower bound */
			emit_long(type->array_bounds[i] - 1);  /* Upper bound */
		}
	}

	/* For enums, emit enumerators */
	if (type->encoding == DBGTYPE_ENUM) {
		int i;

		emit_word(type->enum_value_count);

		for (i = 0; i < type->enum_value_count; i++) {
			emit_counted_string(type->enum_values[i].name);
			emit_long(type->enum_values[i].value);
		}
	}

	vms_state.type_count++;
}

/*
 * Emit variable record
 */
static void
emit_variable(debug_symbol_t *sym)
{
	int storage_class, type_code;

	storage_class = get_vms_storage_class(sym);
	type_code = get_vms_data_type(sym->type);

	printf("\t# Variable: %s\n", sym->name ? sym->name : "(null)");
	emit_dst_record_header(DST_K_VARIABLE, 0);

	/* Variable name */
	emit_counted_string(sym->name);

	/* Storage class */
	printf("\t.byte %d\n", storage_class);

	/* Type code */
	printf("\t.byte %d\n", type_code);

	/* Location/offset */
	if (storage_class == DST_K_SC_REGISTER) {
		printf("\t.byte %d\n", sym->register_num);
	} else {
		emit_long(sym->offset);
	}

	vms_state.symbol_count++;
}

/*
 * Emit parameter record
 */
static void
emit_parameter(debug_symbol_t *sym)
{
	int storage_class, type_code;

	storage_class = get_vms_storage_class(sym);
	type_code = get_vms_data_type(sym->type);

	printf("\t# Parameter: %s\n", sym->name ? sym->name : "(null)");
	emit_dst_record_header(DST_K_PARAMETER, 0);

	/* Parameter name */
	emit_counted_string(sym->name);

	/* Storage class */
	printf("\t.byte %d\n", storage_class);

	/* Type code */
	printf("\t.byte %d\n", type_code);

	/* Location/offset */
	if (storage_class == DST_K_SC_REGISTER) {
		printf("\t.byte %d\n", sym->register_num);
	} else {
		emit_long(sym->offset);
	}

	vms_state.symbol_count++;
}

/*
 * Emit constant record
 */
static void
emit_constant(debug_symbol_t *sym)
{
	printf("\t# Constant: %s\n", sym->name ? sym->name : "(null)");
	emit_dst_record_header(DST_K_CONSTANT, 0);

	emit_counted_string(sym->name);
	emit_long(sym->offset);  /* Value */
}

/*
 * Emit typedef record
 */
static void
emit_typedef(debug_symbol_t *sym)
{
	printf("\t# Typedef: %s\n", sym->name ? sym->name : "(null)");
	emit_dst_record_header(DST_K_TYPEDEF, 0);

	emit_counted_string(sym->name);

	/* Emit type specification */
	if (sym->type)
		emit_type_spec(sym->type);
}

/*
 * Emit a symbol as VMS DST
 */
void
debugsym_vms_emit(debug_symbol_t *sym)
{
	if (sym == NULL)
		return;

	/* Ensure module is started */
	if (vms_state.module_level == 0) {
		emit_module_begin(sym->location.filename);
	}

	/* Emit source file if changed */
	if (sym->location.filename && vms_state.source_file &&
	    strcmp(sym->location.filename, vms_state.source_file) != 0) {
		emit_source_file(sym->location.filename);
	}

	/* Emit line number if available */
	if (sym->location.line > 0 && sym->location.line != vms_state.line_number) {
		emit_line_number(sym->location.line, sym->location.address);
	}

	/* Emit based on symbol kind */
	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		emit_routine_begin(sym);
		break;

	case DBGSYM_VARIABLE:
		emit_variable(sym);
		break;

	case DBGSYM_PARAMETER:
		emit_parameter(sym);
		break;

	case DBGSYM_CONSTANT:
		emit_constant(sym);
		break;

	case DBGSYM_TYPEDEF:
		emit_typedef(sym);
		break;

	case DBGSYM_STRUCT:
	case DBGSYM_UNION:
	case DBGSYM_ENUM:
		if (sym->type)
			emit_type_spec(sym->type);
		break;

	default:
		break;
	}
}

/*
 * Parse VMS DST debug information
 */
debug_symbol_t *
debugsym_vms_parse(void *data, size_t len)
{
	unsigned char *p = (unsigned char *)data;

	if (len < 4)
		return NULL;

	/* Check for DST signature */
	if (p[0] != 'D' || p[1] != 'S' || p[2] != 'T' || p[3] != '$')
		return NULL;

	/* Full parser would be implemented here */
	return NULL;
}

/*
 * Finish VMS DST output
 */
void
debugsym_vms_finish(void)
{
	/* Close any open routines */
	while (vms_state.routine_level > 0)
		emit_routine_end();

	/* Close any open blocks */
	while (vms_state.block_level > 0)
		emit_block_end();

	/* Close module */
	emit_module_end();

	printf("\t# VMS DST: %d symbols, %d types, %d records\n",
	    vms_state.symbol_count, vms_state.type_count, vms_state.record_count);

	/* Cleanup */
	if (vms_state.module_name)
		free(vms_state.module_name);
	if (vms_state.source_file)
		free(vms_state.source_file);
}
