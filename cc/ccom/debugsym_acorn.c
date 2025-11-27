/*
 * Acorn Debug Symbol Support
 *
 * Supports debug formats for Acorn/ARM systems:
 * - AOF (ARM Object Format) debug areas
 * - AIF (ARM Image Format) debug tables
 * - Desktop Debug Table (DDT) format
 * - Acorn C/C++ debug symbols
 *
 * Used by: Acorn C, Acorn C++, Norcroft C, ARM SDT
 * Platforms: Archimedes, RISC PC, RISC OS (ARM2, ARM3, ARM6, ARM7, StrongARM)
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * AOF (ARM Object Format)
 * Standard object file format for ARM Development tools
 */

/* AOF chunk identifiers */
#define AOF_HEADER		0x4F424A5F	/* 'OBJ_' - Object header */
#define AOF_AREA		0x41524541	/* 'AREA' - Area */
#define AOF_IDFN		0x4944464E	/* 'IDFN' - Identification */
#define AOF_SYMT		0x53594D54	/* 'SYMT' - Symbol table */
#define AOF_STRT		0x53545254	/* 'STRT' - String table */
#define AOF_RELOC		0x52454C4F	/* 'RELO' - Relocations */
#define AOF_DEBUG		0x44425547	/* 'DBUG' - Debug info */

/* AOF area attributes */
#define AOF_ATTR_CODE		0x00000001	/* Code area */
#define AOF_ATTR_DATA		0x00000002	/* Data area */
#define AOF_ATTR_READONLY	0x00000004	/* Read-only */
#define AOF_ATTR_DEBUG		0x00000008	/* Debug area */
#define AOF_ATTR_ZEROINIT	0x00000010	/* Zero initialized */

/* AOF symbol types */
#define AOF_SYM_LOCAL		0x00	/* Local symbol */
#define AOF_SYM_GLOBAL		0x01	/* Global symbol */
#define AOF_SYM_ABSOLUTE	0x02	/* Absolute symbol */
#define AOF_SYM_EXTERNAL	0x03	/* External reference */

/*
 * AIF (ARM Image Format)
 * Executable format for RISC OS
 */

/* AIF header */
#define AIF_SIGNATURE		0xE1A00000	/* NOP instruction (MOV r0,r0) */
#define AIF_BRANCH		0xEA000000	/* Branch instruction */

/* AIF debug table types */
#define AIF_DBG_SOURCE		0x01	/* Source file info */
#define AIF_DBG_LINE		0x02	/* Line number table */
#define AIF_DBG_SYMBOL		0x03	/* Symbol table */
#define AIF_DBG_TYPE		0x04	/* Type information */
#define AIF_DBG_FUNCTION	0x05	/* Function table */
#define AIF_DBG_VARIABLE	0x06	/* Variable table */

/*
 * Desktop Debug Table (DDT)
 * Debug format used by Acorn debuggers
 */

/* DDT header */
#define DDT_SIGNATURE		0x44445400	/* 'DDT\0' */
#define DDT_VERSION		0x0100		/* Version 1.0 */

/* DDT record types */
#define DDT_REC_SOURCE		0x01	/* Source file */
#define DDT_REC_FUNCTION	0x02	/* Function entry */
#define DDT_REC_VARIABLE	0x03	/* Variable */
#define DDT_REC_TYPE		0x04	/* Type definition */
#define DDT_REC_LINE		0x05	/* Line number */
#define DDT_REC_BLOCK		0x06	/* Block scope */
#define DDT_REC_STRUCT		0x07	/* Structure */
#define DDT_REC_UNION		0x08	/* Union */
#define DDT_REC_ENUM		0x09	/* Enumeration */
#define DDT_REC_ARRAY		0x0A	/* Array type */
#define DDT_REC_POINTER		0x0B	/* Pointer type */
#define DDT_REC_PROCEDURE	0x0C	/* Procedure type */
#define DDT_REC_END		0xFF	/* End marker */

/* DDT type encodings */
#define DDT_TYPE_VOID		0x00
#define DDT_TYPE_CHAR		0x01
#define DDT_TYPE_UCHAR		0x02
#define DDT_TYPE_SHORT		0x03
#define DDT_TYPE_USHORT		0x04
#define DDT_TYPE_INT		0x05	/* 32-bit on ARM */
#define DDT_TYPE_UINT		0x06
#define DDT_TYPE_LONG		0x07
#define DDT_TYPE_ULONG		0x08
#define DDT_TYPE_LONGLONG	0x09	/* 64-bit */
#define DDT_TYPE_ULONGLONG	0x0A
#define DDT_TYPE_FLOAT		0x0B	/* 32-bit IEEE */
#define DDT_TYPE_DOUBLE		0x0C	/* 64-bit IEEE */
#define DDT_TYPE_LDOUBLE	0x0D	/* 80-bit extended */
#define DDT_TYPE_PTR		0x0E
#define DDT_TYPE_ARRAY		0x0F
#define DDT_TYPE_STRUCT		0x10
#define DDT_TYPE_UNION		0x11
#define DDT_TYPE_ENUM		0x12
#define DDT_TYPE_FUNCTION	0x13

/* DDT storage classes */
#define DDT_SC_AUTO		0x01	/* Automatic (stack) */
#define DDT_SC_STATIC		0x02	/* Static */
#define DDT_SC_EXTERN		0x03	/* External */
#define DDT_SC_REGISTER		0x04	/* Register */

/* ARM register numbers for debug */
#define ARM_REG_R0		0
#define ARM_REG_R1		1
#define ARM_REG_R2		2
#define ARM_REG_R3		3
#define ARM_REG_R4		4
#define ARM_REG_R5		5
#define ARM_REG_R6		6
#define ARM_REG_R7		7
#define ARM_REG_R8		8
#define ARM_REG_R9		9
#define ARM_REG_R10		10
#define ARM_REG_R11		11	/* FP */
#define ARM_REG_R12		12	/* IP */
#define ARM_REG_R13		13	/* SP */
#define ARM_REG_R14		14	/* LR */
#define ARM_REG_R15		15	/* PC */

/* Global state */
static int use_ddt_format = 1;		/* Use DDT format (default) */
static int use_aif_format = 0;		/* Use AIF format */
static unsigned long debug_area_size = 0;
static unsigned long symbol_count = 0;
static char *current_source = NULL;

/* Forward declarations */
static void emit_aof_chunk(unsigned long type, unsigned long size);
static void emit_ddt_record(unsigned char type, const void *data, size_t len);
static unsigned char get_ddt_type(debug_type_t *type);
static void emit_aof_string(const char *str);
static unsigned long encode_arm_register(int reg_num);

/*
 * Initialize Acorn debug symbol generation
 */
void
debugsym_acorn_init(void)
{
	const char *format = getenv("PCC_ACORN_DEBUG_FORMAT");

	/* Check requested format */
	if (format) {
		if (strcmp(format, "ddt") == 0) {
			use_ddt_format = 1;
			use_aif_format = 0;
		} else if (strcmp(format, "aif") == 0) {
			use_ddt_format = 0;
			use_aif_format = 1;
		} else if (strcmp(format, "aof") == 0) {
			use_ddt_format = 0;
			use_aif_format = 0;
		}
	}

	/* Start debug area */
	printf("\t.section .debug\n");
	printf("\t.align 2\n");
	printf("L_acorn_debug_start:\n");

	if (use_ddt_format) {
		/* DDT format */
		printf("\t.long 0x%08lx\n", (unsigned long)DDT_SIGNATURE);
		printf("\t.word 0x%04x\n", DDT_VERSION);
		printf("\t.align 2\n");
	} else if (use_aif_format) {
		/* AIF debug table */
		printf("\t.long 0x%08lx\n", (unsigned long)AIF_DBG_SYMBOL);
		printf("\t.long 0\n");	/* Size (filled later) */
	} else {
		/* AOF debug chunk */
		printf("\t.long 0x%08lx\n", (unsigned long)AOF_DEBUG);
		printf("\t.long 0\n");	/* Size (filled later) */
	}

	debug_area_size = 0;
	symbol_count = 0;
}

/*
 * Emit a debug symbol
 */
void
debugsym_acorn_emit(debug_symbol_t *sym)
{
	unsigned char type_code;
	unsigned long reg_encoding;

	if (!sym)
		return;

	if (use_ddt_format) {
		/* DDT format */
		switch (sym->kind) {
		case DBGSYM_FUNCTION:
			/* Function record */
			printf("\t.byte 0x%02x\n", DDT_REC_FUNCTION);
			emit_aof_string(sym->name);
			printf("\t.long 0x%08lx\n", (unsigned long)sym->low_pc);
			printf("\t.long 0x%08lx\n",
			       (unsigned long)(sym->high_pc - sym->low_pc));

			/* Return type */
			if (sym->type) {
				type_code = get_ddt_type(sym->type);
				printf("\t.byte 0x%02x\n", type_code);
			} else {
				printf("\t.byte 0x%02x\n", DDT_TYPE_VOID);
			}

			/* Function attributes */
			printf("\t.byte 0x%02x\n",
			       (sym->is_external ? 0x01 : 0x00) |
			       (sym->is_inline ? 0x02 : 0x00) |
			       (sym->is_static ? 0x04 : 0x00));

			printf("\t.align 2\n");
			break;

		case DBGSYM_VARIABLE:
		case DBGSYM_PARAMETER:
			/* Variable record */
			printf("\t.byte 0x%02x\n", DDT_REC_VARIABLE);
			emit_aof_string(sym->name);

			/* Storage class */
			if (sym->kind == DBGSYM_PARAMETER) {
				printf("\t.byte 0x%02x\n", DDT_SC_AUTO);
				printf("\t.byte 0x01\n");	/* Is parameter flag */
			} else if (sym->is_external) {
				printf("\t.byte 0x%02x\n", DDT_SC_EXTERN);
				printf("\t.byte 0x00\n");
			} else if (sym->storage_class == STATIC) {
				printf("\t.byte 0x%02x\n", DDT_SC_STATIC);
				printf("\t.byte 0x00\n");
			} else if (sym->storage_class == REGISTER) {
				printf("\t.byte 0x%02x\n", DDT_SC_REGISTER);

				/* ARM register encoding */
				reg_encoding = encode_arm_register(sym->reg_num);
				printf("\t.byte 0x%02lx\n", reg_encoding);
			} else {
				printf("\t.byte 0x%02x\n", DDT_SC_AUTO);
				printf("\t.byte 0x00\n");
			}

			/* Type */
			if (sym->type) {
				type_code = get_ddt_type(sym->type);
				printf("\t.byte 0x%02x\n", type_code);
			}

			/* Location */
			if (sym->storage_class == REGISTER) {
				printf("\t.long 0\n");
			} else if (sym->is_external) {
				printf("\t.long 0x%08lx\n",
				       (unsigned long)sym->low_pc);
			} else {
				/* Stack offset (relative to FP/R11) */
				printf("\t.long 0x%08lx\n",
				       (unsigned long)sym->stack_offset);
			}

			printf("\t.align 2\n");
			break;

		case DBGSYM_TYPE:
			/* Type definition */
			printf("\t.byte 0x%02x\n", DDT_REC_TYPE);
			emit_aof_string(sym->name);

			if (sym->type) {
				type_code = get_ddt_type(sym->type);
				printf("\t.byte 0x%02x\n", type_code);
				printf("\t.long 0x%08lx\n",
				       (unsigned long)sym->type->size);
			}

			printf("\t.align 2\n");
			break;

		default:
			break;
		}
	} else if (use_aif_format) {
		/* AIF format */
		switch (sym->kind) {
		case DBGSYM_FUNCTION:
			printf("\t.byte 0x%02x\n", AIF_DBG_FUNCTION);
			emit_aof_string(sym->name);
			printf("\t.long 0x%08lx\n", (unsigned long)sym->low_pc);
			printf("\t.long 0x%08lx\n",
			       (unsigned long)(sym->high_pc - sym->low_pc));
			printf("\t.align 2\n");
			break;

		case DBGSYM_VARIABLE:
			printf("\t.byte 0x%02x\n", AIF_DBG_VARIABLE);
			emit_aof_string(sym->name);
			printf("\t.byte 0x%02x\n",
			       (sym->is_external ? AOF_SYM_GLOBAL :
				AOF_SYM_LOCAL));
			printf("\t.long 0x%08lx\n",
			       sym->is_external ?
			       (unsigned long)sym->low_pc :
			       (unsigned long)sym->stack_offset);
			printf("\t.align 2\n");
			break;

		default:
			break;
		}
	} else {
		/* AOF format */
		switch (sym->kind) {
		case DBGSYM_FUNCTION:
		case DBGSYM_VARIABLE:
			/* Symbol table entry */
			emit_aof_string(sym->name);

			/* Symbol type */
			printf("\t.byte 0x%02x\n",
			       sym->is_external ? AOF_SYM_GLOBAL : AOF_SYM_LOCAL);

			/* Area number (0 for code, 1 for data) */
			printf("\t.byte 0x%02x\n",
			       (sym->kind == DBGSYM_FUNCTION) ? 0x00 : 0x01);

			/* Symbol value */
			printf("\t.long 0x%08lx\n",
			       (unsigned long)sym->low_pc);

			printf("\t.align 2\n");
			symbol_count++;
			break;

		default:
			break;
		}
	}
}

/*
 * Finish and close Acorn debug info
 */
void
debugsym_acorn_finish(void)
{
	if (use_ddt_format) {
		/* DDT: emit end marker */
		printf("\t.byte 0x%02x\n", DDT_REC_END);
		printf("\t.align 2\n");
	} else if (use_aif_format) {
		/* AIF: end of debug table */
		printf("\t.long 0\n");	/* End marker */
	} else {
		/* AOF: end of debug chunk */
		printf("\t.long 0\n");	/* End marker */
	}

	printf("L_acorn_debug_end:\n");

	/* Calculate size */
	printf("L_debug_size_calc:\n");
	printf("\t.long L_acorn_debug_end - L_acorn_debug_start\n");

	/* Return to text section */
	printf("\t.section .text\n");
	printf("\t.align 2\n");

	if (current_source) {
		free(current_source);
		current_source = NULL;
	}
}

/*
 * Parse Acorn debug symbols (placeholder)
 */
debug_symbol_t *
debugsym_acorn_parse(void *data, size_t len)
{
	/* Parsing not yet implemented */
	return NULL;
}

/*
 * Helper: Emit AOF chunk header
 */
static void
emit_aof_chunk(unsigned long type, unsigned long size)
{
	printf("\t.long 0x%08lx\n", type);
	printf("\t.long 0x%08lx\n", size);
}

/*
 * Helper: Emit AOF/AIF string (null-terminated, aligned)
 */
static void
emit_aof_string(const char *str)
{
	int len = str ? strlen(str) : 0;
	int i;

	if (len > 0) {
		printf("\t.ascii \"");
		for (i = 0; i < len; i++) {
			if (str[i] == '"' || str[i] == '\\') {
				printf("\\%c", str[i]);
			} else if (str[i] >= 32 && str[i] <= 126) {
				printf("%c", str[i]);
			} else {
				printf("\\%03o", (unsigned char)str[i]);
			}
		}
		printf("\"\n");
	}

	/* Null terminator */
	printf("\t.byte 0\n");

	/* Align to word boundary (4 bytes on ARM) */
	printf("\t.align 2\n");
}

/*
 * Helper: Get DDT type code
 */
static unsigned char
get_ddt_type(debug_type_t *type)
{
	if (!type)
		return DDT_TYPE_VOID;

	switch (type->encoding) {
	case DBGTYPE_VOID:
		return DDT_TYPE_VOID;
	case DBGTYPE_CHAR:
	case DBGTYPE_SCHAR:
		return DDT_TYPE_CHAR;
	case DBGTYPE_UCHAR:
		return DDT_TYPE_UCHAR;
	case DBGTYPE_INT16:
		return DDT_TYPE_SHORT;
	case DBGTYPE_UINT16:
		return DDT_TYPE_USHORT;
	case DBGTYPE_INT32:
		return DDT_TYPE_INT;
	case DBGTYPE_UINT32:
		return DDT_TYPE_UINT;
	case DBGTYPE_INT64:
		return DDT_TYPE_LONGLONG;
	case DBGTYPE_UINT64:
		return DDT_TYPE_ULONGLONG;
	case DBGTYPE_FLOAT32:
		return DDT_TYPE_FLOAT;
	case DBGTYPE_FLOAT64:
		return DDT_TYPE_DOUBLE;
	case DBGTYPE_FLOAT80:
		return DDT_TYPE_LDOUBLE;
	case DBGTYPE_PTR:
		return DDT_TYPE_PTR;
	case DBGTYPE_ARRAY:
		return DDT_TYPE_ARRAY;
	case DBGTYPE_STRUCT:
		return DDT_TYPE_STRUCT;
	case DBGTYPE_UNION:
		return DDT_TYPE_UNION;
	case DBGTYPE_ENUM:
		return DDT_TYPE_ENUM;
	case DBGTYPE_FUNCTION:
		return DDT_TYPE_FUNCTION;
	default:
		return DDT_TYPE_VOID;
	}
}

/*
 * Helper: Emit DDT debug record
 */
static void
emit_ddt_record(unsigned char type, const void *data, size_t len)
{
	const unsigned char *bytes = (const unsigned char *)data;
	size_t i;

	printf("\t.byte 0x%02x\n", type);

	if (data && len > 0) {
		for (i = 0; i < len; i++) {
			printf("\t.byte 0x%02x\n", bytes[i]);
		}
	}

	printf("\t.align 2\n");
}

/*
 * Helper: Encode ARM register number
 */
static unsigned long
encode_arm_register(int reg_num)
{
	/* Map PCC register numbers to ARM registers */
	/* This is architecture-specific and may need adjustment */

	if (reg_num >= 0 && reg_num <= 15)
		return (unsigned long)reg_num;

	/* Default to R0 for invalid register */
	return ARM_REG_R0;
}
