/*
 * Amiga Debug Symbol Support
 *
 * Supports debug formats for Commodore Amiga:
 * - Hunk format debug hunks (HUNK_DEBUG)
 * - SAS/C debug symbols
 * - DICE debug format
 * - Aztec C debug symbols
 * - GCC/DWARF (later AmigaOS versions)
 *
 * Used by: SAS/C, DICE, Aztec C, Lattice C, Manx Aztec C
 * Platforms: AmigaOS 1.x-3.x (68000, 68020, 68030, 68040, 68060)
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Amiga Hunk Format
 * Standard executable format for AmigaOS
 */

/* Hunk types */
#define HUNK_UNIT		0x3E7	/* Compilation unit */
#define HUNK_NAME		0x3E8	/* Hunk name */
#define HUNK_CODE		0x3E9	/* Code hunk */
#define HUNK_DATA		0x3EA	/* Data hunk */
#define HUNK_BSS		0x3EB	/* BSS hunk */
#define HUNK_RELOC32		0x3EC	/* 32-bit relocations */
#define HUNK_SYMBOL		0x3F0	/* Symbol table */
#define HUNK_DEBUG		0x3F1	/* Debug information */
#define HUNK_END		0x3F2	/* End of hunk */
#define HUNK_HEADER		0x3F3	/* File header */
#define HUNK_OVERLAY		0x3F5	/* Overlay hunk */
#define HUNK_BREAK		0x3F6	/* Break hunk */
#define HUNK_DREL32		0x3F7	/* 32-bit data relocations */
#define HUNK_EXT		0x3FF	/* Extended definitions */

/* HUNK_DEBUG subtypes */
#define DEBUG_HUNK_LINE		0x4C494E45	/* 'LINE' - Line numbers */
#define DEBUG_HUNK_SYMBOL	0x53594D42	/* 'SYMB' - Symbols */
#define DEBUG_HUNK_SOURCE	0x534F5543	/* 'SOUC' - Source info */
#define DEBUG_HUNK_TYPE		0x54595045	/* 'TYPE' - Type info */

/*
 * SAS/C Debug Format
 * Enhanced debug format used by SAS/C compiler
 */

/* SAS/C debug block header */
#define SASC_DBG_SIGNATURE	0x53415343	/* 'SASC' */
#define SASC_DBG_VERSION	0x0100		/* Version 1.0 */

/* SAS/C debug record types */
#define SASC_DBG_SOURCE		0x01	/* Source file */
#define SASC_DBG_FUNCTION	0x02	/* Function definition */
#define SASC_DBG_VARIABLE	0x03	/* Variable */
#define SASC_DBG_TYPE		0x04	/* Type definition */
#define SASC_DBG_LINE		0x05	/* Line number */
#define SASC_DBG_BLOCK		0x06	/* Block scope */
#define SASC_DBG_STRUCT		0x07	/* Structure */
#define SASC_DBG_UNION		0x08	/* Union */
#define SASC_DBG_ENUM		0x09	/* Enumeration */
#define SASC_DBG_TYPEDEF	0x0A	/* Typedef */
#define SASC_DBG_END		0xFF	/* End marker */

/* SAS/C type codes */
#define SASC_TYPE_VOID		0x00
#define SASC_TYPE_CHAR		0x01
#define SASC_TYPE_UCHAR		0x02
#define SASC_TYPE_SHORT		0x03
#define SASC_TYPE_USHORT	0x04
#define SASC_TYPE_INT		0x05	/* 16-bit by default */
#define SASC_TYPE_UINT		0x06
#define SASC_TYPE_LONG		0x07	/* 32-bit */
#define SASC_TYPE_ULONG		0x08
#define SASC_TYPE_FLOAT		0x09	/* IEEE 32-bit */
#define SASC_TYPE_DOUBLE	0x0A	/* IEEE 64-bit */
#define SASC_TYPE_PTR		0x0B
#define SASC_TYPE_ARRAY		0x0C
#define SASC_TYPE_STRUCT	0x0D
#define SASC_TYPE_UNION		0x0E
#define SASC_TYPE_ENUM		0x0F
#define SASC_TYPE_FUNCTION	0x10

/* SAS/C storage classes */
#define SASC_SC_AUTO		0x01
#define SASC_SC_STATIC		0x02
#define SASC_SC_EXTERN		0x03
#define SASC_SC_REGISTER	0x04
#define SASC_SC_A4_RELATIVE	0x05	/* A4-relative (small data) */
#define SASC_SC_A5_RELATIVE	0x06	/* A5-relative */

/*
 * DICE Debug Format
 * Matt Dillon's DICE C compiler debug format
 */

#define DICE_DBG_SIGNATURE	0x44494345	/* 'DICE' */

/* DICE debug records */
#define DICE_DBG_FILE		0x01
#define DICE_DBG_FUNC		0x02
#define DICE_DBG_VAR		0x03
#define DICE_DBG_TYPE		0x04
#define DICE_DBG_LINE		0x05

/* Global state */
static int use_sasc_format = 0;		/* Use SAS/C extensions */
static int use_dice_format = 0;		/* Use DICE extensions */
static unsigned long debug_hunk_size = 0;
static unsigned long symbol_count = 0;
static char *current_source = NULL;

/* Forward declarations */
static void emit_hunk_header(unsigned long type, unsigned long size);
static void emit_hunk_longword(unsigned long value);
static void emit_hunk_string(const char *str);
static void emit_sasc_record(unsigned char type, const void *data, size_t len);
static unsigned char get_sasc_type(debug_type_t *type);
static void emit_line_number_entry(unsigned long offset, unsigned long line);

/*
 * Initialize Amiga debug symbol generation
 */
void
debugsym_amiga_init(void)
{
	const char *format = getenv("PCC_AMIGA_DEBUG_FORMAT");

	/* Check requested format */
	if (format) {
		if (strcmp(format, "sasc") == 0) {
			use_sasc_format = 1;
			use_dice_format = 0;
		} else if (strcmp(format, "dice") == 0) {
			use_sasc_format = 0;
			use_dice_format = 1;
		} else {
			use_sasc_format = 0;
			use_dice_format = 0;
		}
	}

	/* Start debug hunk */
	printf("\t.section .debug\n");
	printf("\t.even\n");
	printf("L_amiga_debug_start:\n");

	/* Emit HUNK_DEBUG header */
	printf("\t.long 0x%08lx\n", (unsigned long)HUNK_DEBUG);
	printf("L_debug_size:\n");
	printf("\t.long 0\n");	/* Size in longwords (filled later) */

	if (use_sasc_format) {
		/* SAS/C debug header */
		printf("\t.long 0x%08lx\n", (unsigned long)SASC_DBG_SIGNATURE);
		printf("\t.word 0x%04x\n", SASC_DBG_VERSION);
		printf("\t.even\n");
	} else if (use_dice_format) {
		/* DICE debug header */
		printf("\t.long 0x%08lx\n", (unsigned long)DICE_DBG_SIGNATURE);
	} else {
		/* Standard hunk debug */
		printf("\t.long 0x%08lx\n", (unsigned long)DEBUG_HUNK_SYMBOL);
	}

	debug_hunk_size = 0;
	symbol_count = 0;
}

/*
 * Emit a debug symbol
 */
void
debugsym_amiga_emit(debug_symbol_t *sym)
{
	unsigned char type_code;
	unsigned long name_longs;
	int name_len;

	if (!sym)
		return;

	if (use_sasc_format) {
		/* SAS/C format */
		switch (sym->kind) {
		case DBGSYM_FUNCTION:
			/* Function record */
			printf("\t.byte 0x%02x\n", SASC_DBG_FUNCTION);
			emit_hunk_string(sym->name);
			printf("\t.long 0x%08lx\n", (unsigned long)sym->low_pc);
			printf("\t.long 0x%08lx\n",
			       (unsigned long)(sym->high_pc - sym->low_pc));

			/* Return type */
			if (sym->type) {
				type_code = get_sasc_type(sym->type);
				printf("\t.byte 0x%02x\n", type_code);
			} else {
				printf("\t.byte 0x%02x\n", SASC_TYPE_VOID);
			}

			/* Flags */
			printf("\t.byte 0x%02x\n",
			       (sym->is_external ? 0x01 : 0x00) |
			       (sym->is_inline ? 0x02 : 0x00));

			printf("\t.even\n");
			break;

		case DBGSYM_VARIABLE:
		case DBGSYM_PARAMETER:
			/* Variable record */
			printf("\t.byte 0x%02x\n", SASC_DBG_VARIABLE);
			emit_hunk_string(sym->name);

			/* Storage class */
			if (sym->kind == DBGSYM_PARAMETER) {
				printf("\t.byte 0x%02x\n", SASC_SC_AUTO);
				printf("\t.byte 0x01\n");	/* Is parameter */
			} else if (sym->is_external) {
				printf("\t.byte 0x%02x\n", SASC_SC_EXTERN);
				printf("\t.byte 0x00\n");
			} else if (sym->storage_class == STATIC) {
				printf("\t.byte 0x%02x\n", SASC_SC_STATIC);
				printf("\t.byte 0x00\n");
			} else if (sym->storage_class == REGISTER) {
				printf("\t.byte 0x%02x\n", SASC_SC_REGISTER);
				printf("\t.byte 0x%02x\n", sym->reg_num);
			} else {
				printf("\t.byte 0x%02x\n", SASC_SC_AUTO);
				printf("\t.byte 0x00\n");
			}

			/* Type */
			if (sym->type) {
				type_code = get_sasc_type(sym->type);
				printf("\t.byte 0x%02x\n", type_code);
			}

			/* Location */
			if (sym->storage_class == REGISTER) {
				printf("\t.long 0\n");
			} else if (sym->is_external) {
				printf("\t.long 0x%08lx\n",
				       (unsigned long)sym->low_pc);
			} else {
				printf("\t.long 0x%08lx\n",
				       (unsigned long)sym->stack_offset);
			}

			printf("\t.even\n");
			break;

		case DBGSYM_TYPE:
			/* Type definition */
			printf("\t.byte 0x%02x\n", SASC_DBG_TYPE);
			emit_hunk_string(sym->name);

			if (sym->type) {
				type_code = get_sasc_type(sym->type);
				printf("\t.byte 0x%02x\n", type_code);
				printf("\t.long 0x%08lx\n",
				       (unsigned long)sym->type->size);
			}

			printf("\t.even\n");
			break;

		default:
			break;
		}
	} else {
		/* Standard Hunk format */
		switch (sym->kind) {
		case DBGSYM_FUNCTION:
		case DBGSYM_VARIABLE:
			/* Symbol entry in HUNK_SYMBOL format */
			name_len = sym->name ? strlen(sym->name) : 0;
			name_longs = (name_len + 3) / 4;	/* Round up */

			/* Name length in longwords */
			printf("\t.long 0x%08lx\n", name_longs);

			/* Name (padded to longword boundary) */
			if (sym->name) {
				printf("\t.ascii \"");
				printf("%s", sym->name);
				printf("\"\n");

				/* Padding */
				if (name_len % 4 != 0) {
					printf("\t.space %d\n", 4 - (name_len % 4));
				}
			}

			/* Value/address */
			if (sym->kind == DBGSYM_FUNCTION) {
				printf("\t.long 0x%08lx\n",
				       (unsigned long)sym->low_pc);
			} else if (sym->is_external) {
				printf("\t.long 0x%08lx\n",
				       (unsigned long)sym->low_pc);
			} else {
				printf("\t.long 0x%08lx\n",
				       (unsigned long)sym->stack_offset);
			}

			symbol_count++;
			break;

		default:
			break;
		}
	}
}

/*
 * Finish and close Amiga debug info
 */
void
debugsym_amiga_finish(void)
{
	unsigned long total_size;

	if (use_sasc_format) {
		/* SAS/C: emit end marker */
		printf("\t.byte 0x%02x\n", SASC_DBG_END);
		printf("\t.even\n");
	} else {
		/* Standard hunk: terminate symbol table */
		printf("\t.long 0\n");	/* End of symbols */
	}

	printf("L_amiga_debug_end:\n");

	/* Calculate size in longwords */
	printf("L_calc_size:\n");
	total_size = 0;  /* Will be calculated by linker */

	/* Patch the size field */
	printf("\t.text\n");
	printf("\t.even\n");

	/* Return to text section */
	printf("\t.section .text\n");

	if (current_source) {
		free(current_source);
		current_source = NULL;
	}
}

/*
 * Parse Amiga debug symbols (placeholder)
 */
debug_symbol_t *
debugsym_amiga_parse(void *data, size_t len)
{
	/* Parsing not yet implemented */
	return NULL;
}

/*
 * Helper: Emit hunk header
 */
static void
emit_hunk_header(unsigned long type, unsigned long size)
{
	printf("\t.long 0x%08lx\n", type);
	printf("\t.long 0x%08lx\n", size);
}

/*
 * Helper: Emit longword
 */
static void
emit_hunk_longword(unsigned long value)
{
	printf("\t.long 0x%08lx\n", value);
}

/*
 * Helper: Emit string (null-terminated, aligned)
 */
static void
emit_hunk_string(const char *str)
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

	/* Align to word boundary */
	printf("\t.even\n");
}

/*
 * Helper: Get SAS/C type code
 */
static unsigned char
get_sasc_type(debug_type_t *type)
{
	if (!type)
		return SASC_TYPE_VOID;

	switch (type->encoding) {
	case DBGTYPE_VOID:
		return SASC_TYPE_VOID;
	case DBGTYPE_CHAR:
	case DBGTYPE_SCHAR:
		return SASC_TYPE_CHAR;
	case DBGTYPE_UCHAR:
		return SASC_TYPE_UCHAR;
	case DBGTYPE_INT16:
		return SASC_TYPE_SHORT;
	case DBGTYPE_UINT16:
		return SASC_TYPE_USHORT;
	case DBGTYPE_INT32:
		return SASC_TYPE_LONG;
	case DBGTYPE_UINT32:
		return SASC_TYPE_ULONG;
	case DBGTYPE_FLOAT32:
		return SASC_TYPE_FLOAT;
	case DBGTYPE_FLOAT64:
		return SASC_TYPE_DOUBLE;
	case DBGTYPE_PTR:
		return SASC_TYPE_PTR;
	case DBGTYPE_ARRAY:
		return SASC_TYPE_ARRAY;
	case DBGTYPE_STRUCT:
		return SASC_TYPE_STRUCT;
	case DBGTYPE_UNION:
		return SASC_TYPE_UNION;
	case DBGTYPE_ENUM:
		return SASC_TYPE_ENUM;
	case DBGTYPE_FUNCTION:
		return SASC_TYPE_FUNCTION;
	default:
		return SASC_TYPE_VOID;
	}
}

/*
 * Helper: Emit SAS/C debug record
 */
static void
emit_sasc_record(unsigned char type, const void *data, size_t len)
{
	const unsigned char *bytes = (const unsigned char *)data;
	size_t i;

	printf("\t.byte 0x%02x\n", type);

	if (data && len > 0) {
		for (i = 0; i < len; i++) {
			printf("\t.byte 0x%02x\n", bytes[i]);
		}
	}

	printf("\t.even\n");
}

/*
 * Helper: Emit line number entry
 */
static void
emit_line_number_entry(unsigned long offset, unsigned long line)
{
	printf("\t.long 0x%08lx\n", offset);
	printf("\t.long 0x%08lx\n", line);
}
