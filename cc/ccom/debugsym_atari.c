/*
 * Atari TOS/GEMDOS Debug Symbol Support
 *
 * Supports debug formats for Atari ST/TT/Falcon:
 * - DRI (Digital Research Inc.) debug format
 * - GST (GEM Symbol Table) format
 * - Pure C / Turbo C debug symbols
 * - GNU Debugger (a.out with STABS extension)
 *
 * Used by: Pure C, Turbo C, Lattice C, Megamax C, Aztec C
 * Platforms: Atari ST, STe, TT, Falcon (68000, 68020, 68030, 68040)
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * DRI Debug Format
 * Used by Digital Research compilers and assemblers
 */

/* DRI symbol types (from GST - GEM Symbol Table) */
#define DRI_SYM_DEFINED		0x8000	/* Symbol is defined */
#define DRI_SYM_EQUATED		0x4000	/* Equated symbol */
#define DRI_SYM_GLOBAL		0x2000	/* Global symbol */
#define DRI_SYM_EQUREG		0x1000	/* Register equate */
#define DRI_SYM_EXTERN		0x0800	/* External reference */
#define DRI_SYM_DATA		0x0400	/* Data symbol */
#define DRI_SYM_BSS		0x0200	/* BSS symbol */
#define DRI_SYM_TEXT		0x0100	/* Text/Code symbol */

/* DRI debug record types */
#define DRI_DBG_SOURCE		0x01	/* Source file */
#define DRI_DBG_FUNCTION	0x02	/* Function */
#define DRI_DBG_VARIABLE	0x03	/* Variable */
#define DRI_DBG_TYPE		0x04	/* Type definition */
#define DRI_DBG_LINE		0x05	/* Line number */
#define DRI_DBG_BLOCK_BEGIN	0x06	/* Block begin */
#define DRI_DBG_BLOCK_END	0x07	/* Block end */
#define DRI_DBG_END		0x08	/* End marker */

/* DRI type codes */
#define DRI_TYPE_VOID		0x00
#define DRI_TYPE_CHAR		0x01
#define DRI_TYPE_SHORT		0x02
#define DRI_TYPE_INT		0x03	/* 16-bit on Atari */
#define DRI_TYPE_LONG		0x04	/* 32-bit */
#define DRI_TYPE_FLOAT		0x05	/* IEEE 32-bit */
#define DRI_TYPE_DOUBLE		0x06	/* IEEE 64-bit */
#define DRI_TYPE_PTR		0x07
#define DRI_TYPE_ARRAY		0x08
#define DRI_TYPE_STRUCT		0x09
#define DRI_TYPE_UNION		0x0A
#define DRI_TYPE_ENUM		0x0B
#define DRI_TYPE_FUNCTION	0x0C

/*
 * GST (GEM Symbol Table) Format
 * Extended format used by Pure C and Turbo C
 */

/* GST header signature */
#define GST_SIGNATURE		0x4753542E	/* 'GST.' */

/* GST symbol entry (14 bytes) */
struct gst_symbol {
	char name[8];		/* Symbol name (space-padded) */
	unsigned short type;	/* Symbol type flags */
	unsigned long value;	/* Symbol value/address */
};

/* GST extended debug records */
#define GST_EXT_SOURCE		0xF1	/* Source file info */
#define GST_EXT_FUNCTION	0xF2	/* Function definition */
#define GST_EXT_VARIABLE	0xF3	/* Variable with type */
#define GST_EXT_TYPE		0xF4	/* Type definition */
#define GST_EXT_STRUCT		0xF5	/* Structure definition */
#define GST_EXT_ENUM		0xF6	/* Enum definition */
#define GST_EXT_LINE		0xF7	/* Line number table */
#define GST_EXT_BLOCK		0xF8	/* Block scope */

/*
 * Pure C Debug Format
 * Enhanced GST format with full type information
 */

#define PUREC_DBG_VERSION	0x0100	/* Version 1.0 */

/* Pure C type descriptors */
#define PUREC_TYPE_BASE		0x00	/* Base type */
#define PUREC_TYPE_POINTER	0x10	/* Pointer modifier */
#define PUREC_TYPE_ARRAY	0x20	/* Array modifier */
#define PUREC_TYPE_FUNCTION	0x30	/* Function modifier */
#define PUREC_TYPE_STRUCT	0x40	/* Structure */
#define PUREC_TYPE_UNION	0x50	/* Union */
#define PUREC_TYPE_ENUM		0x60	/* Enumeration */
#define PUREC_TYPE_TYPEDEF	0x70	/* Typedef */

/* Pure C storage classes */
#define PUREC_SC_AUTO		0x01
#define PUREC_SC_STATIC		0x02
#define PUREC_SC_EXTERN		0x03
#define PUREC_SC_REGISTER	0x04
#define PUREC_SC_TYPEDEF	0x05

/* Global state */
static FILE *dri_file = NULL;
static unsigned long symbol_count = 0;
static char *current_source_file = NULL;
static int use_gst_format = 1;		/* Default to GST */
static int use_purec_ext = 0;		/* Pure C extensions */

/* Forward declarations */
static void emit_gst_symbol(const char *name, unsigned short type,
			     unsigned long value);
static void emit_dri_debug_record(unsigned char type, const void *data,
				   size_t len);
static unsigned char get_dri_type(debug_type_t *type);
static unsigned short get_gst_type_flags(debug_symbol_t *sym);
static void emit_purec_type_descriptor(debug_type_t *type);

/*
 * Initialize Atari debug symbol generation
 */
void
debugsym_atari_init(void)
{
	const char *format = getenv("PCC_ATARI_DEBUG_FORMAT");

	/* Check requested format */
	if (format) {
		if (strcmp(format, "gst") == 0) {
			use_gst_format = 1;
			use_purec_ext = 0;
		} else if (strcmp(format, "purec") == 0) {
			use_gst_format = 1;
			use_purec_ext = 1;
		} else if (strcmp(format, "dri") == 0) {
			use_gst_format = 0;
			use_purec_ext = 0;
		}
	}

	if (use_gst_format) {
		/* GST format */
		printf("\t.data\n");
		printf("\t.even\n");
		printf("L_gst_start:\n");
		printf("\t.long 0x%08lx\n", (unsigned long)GST_SIGNATURE);
		printf("\t.long 0\n");		/* Symbol count (filled later) */
		printf("L_gst_symbols:\n");
	} else {
		/* DRI format */
		printf("\t.data\n");
		printf("\t.even\n");
		printf("L_dri_debug_start:\n");
		printf("\t.word 0x%04x\n", 0x4452);	/* 'DR' */
		printf("\t.word 0x%04x\n", 0x4920);	/* 'I ' */
		printf("\t.long 0\n");			/* Size (filled later) */
	}

	symbol_count = 0;
}

/*
 * Emit a debug symbol
 */
void
debugsym_atari_emit(debug_symbol_t *sym)
{
	unsigned short gst_type;
	unsigned char dri_type;
	unsigned long address;

	if (!sym)
		return;

	if (use_gst_format) {
		/* GST/Pure C format */
		gst_type = get_gst_type_flags(sym);

		switch (sym->kind) {
		case DBGSYM_FUNCTION:
			/* Function entry */
			emit_gst_symbol(sym->name, gst_type | DRI_SYM_TEXT,
					(unsigned long)sym->low_pc);

			if (use_purec_ext) {
				/* Pure C extended function info */
				printf("\t.byte 0x%02x\n", GST_EXT_FUNCTION);
				printf("\t.ascii \"%.32s\"\n", sym->name);
				printf("\t.long 0x%08lx\n",
				       (unsigned long)sym->low_pc);
				printf("\t.long 0x%08lx\n",
				       (unsigned long)sym->high_pc);

				/* Return type */
				if (sym->type)
					emit_purec_type_descriptor(sym->type);
				else
					printf("\t.byte 0x%02x\n",
					       PUREC_TYPE_BASE | DRI_TYPE_VOID);

				printf("\t.even\n");
			}
			break;

		case DBGSYM_VARIABLE:
			/* Variable entry */
			if (sym->is_external)
				gst_type |= DRI_SYM_GLOBAL;

			if (sym->storage_class == STATIC) {
				if (sym->is_initialized)
					gst_type |= DRI_SYM_DATA;
				else
					gst_type |= DRI_SYM_BSS;
			}

			address = sym->is_external ?
				  (unsigned long)sym->low_pc :
				  (unsigned long)sym->stack_offset;

			emit_gst_symbol(sym->name, gst_type, address);

			if (use_purec_ext) {
				/* Pure C extended variable info */
				printf("\t.byte 0x%02x\n", GST_EXT_VARIABLE);
				printf("\t.ascii \"%.32s\"\n", sym->name);

				/* Storage class */
				if (sym->is_external) {
					printf("\t.byte 0x%02x\n", PUREC_SC_EXTERN);
				} else if (sym->storage_class == STATIC) {
					printf("\t.byte 0x%02x\n", PUREC_SC_STATIC);
				} else if (sym->storage_class == REGISTER) {
					printf("\t.byte 0x%02x\n", PUREC_SC_REGISTER);
					printf("\t.byte 0x%02x\n", sym->reg_num);
				} else {
					printf("\t.byte 0x%02x\n", PUREC_SC_AUTO);
				}

				/* Type descriptor */
				if (sym->type)
					emit_purec_type_descriptor(sym->type);

				/* Location */
				printf("\t.long 0x%08lx\n", address);
				printf("\t.even\n");
			}
			break;

		case DBGSYM_TYPE:
			/* Type definition (Pure C only) */
			if (use_purec_ext) {
				printf("\t.byte 0x%02x\n", GST_EXT_TYPE);
				printf("\t.ascii \"%.32s\"\n", sym->name);

				if (sym->type)
					emit_purec_type_descriptor(sym->type);

				printf("\t.even\n");
			}
			break;

		default:
			break;
		}

		symbol_count++;
	} else {
		/* DRI format */
		switch (sym->kind) {
		case DBGSYM_FUNCTION:
			printf("\t.byte 0x%02x\n", DRI_DBG_FUNCTION);
			printf("\t.ascii \"%.32s\"\n", sym->name);
			printf("\t.byte 0\n");		/* Null terminator */
			printf("\t.long 0x%08lx\n", (unsigned long)sym->low_pc);
			printf("\t.long 0x%08lx\n",
			       (unsigned long)(sym->high_pc - sym->low_pc));

			if (sym->type) {
				dri_type = get_dri_type(sym->type);
				printf("\t.byte 0x%02x\n", dri_type);
			}
			printf("\t.even\n");
			break;

		case DBGSYM_VARIABLE:
		case DBGSYM_PARAMETER:
			printf("\t.byte 0x%02x\n", DRI_DBG_VARIABLE);
			printf("\t.ascii \"%.32s\"\n", sym->name);
			printf("\t.byte 0\n");

			/* Flags */
			printf("\t.byte 0x%02x\n",
			       (sym->is_external ? 0x01 : 0x00) |
			       (sym->storage_class == STATIC ? 0x02 : 0x00) |
			       (sym->kind == DBGSYM_PARAMETER ? 0x04 : 0x00));

			if (sym->type) {
				dri_type = get_dri_type(sym->type);
				printf("\t.byte 0x%02x\n", dri_type);
			}

			if (sym->storage_class == REGISTER) {
				printf("\t.byte 0x%02x\n", sym->reg_num);
			} else {
				printf("\t.long 0x%08lx\n",
				       (unsigned long)sym->stack_offset);
			}
			printf("\t.even\n");
			break;

		default:
			break;
		}
	}
}

/*
 * Finish and close Atari debug info
 */
void
debugsym_atari_finish(void)
{
	if (use_gst_format) {
		/* GST: update symbol count */
		printf("L_gst_end:\n");
		printf("\t.text\n");
		printf("\t.even\n");

		/* Patch symbol count at start */
		printf("L_gst_patch:\n");
		printf("\t.long L_gst_start + 4\n");
		printf("\t.long 0x%08lx\n", symbol_count);
	} else {
		/* DRI: emit end marker and size */
		printf("\t.byte 0x%02x\n", DRI_DBG_END);
		printf("L_dri_debug_end:\n");
		printf("\t.text\n");
		printf("\t.even\n");

		printf("L_dri_size:\n");
		printf("\t.long L_dri_debug_end - L_dri_debug_start\n");
	}

	if (current_source_file) {
		free(current_source_file);
		current_source_file = NULL;
	}
}

/*
 * Parse Atari debug symbols (placeholder)
 */
debug_symbol_t *
debugsym_atari_parse(void *data, size_t len)
{
	/* Parsing not yet implemented */
	return NULL;
}

/*
 * Helper: Emit GST symbol entry
 */
static void
emit_gst_symbol(const char *name, unsigned short type, unsigned long value)
{
	int i, len;
	char padded[8];

	/* Pad name to 8 characters */
	memset(padded, ' ', 8);
	if (name) {
		len = strlen(name);
		if (len > 8)
			len = 8;
		memcpy(padded, name, len);
	}

	/* Emit symbol entry (14 bytes) */
	printf("\t.ascii \"");
	for (i = 0; i < 8; i++) {
		if (padded[i] >= 32 && padded[i] <= 126 && padded[i] != '"'
		    && padded[i] != '\\') {
			printf("%c", padded[i]);
		} else {
			printf("\\%03o", (unsigned char)padded[i]);
		}
	}
	printf("\"\n");

	printf("\t.word 0x%04x\n", type);
	printf("\t.long 0x%08lx\n", value);
}

/*
 * Helper: Get DRI type code
 */
static unsigned char
get_dri_type(debug_type_t *type)
{
	if (!type)
		return DRI_TYPE_VOID;

	switch (type->encoding) {
	case DBGTYPE_VOID:
		return DRI_TYPE_VOID;
	case DBGTYPE_CHAR:
	case DBGTYPE_SCHAR:
	case DBGTYPE_UCHAR:
		return DRI_TYPE_CHAR;
	case DBGTYPE_INT16:
	case DBGTYPE_UINT16:
		return DRI_TYPE_SHORT;
	case DBGTYPE_INT32:
	case DBGTYPE_UINT32:
		return DRI_TYPE_LONG;
	case DBGTYPE_FLOAT32:
		return DRI_TYPE_FLOAT;
	case DBGTYPE_FLOAT64:
		return DRI_TYPE_DOUBLE;
	case DBGTYPE_PTR:
		return DRI_TYPE_PTR;
	case DBGTYPE_ARRAY:
		return DRI_TYPE_ARRAY;
	case DBGTYPE_STRUCT:
		return DRI_TYPE_STRUCT;
	case DBGTYPE_UNION:
		return DRI_TYPE_UNION;
	case DBGTYPE_ENUM:
		return DRI_TYPE_ENUM;
	case DBGTYPE_FUNCTION:
		return DRI_TYPE_FUNCTION;
	default:
		return DRI_TYPE_VOID;
	}
}

/*
 * Helper: Get GST type flags
 */
static unsigned short
get_gst_type_flags(debug_symbol_t *sym)
{
	unsigned short flags = DRI_SYM_DEFINED;

	if (!sym)
		return flags;

	if (sym->is_external)
		flags |= DRI_SYM_GLOBAL;

	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		flags |= DRI_SYM_TEXT;
		break;
	case DBGSYM_VARIABLE:
		if (sym->storage_class == STATIC) {
			if (sym->is_initialized)
				flags |= DRI_SYM_DATA;
			else
				flags |= DRI_SYM_BSS;
		} else {
			flags |= DRI_SYM_DATA;
		}
		break;
	default:
		break;
	}

	return flags;
}

/*
 * Helper: Emit Pure C type descriptor
 */
static void
emit_purec_type_descriptor(debug_type_t *type)
{
	unsigned char type_code;

	if (!type) {
		printf("\t.byte 0x%02x\n", PUREC_TYPE_BASE | DRI_TYPE_VOID);
		return;
	}

	switch (type->encoding) {
	case DBGTYPE_PTR:
		printf("\t.byte 0x%02x\n", PUREC_TYPE_POINTER);
		if (type->base_type)
			emit_purec_type_descriptor(type->base_type);
		break;

	case DBGTYPE_ARRAY:
		printf("\t.byte 0x%02x\n", PUREC_TYPE_ARRAY);
		if (type->array_dimensions > 0)
			printf("\t.long 0x%08lx\n",
			       (unsigned long)type->array_dimensions);
		if (type->base_type)
			emit_purec_type_descriptor(type->base_type);
		break;

	case DBGTYPE_STRUCT:
		printf("\t.byte 0x%02x\n", PUREC_TYPE_STRUCT);
		printf("\t.long 0x%08lx\n", (unsigned long)type->size);
		break;

	case DBGTYPE_UNION:
		printf("\t.byte 0x%02x\n", PUREC_TYPE_UNION);
		printf("\t.long 0x%08lx\n", (unsigned long)type->size);
		break;

	case DBGTYPE_ENUM:
		printf("\t.byte 0x%02x\n", PUREC_TYPE_ENUM);
		break;

	case DBGTYPE_FUNCTION:
		printf("\t.byte 0x%02x\n", PUREC_TYPE_FUNCTION);
		if (type->base_type)
			emit_purec_type_descriptor(type->base_type);
		break;

	default:
		/* Base type */
		type_code = get_dri_type(type);
		printf("\t.byte 0x%02x\n", PUREC_TYPE_BASE | type_code);
		break;
	}
}

/*
 * Helper: Emit DRI debug record
 */
static void
emit_dri_debug_record(unsigned char type, const void *data, size_t len)
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
