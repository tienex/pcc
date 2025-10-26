/*
 * Symbian OS Debug Symbol Support
 *
 * Symbian OS was the dominant smartphone operating system from 2000-2010,
 * with over 50% market share at its peak. It powered millions of devices
 * from Nokia, Sony Ericsson, Samsung, Motorola, and others.
 *
 * This implementation supports the Symbian debug symbol format used by:
 * - EPOC32 (Symbian OS kernel)
 * - Carbide.c++ IDE
 * - CodeWarrior for Symbian
 * - MetroTRK debugger
 * - Lauterbach TRACE32
 *
 * Format: E32Image format with embedded debug symbols
 * Platforms: Symbian OS 6.0 - 9.5, S60, UIQ (2000-2012)
 * Architectures: ARM (primarily ARMv5, ARMv6, ARMv7)
 */

#include "pass1.h"
#include "debugsym.h"
#include <string.h>
#include <stdlib.h>

/*
 * Symbian E32Image Magic Numbers
 */
#define E32_IMAGE_SIGNATURE	0x434F5045	/* 'EPOC' */
#define E32_IMAGE_VERSION	0x00010000	/* Version 1.0 */

/*
 * Symbian Debug Record Types
 */
#define SYM_DBG_HEADER		0x01	/* Debug info header */
#define SYM_DBG_MODULE		0x02	/* Module/DLL info */
#define SYM_DBG_FUNCTION	0x03	/* Function definition */
#define SYM_DBG_LOCAL		0x04	/* Local variable */
#define SYM_DBG_GLOBAL		0x05	/* Global variable */
#define SYM_DBG_CLASS		0x06	/* C++ class */
#define SYM_DBG_LINE		0x07	/* Line number table */
#define SYM_DBG_FILE		0x08	/* Source file */
#define SYM_DBG_TYPE		0x09	/* Type definition */

/*
 * Symbian Type Encodings
 */
#define SYM_TYPE_VOID		0x00
#define SYM_TYPE_CHAR		0x01
#define SYM_TYPE_SHORT		0x02
#define SYM_TYPE_INT		0x03
#define SYM_TYPE_LONG		0x04
#define SYM_TYPE_FLOAT		0x05
#define SYM_TYPE_DOUBLE		0x06
#define SYM_TYPE_POINTER	0x07
#define SYM_TYPE_ARRAY		0x08
#define SYM_TYPE_STRUCT		0x09
#define SYM_TYPE_UNION		0x0A
#define SYM_TYPE_CLASS		0x0B	/* C++ class */
#define SYM_TYPE_ENUM		0x0C
#define SYM_TYPE_FUNCTION	0x0D
#define SYM_TYPE_REFERENCE	0x0E	/* C++ reference */

/*
 * Symbian Symbol Flags
 */
#define SYM_FLAG_STATIC		0x01
#define SYM_FLAG_EXTERN		0x02
#define SYM_FLAG_VIRTUAL	0x04	/* C++ virtual function */
#define SYM_FLAG_CONST		0x08
#define SYM_FLAG_VOLATILE	0x10
#define SYM_FLAG_INLINE		0x20

/*
 * Symbian Debug Symbol State
 */
typedef struct {
	FILE *debug_file;		/* Debug output file (.sym) */
	unsigned int string_table_offset;
	unsigned int type_index;
	unsigned int line_count;
	char *current_file;
	int in_class;			/* C++ class context */
} symbian_state_t;

static symbian_state_t sym_state;

/*
 * Write Symbian 32-bit value (little-endian)
 */
static void
sym_write_u32(unsigned int value)
{
	fputc(value & 0xFF, sym_state.debug_file);
	fputc((value >> 8) & 0xFF, sym_state.debug_file);
	fputc((value >> 16) & 0xFF, sym_state.debug_file);
	fputc((value >> 24) & 0xFF, sym_state.debug_file);
}

/*
 * Write Symbian 16-bit value (little-endian)
 */
static void
sym_write_u16(unsigned short value)
{
	fputc(value & 0xFF, sym_state.debug_file);
	fputc((value >> 8) & 0xFF, sym_state.debug_file);
}

/*
 * Write Symbian string (null-terminated)
 */
static void
sym_write_string(const char *str)
{
	if (!str) {
		fputc(0, sym_state.debug_file);
		return;
	}

	fwrite(str, 1, strlen(str) + 1, sym_state.debug_file);
}

/*
 * Write Symbian descriptor string (16-bit length + data)
 */
static void
sym_write_descriptor(const char *str)
{
	unsigned short len;

	if (!str) {
		sym_write_u16(0);
		return;
	}

	len = strlen(str);
	sym_write_u16(len);
	fwrite(str, 1, len, sym_state.debug_file);
}

/*
 * Get Symbian type encoding
 */
static unsigned char
sym_get_type(debug_type_t *type)
{
	if (!type)
		return SYM_TYPE_VOID;

	switch (type->encoding) {
	case DBGTYPE_VOID:
		return SYM_TYPE_VOID;

	case DBGTYPE_CHAR:
	case DBGTYPE_INT8:
	case DBGTYPE_UINT8:
	case DBGTYPE_BOOL:
		return SYM_TYPE_CHAR;

	case DBGTYPE_INT16:
	case DBGTYPE_UINT16:
		return SYM_TYPE_SHORT;

	case DBGTYPE_INT32:
	case DBGTYPE_UINT32:
		return SYM_TYPE_INT;

	case DBGTYPE_INT64:
	case DBGTYPE_UINT64:
		return SYM_TYPE_LONG;

	case DBGTYPE_FLOAT32:
		return SYM_TYPE_FLOAT;

	case DBGTYPE_FLOAT64:
		return SYM_TYPE_DOUBLE;

	case DBGTYPE_POINTER:
		return SYM_TYPE_POINTER;

	case DBGTYPE_ARRAY:
		return SYM_TYPE_ARRAY;

	case DBGTYPE_STRUCT:
		return SYM_TYPE_STRUCT;

	case DBGTYPE_UNION:
		return SYM_TYPE_UNION;

	case DBGTYPE_CLASS:
		return SYM_TYPE_CLASS;

	case DBGTYPE_ENUM:
		return SYM_TYPE_ENUM;

	case DBGTYPE_FUNCTION:
		return SYM_TYPE_FUNCTION;

	case DBGTYPE_REFERENCE:
		return SYM_TYPE_REFERENCE;

	default:
		return SYM_TYPE_VOID;
	}
}

/*
 * Get Symbian symbol flags
 */
static unsigned char
sym_get_flags(debug_symbol_t *sym)
{
	unsigned char flags = 0;

	if (sym->is_static)
		flags |= SYM_FLAG_STATIC;
	if (sym->is_extern)
		flags |= SYM_FLAG_EXTERN;
	if (sym->is_inline)
		flags |= SYM_FLAG_INLINE;

	if (sym->type) {
		if (sym->type->is_const)
			flags |= SYM_FLAG_CONST;
		if (sym->type->is_volatile)
			flags |= SYM_FLAG_VOLATILE;
	}

	/* Check for C++ virtual function */
	if (sym->lang_attrs && sym->lang_attrs->cpp.virtual_function)
		flags |= SYM_FLAG_VIRTUAL;

	return flags;
}

/*
 * Initialize Symbian debug symbol generation
 */
void
debugsym_symbian_init(void)
{
	memset(&sym_state, 0, sizeof(sym_state));

	/* Open debug output file (.sym file for Symbian) */
	sym_state.debug_file = fopen("output.sym", "wb");
	if (!sym_state.debug_file) {
		fprintf(stderr, "debugsym_symbian_init: cannot create debug file\n");
		return;
	}

	/* Write E32Image debug header */
	fputc(SYM_DBG_HEADER, sym_state.debug_file);

	/* Write signature */
	sym_write_u32(E32_IMAGE_SIGNATURE);

	/* Write version */
	sym_write_u32(E32_IMAGE_VERSION);

	/* Write timestamp */
	sym_write_u32(0);	/* Placeholder */

	sym_state.type_index = 0x1000;
}

/*
 * Emit Symbian debug symbol
 */
void
debugsym_symbian_emit(debug_symbol_t *sym)
{
	unsigned char type_code;
	unsigned char flags;

	if (!sym_state.debug_file)
		return;

	type_code = sym_get_type(sym->type);
	flags = sym_get_flags(sym);

	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		/* Write function record */
		fputc(SYM_DBG_FUNCTION, sym_state.debug_file);

		/* Write name (descriptor format) */
		sym_write_descriptor(sym->name);

		/* Write mangled name for C++ */
		if (sym->linkage_name)
			sym_write_descriptor(sym->linkage_name);
		else
			sym_write_u16(0);

		/* Write address */
		sym_write_u32(sym->low_pc);

		/* Write size */
		sym_write_u32(sym->high_pc - sym->low_pc);

		/* Write return type */
		fputc(type_code, sym_state.debug_file);

		/* Write flags */
		fputc(flags, sym_state.debug_file);

		/* Write source file and line */
		if (sym->location.filename)
			sym_write_descriptor(sym->location.filename);
		else
			sym_write_u16(0);

		sym_write_u16(sym->location.line);
		break;

	case DBGSYM_VARIABLE:
	case DBGSYM_PARAMETER:
		/* Write variable record */
		fputc(SYM_DBG_LOCAL, sym_state.debug_file);

		/* Write name */
		sym_write_descriptor(sym->name);

		/* Write type */
		fputc(type_code, sym_state.debug_file);

		/* Write flags */
		fputc(flags, sym_state.debug_file);

		/* Write location (offset or register) */
		if (sym->is_register) {
			fputc(0xFF, sym_state.debug_file);	/* Register marker */
			fputc(sym->register_num, sym_state.debug_file);
		} else {
			sym_write_u32(sym->offset);
		}
		break;

	case DBGSYM_STRUCT:
		/* Write class/struct record */
		fputc(sym->type && sym->type->encoding == DBGTYPE_CLASS ?
		      SYM_DBG_CLASS : SYM_DBG_TYPE, sym_state.debug_file);

		/* Write name */
		sym_write_descriptor(sym->name);

		/* Write size */
		if (sym->type)
			sym_write_u32(sym->type->size);
		else
			sym_write_u32(0);

		/* Write flags */
		fputc(flags, sym_state.debug_file);

		sym_state.in_class = 1;
		break;

	case DBGSYM_TYPE:
		/* Write type definition */
		fputc(SYM_DBG_TYPE, sym_state.debug_file);

		/* Write name */
		sym_write_descriptor(sym->name);

		/* Write type code */
		fputc(type_code, sym_state.debug_file);

		/* Write size */
		if (sym->type)
			sym_write_u32(sym->type->size);
		else
			sym_write_u32(0);
		break;

	default:
		/* Ignore other symbol types */
		break;
	}
}

/*
 * Finish Symbian debug symbol generation
 */
void
debugsym_symbian_finish(void)
{
	if (!sym_state.debug_file)
		return;

	/* Write end marker */
	fputc(0xFF, sym_state.debug_file);

	/* Close debug file */
	fclose(sym_state.debug_file);
	sym_state.debug_file = NULL;

	if (sym_state.current_file) {
		free(sym_state.current_file);
		sym_state.current_file = NULL;
	}
}

/*
 * Parse Symbian debug symbols (stub for future implementation)
 */
int
debugsym_symbian_parse(void *data, size_t len)
{
	/* Not implemented yet */
	return -1;
}
