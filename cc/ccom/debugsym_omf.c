/*
 * OMF (Object Module Format) Debug Symbol Support
 *
 * Supports debug formats for OMF object files:
 * - Microsoft OMF (Object Module Format)
 * - Intel OMF-86 and OMF-386
 * - OS/2 OMF
 * - Borland OMF extensions
 * - LINNUM records for line numbers
 * - COMENT records for debug info
 *
 * Used by: Microsoft compilers (pre-PE), Borland C/C++, Watcom C/C++,
 *          Intel compilers, OS/2 compilers
 * Platforms: MS-DOS, OS/2, Windows 3.x (16-bit), 286/386 protected mode
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * OMF record types
 */
#define THEADR		0x80	/* Translator Header */
#define LHEADR		0x82	/* Library Module Header */
#define COMENT		0x88	/* Comment */
#define MODEND		0x8A	/* Module End (16-bit) */
#define MODEND32	0x8B	/* Module End (32-bit) */
#define EXTDEF		0x8C	/* External Names Definition */
#define TYPDEF		0x8E	/* Type Definition */
#define PUBDEF		0x90	/* Public Names Definition (16-bit) */
#define PUBDEF32	0x91	/* Public Names Definition (32-bit) */
#define LINNUM		0x94	/* Line Numbers (16-bit) */
#define LINNUM32	0x95	/* Line Numbers (32-bit) */
#define LNAMES		0x96	/* List of Names */
#define SEGDEF		0x98	/* Segment Definition (16-bit) */
#define SEGDEF32	0x99	/* Segment Definition (32-bit) */
#define GRPDEF		0x9A	/* Group Definition */
#define FIXUPP		0x9C	/* Fixup (16-bit) */
#define FIXUPP32	0x9D	/* Fixup (32-bit) */
#define LEDATA		0xA0	/* Logical Enumerated Data (16-bit) */
#define LEDATA32	0xA1	/* Logical Enumerated Data (32-bit) */
#define LIDATA		0xA2	/* Logical Iterated Data (16-bit) */
#define LIDATA32	0xA3	/* Logical Iterated Data (32-bit) */
#define COMDEF		0xB0	/* Communal Names Definition */
#define BAKPAT		0xB2	/* Backpatch */
#define BAKPAT32	0xB3	/* Backpatch (32-bit) */
#define LEXTDEF		0xB4	/* Local External Names */
#define LPUBDEF		0xB6	/* Local Public Names (16-bit) */
#define LPUBDEF32	0xB7	/* Local Public Names (32-bit) */
#define LCOMDEF		0xB8	/* Local Communal Names */
#define CEXTDEF		0xBC	/* COMDAT External Names */
#define COMDAT		0xC2	/* Initialized Communal Data */
#define COMDAT32	0xC3	/* Initialized Communal Data (32-bit) */
#define LINSYM		0xC4	/* Symbol Line Numbers (16-bit) */
#define LINSYM32	0xC5	/* Symbol Line Numbers (32-bit) */
#define ALIAS		0xC6	/* Alias Definition */
#define NBKPAT		0xC8	/* Named Backpatch */
#define NBKPAT32	0xC9	/* Named Backpatch (32-bit) */
#define LLNAMES		0xCA	/* Local Logical Names */
#define VERNUM		0xCC	/* OMF Version Number */
#define VENDEXT		0xCE	/* Vendor-specific Extension */

/*
 * COMENT class types (for debug info)
 */
#define CMT_TRANSLATOR		0x00	/* Translator comment */
#define CMT_COPYRIGHT		0x01	/* Copyright */
#define CMT_LINKPASS2		0x9C	/* Link pass 2 */
#define CMT_LIBMOD		0x9D	/* Library module */
#define CMT_EXESTR		0x9E	/* Executable string */
#define CMT_INCERR		0x9F	/* Incremental compilation error */
#define CMT_NOPAD		0xA0	/* No segment padding */
#define CMT_WKEXT		0xA1	/* Weak extern */
#define CMT_LZEXT		0xA2	/* Lazy extern */
#define CMT_CODESEG		0xA3	/* Code segment */
#define CMT_COMMENT		0xA4	/* Comment */
#define CMT_COMPILER		0xA6	/* Compiler */
#define CMT_DATE		0xA7	/* Date stamp */
#define CMT_TIMESTAMP		0xA8	/* Timestamp */
#define CMT_USER		0xA9	/* User */
#define CMT_DEPENDENCY		0xE9	/* Dependency file */

/*
 * OMF Debug info COMENT subtypes (Microsoft)
 */
#define DBG_SYMBOLS		0x01	/* Symbol information */
#define DBG_TYPES		0x02	/* Type information */
#define DBG_LOCALS		0x03	/* Local symbols */
#define DBG_SRCMODULE		0x04	/* Source module */
#define DBG_LINNUM		0x05	/* Line numbers */

/* Global state */
static int is_32bit = 0;		/* 32-bit OMF */
static unsigned int record_number = 0;	/* Current record number */
static unsigned int segment_index = 1;	/* Current segment (1-based) */
static unsigned int name_index = 1;	/* Current name index */
static char *current_source = NULL;

/* Forward declarations */
static void emit_omf_record_header(unsigned char type, unsigned int length);
static void emit_omf_record_checksum(void);
static void emit_omf_byte(unsigned char value);
static void emit_omf_word(unsigned short value);
static void emit_omf_dword(unsigned int value);
static void emit_omf_index(unsigned int index);
static void emit_omf_string(const char *str);
static void emit_pubdef_entry(const char *name, unsigned int offset,
			       unsigned char type_index);
static void emit_linnum_entry(unsigned short line, unsigned int offset);

/*
 * Initialize OMF debug symbol generation
 */
void
debugsym_omf_init(void)
{
	const char *arch = getenv("PCC_TARGET_ARCH");

	/* Determine if 32-bit */
	if (arch) {
		if (strcmp(arch, "i386") == 0 ||
		    strcmp(arch, "i486") == 0 ||
		    strcmp(arch, "i586") == 0 ||
		    strcmp(arch, "i686") == 0) {
			is_32bit = 1;
		}
	}

	/* Emit THEADR record (module name) */
	printf("\t.data\n");
	printf("\t.align 1\n");
	printf("L_omf_start:\n");

	/* THEADR: Translator Header */
	printf("\t.byte 0x%02x\n", THEADR);
	printf("\t.word 0x0000\n");		/* Length (filled later) */
	emit_omf_string("PCC_MODULE");		/* Module name */
	emit_omf_record_checksum();

	record_number = 0;
	segment_index = 1;
	name_index = 1;
}

/*
 * Emit a debug symbol
 */
void
debugsym_omf_emit(debug_symbol_t *sym)
{
	unsigned char type_index;
	unsigned int offset;

	if (!sym)
		return;

	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		/* PUBDEF record for function */
		if (is_32bit) {
			printf("\t.byte 0x%02x\n", PUBDEF32);
		} else {
			printf("\t.byte 0x%02x\n", PUBDEF);
		}
		printf("\t.word 0x0000\n");	/* Length (filled later) */

		/* Base group (0 = none) */
		emit_omf_index(0);

		/* Base segment */
		emit_omf_index(segment_index);

		/* Base frame (0 if segment specified) */
		if (segment_index == 0) {
			emit_omf_word(0);
		}

		/* Public name entry */
		emit_pubdef_entry(sym->name,
				  (unsigned int)sym->low_pc,
				  0);	/* Type index (0 = none) */

		emit_omf_record_checksum();
		record_number++;
		break;

	case DBGSYM_VARIABLE:
		if (sym->is_external) {
			/* PUBDEF for global variable */
			if (is_32bit) {
				printf("\t.byte 0x%02x\n", PUBDEF32);
			} else {
				printf("\t.byte 0x%02x\n", PUBDEF);
			}
			printf("\t.word 0x0000\n");	/* Length */

			emit_omf_index(0);		/* Group */
			emit_omf_index(segment_index);	/* Segment */

			offset = sym->is_initialized ?
				 (unsigned int)sym->low_pc : 0;

			emit_pubdef_entry(sym->name, offset, 0);
			emit_omf_record_checksum();
			record_number++;
		}
		break;

	default:
		break;
	}
}

/*
 * Finish and close OMF debug info
 */
void
debugsym_omf_finish(void)
{
	/* MODEND record */
	if (is_32bit) {
		printf("\t.byte 0x%02x\n", MODEND32);
	} else {
		printf("\t.byte 0x%02x\n", MODEND);
	}
	printf("\t.word 0x0002\n");	/* Length = 2 */
	printf("\t.byte 0x00\n");	/* Module type (non-main) */
	printf("\t.byte 0x00\n");	/* No start address */
	emit_omf_record_checksum();

	printf("L_omf_end:\n");
	printf("\t.text\n");

	if (current_source) {
		free(current_source);
		current_source = NULL;
	}
}

/*
 * Parse OMF debug symbols (placeholder)
 */
debug_symbol_t *
debugsym_omf_parse(void *data, size_t len)
{
	/* Parsing not yet implemented */
	return NULL;
}

/*
 * Helper: Emit OMF record header
 */
static void
emit_omf_record_header(unsigned char type, unsigned int length)
{
	printf("\t.byte 0x%02x\n", type);
	printf("\t.word 0x%04x\n", (unsigned short)length);
}

/*
 * Helper: Emit OMF record checksum (placeholder)
 */
static void
emit_omf_record_checksum(void)
{
	/* Checksum computation would go here */
	printf("\t.byte 0x00\n");	/* Checksum (0 for now) */
}

/*
 * Helper: Emit single byte
 */
static void
emit_omf_byte(unsigned char value)
{
	printf("\t.byte 0x%02x\n", value);
}

/*
 * Helper: Emit 16-bit word (little-endian)
 */
static void
emit_omf_word(unsigned short value)
{
	printf("\t.word 0x%04x\n", value);
}

/*
 * Helper: Emit 32-bit dword (little-endian)
 */
static void
emit_omf_dword(unsigned int value)
{
	printf("\t.long 0x%08x\n", value);
}

/*
 * Helper: Emit OMF index (1 or 2 bytes)
 */
static void
emit_omf_index(unsigned int index)
{
	if (index < 128) {
		/* 1-byte index */
		emit_omf_byte((unsigned char)index);
	} else {
		/* 2-byte index (high bit set in first byte) */
		emit_omf_byte((unsigned char)(0x80 | (index >> 8)));
		emit_omf_byte((unsigned char)(index & 0xFF));
	}
}

/*
 * Helper: Emit OMF string (length-prefixed)
 */
static void
emit_omf_string(const char *str)
{
	int len;
	int i;

	if (!str) {
		emit_omf_byte(0);
		return;
	}

	len = strlen(str);
	if (len > 255)
		len = 255;

	emit_omf_byte((unsigned char)len);

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
}

/*
 * Helper: Emit PUBDEF entry
 */
static void
emit_pubdef_entry(const char *name, unsigned int offset,
		   unsigned char type_index)
{
	/* Public name string */
	emit_omf_string(name);

	/* Public offset */
	if (is_32bit) {
		emit_omf_dword(offset);
	} else {
		emit_omf_word((unsigned short)offset);
	}

	/* Type index */
	emit_omf_index(type_index);
}

/*
 * Helper: Emit LINNUM entry
 */
static void
emit_linnum_entry(unsigned short line, unsigned int offset)
{
	/* Line number */
	emit_omf_word(line);

	/* Offset */
	if (is_32bit) {
		emit_omf_dword(offset);
	} else {
		emit_omf_word((unsigned short)offset);
	}
}
