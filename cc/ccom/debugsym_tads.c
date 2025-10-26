/*
 * TADS (Turbo Assembler Debug Symbols) Support
 *
 * Supports TADS (Turbo Assembler Debug Symbols) format:
 * - Borland Turbo Debugger symbol format
 * - TASM (Turbo Assembler) debug output
 * - Compatible with TD32 (32-bit) and TD (16-bit)
 * - Line number information
 * - Segment and group information
 *
 * Used by: Borland Turbo Assembler (TASM), Turbo Debugger (TD/TD32)
 * Platforms: MS-DOS (16-bit), Windows 3.x, Windows 95/98/NT (32-bit)
 *
 * TADS is similar to but distinct from CodeView and TD32 formats.
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * TADS Record Types
 */
#define TADS_MODULE_BEGIN	0x01	/* Module begin */
#define TADS_MODULE_END		0x02	/* Module end */
#define TADS_SEGMENT		0x03	/* Segment definition */
#define TADS_GROUP		0x04	/* Group definition */
#define TADS_SYMBOL		0x05	/* Symbol */
#define TADS_LINE		0x06	/* Line number */
#define TADS_TYPEDEF		0x07	/* Type definition */
#define TADS_LOCAL		0x08	/* Local symbol */
#define TADS_GLOBAL		0x09	/* Global symbol */
#define TADS_PROCEDURE		0x0A	/* Procedure */
#define TADS_ENDPROC		0x0B	/* End procedure */
#define TADS_LABEL		0x0C	/* Label */
#define TADS_SOURCE		0x0D	/* Source file */

/*
 * TADS Symbol Types
 */
#define TADS_SYM_NEAR		0x01	/* Near symbol */
#define TADS_SYM_FAR		0x02	/* Far symbol */
#define TADS_SYM_PROC_NEAR	0x03	/* Near procedure */
#define TADS_SYM_PROC_FAR	0x04	/* Far procedure */
#define TADS_SYM_DATA		0x05	/* Data symbol */
#define TADS_SYM_LABEL		0x06	/* Label */
#define TADS_SYM_CONST		0x07	/* Constant */

/*
 * TADS Storage Classes
 */
#define TADS_SC_AUTO		0x01	/* Automatic */
#define TADS_SC_REGISTER	0x02	/* Register */
#define TADS_SC_STATIC		0x03	/* Static */
#define TADS_SC_EXTERN		0x04	/* External */

/*
 * TADS Type Encodings
 */
#define TADS_TYPE_VOID		0x00
#define TADS_TYPE_BYTE		0x01	/* 8-bit */
#define TADS_TYPE_WORD		0x02	/* 16-bit */
#define TADS_TYPE_DWORD		0x03	/* 32-bit */
#define TADS_TYPE_QWORD		0x04	/* 64-bit */
#define TADS_TYPE_TBYTE		0x05	/* 80-bit (10 bytes) */
#define TADS_TYPE_REAL4		0x06	/* 32-bit float */
#define TADS_TYPE_REAL8		0x07	/* 64-bit double */
#define TADS_TYPE_REAL10	0x08	/* 80-bit extended */
#define TADS_TYPE_NEAR_PTR	0x09	/* Near pointer */
#define TADS_TYPE_FAR_PTR	0x0A	/* Far pointer */
#define TADS_TYPE_STRUCT	0x0B	/* Structure */
#define TADS_TYPE_UNION		0x0C	/* Union */
#define TADS_TYPE_ARRAY		0x0D	/* Array */

/* Global state */
static int is_32bit = 1;		/* Default to 32-bit */
static unsigned int record_number = 0;
static unsigned int segment_index = 1;
static char *module_name = NULL;

/* Forward declarations */
static void emit_tads_record(unsigned char type, const void *data, size_t len);
static void emit_tads_symbol_record(unsigned char sym_type, const char *name,
				     unsigned int offset, unsigned char storage_class);
static void emit_tads_procedure_record(const char *name, unsigned int offset,
					unsigned int length, unsigned char is_far);
static void emit_tads_local_record(const char *name, int offset,
				    unsigned char type);
static unsigned char get_tads_type(debug_type_t *type);

/*
 * Initialize TADS debug symbol generation
 */
void
debugsym_tads_init(void)
{
	const char *bits = getenv("PCC_TARGET_BITS");
	const char *modname = getenv("PCC_MODULE_NAME");

	/* Determine 16-bit or 32-bit */
	if (bits) {
		if (strcmp(bits, "16") == 0) {
			is_32bit = 0;
		} else if (strcmp(bits, "32") == 0) {
			is_32bit = 1;
		}
	}

	/* Module name */
	if (modname) {
		module_name = strdup(modname);
	} else {
		module_name = strdup("module");
	}

	/* Emit TADS debug section */
	printf("\t.section .debug$T,\"dr\"\n");
	printf("\t.align 4\n");
	printf("L_tads_start:\n");

	/* TADS signature */
	printf("\t.ascii \"TADS\"\n");
	printf("\t.word 0x%04x\n", is_32bit ? 32 : 16);	/* Version/bitness */

	/* Module begin record */
	printf("\t.byte 0x%02x\n", TADS_MODULE_BEGIN);
	printf("\t.word 0x0000\n");	/* Record length (filled later) */
	printf("\t.ascii \"");
	printf("%s", module_name);
	printf("\\0\"\n");
	printf("\t.align 2\n");

	record_number = 0;
	segment_index = 1;
}

/*
 * Emit a debug symbol
 */
void
debugsym_tads_emit(debug_symbol_t *sym)
{
	unsigned char type_code;
	unsigned char storage_class;
	unsigned int offset;
	unsigned int length;

	if (!sym)
		return;

	/* Get type code */
	type_code = get_tads_type(sym->type);

	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		/* Procedure record */
		offset = (unsigned int)sym->low_pc;
		length = (unsigned int)(sym->high_pc - sym->low_pc);

		emit_tads_procedure_record(sym->name,
					    offset,
					    length,
					    0);	/* Near procedure */
		record_number++;
		break;

	case DBGSYM_VARIABLE:
		/* Symbol record */
		if (sym->is_extern) {
			storage_class = TADS_SC_EXTERN;
		} else if (sym->storage_class == STATIC) {
			storage_class = TADS_SC_STATIC;
		} else if (sym->storage_class == REGISTER) {
			storage_class = TADS_SC_REGISTER;
		} else {
			storage_class = TADS_SC_AUTO;
		}

		offset = sym->is_extern ?
			 (unsigned int)sym->low_pc :
			 (unsigned int)sym->stack_offset;

		if (sym->is_extern) {
			emit_tads_symbol_record(TADS_SYM_DATA,
						 sym->name,
						 offset,
						 storage_class);
		} else {
			emit_tads_local_record(sym->name,
						(int)offset,
						type_code);
		}
		record_number++;
		break;

	case DBGSYM_PARAMETER:
		/* Local parameter */
		emit_tads_local_record(sym->name,
					(int)sym->stack_offset,
					type_code);
		record_number++;
		break;

	default:
		break;
	}
}

/*
 * Finish and close TADS debug info
 */
void
debugsym_tads_finish(void)
{
	/* Module end record */
	printf("\t.byte 0x%02x\n", TADS_MODULE_END);
	printf("\t.word 0x0000\n");	/* Record length */

	printf("L_tads_end:\n");
	printf("\t.text\n");

	if (module_name) {
		free(module_name);
		module_name = NULL;
	}
}

/*
 * Parse TADS debug symbols (placeholder)
 */
debug_symbol_t *
debugsym_tads_parse(void *data, size_t len)
{
	/* Parsing not yet implemented */
	return NULL;
}

/*
 * Helper: Emit TADS record
 */
static void
emit_tads_record(unsigned char type, const void *data, size_t len)
{
	const unsigned char *bytes = (const unsigned char *)data;
	size_t i;

	printf("\t.byte 0x%02x\n", type);
	printf("\t.word 0x%04x\n", (unsigned short)len);

	if (data && len > 0) {
		for (i = 0; i < len; i++) {
			printf("\t.byte 0x%02x\n", bytes[i]);
		}
	}

	printf("\t.align 2\n");
}

/*
 * Helper: Emit TADS symbol record
 */
static void
emit_tads_symbol_record(unsigned char sym_type, const char *name,
			 unsigned int offset, unsigned char storage_class)
{
	int name_len;

	printf("\t.byte 0x%02x\n", TADS_SYMBOL);

	name_len = name ? strlen(name) : 0;
	printf("\t.word 0x%04x\n", (unsigned short)(7 + name_len));

	printf("\t.byte 0x%02x\n", sym_type);
	printf("\t.byte 0x%02x\n", storage_class);
	printf("\t.long 0x%08x\n", offset);

	if (name && name_len > 0) {
		printf("\t.ascii \"");
		printf("%s", name);
		printf("\\0\"\n");
	} else {
		printf("\t.byte 0\n");
	}

	printf("\t.align 2\n");
}

/*
 * Helper: Emit TADS procedure record
 */
static void
emit_tads_procedure_record(const char *name, unsigned int offset,
			    unsigned int length, unsigned char is_far)
{
	int name_len;
	unsigned char proc_type;

	proc_type = is_far ? TADS_SYM_PROC_FAR : TADS_SYM_PROC_NEAR;

	printf("\t.byte 0x%02x\n", TADS_PROCEDURE);

	name_len = name ? strlen(name) : 0;
	printf("\t.word 0x%04x\n", (unsigned short)(10 + name_len));

	printf("\t.byte 0x%02x\n", proc_type);
	printf("\t.long 0x%08x\n", offset);
	printf("\t.long 0x%08x\n", length);

	if (name && name_len > 0) {
		printf("\t.ascii \"");
		printf("%s", name);
		printf("\\0\"\n");
	} else {
		printf("\t.byte 0\n");
	}

	printf("\t.align 2\n");
}

/*
 * Helper: Emit TADS local symbol record
 */
static void
emit_tads_local_record(const char *name, int offset,
			unsigned char type)
{
	int name_len;

	printf("\t.byte 0x%02x\n", TADS_LOCAL);

	name_len = name ? strlen(name) : 0;
	printf("\t.word 0x%04x\n", (unsigned short)(6 + name_len));

	printf("\t.byte 0x%02x\n", type);

	if (is_32bit) {
		printf("\t.long 0x%08x\n", (unsigned int)offset);
	} else {
		printf("\t.word 0x%04x\n", (unsigned short)offset);
	}

	if (name && name_len > 0) {
		printf("\t.ascii \"");
		printf("%s", name);
		printf("\\0\"\n");
	} else {
		printf("\t.byte 0\n");
	}

	printf("\t.align 2\n");
}

/*
 * Helper: Get TADS type code
 */
static unsigned char
get_tads_type(debug_type_t *type)
{
	if (!type)
		return TADS_TYPE_VOID;

	switch (type->encoding) {
	case DBGTYPE_VOID:
		return TADS_TYPE_VOID;
	case DBGTYPE_CHAR:
	case DBGTYPE_SCHAR:
	case DBGTYPE_UCHAR:
		return TADS_TYPE_BYTE;
	case DBGTYPE_INT16:
	case DBGTYPE_UINT16:
		return TADS_TYPE_WORD;
	case DBGTYPE_INT32:
	case DBGTYPE_UINT32:
		return TADS_TYPE_DWORD;
	case DBGTYPE_INT64:
	case DBGTYPE_UINT64:
		return TADS_TYPE_QWORD;
	case DBGTYPE_FLOAT32:
		return TADS_TYPE_REAL4;
	case DBGTYPE_FLOAT64:
		return TADS_TYPE_REAL8;
	case DBGTYPE_FLOAT80:
		return TADS_TYPE_REAL10;
	case DBGTYPE_PTR:
		return is_32bit ? TADS_TYPE_FAR_PTR : TADS_TYPE_NEAR_PTR;
	case DBGTYPE_ARRAY:
		return TADS_TYPE_ARRAY;
	case DBGTYPE_STRUCT:
		return TADS_TYPE_STRUCT;
	case DBGTYPE_UNION:
		return TADS_TYPE_UNION;
	default:
		return TADS_TYPE_VOID;
	}
}
