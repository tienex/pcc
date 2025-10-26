/*
 * Classic Mac OS Debug Symbol Support
 *
 * Supports debug formats for classic Mac OS (System 1-9):
 * - MPW (Macintosh Programmer's Workshop) debug format (68k)
 * - PEF (Preferred Executable Format) debug containers (PowerPC)
 * - Code Fragment Manager debug info
 *
 * Used by: MPW C/C++, Metrowerks CodeWarrior, THINK C, Symantec C++
 * Platforms: Mac OS Classic (68000, 68020, 68030, 68040, PowerPC)
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * MPW Debug Format (68k Macintosh)
 * Used by MPW C/C++ compiler and debuggers like SADE, Macsbug
 */

/* MPW debug record types */
#define MPW_DBG_SOURCE		0x01	/* Source file name */
#define MPW_DBG_FUNCTION	0x02	/* Function definition */
#define MPW_DBG_BLOCK_BEGIN	0x03	/* Block begin */
#define MPW_DBG_BLOCK_END	0x04	/* Block end */
#define MPW_DBG_VARIABLE	0x05	/* Variable definition */
#define MPW_DBG_TYPE		0x06	/* Type definition */
#define MPW_DBG_LINE		0x07	/* Line number entry */
#define MPW_DBG_SEGMENT		0x08	/* Code segment info */
#define MPW_DBG_RESOURCE	0x09	/* Resource info */
#define MPW_DBG_END		0x0A	/* End of debug info */

/* MPW type encodings */
#define MPW_TYPE_VOID		0x00
#define MPW_TYPE_CHAR		0x01
#define MPW_TYPE_SHORT		0x02
#define MPW_TYPE_INT		0x03
#define MPW_TYPE_LONG		0x04
#define MPW_TYPE_FLOAT		0x05
#define MPW_TYPE_DOUBLE		0x06
#define MPW_TYPE_EXTENDED	0x07	/* 80/96-bit extended */
#define MPW_TYPE_PTR		0x08
#define MPW_TYPE_ARRAY		0x09
#define MPW_TYPE_STRUCT		0x0A
#define MPW_TYPE_UNION		0x0B
#define MPW_TYPE_ENUM		0x0C
#define MPW_TYPE_FUNCTION	0x0D
#define MPW_TYPE_PASCAL_STR	0x0E	/* Pascal string */
#define MPW_TYPE_SET		0x0F	/* Pascal set */

/* MPW storage classes */
#define MPW_SC_AUTO		0x01	/* Automatic (stack) */
#define MPW_SC_REGISTER		0x02	/* Register */
#define MPW_SC_STATIC		0x03	/* Static */
#define MPW_SC_GLOBAL		0x04	/* Global */
#define MPW_SC_PARAMETER	0x05	/* Function parameter */
#define MPW_SC_A5_RELATIVE	0x06	/* A5-relative (globals) */

/*
 * PEF Debug Format (PowerPC Macintosh)
 * Used by Code Fragment Manager and CodeWarrior debugger
 */

/* PEF container types */
#define PEF_TAG_DEBUG		0x64626720	/* 'dbg ' */
#define PEF_TAG_CFRG		0x63667267	/* 'cfrg' - Code Fragment */
#define PEF_TAG_SOURCE		0x73726320	/* 'src ' - Source info */
#define PEF_TAG_SYMBOLS		0x73796D20	/* 'sym ' - Symbol table */
#define PEF_TAG_TYPES		0x74797020	/* 'typ ' - Type info */
#define PEF_TAG_LINES		0x6C696E20	/* 'lin ' - Line numbers */

/* PEF debug record types */
#define PEF_DBG_FUNCTION	0x01	/* Function entry */
#define PEF_DBG_VARIABLE	0x02	/* Variable */
#define PEF_DBG_TYPE		0x03	/* Type definition */
#define PEF_DBG_LABEL		0x04	/* Label */
#define PEF_DBG_BLOCK		0x05	/* Block scope */
#define PEF_DBG_WITH		0x06	/* Pascal WITH statement */
#define PEF_DBG_FILE		0x07	/* Source file */
#define PEF_DBG_LINE		0x08	/* Line number */

/* PEF type encodings (similar to MPW but extended) */
#define PEF_TYPE_VOID		0x00
#define PEF_TYPE_BOOLEAN	0x01
#define PEF_TYPE_CHAR		0x02
#define PEF_TYPE_UCHAR		0x03
#define PEF_TYPE_SHORT		0x04
#define PEF_TYPE_USHORT		0x05
#define PEF_TYPE_INT		0x06
#define PEF_TYPE_UINT		0x07
#define PEF_TYPE_LONG		0x08
#define PEF_TYPE_ULONG		0x09
#define PEF_TYPE_LONGLONG	0x0A	/* long long (C99) */
#define PEF_TYPE_ULONGLONG	0x0B
#define PEF_TYPE_FLOAT		0x0C	/* 32-bit IEEE */
#define PEF_TYPE_DOUBLE		0x0D	/* 64-bit IEEE */
#define PEF_TYPE_EXTENDED	0x0E	/* 80-bit extended */
#define PEF_TYPE_PTR		0x0F
#define PEF_TYPE_ARRAY		0x10
#define PEF_TYPE_STRUCT		0x11
#define PEF_TYPE_UNION		0x12
#define PEF_TYPE_ENUM		0x13
#define PEF_TYPE_FUNCTION	0x14
#define PEF_TYPE_PASCAL_STR	0x15
#define PEF_TYPE_CSTRING	0x16	/* C string (char*) */

/*
 * CodeWarrior Debug Format
 * Extended PEF format used by Metrowerks CodeWarrior
 */

#define CW_DBG_VERSION		0x01	/* CodeWarrior debug version */

/* CodeWarrior symbol types */
#define CW_SYM_FUNCTION		0x01
#define CW_SYM_VARIABLE		0x02
#define CW_SYM_PARAMETER	0x03
#define CW_SYM_LABEL		0x04
#define CW_SYM_TYPE		0x05
#define CW_SYM_CLASS		0x06	/* C++ class */
#define CW_SYM_MEMBER		0x07	/* C++ member */
#define CW_SYM_NAMESPACE	0x08	/* C++ namespace */

/* Global state */
static FILE *mpw_file = NULL;		/* MPW debug output */
static FILE *pef_file = NULL;		/* PEF debug output */
static int current_arch = 0;		/* 0=68k, 1=PPC */
static unsigned int current_offset = 0;	/* Current debug offset */
static char *current_source = NULL;	/* Current source file */
static int segment_number = 0;		/* Current code segment */

/* Forward declarations */
static void emit_mpw_record(unsigned char type, const void *data, size_t len);
static void emit_pef_record(unsigned int tag, const void *data, size_t len);
static unsigned char get_mpw_type(debug_type_t *type);
static unsigned char get_pef_type(debug_type_t *type);
static void emit_mpw_pascal_string(const char *str);
static void emit_pef_cstring(const char *str);

/*
 * Initialize Mac OS debug symbol generation
 */
void
debugsym_macos_init(void)
{
	/* Determine architecture from target */
	const char *arch = getenv("PCC_TARGET_ARCH");

	if (arch && (strcmp(arch, "m68k") == 0 || strcmp(arch, "68k") == 0)) {
		current_arch = 0;	/* 68k - use MPW format */
	} else {
		current_arch = 1;	/* PowerPC - use PEF format */
	}

	/* Open appropriate debug section */
	if (current_arch == 0) {
		/* MPW debug goes to .debug section */
		printf("\t.section .debug\n");
		printf("\t.align 2\n");
		printf("L_mpw_debug_start:\n");
	} else {
		/* PEF debug container */
		printf("\t.section .pef_debug\n");
		printf("\t.align 4\n");
		printf("L_pef_debug_start:\n");

		/* Emit PEF container header */
		printf("\t.long 0x%08x\n", PEF_TAG_DEBUG);	/* container tag */
		printf("\t.long 0\n");				/* size (filled later) */
		printf("\t.long 0x%08x\n", CW_DBG_VERSION);	/* version */
	}

	current_offset = 0;
	segment_number = 1;
}

/*
 * Emit a debug symbol
 */
void
debugsym_macos_emit(debug_symbol_t *sym)
{
	unsigned char mpw_type;
	unsigned char pef_type;

	if (!sym)
		return;

	if (current_arch == 0) {
		/* MPW format (68k) */
		switch (sym->kind) {
		case DBGSYM_FUNCTION:
			/* Function record */
			emit_mpw_pascal_string(sym->name);
			printf("\t.byte 0x%02x\n", MPW_DBG_FUNCTION);
			printf("\t.long 0x%08x\n", (unsigned int)sym->low_pc);
			printf("\t.long 0x%08x\n",
			       (unsigned int)(sym->high_pc - sym->low_pc));
			if (sym->type) {
				mpw_type = get_mpw_type(sym->type);
				printf("\t.byte 0x%02x\n", mpw_type);
			} else {
				printf("\t.byte 0x%02x\n", MPW_TYPE_VOID);
			}
			break;

		case DBGSYM_VARIABLE:
		case DBGSYM_PARAMETER:
			/* Variable/parameter record */
			emit_mpw_pascal_string(sym->name);
			printf("\t.byte 0x%02x\n", MPW_DBG_VARIABLE);

			/* Storage class */
			if (sym->kind == DBGSYM_PARAMETER) {
				printf("\t.byte 0x%02x\n", MPW_SC_PARAMETER);
			} else if (sym->is_external) {
				printf("\t.byte 0x%02x\n", MPW_SC_GLOBAL);
			} else if (sym->storage_class == STATIC) {
				printf("\t.byte 0x%02x\n", MPW_SC_STATIC);
			} else if (sym->storage_class == REGISTER) {
				printf("\t.byte 0x%02x\n", MPW_SC_REGISTER);
				printf("\t.byte 0x%02x\n", sym->reg_num);
			} else {
				printf("\t.byte 0x%02x\n", MPW_SC_AUTO);
			}

			/* Type */
			if (sym->type) {
				mpw_type = get_mpw_type(sym->type);
				printf("\t.byte 0x%02x\n", mpw_type);
			}

			/* Location/offset */
			if (sym->storage_class != REGISTER) {
				printf("\t.word 0x%04x\n",
				       (unsigned short)sym->stack_offset);
			}
			break;

		case DBGSYM_TYPE:
			/* Type definition */
			emit_mpw_pascal_string(sym->name);
			printf("\t.byte 0x%02x\n", MPW_DBG_TYPE);
			if (sym->type) {
				mpw_type = get_mpw_type(sym->type);
				printf("\t.byte 0x%02x\n", mpw_type);
				printf("\t.long 0x%08x\n", sym->type->size);
			}
			break;

		default:
			break;
		}
	} else {
		/* PEF format (PowerPC) */
		switch (sym->kind) {
		case DBGSYM_FUNCTION:
			/* Function container */
			printf("\t.long 0x%08x\n", PEF_TAG_SYMBOLS);
			printf("\t.long 0\n");	/* size (filled later) */
			printf("\t.byte 0x%02x\n", PEF_DBG_FUNCTION);
			emit_pef_cstring(sym->name);
			printf("\t.long 0x%08x\n", (unsigned int)sym->low_pc);
			printf("\t.long 0x%08x\n",
			       (unsigned int)(sym->high_pc - sym->low_pc));
			if (sym->type) {
				pef_type = get_pef_type(sym->type);
				printf("\t.byte 0x%02x\n", pef_type);
			}
			printf("\t.align 4\n");
			break;

		case DBGSYM_VARIABLE:
		case DBGSYM_PARAMETER:
			/* Variable container */
			printf("\t.byte 0x%02x\n", PEF_DBG_VARIABLE);
			emit_pef_cstring(sym->name);

			/* Flags: external, register, etc. */
			printf("\t.byte 0x%02x\n",
			       (sym->is_external ? 0x01 : 0x00) |
			       (sym->storage_class == REGISTER ? 0x02 : 0x00));

			if (sym->storage_class == REGISTER) {
				printf("\t.byte 0x%02x\n", sym->reg_num);
			} else {
				printf("\t.long 0x%08x\n",
				       (unsigned int)sym->stack_offset);
			}

			if (sym->type) {
				pef_type = get_pef_type(sym->type);
				printf("\t.byte 0x%02x\n", pef_type);
			}
			printf("\t.align 4\n");
			break;

		case DBGSYM_TYPE:
			/* Type container */
			printf("\t.long 0x%08x\n", PEF_TAG_TYPES);
			printf("\t.long 0\n");	/* size */
			printf("\t.byte 0x%02x\n", PEF_DBG_TYPE);
			emit_pef_cstring(sym->name);
			if (sym->type) {
				pef_type = get_pef_type(sym->type);
				printf("\t.byte 0x%02x\n", pef_type);
				printf("\t.long 0x%08x\n", sym->type->size);
			}
			printf("\t.align 4\n");
			break;

		default:
			break;
		}
	}
}

/*
 * Finish and close Mac OS debug info
 */
void
debugsym_macos_finish(void)
{
	if (current_arch == 0) {
		/* MPW: emit end record */
		printf("\t.byte 0x%02x\n", MPW_DBG_END);
		printf("L_mpw_debug_end:\n");
		printf("\t.long L_mpw_debug_end - L_mpw_debug_start\n");
	} else {
		/* PEF: close container */
		printf("L_pef_debug_end:\n");
		printf("\t.long L_pef_debug_end - L_pef_debug_start\n");
	}

	printf("\t.text\n");	/* Return to text section */

	if (current_source) {
		free(current_source);
		current_source = NULL;
	}
}

/*
 * Parse Mac OS debug symbols (placeholder)
 */
debug_symbol_t *
debugsym_macos_parse(void *data, size_t len)
{
	/* Parsing not yet implemented */
	return NULL;
}

/*
 * Helper: Get MPW type code
 */
static unsigned char
get_mpw_type(debug_type_t *type)
{
	if (!type)
		return MPW_TYPE_VOID;

	switch (type->encoding) {
	case DBGTYPE_VOID:
		return MPW_TYPE_VOID;
	case DBGTYPE_CHAR:
	case DBGTYPE_SCHAR:
	case DBGTYPE_UCHAR:
		return MPW_TYPE_CHAR;
	case DBGTYPE_INT16:
	case DBGTYPE_UINT16:
		return MPW_TYPE_SHORT;
	case DBGTYPE_INT32:
	case DBGTYPE_UINT32:
		return MPW_TYPE_INT;
	case DBGTYPE_INT64:
	case DBGTYPE_UINT64:
		return MPW_TYPE_LONG;
	case DBGTYPE_FLOAT32:
		return MPW_TYPE_FLOAT;
	case DBGTYPE_FLOAT64:
		return MPW_TYPE_DOUBLE;
	case DBGTYPE_FLOAT80:
		return MPW_TYPE_EXTENDED;
	case DBGTYPE_PTR:
		return MPW_TYPE_PTR;
	case DBGTYPE_ARRAY:
		return MPW_TYPE_ARRAY;
	case DBGTYPE_STRUCT:
		return MPW_TYPE_STRUCT;
	case DBGTYPE_UNION:
		return MPW_TYPE_UNION;
	case DBGTYPE_ENUM:
		return MPW_TYPE_ENUM;
	case DBGTYPE_FUNCTION:
		return MPW_TYPE_FUNCTION;
	default:
		return MPW_TYPE_VOID;
	}
}

/*
 * Helper: Get PEF type code
 */
static unsigned char
get_pef_type(debug_type_t *type)
{
	if (!type)
		return PEF_TYPE_VOID;

	switch (type->encoding) {
	case DBGTYPE_VOID:
		return PEF_TYPE_VOID;
	case DBGTYPE_BOOL:
		return PEF_TYPE_BOOLEAN;
	case DBGTYPE_CHAR:
	case DBGTYPE_SCHAR:
		return PEF_TYPE_CHAR;
	case DBGTYPE_UCHAR:
		return PEF_TYPE_UCHAR;
	case DBGTYPE_INT16:
		return PEF_TYPE_SHORT;
	case DBGTYPE_UINT16:
		return PEF_TYPE_USHORT;
	case DBGTYPE_INT32:
		return PEF_TYPE_INT;
	case DBGTYPE_UINT32:
		return PEF_TYPE_UINT;
	case DBGTYPE_INT64:
		return PEF_TYPE_LONGLONG;
	case DBGTYPE_UINT64:
		return PEF_TYPE_ULONGLONG;
	case DBGTYPE_FLOAT32:
		return PEF_TYPE_FLOAT;
	case DBGTYPE_FLOAT64:
		return PEF_TYPE_DOUBLE;
	case DBGTYPE_FLOAT80:
		return PEF_TYPE_EXTENDED;
	case DBGTYPE_PTR:
		return PEF_TYPE_PTR;
	case DBGTYPE_ARRAY:
		return PEF_TYPE_ARRAY;
	case DBGTYPE_STRUCT:
		return PEF_TYPE_STRUCT;
	case DBGTYPE_UNION:
		return PEF_TYPE_UNION;
	case DBGTYPE_ENUM:
		return PEF_TYPE_ENUM;
	case DBGTYPE_FUNCTION:
		return PEF_TYPE_FUNCTION;
	default:
		return PEF_TYPE_VOID;
	}
}

/*
 * Helper: Emit Pascal string (length-prefixed)
 */
static void
emit_mpw_pascal_string(const char *str)
{
	int len = str ? strlen(str) : 0;
	int i;

	if (len > 255)
		len = 255;

	printf("\t.byte 0x%02x\n", len);

	if (str && len > 0) {
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
 * Helper: Emit C string (null-terminated)
 */
static void
emit_pef_cstring(const char *str)
{
	int i, len;

	if (!str) {
		printf("\t.byte 0\n");
		return;
	}

	len = strlen(str);
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
	printf("\\0\"\n");
}

/*
 * Helper: Emit MPW debug record
 */
static void
emit_mpw_record(unsigned char type, const void *data, size_t len)
{
	const unsigned char *bytes = (const unsigned char *)data;
	size_t i;

	printf("\t.byte 0x%02x\n", type);

	if (data && len > 0) {
		for (i = 0; i < len; i++) {
			printf("\t.byte 0x%02x\n", bytes[i]);
		}
	}
}

/*
 * Helper: Emit PEF debug record
 */
static void
emit_pef_record(unsigned int tag, const void *data, size_t len)
{
	const unsigned char *bytes = (const unsigned char *)data;
	size_t i;

	printf("\t.long 0x%08x\n", tag);
	printf("\t.long 0x%08x\n", (unsigned int)len);

	if (data && len > 0) {
		for (i = 0; i < len; i++) {
			printf("\t.byte 0x%02x\n", bytes[i]);
		}
		/* Align to 4 bytes */
		if (len % 4 != 0) {
			printf("\t.align 4\n");
		}
	}
}
