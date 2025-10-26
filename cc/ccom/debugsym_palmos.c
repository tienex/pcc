/*
 * Palm OS Debug Symbol Support
 *
 * Palm OS was the dominant PDA (Personal Digital Assistant) operating system
 * from 1996-2010, powering millions of Palm Pilot, Handspring, Sony Clié,
 * and other devices.
 *
 * This implementation supports the Palm OS debug symbol format used by:
 * - CodeWarrior for Palm OS
 * - Palm Debugger
 * - PRC Tools (GNU toolchain for Palm OS)
 * - POSE (Palm OS Emulator)
 * - Palm OS Simulator
 *
 * Format: PRC (Palm Resource Code) with debug resources
 * Platforms: Palm OS 1.0 - 5.x (Garnet) (1996-2007)
 * Architectures: 68000 (Palm OS 1-4), ARM (Palm OS 5+)
 */

#include "pass1.h"
#include "debugsym.h"
#include <string.h>
#include <stdlib.h>

/*
 * Palm OS PRC File Format
 */
#define PALM_PRC_SIGNATURE	"application"
#define PALM_DEBUG_TYPE		"gdbS"		/* GDB symbols resource */
#define PALM_DEBUG_ID		1000		/* Debug resource ID */

/*
 * Palm Debug Record Types
 */
#define PALM_DBG_FUNCTION	0x01	/* Function definition */
#define PALM_DBG_VARIABLE	0x02	/* Variable */
#define PALM_DBG_LOCAL		0x03	/* Local variable */
#define PALM_DBG_TYPE		0x04	/* Type definition */
#define PALM_DBG_LINE		0x05	/* Line number info */
#define PALM_DBG_FILE		0x06	/* Source file */

/*
 * Palm Type Encodings (based on STABS-like format)
 */
#define PALM_TYPE_VOID		0
#define PALM_TYPE_BYTE		1	/* 8-bit */
#define PALM_TYPE_WORD		2	/* 16-bit */
#define PALM_TYPE_LONG		3	/* 32-bit */
#define PALM_TYPE_POINTER	4
#define PALM_TYPE_ARRAY		5
#define PALM_TYPE_STRUCT	6
#define PALM_TYPE_FUNCTION	7

/*
 * Palm Symbol Storage Classes
 */
#define PALM_SC_AUTO		1
#define PALM_SC_REGISTER	2
#define PALM_SC_STATIC		3
#define PALM_SC_GLOBAL		4

/*
 * Palm Debug State
 */
typedef struct {
	FILE *debug_file;		/* Debug resource file */
	unsigned int symbol_count;	/* Number of symbols */
	unsigned int string_offset;	/* String table offset */
	unsigned int line_count;	/* Number of line entries */
	char *module_name;		/* Current module */
} palmos_state_t;

static palmos_state_t palm_state;

/*
 * Write Palm OS 32-bit value (big-endian, Palm OS native format)
 */
static void
palm_write_u32(unsigned int value)
{
	fputc((value >> 24) & 0xFF, palm_state.debug_file);
	fputc((value >> 16) & 0xFF, palm_state.debug_file);
	fputc((value >> 8) & 0xFF, palm_state.debug_file);
	fputc(value & 0xFF, palm_state.debug_file);
}

/*
 * Write Palm OS 16-bit value (big-endian)
 */
static void
palm_write_u16(unsigned short value)
{
	fputc((value >> 8) & 0xFF, palm_state.debug_file);
	fputc(value & 0xFF, palm_state.debug_file);
}

/*
 * Write Palm OS string (null-terminated)
 */
static void
palm_write_string(const char *str)
{
	if (!str) {
		fputc(0, palm_state.debug_file);
		return;
	}

	fwrite(str, 1, strlen(str) + 1, palm_state.debug_file);
}

/*
 * Get Palm type encoding
 */
static unsigned char
palm_get_type(debug_type_t *type)
{
	if (!type)
		return PALM_TYPE_VOID;

	switch (type->encoding) {
	case DBGTYPE_VOID:
		return PALM_TYPE_VOID;

	case DBGTYPE_INT8:
	case DBGTYPE_UINT8:
	case DBGTYPE_CHAR:
	case DBGTYPE_BOOL:
		return PALM_TYPE_BYTE;

	case DBGTYPE_INT16:
	case DBGTYPE_UINT16:
		return PALM_TYPE_WORD;

	case DBGTYPE_INT32:
	case DBGTYPE_UINT32:
	case DBGTYPE_FLOAT32:
		return PALM_TYPE_LONG;

	case DBGTYPE_POINTER:
		return PALM_TYPE_POINTER;

	case DBGTYPE_ARRAY:
		return PALM_TYPE_ARRAY;

	case DBGTYPE_STRUCT:
	case DBGTYPE_UNION:
		return PALM_TYPE_STRUCT;

	case DBGTYPE_FUNCTION:
		return PALM_TYPE_FUNCTION;

	default:
		return PALM_TYPE_VOID;
	}
}

/*
 * Get Palm storage class
 */
static unsigned char
palm_get_storage_class(debug_symbol_t *sym)
{
	if (sym->is_register)
		return PALM_SC_REGISTER;

	switch (sym->storage_class) {
	case AUTO:
		return PALM_SC_AUTO;
	case STATIC:
		return PALM_SC_STATIC;
	case EXTERN:
		return PALM_SC_GLOBAL;
	default:
		return PALM_SC_AUTO;
	}
}

/*
 * Initialize Palm OS debug symbol generation
 */
void
debugsym_palmos_init(void)
{
	memset(&palm_state, 0, sizeof(palm_state));

	/* Open debug resource file */
	palm_state.debug_file = fopen("output.gdbS", "wb");
	if (!palm_state.debug_file) {
		fprintf(stderr, "debugsym_palmos_init: cannot create debug file\n");
		return;
	}

	/* Write Palm resource header */
	fwrite(PALM_DEBUG_TYPE, 1, 4, palm_state.debug_file);

	/* Write resource ID */
	palm_write_u16(PALM_DEBUG_ID);

	/* Write version (1.0) */
	palm_write_u16(0x0100);

	/* Write placeholder for symbol count */
	palm_write_u32(0);	/* Will be updated in finish() */
}

/*
 * Emit Palm OS debug symbol
 */
void
debugsym_palmos_emit(debug_symbol_t *sym)
{
	unsigned char type_code;
	unsigned char storage_class;

	if (!palm_state.debug_file)
		return;

	palm_state.symbol_count++;

	type_code = palm_get_type(sym->type);

	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		/* Write function record */
		fputc(PALM_DBG_FUNCTION, palm_state.debug_file);

		/* Write name */
		palm_write_string(sym->name);

		/* Write address */
		palm_write_u32(sym->low_pc);

		/* Write size */
		palm_write_u32(sym->high_pc - sym->low_pc);

		/* Write return type */
		fputc(type_code, palm_state.debug_file);

		/* Write file and line */
		if (sym->location.filename) {
			palm_write_string(sym->location.filename);
		} else {
			fputc(0, palm_state.debug_file);
		}
		palm_write_u16(sym->location.line);
		break;

	case DBGSYM_VARIABLE:
	case DBGSYM_PARAMETER:
		/* Write variable record */
		fputc(PALM_DBG_VARIABLE, palm_state.debug_file);

		/* Write name */
		palm_write_string(sym->name);

		/* Write storage class */
		storage_class = palm_get_storage_class(sym);
		fputc(storage_class, palm_state.debug_file);

		/* Write location */
		if (sym->is_register) {
			fputc(0xFF, palm_state.debug_file);	/* Register marker */
			fputc(sym->register_num, palm_state.debug_file);
		} else {
			palm_write_u32(sym->offset);
		}

		/* Write type */
		fputc(type_code, palm_state.debug_file);
		break;

	case DBGSYM_TYPE:
		/* Write type definition */
		fputc(PALM_DBG_TYPE, palm_state.debug_file);

		/* Write name */
		palm_write_string(sym->name);

		/* Write type code */
		fputc(type_code, palm_state.debug_file);

		/* Write size */
		if (sym->type)
			palm_write_u32(sym->type->size);
		else
			palm_write_u32(0);
		break;

	default:
		/* Ignore other symbol types */
		palm_state.symbol_count--;
		break;
	}
}

/*
 * Finish Palm OS debug symbol generation
 */
void
debugsym_palmos_finish(void)
{
	long pos;

	if (!palm_state.debug_file)
		return;

	/* Update symbol count in header */
	pos = ftell(palm_state.debug_file);
	fseek(palm_state.debug_file, 8, SEEK_SET);	/* Skip to count field */
	palm_write_u32(palm_state.symbol_count);
	fseek(palm_state.debug_file, pos, SEEK_SET);

	/* Write end marker */
	fputc(0xFF, palm_state.debug_file);

	/* Close debug file */
	fclose(palm_state.debug_file);
	palm_state.debug_file = NULL;

	if (palm_state.module_name) {
		free(palm_state.module_name);
		palm_state.module_name = NULL;
	}
}

/*
 * Parse Palm OS debug symbols (stub for future implementation)
 */
int
debugsym_palmos_parse(void *data, size_t len)
{
	/* Not implemented yet */
	return -1;
}
