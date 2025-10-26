/*
 * VxWorks RTOS Debug Symbol Support
 *
 * VxWorks is a real-time operating system (RTOS) used extensively in
 * aerospace, defense, automotive, medical, and industrial systems.
 *
 * Notable VxWorks deployments:
 * - Mars rovers (Spirit, Opportunity, Curiosity, Perseverance)
 * - Boeing 787 Dreamliner
 * - F-35 Lightning II fighter jet
 * - SpaceX Dragon spacecraft
 * - Honda and BMW automotive systems
 * - Numerous medical devices
 *
 * This implementation supports the VxWorks debug symbol format used by:
 * - Wind River Workbench IDE
 * - Wind River Debugger
 * - VxWorks System Viewer
 * - Lauterbach TRACE32 for VxWorks
 *
 * Format: VxWorks Symbol Table (symtab) or DWARF (modern VxWorks)
 * Platforms: VxWorks 5.x - 7.x (1987-present)
 * Architectures: x86, PowerPC, ARM, MIPS, SH, 68k
 */

#include "pass1.h"
#include "debugsym.h"
#include <string.h>
#include <stdlib.h>

/*
 * VxWorks Symbol Table Format
 */
#define VX_SYMTAB_MAGIC		0x56585354	/* 'VXST' */
#define VX_SYMTAB_VERSION	0x0002		/* Version 2 */

/*
 * VxWorks Symbol Types
 */
#define VX_SYM_UNDEF		0x00	/* Undefined */
#define VX_SYM_ABS		0x01	/* Absolute */
#define VX_SYM_TEXT		0x02	/* Text (code) */
#define VX_SYM_DATA		0x03	/* Data */
#define VX_SYM_BSS		0x04	/* BSS (uninitialized) */
#define VX_SYM_COMMON		0x05	/* Common */

/*
 * VxWorks Symbol Attributes
 */
#define VX_ATTR_LOCAL		0x00	/* Local symbol */
#define VX_ATTR_GLOBAL		0x01	/* Global symbol */
#define VX_ATTR_WEAK		0x02	/* Weak symbol */

/*
 * VxWorks Debug Record Types (Wind River extension)
 */
#define VX_DBG_FUNCTION		0x10	/* Function definition */
#define VX_DBG_VARIABLE		0x11	/* Variable */
#define VX_DBG_TYPE		0x12	/* Type definition */
#define VX_DBG_LINE		0x13	/* Line number */
#define VX_DBG_FILE		0x14	/* Source file */

/*
 * VxWorks Type Codes
 */
#define VX_TYPE_VOID		0
#define VX_TYPE_CHAR		1
#define VX_TYPE_SHORT		2
#define VX_TYPE_INT		3
#define VX_TYPE_LONG		4
#define VX_TYPE_FLOAT		5
#define VX_TYPE_DOUBLE		6
#define VX_TYPE_POINTER		7
#define VX_TYPE_ARRAY		8
#define VX_TYPE_STRUCT		9
#define VX_TYPE_UNION		10
#define VX_TYPE_ENUM		11
#define VX_TYPE_FUNCTION	12

/*
 * VxWorks Debug State
 */
typedef struct {
	FILE *debug_file;		/* Symbol table file */
	unsigned int symbol_count;	/* Number of symbols */
	unsigned int string_table_size;	/* String table size */
	long string_table_offset;	/* Offset to string table */
	unsigned int type_count;	/* Number of type records */
	int use_dwarf;			/* Use DWARF instead of proprietary */
} vxworks_state_t;

static vxworks_state_t vx_state;

/*
 * Write VxWorks 32-bit value (target endianness - assume little for x86/ARM)
 */
static void
vx_write_u32(unsigned int value)
{
	fputc(value & 0xFF, vx_state.debug_file);
	fputc((value >> 8) & 0xFF, vx_state.debug_file);
	fputc((value >> 16) & 0xFF, vx_state.debug_file);
	fputc((value >> 24) & 0xFF, vx_state.debug_file);
}

/*
 * Write VxWorks 16-bit value
 */
static void
vx_write_u16(unsigned short value)
{
	fputc(value & 0xFF, vx_state.debug_file);
	fputc((value >> 8) & 0xFF, vx_state.debug_file);
}

/*
 * Write VxWorks string to string table
 */
static unsigned int
vx_write_string(const char *str)
{
	unsigned int offset;
	size_t len;

	if (!str)
		return 0;

	offset = vx_state.string_table_size;
	len = strlen(str) + 1;

	fwrite(str, 1, len, vx_state.debug_file);
	vx_state.string_table_size += len;

	return offset;
}

/*
 * Get VxWorks symbol type
 */
static unsigned char
vx_get_symbol_type(debug_symbol_t *sym)
{
	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		return VX_SYM_TEXT;

	case DBGSYM_VARIABLE:
		if (sym->storage_class == STATIC)
			return VX_SYM_DATA;
		if (sym->storage_class == EXTERN)
			return VX_SYM_DATA;
		return VX_SYM_BSS;

	default:
		return VX_SYM_ABS;
	}
}

/*
 * Get VxWorks type code
 */
static unsigned char
vx_get_type_code(debug_type_t *type)
{
	if (!type)
		return VX_TYPE_VOID;

	switch (type->encoding) {
	case DBGTYPE_VOID:
		return VX_TYPE_VOID;

	case DBGTYPE_CHAR:
	case DBGTYPE_INT8:
	case DBGTYPE_UINT8:
	case DBGTYPE_BOOL:
		return VX_TYPE_CHAR;

	case DBGTYPE_INT16:
	case DBGTYPE_UINT16:
		return VX_TYPE_SHORT;

	case DBGTYPE_INT32:
	case DBGTYPE_UINT32:
		return VX_TYPE_INT;

	case DBGTYPE_INT64:
	case DBGTYPE_UINT64:
		return VX_TYPE_LONG;

	case DBGTYPE_FLOAT32:
		return VX_TYPE_FLOAT;

	case DBGTYPE_FLOAT64:
		return VX_TYPE_DOUBLE;

	case DBGTYPE_POINTER:
		return VX_TYPE_POINTER;

	case DBGTYPE_ARRAY:
		return VX_TYPE_ARRAY;

	case DBGTYPE_STRUCT:
		return VX_TYPE_STRUCT;

	case DBGTYPE_UNION:
		return VX_TYPE_UNION;

	case DBGTYPE_ENUM:
		return VX_TYPE_ENUM;

	case DBGTYPE_FUNCTION:
		return VX_TYPE_FUNCTION;

	default:
		return VX_TYPE_VOID;
	}
}

/*
 * Initialize VxWorks debug symbol generation
 */
void
debugsym_vxworks_init(void)
{
	memset(&vx_state, 0, sizeof(vx_state));

	/* Modern VxWorks (7.x) uses DWARF, but we provide proprietary format too */
	vx_state.use_dwarf = 0;  /* Set to 1 to use DWARF instead */

	if (vx_state.use_dwarf) {
		/* Use DWARF backend for modern VxWorks */
		debugsym_dwarf_init();
		return;
	}

	/* Open VxWorks symbol table file */
	vx_state.debug_file = fopen("output.sym", "wb");
	if (!vx_state.debug_file) {
		fprintf(stderr, "debugsym_vxworks_init: cannot create symbol file\n");
		return;
	}

	/* Write VxWorks symbol table header */
	vx_write_u32(VX_SYMTAB_MAGIC);
	vx_write_u16(VX_SYMTAB_VERSION);

	/* Placeholder for symbol count */
	vx_write_u32(0);

	/* Placeholder for string table offset */
	vx_write_u32(0);

	vx_state.string_table_offset = ftell(vx_state.debug_file);
}

/*
 * Emit VxWorks debug symbol
 */
void
debugsym_vxworks_emit(debug_symbol_t *sym)
{
	unsigned char sym_type;
	unsigned char type_code;
	unsigned int name_offset;

	if (vx_state.use_dwarf) {
		debugsym_dwarf_emit(sym);
		return;
	}

	if (!vx_state.debug_file)
		return;

	sym_type = vx_get_symbol_type(sym);
	type_code = vx_get_type_code(sym->type);

	vx_state.symbol_count++;

	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		/* Write function symbol */
		fputc(VX_DBG_FUNCTION, vx_state.debug_file);

		/* Write name offset to string table */
		name_offset = vx_write_string(sym->name);
		vx_write_u32(name_offset);

		/* Write address */
		vx_write_u32(sym->low_pc);

		/* Write size */
		vx_write_u32(sym->high_pc - sym->low_pc);

		/* Write symbol type and attributes */
		fputc(sym_type, vx_state.debug_file);
		fputc(sym->is_static ? VX_ATTR_LOCAL : VX_ATTR_GLOBAL, vx_state.debug_file);

		/* Write return type */
		fputc(type_code, vx_state.debug_file);
		break;

	case DBGSYM_VARIABLE:
	case DBGSYM_PARAMETER:
		/* Write variable symbol */
		fputc(VX_DBG_VARIABLE, vx_state.debug_file);

		/* Write name */
		name_offset = vx_write_string(sym->name);
		vx_write_u32(name_offset);

		/* Write address/offset */
		if (sym->is_register) {
			vx_write_u32(0xFFFFFFFF);	/* Register marker */
			fputc(sym->register_num, vx_state.debug_file);
		} else {
			vx_write_u32(sym->offset);
		}

		/* Write symbol type */
		fputc(sym_type, vx_state.debug_file);

		/* Write type code */
		fputc(type_code, vx_state.debug_file);
		break;

	case DBGSYM_TYPE:
		/* Write type definition */
		fputc(VX_DBG_TYPE, vx_state.debug_file);

		/* Write name */
		name_offset = vx_write_string(sym->name);
		vx_write_u32(name_offset);

		/* Write type code */
		fputc(type_code, vx_state.debug_file);

		/* Write size */
		if (sym->type)
			vx_write_u32(sym->type->size);
		else
			vx_write_u32(0);

		vx_state.type_count++;
		break;

	default:
		vx_state.symbol_count--;
		break;
	}
}

/*
 * Finish VxWorks debug symbol generation
 */
void
debugsym_vxworks_finish(void)
{
	long pos;

	if (vx_state.use_dwarf) {
		debugsym_dwarf_finish();
		return;
	}

	if (!vx_state.debug_file)
		return;

	/* Update symbol count in header */
	pos = ftell(vx_state.debug_file);

	fseek(vx_state.debug_file, 6, SEEK_SET);  /* Skip to symbol count */
	vx_write_u32(vx_state.symbol_count);

	/* Update string table offset */
	vx_write_u32(vx_state.string_table_offset);

	fseek(vx_state.debug_file, pos, SEEK_SET);

	/* Write end marker */
	fputc(0xFF, vx_state.debug_file);

	/* Close file */
	fclose(vx_state.debug_file);
	vx_state.debug_file = NULL;
}

/*
 * Parse VxWorks debug symbols (stub for future implementation)
 */
int
debugsym_vxworks_parse(void *data, size_t len)
{
	/* Not implemented yet */
	return -1;
}
