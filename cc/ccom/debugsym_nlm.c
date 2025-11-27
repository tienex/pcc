/*
 * NetWare NLM (NetWare Loadable Module) Debug Symbol Support
 *
 * Novell NetWare was the dominant network operating system in the 1990s-2000s,
 * with over 70% market share at its peak. NetWare Loadable Modules (NLMs) were
 * the equivalent of device drivers and server applications for NetWare.
 *
 * This implementation supports the NetWare debug symbol format used by:
 * - Novell NetWare 3.x, 4.x, 5.x, 6.x
 * - Watcom C/C++ for NetWare
 * - Metrowerks CodeWarrior for NetWare
 * - Novell NetWare Debugger (NWDBG)
 *
 * Format: NLM debug symbols are embedded in the NLM file format
 * Platforms: NetWare 3.x - 6.x (1990-2009)
 */

#include "pass1.h"
#include "debugsym.h"
#include <string.h>
#include <stdlib.h>

/*
 * NLM File Format Magic Numbers
 */
#define NLM_SIGNATURE		"NetWare Loadable Module"
#define NLM_VERSION_OLD		0x00000001	/* NetWare 3.x */
#define NLM_VERSION_NEW		0x00000002	/* NetWare 4.x+ */

/*
 * NLM Debug Record Types
 */
#define NLM_DBG_MODULE		0x01	/* Module start */
#define NLM_DBG_FUNCTION	0x02	/* Function definition */
#define NLM_DBG_LOCAL		0x03	/* Local variable */
#define NLM_DBG_GLOBAL		0x04	/* Global variable */
#define NLM_DBG_TYPE		0x05	/* Type definition */
#define NLM_DBG_LINE		0x06	/* Line number info */
#define NLM_DBG_SEGMENT		0x07	/* Segment info */
#define NLM_DBG_MODULE_END	0x08	/* Module end */

/*
 * NLM Type Encodings
 */
#define NLM_TYPE_VOID		0x00
#define NLM_TYPE_BYTE		0x01
#define NLM_TYPE_WORD		0x02
#define NLM_TYPE_DWORD		0x03
#define NLM_TYPE_QWORD		0x04
#define NLM_TYPE_REAL		0x05	/* float */
#define NLM_TYPE_DOUBLE		0x06
#define NLM_TYPE_POINTER	0x07
#define NLM_TYPE_ARRAY		0x08
#define NLM_TYPE_STRUCT		0x09
#define NLM_TYPE_UNION		0x0A
#define NLM_TYPE_FUNCTION	0x0B

/*
 * NLM Symbol Storage Classes
 */
#define NLM_SC_AUTO		0x01	/* Automatic (stack) */
#define NLM_SC_REGISTER		0x02	/* Register */
#define NLM_SC_STATIC		0x03	/* Static */
#define NLM_SC_EXTERN		0x04	/* External */
#define NLM_SC_TYPEDEF		0x05	/* Type definition */

/*
 * NLM Debug Symbol State
 */
typedef struct {
	FILE *debug_file;		/* Debug output file */
	unsigned int module_offset;	/* Current module offset */
	unsigned int string_offset;	/* String table offset */
	unsigned int type_index;	/* Next type index */
	char *module_name;		/* Current module name */
	int in_function;		/* Inside function flag */
} nlm_state_t;

static nlm_state_t nlm_state;

/*
 * Write NLM debug record header
 */
static void
nlm_write_record_header(unsigned char type, unsigned short length)
{
	fputc(type, nlm_state.debug_file);
	fputc(length & 0xFF, nlm_state.debug_file);
	fputc((length >> 8) & 0xFF, nlm_state.debug_file);
}

/*
 * Write NLM string (length-prefixed)
 */
static void
nlm_write_string(const char *str)
{
	unsigned char len;

	if (!str) {
		fputc(0, nlm_state.debug_file);
		return;
	}

	len = strlen(str);
	if (len > 255)
		len = 255;

	fputc(len, nlm_state.debug_file);
	fwrite(str, 1, len, nlm_state.debug_file);
}

/*
 * Write NLM type encoding
 */
static unsigned short
nlm_write_type(debug_type_t *type)
{
	unsigned short type_id;

	if (!type)
		return NLM_TYPE_VOID;

	type_id = nlm_state.type_index++;

	switch (type->encoding) {
	case DBGTYPE_VOID:
		return NLM_TYPE_VOID;

	case DBGTYPE_INT8:
	case DBGTYPE_UINT8:
	case DBGTYPE_CHAR:
	case DBGTYPE_BOOL:
		return NLM_TYPE_BYTE;

	case DBGTYPE_INT16:
	case DBGTYPE_UINT16:
		return NLM_TYPE_WORD;

	case DBGTYPE_INT32:
	case DBGTYPE_UINT32:
		return NLM_TYPE_DWORD;

	case DBGTYPE_INT64:
	case DBGTYPE_UINT64:
		return NLM_TYPE_QWORD;

	case DBGTYPE_FLOAT32:
		return NLM_TYPE_REAL;

	case DBGTYPE_FLOAT64:
		return NLM_TYPE_DOUBLE;

	case DBGTYPE_POINTER:
		return NLM_TYPE_POINTER;

	case DBGTYPE_ARRAY:
		return NLM_TYPE_ARRAY;

	case DBGTYPE_STRUCT:
		return NLM_TYPE_STRUCT;

	case DBGTYPE_UNION:
		return NLM_TYPE_UNION;

	case DBGTYPE_FUNCTION:
		return NLM_TYPE_FUNCTION;

	default:
		return NLM_TYPE_VOID;
	}
}

/*
 * Get NLM storage class
 */
static unsigned char
nlm_get_storage_class(debug_symbol_t *sym)
{
	if (sym->is_register)
		return NLM_SC_REGISTER;

	switch (sym->storage_class) {
	case AUTO:
		return NLM_SC_AUTO;
	case STATIC:
		return NLM_SC_STATIC;
	case EXTERN:
		return NLM_SC_EXTERN;
	case TYPEDEF:
		return NLM_SC_TYPEDEF;
	default:
		return NLM_SC_AUTO;
	}
}

/*
 * Initialize NLM debug symbol generation
 */
void
debugsym_nlm_init(void)
{
	memset(&nlm_state, 0, sizeof(nlm_state));

	/* Open debug output file (separate .dbg file for NLM) */
	nlm_state.debug_file = fopen("output.dbg", "wb");
	if (!nlm_state.debug_file) {
		fprintf(stderr, "debugsym_nlm_init: cannot create debug file\n");
		return;
	}

	/* Write NLM debug signature */
	fwrite(NLM_SIGNATURE, 1, strlen(NLM_SIGNATURE), nlm_state.debug_file);

	/* Write version */
	fputc(NLM_VERSION_NEW & 0xFF, nlm_state.debug_file);
	fputc((NLM_VERSION_NEW >> 8) & 0xFF, nlm_state.debug_file);
	fputc((NLM_VERSION_NEW >> 16) & 0xFF, nlm_state.debug_file);
	fputc((NLM_VERSION_NEW >> 24) & 0xFF, nlm_state.debug_file);

	nlm_state.type_index = 0x1000;	/* Start type indices at 0x1000 */
}

/*
 * Emit NLM debug symbol
 */
void
debugsym_nlm_emit(debug_symbol_t *sym)
{
	unsigned char record_type;
	unsigned short type_id;
	unsigned char storage_class;

	if (!nlm_state.debug_file)
		return;

	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		/* Emit function record */
		nlm_write_record_header(NLM_DBG_FUNCTION, 0);
		nlm_write_string(sym->name);

		/* Write address */
		fputc(sym->low_pc & 0xFF, nlm_state.debug_file);
		fputc((sym->low_pc >> 8) & 0xFF, nlm_state.debug_file);
		fputc((sym->low_pc >> 16) & 0xFF, nlm_state.debug_file);
		fputc((sym->low_pc >> 24) & 0xFF, nlm_state.debug_file);

		/* Write size */
		{
			unsigned int size = sym->high_pc - sym->low_pc;
			fputc(size & 0xFF, nlm_state.debug_file);
			fputc((size >> 8) & 0xFF, nlm_state.debug_file);
			fputc((size >> 16) & 0xFF, nlm_state.debug_file);
			fputc((size >> 24) & 0xFF, nlm_state.debug_file);
		}

		/* Write return type */
		type_id = nlm_write_type(sym->type);
		fputc(type_id & 0xFF, nlm_state.debug_file);
		fputc((type_id >> 8) & 0xFF, nlm_state.debug_file);

		nlm_state.in_function = 1;
		break;

	case DBGSYM_VARIABLE:
	case DBGSYM_PARAMETER:
		/* Emit variable record */
		record_type = nlm_state.in_function ? NLM_DBG_LOCAL : NLM_DBG_GLOBAL;
		nlm_write_record_header(record_type, 0);
		nlm_write_string(sym->name);

		/* Write storage class */
		storage_class = nlm_get_storage_class(sym);
		fputc(storage_class, nlm_state.debug_file);

		/* Write offset or register number */
		if (sym->is_register) {
			fputc(sym->register_num, nlm_state.debug_file);
		} else {
			fputc(sym->offset & 0xFF, nlm_state.debug_file);
			fputc((sym->offset >> 8) & 0xFF, nlm_state.debug_file);
			fputc((sym->offset >> 16) & 0xFF, nlm_state.debug_file);
			fputc((sym->offset >> 24) & 0xFF, nlm_state.debug_file);
		}

		/* Write type */
		type_id = nlm_write_type(sym->type);
		fputc(type_id & 0xFF, nlm_state.debug_file);
		fputc((type_id >> 8) & 0xFF, nlm_state.debug_file);
		break;

	case DBGSYM_TYPE:
		/* Emit type definition */
		nlm_write_record_header(NLM_DBG_TYPE, 0);
		nlm_write_string(sym->name);

		type_id = nlm_write_type(sym->type);
		fputc(type_id & 0xFF, nlm_state.debug_file);
		fputc((type_id >> 8) & 0xFF, nlm_state.debug_file);
		break;

	default:
		/* Ignore other symbol types */
		break;
	}
}

/*
 * Finish NLM debug symbol generation
 */
void
debugsym_nlm_finish(void)
{
	if (!nlm_state.debug_file)
		return;

	/* Write module end record */
	nlm_write_record_header(NLM_DBG_MODULE_END, 0);

	/* Close debug file */
	fclose(nlm_state.debug_file);
	nlm_state.debug_file = NULL;

	if (nlm_state.module_name) {
		free(nlm_state.module_name);
		nlm_state.module_name = NULL;
	}
}

/*
 * Parse NLM debug symbols (stub for future implementation)
 */
int
debugsym_nlm_parse(void *data, size_t len)
{
	/* Not implemented yet */
	return -1;
}
