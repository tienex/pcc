/*	$Id$	*/

/*
 * Watcom Debug Format Support (WDI)
 * Watcom Debugger Information format
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Watcom debug record types */
#define WAT_HEADER	0x00
#define WAT_LOCALS	0x01
#define WAT_TYPES	0x02
#define WAT_LINES	0x03
#define WAT_MODULES	0x04
#define WAT_GLOBALS	0x05
#define WAT_ADDRINFO	0x06
#define WAT_TYPEINFO	0x07

/* Watcom type codes */
#define WT_VOID		0x00
#define WT_BYTE		0x01
#define WT_WORD		0x02
#define WT_DWORD	0x03
#define WT_QWORD	0x04
#define WT_FLOAT	0x05
#define WT_DOUBLE	0x06
#define WT_POINTER	0x10
#define WT_ARRAY	0x11
#define WT_STRUCT	0x12
#define WT_PROC		0x13

static struct {
	int initialized;
	int record_count;
} wat_state;

void
debugsym_watcom_init(void)
{
	memset(&wat_state, 0, sizeof(wat_state));
	wat_state.initialized = 1;
	wat_state.record_count = 0;

	printf("\t# Watcom debug information (WDI)\n");
	printf("\t.section .debug$W\n");

	/* Emit WDI header */
	printf("\t.ascii \"WDBI\"\n");  /* Watcom debug signature */
	printf("\t.long 1\n");  /* Version */
}

static int
get_watcom_type(debug_type_t *type)
{
	if (type == NULL)
		return WT_VOID;

	switch (type->encoding) {
	case DBGTYPE_VOID:	return WT_VOID;
	case DBGTYPE_CHAR:
	case DBGTYPE_INT8:
	case DBGTYPE_UINT8:	return WT_BYTE;
	case DBGTYPE_INT16:
	case DBGTYPE_UINT16:	return WT_WORD;
	case DBGTYPE_INT32:
	case DBGTYPE_UINT32:	return WT_DWORD;
	case DBGTYPE_INT64:
	case DBGTYPE_UINT64:	return WT_QWORD;
	case DBGTYPE_FLOAT32:	return WT_FLOAT;
	case DBGTYPE_FLOAT64:	return WT_DOUBLE;
	case DBGTYPE_POINTER:	return WT_POINTER;
	case DBGTYPE_ARRAY:	return WT_ARRAY;
	case DBGTYPE_STRUCT:
	case DBGTYPE_UNION:	return WT_STRUCT;
	case DBGTYPE_FUNCTION:	return WT_PROC;
	default:		return WT_DWORD;
	}
}

void
debugsym_watcom_emit(debug_symbol_t *sym)
{
	int wat_type;

	if (sym == NULL)
		return;

	wat_type = get_watcom_type(sym->type);

	printf("\t# Watcom symbol: %s\n", sym->name ? sym->name : "(null)");

	/* Record type */
	if (sym->kind == DBGSYM_FUNCTION)
		printf("\t.byte 0x%x\n", WAT_GLOBALS);
	else if (sym->storage_class == AUTO)
		printf("\t.byte 0x%x\n", WAT_LOCALS);
	else
		printf("\t.byte 0x%x\n", WAT_GLOBALS);

	/* Record length */
	printf("\t.short 0  # length\n");

	/* Symbol type */
	printf("\t.byte 0x%x\n", wat_type);

	/* Symbol name (null-terminated) */
	if (sym->name)
		printf("\t.asciz \"%s\"\n", sym->name);
	else
		printf("\t.byte 0\n");

	/* Address/offset */
	printf("\t.long %ld\n", sym->offset);

	wat_state.record_count++;
}

debug_symbol_t *
debugsym_watcom_parse(void *data, size_t len)
{
	return NULL;
}

void
debugsym_watcom_finish(void)
{
	printf("\t# Watcom debug records: %d\n", wat_state.record_count);
}
