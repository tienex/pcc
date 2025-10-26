/*	$Id$	*/

/*
 * COFF/ECOFF/XCOFF/PECOFF Debug Support
 * Implements debug symbol generation for COFF-family object formats
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* COFF symbol storage classes */
#define C_NULL		0
#define C_AUTO		1	/* Automatic variable */
#define C_EXT		2	/* External symbol */
#define C_STAT		3	/* Static */
#define C_REG		4	/* Register variable */
#define C_EXTDEF	5	/* External definition */
#define C_LABEL		6	/* Label */
#define C_ULABEL	7	/* Undefined label */
#define C_MOS		8	/* Member of structure */
#define C_ARG		9	/* Function argument */
#define C_STRTAG	10	/* Structure tag */
#define C_MOU		11	/* Member of union */
#define C_UNTAG		12	/* Union tag */
#define C_TPDEF		13	/* Type definition */
#define C_USTATIC	14	/* Undefined static */
#define C_ENTAG		15	/* Enumeration tag */
#define C_MOE		16	/* Member of enumeration */
#define C_REGPARM	17	/* Register parameter */
#define C_FIELD		18	/* Bit field */
#define C_BLOCK		100	/* ".bb" or ".eb" */
#define C_FCN		101	/* ".bf" or ".ef" */
#define C_EOS		102	/* End of structure */
#define C_FILE		103	/* File name */
#define C_LINE		104	/* Line number */
#define C_ALIAS		105	/* Duplicate tag */
#define C_HIDDEN	106	/* Special storage class */

/* COFF type encoding */
#define T_NULL		0
#define T_VOID		1
#define T_CHAR		2
#define T_SHORT		3
#define T_INT		4
#define T_LONG		5
#define T_FLOAT		6
#define T_DOUBLE	7
#define T_STRUCT	8
#define T_UNION		9
#define T_ENUM		10
#define T_MOE		11
#define T_UCHAR		12
#define T_USHORT	13
#define T_UINT		14
#define T_ULONG		15

/* Derived types */
#define DT_NON		0	/* No derived type */
#define DT_PTR		1	/* Pointer */
#define DT_FCN		2	/* Function */
#define DT_ARY		3	/* Array */

static struct {
	debug_format_t format;	/* COFF variant */
	int symbol_count;
	FILE *symtab;
	FILE *strtab;
	int string_offset;
} coff_state;

void
debugsym_coff_init(debug_format_t type)
{
	memset(&coff_state, 0, sizeof(coff_state));
	coff_state.format = type;
	coff_state.symbol_count = 0;
	coff_state.string_offset = 4;  /* String table starts at 4 */

	/* Emit COFF symbol table header */
	printf("\t.section .debug\n");
	printf("\t# COFF debug symbols\n");
}

static int
get_coff_storage_class(debug_symbol_t *sym)
{
	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		return sym->is_static ? C_STAT : C_EXT;
	case DBGSYM_VARIABLE:
		if (sym->storage_class == AUTO)
			return C_AUTO;
		else if (sym->storage_class == STATIC)
			return C_STAT;
		else
			return C_EXT;
	case DBGSYM_PARAMETER:
		return sym->is_register ? C_REGPARM : C_ARG;
	case DBGSYM_TYPEDEF:
		return C_TPDEF;
	case DBGSYM_STRUCT:
		return C_STRTAG;
	case DBGSYM_ENUM:
		return C_ENTAG;
	default:
		return C_NULL;
	}
}

static int
get_coff_type(debug_type_t *type)
{
	if (type == NULL)
		return T_NULL;

	switch (type->encoding) {
	case DBGTYPE_VOID:	return T_VOID;
	case DBGTYPE_CHAR:	return T_CHAR;
	case DBGTYPE_INT16:	return T_SHORT;
	case DBGTYPE_INT32:	return T_INT;
	case DBGTYPE_INT64:	return T_LONG;
	case DBGTYPE_UINT16:	return T_USHORT;
	case DBGTYPE_UINT32:	return T_UINT;
	case DBGTYPE_UINT64:	return T_ULONG;
	case DBGTYPE_FLOAT32:	return T_FLOAT;
	case DBGTYPE_FLOAT64:	return T_DOUBLE;
	case DBGTYPE_STRUCT:	return T_STRUCT;
	case DBGTYPE_UNION:	return T_UNION;
	case DBGTYPE_ENUM:	return T_ENUM;
	default:		return T_INT;
	}
}

void
debugsym_coff_emit(debug_symbol_t *sym)
{
	int storage_class, type;

	if (sym == NULL)
		return;

	storage_class = get_coff_storage_class(sym);
	type = get_coff_type(sym->type);

	printf("\t# COFF symbol: %s\n", sym->name ? sym->name : "(null)");
	printf("\t.long 0  # name/string offset\n");
	printf("\t.long 0  # value\n");
	printf("\t.short 0 # section number\n");
	printf("\t.short %d # type\n", type);
	printf("\t.byte %d # storage class\n", storage_class);
	printf("\t.byte 0  # aux count\n");

	coff_state.symbol_count++;
}

debug_symbol_t *
debugsym_coff_parse(void *data, size_t len)
{
	/* COFF parser implementation */
	return NULL;
}

void
debugsym_coff_finish(void)
{
	printf("\t# Total COFF symbols: %d\n", coff_state.symbol_count);
}
