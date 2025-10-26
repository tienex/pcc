/*
 * PDB (Program Database) Debug Symbol Support
 *
 * Supports Microsoft's PDB (Program Database) format:
 * - PDB 2.0 (Visual C++ 2.0-6.0)
 * - PDB 7.0 (Visual Studio 2002+, also called MSF - Multi-Stream Format)
 * - Type records (TPI - Type Info stream)
 * - Symbol records (DBI - Debug Info stream)
 * - Source line information
 * - Public symbols and global symbols
 *
 * Used by: Microsoft Visual Studio, Windows Debugger (WinDbg), Visual Studio Code
 * Platforms: Windows (x86, x64, ARM, ARM64)
 *
 * Note: PDB is a complex binary format. This is a simplified implementation
 * that generates basic PDB structure and references.
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * PDB File Signatures
 */
#define PDB20_SIGNATURE		"Microsoft C/C++ program database 2.00\r\n\032JG\0"
#define PDB70_SIGNATURE		"Microsoft C/C++ MSF 7.00\r\n\032DS\0\0"

/*
 * PDB Stream Numbers
 */
#define PDB_STREAM_ROOT		0	/* Root directory */
#define PDB_STREAM_PDB		1	/* PDB stream (header info) */
#define PDB_STREAM_TPI		2	/* Type info */
#define PDB_STREAM_DBI		3	/* Debug info */
#define PDB_STREAM_IPI		4	/* ID info (PDB 7.0) */

/*
 * PDB 7.0 Page Size
 */
#define PDB70_PAGE_SIZE		4096

/*
 * DBI Stream Version
 */
#define DBI_VERSION_41		930803
#define DBI_VERSION_50		19960307
#define DBI_VERSION_60		19970606
#define DBI_VERSION_70		19990903
#define DBI_VERSION_110		20091201

/*
 * Symbol record types (subset)
 */
#define S_END			0x0006	/* End of block */
#define S_SKIP			0x0007	/* Skip record */
#define S_CVRESERVE		0x0008	/* Reserved */
#define S_OBJNAME		0x0009	/* Object file name */
#define S_ENDARG		0x000a	/* End of arguments */
#define S_COBOLUDT		0x000b	/* COBOL user-defined type */
#define S_MANYREG		0x000c	/* Many register variable */
#define S_RETURN		0x000d	/* Function return description */
#define S_ENTRYTHIS		0x000e	/* Entry this pointer */
#define S_BPREL32		0x0200	/* BP-relative (32-bit offset) */
#define S_LDATA32		0x0201	/* Local data (32-bit) */
#define S_GDATA32		0x0202	/* Global data (32-bit) */
#define S_PUB32			0x0203	/* Public symbol (32-bit) */
#define S_LPROC32		0x0204	/* Local procedure (32-bit) */
#define S_GPROC32		0x0205	/* Global procedure (32-bit) */
#define S_THUNK32		0x0206	/* Thunk (32-bit) */
#define S_BLOCK32		0x0207	/* Block (32-bit) */
#define S_WITH32		0x0208	/* With (32-bit) */
#define S_LABEL32		0x0209	/* Label (32-bit) */
#define S_REGISTER		0x020a	/* Register variable */
#define S_CONSTANT		0x020b	/* Constant */
#define S_UDT			0x020c	/* User-defined type */

/*
 * Type record types (subset)
 */
#define LF_MODIFIER		0x0001	/* Type modifier */
#define LF_POINTER		0x0002	/* Pointer */
#define LF_ARRAY		0x0003	/* Array */
#define LF_CLASS		0x0004	/* Class */
#define LF_STRUCTURE		0x0005	/* Structure */
#define LF_UNION		0x0006	/* Union */
#define LF_ENUM			0x0007	/* Enumeration */
#define LF_PROCEDURE		0x0008	/* Procedure */
#define LF_MFUNCTION		0x0009	/* Member function */
#define LF_VTSHAPE		0x000a	/* Virtual function table shape */
#define LF_COBOL0		0x000b	/* COBOL0 */
#define LF_COBOL1		0x000c	/* COBOL1 */
#define LF_BARRAY		0x000d	/* Basic array */
#define LF_LABEL		0x000e	/* Label */
#define LF_NULL			0x000f	/* Null */
#define LF_NOTTRAN		0x0010	/* Not translated */
#define LF_DIMARRAY		0x0011	/* Multi-dimensional array */
#define LF_VFTPATH		0x0012	/* Virtual function table path */
#define LF_PRECOMP		0x0013	/* Precompiled types */
#define LF_ENDPRECOMP		0x0014	/* End of precompiled types */

/* Global state */
static int pdb_version = 70;		/* Default to PDB 7.0 */
static unsigned int type_index = 0x1000; /* Type indices start at 0x1000 */
static unsigned int symbol_count = 0;
static char pdb_filename[256];

/* Forward declarations */
static void emit_pdb_header(void);
static void emit_pdb_symbol_record(unsigned short type, const char *name,
				    unsigned int offset, unsigned int type_index);
static void emit_pdb_type_record(unsigned short leaf, unsigned int size);
static unsigned int get_pdb_type_index(debug_type_t *type);

/*
 * Initialize PDB debug symbol generation
 */
void
debugsym_pdb_init(void)
{
	const char *pdb_ver = getenv("PCC_PDB_VERSION");
	const char *source_file = getenv("PCC_SOURCE_FILE");

	/* Determine PDB version */
	if (pdb_ver) {
		if (strcmp(pdb_ver, "2.0") == 0 || strcmp(pdb_ver, "20") == 0) {
			pdb_version = 20;
		} else if (strcmp(pdb_ver, "7.0") == 0 || strcmp(pdb_ver, "70") == 0) {
			pdb_version = 70;
		}
	}

	/* Generate PDB filename */
	if (source_file) {
		char *dot;
		snprintf(pdb_filename, sizeof(pdb_filename), "%s", source_file);
		dot = strrchr(pdb_filename, '.');
		if (dot) {
			strcpy(dot, ".pdb");
		} else {
			strcat(pdb_filename, ".pdb");
		}
	} else {
		strcpy(pdb_filename, "output.pdb");
	}

	/* Emit reference to PDB file */
	printf("\t.section .debug$S,\"dr\"\n");
	printf("\t.align 4\n");
	printf("L_pdb_debug_start:\n");

	/* CV signature (CV4 for PDB reference) */
	printf("\t.long 0x00000004\n");	/* CV signature */

	/* Emit PDB file path reference */
	printf("\t.ascii \"");
	printf("%s", pdb_filename);
	printf("\\0\"\n");
	printf("\t.align 4\n");

	/* Start symbol stream */
	printf("L_pdb_symbols:\n");

	type_index = 0x1000;
	symbol_count = 0;
}

/*
 * Emit a debug symbol
 */
void
debugsym_pdb_emit(debug_symbol_t *sym)
{
	unsigned int pdb_type_idx;
	unsigned int offset;

	if (!sym)
		return;

	/* Get PDB type index for symbol type */
	pdb_type_idx = get_pdb_type_index(sym->type);

	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		/* Global or local procedure */
		offset = (unsigned int)sym->low_pc;

		if (sym->is_external) {
			emit_pdb_symbol_record(S_GPROC32,
						sym->name,
						offset,
						pdb_type_idx);
		} else {
			emit_pdb_symbol_record(S_LPROC32,
						sym->name,
						offset,
						pdb_type_idx);
		}
		symbol_count++;
		break;

	case DBGSYM_VARIABLE:
		offset = sym->is_external ?
			 (unsigned int)sym->low_pc :
			 (unsigned int)sym->stack_offset;

		if (sym->is_external) {
			/* Global data */
			emit_pdb_symbol_record(S_GDATA32,
						sym->name,
						offset,
						pdb_type_idx);
		} else if (sym->storage_class == REGISTER) {
			/* Register variable */
			emit_pdb_symbol_record(S_REGISTER,
						sym->name,
						sym->reg_num,
						pdb_type_idx);
		} else {
			/* Local data or BP-relative */
			emit_pdb_symbol_record(S_BPREL32,
						sym->name,
						offset,
						pdb_type_idx);
		}
		symbol_count++;
		break;

	case DBGSYM_PARAMETER:
		/* BP-relative parameter */
		emit_pdb_symbol_record(S_BPREL32,
					sym->name,
					(unsigned int)sym->stack_offset,
					pdb_type_idx);
		symbol_count++;
		break;

	case DBGSYM_TYPE:
		/* User-defined type */
		emit_pdb_symbol_record(S_UDT,
					sym->name,
					0,
					pdb_type_idx);
		symbol_count++;
		break;

	default:
		break;
	}
}

/*
 * Finish and close PDB debug info
 */
void
debugsym_pdb_finish(void)
{
	printf("L_pdb_debug_end:\n");

	/* Emit PDB metadata */
	printf("\t.section .debug$T,\"dr\"\n");
	printf("\t.align 4\n");
	printf("L_pdb_types:\n");
	printf("\t.long 0x00000004\n");	/* CV signature */
	printf("L_pdb_types_end:\n");

	/* Return to code section */
	printf("\t.text\n");

	/* Output informational comment */
	printf("\t# PDB file: %s\n", pdb_filename);
	printf("\t# PDB version: %d.0\n", pdb_version / 10);
	printf("\t# Symbols: %u\n", symbol_count);
}

/*
 * Parse PDB debug symbols (placeholder)
 */
debug_symbol_t *
debugsym_pdb_parse(void *data, size_t len)
{
	/* PDB parsing is complex and not yet implemented */
	return NULL;
}

/*
 * Helper: Emit PDB header
 */
static void
emit_pdb_header(void)
{
	if (pdb_version == 70) {
		/* PDB 7.0 header */
		printf("\t.ascii \"");
		printf("%s", PDB70_SIGNATURE);
		printf("\"\n");
		printf("\t.long 0x%08x\n", PDB70_PAGE_SIZE);
	} else {
		/* PDB 2.0 header */
		printf("\t.ascii \"");
		printf("%s", PDB20_SIGNATURE);
		printf("\"\n");
	}
}

/*
 * Helper: Emit PDB symbol record
 */
static void
emit_pdb_symbol_record(unsigned short type, const char *name,
			unsigned int offset, unsigned int type_idx)
{
	int name_len;
	int record_len;

	if (!name)
		name = "";

	name_len = strlen(name);

	/* Calculate record length (type + data + name + null) */
	record_len = 2 + 4 + 4 + name_len + 1; /* Simplified */

	/* Align to 4 bytes */
	if (record_len % 4 != 0) {
		record_len += 4 - (record_len % 4);
	}

	/* Record length (excluding length field itself) */
	printf("\t.word 0x%04x\n", (unsigned short)(record_len - 2));

	/* Record type */
	printf("\t.word 0x%04x\n", type);

	/* Type index */
	printf("\t.long 0x%08x\n", type_idx);

	/* Offset/value */
	printf("\t.long 0x%08x\n", offset);

	/* Name (null-terminated) */
	if (name_len > 0) {
		printf("\t.ascii \"");
		printf("%s", name);
		printf("\\0\"\n");
	} else {
		printf("\t.byte 0\n");
	}

	/* Align to 4 bytes */
	if ((name_len + 1) % 4 != 0) {
		int padding = 4 - ((name_len + 1) % 4);
		printf("\t.space %d\n", padding);
	}
}

/*
 * Helper: Emit PDB type record
 */
static void
emit_pdb_type_record(unsigned short leaf, unsigned int size)
{
	/* Record length */
	printf("\t.word 0x000a\n");	/* 10 bytes */

	/* Leaf type */
	printf("\t.word 0x%04x\n", leaf);

	/* Type-specific data (simplified) */
	printf("\t.long 0x%08x\n", size);
	printf("\t.word 0x0000\n");	/* Attributes */
}

/*
 * Helper: Get PDB type index for debug type
 */
static unsigned int
get_pdb_type_index(debug_type_t *type)
{
	unsigned int idx;

	if (!type) {
		return 0x0003;	/* T_VOID */
	}

	/* Primitive types have predefined indices */
	switch (type->encoding) {
	case DBGTYPE_VOID:
		return 0x0003;	/* T_VOID */
	case DBGTYPE_CHAR:
	case DBGTYPE_SCHAR:
		return 0x0010;	/* T_CHAR */
	case DBGTYPE_UCHAR:
		return 0x0020;	/* T_UCHAR */
	case DBGTYPE_INT16:
		return 0x0011;	/* T_SHORT */
	case DBGTYPE_UINT16:
		return 0x0021;	/* T_USHORT */
	case DBGTYPE_INT32:
		return 0x0074;	/* T_INT4 */
	case DBGTYPE_UINT32:
		return 0x0075;	/* T_UINT4 */
	case DBGTYPE_INT64:
		return 0x0076;	/* T_INT8 */
	case DBGTYPE_UINT64:
		return 0x0077;	/* T_UINT8 */
	case DBGTYPE_FLOAT32:
		return 0x0040;	/* T_REAL32 */
	case DBGTYPE_FLOAT64:
		return 0x0041;	/* T_REAL64 */
	case DBGTYPE_FLOAT80:
		return 0x0042;	/* T_REAL80 */
	case DBGTYPE_PTR:
		/* Pointer type - use generic pointer */
		return 0x0603;	/* T_32PVOID */
	case DBGTYPE_ARRAY:
	case DBGTYPE_STRUCT:
	case DBGTYPE_UNION:
	case DBGTYPE_ENUM:
	case DBGTYPE_FUNCTION:
		/* Complex types get allocated indices */
		idx = type_index++;
		return idx;
	default:
		return 0x0003;	/* T_VOID */
	}
}
