/*
 * a.out Debug Symbol Support
 *
 * Supports debug formats for a.out executable format:
 * - Original Unix a.out debug symbols
 * - BSD a.out with STABS extensions
 * - OMAGIC, NMAGIC, ZMAGIC, QMAGIC variants
 * - Symbol table (.stab) and string table (.stabstr)
 *
 * Used by: Early Unix systems, BSD, Linux (pre-ELF), SunOS, Ultrix
 * Platforms: VAX, PDP-11, 68k, SPARC, i386 (pre-ELF era)
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * a.out executable header magic numbers
 */
#define OMAGIC		0407	/* Old impure format */
#define NMAGIC		0410	/* Read-only text */
#define ZMAGIC		0413	/* Demand load format */
#define QMAGIC		0314	/* Compact format */

/*
 * a.out symbol table entry (12 or 16 bytes depending on variant)
 */
struct nlist {
	union {
		char *n_name;		/* Symbol name (for in-core) */
		long n_strx;		/* String table index */
	} n_un;
	unsigned char n_type;		/* Symbol type */
	char n_other;			/* Misc info */
	short n_desc;			/* Description field */
	unsigned long n_value;		/* Value of symbol */
};

/*
 * a.out symbol types (n_type field)
 */
#define N_UNDF		0x00	/* Undefined */
#define N_ABS		0x02	/* Absolute */
#define N_TEXT		0x04	/* Text segment */
#define N_DATA		0x06	/* Data segment */
#define N_BSS		0x08	/* BSS segment */
#define N_COMM		0x12	/* Common symbol */
#define N_FN		0x1f	/* File name */
#define N_EXT		0x01	/* External bit */
#define N_TYPE		0x1e	/* Type mask */

/*
 * STABS debug symbol types (when used with a.out)
 */
#define N_GSYM		0x20	/* Global symbol */
#define N_FNAME		0x22	/* Function name (BSD) */
#define N_FUN		0x24	/* Function */
#define N_STSYM		0x26	/* Static symbol */
#define N_LCSYM		0x28	/* .lcomm symbol */
#define N_MAIN		0x2a	/* Main routine name */
#define N_PC		0x30	/* PC (for line numbers) */
#define N_RSYM		0x40	/* Register variable */
#define N_SLINE		0x44	/* Line number in text */
#define N_DSLINE	0x46	/* Line number in data */
#define N_BSLINE	0x48	/* Line number in BSS */
#define N_SSYM		0x60	/* Structure element */
#define N_SO		0x64	/* Source file name */
#define N_LSYM		0x80	/* Local symbol */
#define N_BINCL		0x82	/* Begin include file */
#define N_SOL		0x84	/* Include file name */
#define N_PSYM		0xa0	/* Parameter */
#define N_EINCL		0xa2	/* End include file */
#define N_ENTRY		0xa4	/* Alternate entry point */
#define N_LBRAC		0xc0	/* Left bracket */
#define N_EXCL		0xc2	/* Deleted include file */
#define N_RBRAC		0xe0	/* Right bracket */
#define N_BCOMM		0xe2	/* Begin common */
#define N_ECOMM		0xe4	/* End common */
#define N_ECOML		0xe8	/* End common (local name) */
#define N_LENG		0xfe	/* Length */

/* Global state */
static FILE *aout_file = NULL;
static unsigned long string_table_offset = 4;	/* Starts after length */
static unsigned long symbol_count = 0;
static char *current_source_file = NULL;
static int use_stabs = 1;	/* Use STABS by default */

/* Forward declarations */
static void emit_aout_symbol(unsigned char type, const char *name,
			      unsigned long value, short desc);
static void emit_stabs_entry(unsigned char type, const char *name,
			      const char *typestr, unsigned long value);
static unsigned long add_to_string_table(const char *str);
static const char *get_stabs_type_string(debug_type_t *type);

/*
 * Initialize a.out debug symbol generation
 */
void
debugsym_aout_init(void)
{
	const char *format = getenv("PCC_AOUT_DEBUG_FORMAT");

	/* Check if STABS should be used */
	if (format) {
		if (strcmp(format, "stabs") == 0) {
			use_stabs = 1;
		} else if (strcmp(format, "plain") == 0) {
			use_stabs = 0;
		}
	}

	/* Start symbol table section */
	printf("\t.data\n");
	printf("\t.align 4\n");
	printf("L_aout_symtab_start:\n");

	/* String table starts with size field */
	printf("L_string_table:\n");
	printf("\t.long 0\n");	/* Size (filled later) */

	string_table_offset = 4;
	symbol_count = 0;
}

/*
 * Emit a debug symbol
 */
void
debugsym_aout_emit(debug_symbol_t *sym)
{
	unsigned char sym_type;
	const char *type_string;
	unsigned long value;

	if (!sym)
		return;

	if (use_stabs) {
		/* STABS format */
		switch (sym->kind) {
		case DBGSYM_FUNCTION:
			/* Function symbol */
			type_string = get_stabs_type_string(sym->type);
			emit_stabs_entry(N_FUN,
					 sym->name,
					 type_string,
					 (unsigned long)sym->low_pc);
			symbol_count++;
			break;

		case DBGSYM_VARIABLE:
			/* Variable symbol */
			type_string = get_stabs_type_string(sym->type);

			if (sym->is_external) {
				/* Global variable */
				emit_stabs_entry(N_GSYM,
						 sym->name,
						 type_string,
						 0);
			} else if (sym->storage_class == STATIC) {
				/* Static variable */
				emit_stabs_entry(N_STSYM,
						 sym->name,
						 type_string,
						 (unsigned long)sym->low_pc);
			} else if (sym->storage_class == REGISTER) {
				/* Register variable */
				emit_stabs_entry(N_RSYM,
						 sym->name,
						 type_string,
						 sym->reg_num);
			} else {
				/* Local variable */
				emit_stabs_entry(N_LSYM,
						 sym->name,
						 type_string,
						 (unsigned long)sym->stack_offset);
			}
			symbol_count++;
			break;

		case DBGSYM_PARAMETER:
			/* Parameter symbol */
			type_string = get_stabs_type_string(sym->type);
			emit_stabs_entry(N_PSYM,
					 sym->name,
					 type_string,
					 (unsigned long)sym->stack_offset);
			symbol_count++;
			break;

		default:
			break;
		}
	} else {
		/* Plain a.out symbols */
		switch (sym->kind) {
		case DBGSYM_FUNCTION:
			sym_type = N_TEXT;
			if (sym->is_external)
				sym_type |= N_EXT;

			emit_aout_symbol(sym_type,
					 sym->name,
					 (unsigned long)sym->low_pc,
					 0);
			symbol_count++;
			break;

		case DBGSYM_VARIABLE:
			if (sym->is_initialized)
				sym_type = N_DATA;
			else
				sym_type = N_BSS;

			if (sym->is_external)
				sym_type |= N_EXT;

			value = sym->is_external ?
				(unsigned long)sym->low_pc :
				(unsigned long)sym->stack_offset;

			emit_aout_symbol(sym_type,
					 sym->name,
					 value,
					 0);
			symbol_count++;
			break;

		default:
			break;
		}
	}
}

/*
 * Finish and close a.out debug info
 */
void
debugsym_aout_finish(void)
{
	unsigned long string_table_size;

	printf("L_aout_symtab_end:\n");

	/* Calculate string table size */
	string_table_size = string_table_offset;

	/* Patch string table size */
	printf("\t.text\n");
	printf("\t.align 4\n");

	/* Symbol count */
	printf("L_aout_symbol_count:\n");
	printf("\t.long 0x%08lx\n", symbol_count);

	/* String table size */
	printf("L_aout_string_size:\n");
	printf("\t.long 0x%08lx\n", string_table_size);

	if (current_source_file) {
		free(current_source_file);
		current_source_file = NULL;
	}
}

/*
 * Parse a.out debug symbols (placeholder)
 */
debug_symbol_t *
debugsym_aout_parse(void *data, size_t len)
{
	/* Parsing not yet implemented */
	return NULL;
}

/*
 * Helper: Emit a.out symbol table entry
 */
static void
emit_aout_symbol(unsigned char type, const char *name,
		  unsigned long value, short desc)
{
	unsigned long strx;

	/* Add name to string table */
	strx = add_to_string_table(name);

	/* Emit symbol entry (struct nlist) */
	printf("\t.long 0x%08lx\n", strx);		/* n_strx */
	printf("\t.byte 0x%02x\n", type);		/* n_type */
	printf("\t.byte 0x00\n");			/* n_other */
	printf("\t.word 0x%04x\n", (unsigned short)desc); /* n_desc */
	printf("\t.long 0x%08lx\n", value);		/* n_value */
}

/*
 * Helper: Emit STABS entry
 */
static void
emit_stabs_entry(unsigned char type, const char *name,
		  const char *typestr, unsigned long value)
{
	char full_name[512];
	unsigned long strx;

	/* Construct full STABS string: "name:typestr" */
	if (typestr) {
		snprintf(full_name, sizeof(full_name), "%s:%s",
			 name ? name : "", typestr);
	} else {
		snprintf(full_name, sizeof(full_name), "%s",
			 name ? name : "");
	}

	/* Add to string table */
	strx = add_to_string_table(full_name);

	/* Emit STABS entry */
	printf("\t.long 0x%08lx\n", strx);		/* n_strx */
	printf("\t.byte 0x%02x\n", type);		/* n_type */
	printf("\t.byte 0x00\n");			/* n_other */
	printf("\t.word 0x0000\n");			/* n_desc */
	printf("\t.long 0x%08lx\n", value);		/* n_value */
}

/*
 * Helper: Add string to string table
 */
static unsigned long
add_to_string_table(const char *str)
{
	unsigned long offset;
	int len;

	if (!str)
		return 0;

	offset = string_table_offset;
	len = strlen(str);

	/* Emit string to string table */
	printf("\t.ascii \"");
	printf("%s", str);
	printf("\\0\"\n");

	string_table_offset += len + 1;	/* Include null terminator */

	return offset;
}

/*
 * Helper: Get STABS type string for a debug type
 */
static const char *
get_stabs_type_string(debug_type_t *type)
{
	static char type_buf[256];

	if (!type) {
		return "void";
	}

	/* Basic STABS type descriptors */
	switch (type->encoding) {
	case DBGTYPE_VOID:
		return "void";
	case DBGTYPE_CHAR:
	case DBGTYPE_SCHAR:
		return "char";
	case DBGTYPE_UCHAR:
		return "unsigned char";
	case DBGTYPE_INT16:
		return "short";
	case DBGTYPE_UINT16:
		return "unsigned short";
	case DBGTYPE_INT32:
		return "int";
	case DBGTYPE_UINT32:
		return "unsigned int";
	case DBGTYPE_INT64:
		return "long long";
	case DBGTYPE_UINT64:
		return "unsigned long long";
	case DBGTYPE_FLOAT32:
		return "float";
	case DBGTYPE_FLOAT64:
		return "double";
	case DBGTYPE_PTR:
		if (type->base_type) {
			snprintf(type_buf, sizeof(type_buf), "*%s",
				 get_stabs_type_string(type->base_type));
			return type_buf;
		}
		return "*void";
	case DBGTYPE_ARRAY:
		if (type->base_type) {
			snprintf(type_buf, sizeof(type_buf), "ar%s",
				 get_stabs_type_string(type->base_type));
			return type_buf;
		}
		return "array";
	case DBGTYPE_STRUCT:
		if (type->name) {
			snprintf(type_buf, sizeof(type_buf), "struct %s",
				 type->name);
			return type_buf;
		}
		return "struct";
	case DBGTYPE_UNION:
		if (type->name) {
			snprintf(type_buf, sizeof(type_buf), "union %s",
				 type->name);
			return type_buf;
		}
		return "union";
	case DBGTYPE_ENUM:
		if (type->name) {
			snprintf(type_buf, sizeof(type_buf), "enum %s",
				 type->name);
			return type_buf;
		}
		return "enum";
	case DBGTYPE_FUNCTION:
		return "function";
	default:
		return "unknown";
	}
}
