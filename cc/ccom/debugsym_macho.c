/*
 * Mach-O Debug Symbol Support
 *
 * Supports debug formats for Mach-O executable format (macOS/iOS/Darwin):
 * - Mach-O symbol table (LC_SYMTAB)
 * - DWARF in Mach-O sections (__DWARF)
 * - dSYM bundles (.dSYM)
 * - LC_UUID for symbol identification
 * - Mach-O 32-bit and 64-bit variants
 *
 * Used by: macOS, iOS, iPadOS, tvOS, watchOS, Darwin
 * Platforms: x86, x86_64, ARM (32-bit), ARM64, PowerPC (legacy)
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Mach-O magic numbers
 */
#define MH_MAGIC	0xfeedface	/* 32-bit */
#define MH_MAGIC_64	0xfeedfacf	/* 64-bit */
#define MH_CIGAM	0xcefaedfe	/* 32-bit byte-swapped */
#define MH_CIGAM_64	0xcffaedfe	/* 64-bit byte-swapped */

/*
 * Mach-O file types
 */
#define MH_OBJECT	0x1		/* Relocatable object file */
#define MH_EXECUTE	0x2		/* Executable */
#define MH_DYLIB	0x6		/* Dynamic library */
#define MH_DSYM		0xa		/* Debug symbols file */

/*
 * Mach-O load commands
 */
#define LC_SEGMENT	0x1		/* 32-bit segment */
#define LC_SYMTAB	0x2		/* Symbol table */
#define LC_DYSYMTAB	0xb		/* Dynamic symbol table */
#define LC_UUID		0x1b		/* UUID */
#define LC_SEGMENT_64	0x19		/* 64-bit segment */
#define LC_SOURCE_VERSION 0x2a		/* Source version */

/*
 * Mach-O sections
 */
#define SECT_DEBUG_ABBREV	"__debug_abbrev"
#define SECT_DEBUG_INFO		"__debug_info"
#define SECT_DEBUG_LINE		"__debug_line"
#define SECT_DEBUG_STR		"__debug_str"
#define SECT_DEBUG_RANGES	"__debug_ranges"
#define SECT_DEBUG_LOC		"__debug_loc"

/*
 * Mach-O symbol types (n_type field)
 */
#define N_UNDF		0x0	/* Undefined */
#define N_ABS		0x2	/* Absolute */
#define N_SECT		0xe	/* Defined in section */
#define N_PBUD		0xc	/* Prebound undefined */
#define N_INDR		0xa	/* Indirect */

/* Symbol type mask */
#define N_TYPE		0x0e

/* Symbol scope bits */
#define N_EXT		0x01	/* External */
#define N_PEXT		0x10	/* Private external */

/*
 * Mach-O symbol descriptor values (n_desc field)
 */
#define N_NO_DEAD_STRIP		0x0020	/* Don't dead-strip */
#define N_WEAK_REF		0x0040	/* Weak reference */
#define N_WEAK_DEF		0x0080	/* Weak definition */
#define REFERENCE_TYPE		0x0007	/* Reference type */
#define REFERENCED_DYNAMICALLY	0x0010	/* Referenced dynamically */

/*
 * Mach-O STABS types (when n_type has N_STAB bit set)
 */
#define N_STAB		0xe0	/* Mask for STABS */
#define N_GSYM		0x20	/* Global symbol */
#define N_FNAME		0x22	/* Function name */
#define N_FUN		0x24	/* Function */
#define N_STSYM		0x26	/* Static symbol */
#define N_LCSYM		0x28	/* .lcomm symbol */
#define N_BNSYM		0x2e	/* Begin symbol */
#define N_OPT		0x3c	/* Options */
#define N_RSYM		0x40	/* Register variable */
#define N_SLINE		0x44	/* Line number */
#define N_ENSYM		0x4e	/* End symbol */
#define N_SSYM		0x60	/* Structure element */
#define N_SO		0x64	/* Source file */
#define N_OSO		0x66	/* Object file */
#define N_LSYM		0x80	/* Local symbol */
#define N_BINCL		0x82	/* Begin include */
#define N_SOL		0x84	/* Include file */
#define N_PARAMS	0x86	/* Compiler parameters */
#define N_VERSION	0x88	/* Compiler version */
#define N_OLEVEL	0x8a	/* Optimization level */
#define N_PSYM		0xa0	/* Parameter */
#define N_EINCL		0xa2	/* End include */
#define N_ENTRY		0xa4	/* Alternate entry */
#define N_LBRAC		0xc0	/* Left bracket */
#define N_EXCL		0xc2	/* Deleted include */
#define N_RBRAC		0xe0	/* Right bracket */
#define N_BCOMM		0xe2	/* Begin common */
#define N_ECOMM		0xe4	/* End common */
#define N_ECOML		0xe8	/* End common (local) */
#define N_LENG		0xfe	/* Length */

/* Global state */
static int is_64bit = 0;		/* 64-bit Mach-O */
static int use_dwarf = 1;		/* Use DWARF in __DWARF segment */
static unsigned long symbol_count = 0;
static unsigned long string_table_offset = 1; /* Starts at 1 (0 = empty) */
static char *current_source = NULL;

/* Forward declarations */
static void emit_macho_symbol_32(const char *name, unsigned char type,
				  unsigned char sect, unsigned short desc,
				  unsigned int value);
static void emit_macho_symbol_64(const char *name, unsigned char type,
				  unsigned char sect, unsigned short desc,
				  unsigned long value);
static void emit_stabs_symbol(unsigned char type, const char *name,
			       unsigned char sect, unsigned short desc,
			       unsigned long value);
static unsigned long add_to_string_table(const char *str);

/*
 * Initialize Mach-O debug symbol generation
 */
void
debugsym_macho_init(void)
{
	const char *arch = getenv("PCC_TARGET_ARCH");
	const char *format = getenv("PCC_MACHO_DEBUG_FORMAT");

	/* Determine if 64-bit */
	if (arch) {
		if (strcmp(arch, "x86_64") == 0 ||
		    strcmp(arch, "arm64") == 0 ||
		    strcmp(arch, "aarch64") == 0) {
			is_64bit = 1;
		}
	}

	/* Check debug format */
	if (format) {
		if (strcmp(format, "dwarf") == 0) {
			use_dwarf = 1;
		} else if (strcmp(format, "stabs") == 0) {
			use_dwarf = 0;
		}
	}

	if (use_dwarf) {
		/* DWARF in __DWARF segment */
		printf("\t.section __DWARF,__debug_info,regular,debug\n");
		printf("L_macho_dwarf_info_start:\n");
	} else {
		/* Symbol table with STABS */
		printf("\t.section __DATA,__data\n");
		printf("\t.align 3\n");
		printf("L_macho_symtab_start:\n");
	}

	/* String table */
	printf("L_macho_strtab:\n");
	printf("\t.byte 0\n");	/* First byte is always null */

	symbol_count = 0;
	string_table_offset = 1;
}

/*
 * Emit a debug symbol
 */
void
debugsym_macho_emit(debug_symbol_t *sym)
{
	unsigned char sym_type;
	unsigned char section;
	unsigned short desc;
	unsigned long value;

	if (!sym)
		return;

	if (use_dwarf) {
		/* DWARF symbols - handled by DWARF backend */
		/* Just emit symbol table entries */
		section = 1;	/* Section index (1 = __text) */

		switch (sym->kind) {
		case DBGSYM_FUNCTION:
			sym_type = N_SECT;
			if (sym->is_external)
				sym_type |= N_EXT;

			desc = 0;
			value = (unsigned long)sym->low_pc;

			if (is_64bit) {
				emit_macho_symbol_64(sym->name, sym_type,
						     section, desc, value);
			} else {
				emit_macho_symbol_32(sym->name, sym_type,
						     section, desc,
						     (unsigned int)value);
			}
			symbol_count++;
			break;

		case DBGSYM_VARIABLE:
			if (sym->is_external) {
				sym_type = N_SECT | N_EXT;
				section = sym->is_initialized ? 2 : 3; /* __data or __bss */
				value = (unsigned long)sym->low_pc;

				if (is_64bit) {
					emit_macho_symbol_64(sym->name,
							     sym_type, section,
							     0, value);
				} else {
					emit_macho_symbol_32(sym->name,
							     sym_type, section,
							     0, (unsigned int)value);
				}
				symbol_count++;
			}
			break;

		default:
			break;
		}
	} else {
		/* STABS format */
		section = 0;
		desc = 0;

		switch (sym->kind) {
		case DBGSYM_FUNCTION:
			/* Function STABS entry */
			emit_stabs_symbol(N_FUN,
					  sym->name,
					  section,
					  desc,
					  (unsigned long)sym->low_pc);
			symbol_count++;
			break;

		case DBGSYM_VARIABLE:
			if (sym->is_external) {
				emit_stabs_symbol(N_GSYM,
						  sym->name,
						  section,
						  desc,
						  0);
			} else if (sym->storage_class == STATIC) {
				emit_stabs_symbol(N_STSYM,
						  sym->name,
						  section,
						  desc,
						  (unsigned long)sym->low_pc);
			} else if (sym->storage_class == REGISTER) {
				emit_stabs_symbol(N_RSYM,
						  sym->name,
						  section,
						  desc,
						  sym->reg_num);
			} else {
				emit_stabs_symbol(N_LSYM,
						  sym->name,
						  section,
						  desc,
						  (unsigned long)sym->stack_offset);
			}
			symbol_count++;
			break;

		case DBGSYM_PARAMETER:
			emit_stabs_symbol(N_PSYM,
					  sym->name,
					  section,
					  desc,
					  (unsigned long)sym->stack_offset);
			symbol_count++;
			break;

		default:
			break;
		}
	}
}

/*
 * Finish and close Mach-O debug info
 */
void
debugsym_macho_finish(void)
{
	printf("L_macho_symtab_end:\n");

	/* Emit load command information */
	printf("\t.text\n");
	printf("\t.align 3\n");

	/* Symbol count */
	printf("L_macho_nsyms:\n");
	if (is_64bit) {
		printf("\t.quad 0x%016lx\n", symbol_count);
	} else {
		printf("\t.long 0x%08lx\n", symbol_count);
	}

	/* String table size */
	printf("L_macho_strsize:\n");
	if (is_64bit) {
		printf("\t.quad 0x%016lx\n", string_table_offset);
	} else {
		printf("\t.long 0x%08lx\n", string_table_offset);
	}

	if (current_source) {
		free(current_source);
		current_source = NULL;
	}
}

/*
 * Parse Mach-O debug symbols (placeholder)
 */
debug_symbol_t *
debugsym_macho_parse(void *data, size_t len)
{
	/* Parsing not yet implemented */
	return NULL;
}

/*
 * Helper: Emit 32-bit Mach-O symbol (struct nlist)
 */
static void
emit_macho_symbol_32(const char *name, unsigned char type,
		      unsigned char sect, unsigned short desc,
		      unsigned int value)
{
	unsigned int strx;

	strx = (unsigned int)add_to_string_table(name);

	printf("\t.long 0x%08x\n", strx);		/* n_strx */
	printf("\t.byte 0x%02x\n", type);		/* n_type */
	printf("\t.byte 0x%02x\n", sect);		/* n_sect */
	printf("\t.short 0x%04x\n", desc);		/* n_desc */
	printf("\t.long 0x%08x\n", value);		/* n_value */
}

/*
 * Helper: Emit 64-bit Mach-O symbol (struct nlist_64)
 */
static void
emit_macho_symbol_64(const char *name, unsigned char type,
		      unsigned char sect, unsigned short desc,
		      unsigned long value)
{
	unsigned int strx;

	strx = (unsigned int)add_to_string_table(name);

	printf("\t.long 0x%08x\n", strx);		/* n_strx */
	printf("\t.byte 0x%02x\n", type);		/* n_type */
	printf("\t.byte 0x%02x\n", sect);		/* n_sect */
	printf("\t.short 0x%04x\n", desc);		/* n_desc */
	printf("\t.quad 0x%016lx\n", value);		/* n_value (64-bit) */
}

/*
 * Helper: Emit STABS symbol entry
 */
static void
emit_stabs_symbol(unsigned char type, const char *name,
		   unsigned char sect, unsigned short desc,
		   unsigned long value)
{
	unsigned int strx;

	strx = (unsigned int)add_to_string_table(name);

	printf("\t.long 0x%08x\n", strx);		/* n_strx */
	printf("\t.byte 0x%02x\n", type | N_STAB);	/* n_type (with STAB bit) */
	printf("\t.byte 0x%02x\n", sect);		/* n_sect */
	printf("\t.short 0x%04x\n", desc);		/* n_desc */

	if (is_64bit) {
		printf("\t.quad 0x%016lx\n", value);	/* n_value (64-bit) */
	} else {
		printf("\t.long 0x%08lx\n", value);	/* n_value (32-bit) */
	}
}

/*
 * Helper: Add string to string table
 */
static unsigned long
add_to_string_table(const char *str)
{
	unsigned long offset;
	int len;

	if (!str || !*str)
		return 0;

	offset = string_table_offset;
	len = strlen(str);

	/* Emit string */
	printf("\t.ascii \"");
	printf("%s", str);
	printf("\\0\"\n");

	string_table_offset += len + 1;

	return offset;
}
