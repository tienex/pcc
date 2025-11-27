/*
 * Plan 9 Debug Symbol Support
 *
 * Supports debug formats for Plan 9 from Bell Labs:
 * - Plan 9 object file format (8.out, 6.out, v.out, etc.)
 * - Plan 9 symbol table format
 * - Acid debugger symbol format
 * - Text/data/bss segment symbols
 *
 * Used by: Plan 9, Inferno, Plan 9 from User Space
 * Platforms: 386 (8.out), amd64 (6.out), ARM (5.out), MIPS (v.out),
 *            PowerPC (q.out), SPARC (k.out), 68020 (2.out)
 *
 * Plan 9's object format is simpler than Unix a.out and uses
 * single-character tool prefixes (e.g., 8c, 8l for 386).
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Plan 9 Object Magic Numbers (from a.out.h)
 */
#define A_MAGIC_386	0x8000		/* 8.out (386) */
#define A_MAGIC_68020	0x2000		/* 2.out (68020) */
#define A_MAGIC_ARM	0x5000		/* 5.out (ARM) */
#define A_MAGIC_AMD64	0x6000		/* 6.out (amd64) */
#define A_MAGIC_MIPS	0x4000		/* v.out (MIPS) */
#define A_MAGIC_PPC	0x9000		/* q.out (PowerPC) */
#define A_MAGIC_SPARC	0xB000		/* k.out (SPARC) */

/*
 * Plan 9 Symbol Types (from a.out.h)
 */
#define STEXT		'T'		/* Text symbol */
#define SDATA		'D'		/* Data symbol */
#define SBSS		'B'		/* BSS symbol */
#define SFILE		'f'		/* File name */
#define SFRAME		'm'		/* Stack frame */
#define SPARAM		'p'		/* Parameter */
#define SAUTO		'a'		/* Automatic (local) */

/*
 * Plan 9 Extended Symbol Types (for Acid)
 */
#define STYPEMASK	0x7f		/* Type mask */
#define SLOCAL		0x80		/* Local symbol flag */

/* Global state */
static int arch_magic = A_MAGIC_386;	/* Default to 386 */
static unsigned int symbol_count = 0;
static unsigned int string_table_size = 0;
static char arch_prefix = '8';		/* Tool prefix */

/* Forward declarations */
static void emit_plan9_symbol(char type, unsigned long value, const char *name);
static void emit_string_table_entry(const char *str);
static char get_plan9_type(debug_symbol_t *sym);

/*
 * Initialize Plan 9 debug symbol generation
 */
void
debugsym_plan9_init(void)
{
	const char *arch = getenv("PCC_TARGET_ARCH");

	/* Determine architecture */
	if (arch) {
		if (strcmp(arch, "i386") == 0 || strcmp(arch, "386") == 0) {
			arch_magic = A_MAGIC_386;
			arch_prefix = '8';
		} else if (strcmp(arch, "amd64") == 0 || strcmp(arch, "x86_64") == 0) {
			arch_magic = A_MAGIC_AMD64;
			arch_prefix = '6';
		} else if (strcmp(arch, "arm") == 0) {
			arch_magic = A_MAGIC_ARM;
			arch_prefix = '5';
		} else if (strcmp(arch, "m68k") == 0 || strcmp(arch, "68020") == 0) {
			arch_magic = A_MAGIC_68020;
			arch_prefix = '2';
		} else if (strcmp(arch, "mips") == 0) {
			arch_magic = A_MAGIC_MIPS;
			arch_prefix = 'v';
		} else if (strcmp(arch, "powerpc") == 0 || strcmp(arch, "ppc") == 0) {
			arch_magic = A_MAGIC_PPC;
			arch_prefix = 'q';
		} else if (strcmp(arch, "sparc") == 0) {
			arch_magic = A_MAGIC_SPARC;
			arch_prefix = 'k';
		}
	}

	/* Emit symbol table section */
	printf("\t.data\n");
	printf("\t.align 4\n");
	printf("L_plan9_symtab:\n");

	/* Symbol table header (Plan 9 specific) */
	printf("\t# Plan 9 %c.out symbol table\n", arch_prefix);
	printf("\t.long 0x%08x\n", arch_magic);	/* Magic number */

	symbol_count = 0;
	string_table_size = 0;
}

/*
 * Emit a debug symbol
 */
void
debugsym_plan9_emit(debug_symbol_t *sym)
{
	char sym_type;
	unsigned long value;

	if (!sym)
		return;

	sym_type = get_plan9_type(sym);

	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		/* Text symbol */
		value = (unsigned long)sym->low_pc;
		emit_plan9_symbol(STEXT, value, sym->name);
		symbol_count++;
		break;

	case DBGSYM_VARIABLE:
		/* Data or BSS symbol */
		if (sym->is_extern) {
			if (sym->is_initialized) {
				value = (unsigned long)sym->low_pc;
				emit_plan9_symbol(SDATA, value, sym->name);
			} else {
				value = (unsigned long)sym->low_pc;
				emit_plan9_symbol(SBSS, value, sym->name);
			}
		} else {
			/* Local/auto variable */
			value = (unsigned long)sym->stack_offset;
			emit_plan9_symbol(SAUTO | SLOCAL, value, sym->name);
		}
		symbol_count++;
		break;

	case DBGSYM_PARAMETER:
		/* Parameter symbol */
		value = (unsigned long)sym->stack_offset;
		emit_plan9_symbol(SPARAM | SLOCAL, value, sym->name);
		symbol_count++;
		break;

	default:
		break;
	}
}

/*
 * Finish and close Plan 9 debug info
 */
void
debugsym_plan9_finish(void)
{
	printf("L_plan9_symtab_end:\n");

	/* Symbol count */
	printf("L_plan9_nsyms:\n");
	printf("\t.long 0x%08lx\n", (unsigned long)symbol_count);

	/* String table size */
	printf("L_plan9_strsize:\n");
	printf("\t.long 0x%08lx\n", (unsigned long)string_table_size);

	printf("\t.text\n");
}

/*
 * Parse Plan 9 debug symbols (placeholder)
 */
debug_symbol_t *
debugsym_plan9_parse(void *data, size_t len)
{
	/* Parsing not yet implemented */
	return NULL;
}

/*
 * Helper: Emit Plan 9 symbol entry
 */
static void
emit_plan9_symbol(char type, unsigned long value, const char *name)
{
	/* Plan 9 symbol entry format:
	 * - 1 byte: type
	 * - 4 bytes: value (address/offset)
	 * - variable: name (null-terminated string)
	 */

	printf("\t.byte 0x%02x\n", (unsigned char)type);
	printf("\t.long 0x%08lx\n", value);

	/* Emit name */
	emit_string_table_entry(name);
}

/*
 * Helper: Emit string to string table
 */
static void
emit_string_table_entry(const char *str)
{
	int len;

	if (!str) {
		printf("\t.byte 0\n");
		string_table_size += 1;
		return;
	}

	len = strlen(str);

	if (len > 0) {
		printf("\t.ascii \"");
		printf("%s", str);
		printf("\\0\"\n");
		string_table_size += len + 1;
	} else {
		printf("\t.byte 0\n");
		string_table_size += 1;
	}
}

/*
 * Helper: Get Plan 9 symbol type
 */
static char
get_plan9_type(debug_symbol_t *sym)
{
	if (!sym)
		return 0;

	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		return STEXT;
	case DBGSYM_VARIABLE:
		if (sym->is_extern) {
			return sym->is_initialized ? SDATA : SBSS;
		} else {
			return SAUTO | SLOCAL;
		}
	case DBGSYM_PARAMETER:
		return SPARAM | SLOCAL;
	default:
		return 0;
	}
}
