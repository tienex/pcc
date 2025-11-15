/*
 * Copyright (c) 2025 PCC DEC MACRO Compiler
 *
 * Symbol table management
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

#define HASH_SIZE 1024

/* Symbol table hash table */
static SYMTAB *symtab[HASH_SIZE];

/*
 * Hash function
 */
static unsigned int
hash(const char *str)
{
	unsigned int h = 0;
	while (*str)
		h = h * 31 + (unsigned char)*str++;
	return h % HASH_SIZE;
}

/*
 * Initialize symbol table
 */
void
symtab_init(void)
{
	int i;
	for (i = 0; i < HASH_SIZE; i++)
		symtab[i] = NULL;
}

/*
 * Look up a symbol
 */
SYMTAB *
lookup(const char *name)
{
	unsigned int h;
	SYMTAB *sp;

	h = hash(name);
	for (sp = symtab[h]; sp != NULL; sp = sp->snext) {
		if (strcmp(sp->sname, name) == 0)
			return sp;
	}
	return NULL;
}

/*
 * Install a new symbol
 */
SYMTAB *
install(const char *name, int class)
{
	unsigned int h;
	SYMTAB *sp;

	/* Check if already exists */
	sp = lookup(name);
	if (sp != NULL) {
		if (sp->sflags & SF_DEFINED) {
			error("symbol '%s' already defined", name);
			return sp;
		}
	} else {
		/* Create new entry */
		sp = (SYMTAB *)malloc(sizeof(SYMTAB));
		if (sp == NULL)
			fatal("out of memory");

		sp->sname = strdup(name);
		if (sp->sname == NULL)
			fatal("out of memory");

		sp->sclass = class;
		sp->svalue = 0;
		sp->sflags = 0;
		sp->lineno = lineno;

		/* Add to hash table */
		h = hash(name);
		sp->snext = symtab[h];
		symtab[h] = sp;
	}

	return sp;
}

/*
 * Define a symbol with a value
 */
void
define_symbol(SYMTAB *sym, long value)
{
	if (sym->sflags & SF_DEFINED) {
		error("symbol '%s' redefined", sym->sname);
		return;
	}
	sym->svalue = value;
	sym->sflags |= SF_DEFINED;
}

/*
 * Export a symbol
 */
void
export_symbol(SYMTAB *sym)
{
	sym->sflags |= SF_EXPORTED;
}

/*
 * Dump symbol table (for debugging)
 */
void
dump_symtab(void)
{
	int i;
	SYMTAB *sp;

	fprintf(stderr, "\nSymbol Table:\n");
	fprintf(stderr, "%-20s %-10s %-10s %s\n",
	        "Name", "Class", "Value", "Flags");
	fprintf(stderr, "------------------------------------------------\n");

	for (i = 0; i < HASH_SIZE; i++) {
		for (sp = symtab[i]; sp != NULL; sp = sp->snext) {
			const char *class_name;
			switch (sp->sclass) {
			case SYM_LABEL:    class_name = "LABEL"; break;
			case SYM_MACRO:    class_name = "MACRO"; break;
			case SYM_CONSTANT: class_name = "CONSTANT"; break;
			case SYM_REGISTER: class_name = "REGISTER"; break;
			case SYM_EXTERNAL: class_name = "EXTERNAL"; break;
			case SYM_GLOBAL:   class_name = "GLOBAL"; break;
			case SYM_LOCAL:    class_name = "LOCAL"; break;
			default:           class_name = "UNKNOWN"; break;
			}

			fprintf(stderr, "%-20s %-10s 0x%08lx %c%c%c\n",
			        sp->sname, class_name, sp->svalue,
			        (sp->sflags & SF_DEFINED) ? 'D' : '-',
			        (sp->sflags & SF_REFERENCED) ? 'R' : '-',
			        (sp->sflags & SF_EXPORTED) ? 'E' : '-');
		}
	}
}
