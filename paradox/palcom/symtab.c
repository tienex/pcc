/*
 * Copyright (c) 2025 PCC Paradox PAL Compiler
 *
 * Symbol table implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

#define HASH_SIZE 211

/* Symbol table hash table */
static SYMTAB *symtab[HASH_SIZE];

/* Current scope level */
static int current_level = 0;

/* Hash function */
static unsigned int hash(const char *str)
{
	unsigned int hash = 5381;
	int c;

	while ((c = *str++))
		hash = ((hash << 5) + hash) + c;  /* hash * 33 + c */

	return hash % HASH_SIZE;
}

void symtab_init(void)
{
	int i;
	for (i = 0; i < HASH_SIZE; i++) {
		symtab[i] = NULL;
	}
	current_level = 0;
}

SYMTAB *lookup(char *name, int level)
{
	unsigned int h = hash(name);
	SYMTAB *sym;

	for (sym = symtab[h]; sym != NULL; sym = sym->snext) {
		if (sym->slevel == level && strcmp(sym->sname, name) == 0) {
			return sym;
		}
	}

	return NULL;
}

SYMTAB *install(char *name, int class, int level)
{
	unsigned int h = hash(name);
	SYMTAB *sym;

	/* Check if already exists at this level */
	sym = lookup(name, level);
	if (sym != NULL) {
		error("Symbol '%s' already declared at this scope", name);
		return sym;
	}

	/* Allocate new symbol */
	sym = (SYMTAB *)malloc(sizeof(SYMTAB));
	if (!sym) {
		fatal("Out of memory");
	}

	sym->sname = strdup(name);
	sym->sclass = class;
	sym->slevel = level;
	sym->stype = NULL;
	sym->soffset = 0;
	sym->sflags = 0;
	sym->sloc = current_loc;
	sym->sowner = NULL;

	/* Insert at head of hash chain */
	sym->snext = symtab[h];
	symtab[h] = sym;

	return sym;
}

void hide(int level)
{
	int i;
	SYMTAB *sym, *prev, *next;

	/* Remove all symbols at the given level */
	for (i = 0; i < HASH_SIZE; i++) {
		prev = NULL;
		for (sym = symtab[i]; sym != NULL; sym = next) {
			next = sym->snext;
			if (sym->slevel == level) {
				/* Remove from chain */
				if (prev == NULL) {
					symtab[i] = next;
				} else {
					prev->snext = next;
				}
				/* Free symbol (but not in this simple version) */
				/* free(sym->sname); */
				/* free(sym); */
			} else {
				prev = sym;
			}
		}
	}
}

SYMTAB *find_symbol(char *name)
{
	unsigned int h = hash(name);
	SYMTAB *sym;
	int level;

	/* Search from current level down to global level */
	for (level = current_level; level >= 0; level--) {
		for (sym = symtab[h]; sym != NULL; sym = sym->snext) {
			if (sym->slevel == level && strcmp(sym->sname, name) == 0) {
				return sym;
			}
		}
	}

	return NULL;
}
