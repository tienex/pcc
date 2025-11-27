/*
 * Copyright (c) 2025 PCC CHILL Compiler
 *
 * Symbol table implementation for CHILL
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

#define HASHSIZE 211

static SYMTAB *hashtab[HASHSIZE];

/*
 * Hash function
 */
static unsigned int
hash(const char *s)
{
	unsigned int h = 0;
	while (*s)
		h = h * 9 + *s++;
	return h % HASHSIZE;
}

/*
 * Initialize symbol table
 */
void
symtab_init(void)
{
	int i;
	for (i = 0; i < HASHSIZE; i++)
		hashtab[i] = NULL;
}

/*
 * Lookup symbol at a specific level
 */
SYMTAB *
lookup(const char *name, int level)
{
	unsigned int h = hash(name);
	SYMTAB *sp;

	for (sp = hashtab[h]; sp != NULL; sp = sp->snext) {
		if (strcmp(sp->sname, name) == 0 && sp->slevel == level)
			return sp;
	}
	return NULL;
}

/*
 * Find symbol in current or enclosing scopes
 */
SYMTAB *
find_symbol(const char *name)
{
	unsigned int h = hash(name);
	SYMTAB *sp;
	int max_level = -1;
	SYMTAB *found = NULL;

	for (sp = hashtab[h]; sp != NULL; sp = sp->snext) {
		if (strcmp(sp->sname, name) == 0 && sp->slevel > max_level) {
			found = sp;
			max_level = sp->slevel;
		}
	}
	return found;
}

/*
 * Install symbol in symbol table
 */
SYMTAB *
install(const char *name, int sclass, int level)
{
	unsigned int h = hash(name);
	SYMTAB *sp;

	/* Check if already exists at this level */
	sp = lookup(name, level);
	if (sp != NULL) {
		error("redeclaration of '%s'", name);
		return sp;
	}

	/* Allocate new entry */
	sp = (SYMTAB *)malloc(sizeof(SYMTAB));
	if (sp == NULL) {
		error("out of memory");
		exit(1);
	}

	/* Initialize */
	sp->sname = strdup(name);
	sp->sclass = sclass;
	sp->slevel = level;
	sp->smode = NULL;
	sp->soffset = 0;
	sp->sflags = 0;
	sp->sline = lineno;

	/* Add to hash table */
	sp->snext = hashtab[h];
	hashtab[h] = sp;

	return sp;
}

/*
 * Hide symbols at a given level (when exiting scope)
 */
void
hide(int level)
{
	int i;
	SYMTAB *sp, *prev, *next;

	for (i = 0; i < HASHSIZE; i++) {
		prev = NULL;
		for (sp = hashtab[i]; sp != NULL; sp = next) {
			next = sp->snext;
			if (sp->slevel >= level) {
				/* Remove from chain */
				if (prev == NULL)
					hashtab[i] = next;
				else
					prev->snext = next;
				/* Free symbol */
				free(sp->sname);
				free(sp);
			} else {
				prev = sp;
			}
		}
	}
}
