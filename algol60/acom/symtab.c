/*
 * Copyright (c) 2025 PCC ALGOL 60+ Compiler
 *
 * Symbol table management
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

#define HASHSIZE 509

/* Hash table */
static SYMTAB *hashtab[HASHSIZE];

/* Current block level */
int blevel = 0;

/*
 * Hash function for symbol names
 */
static unsigned int
hash(const char *name)
{
	unsigned int h = 0;
	const char *p;

	for (p = name; *p != '\0'; p++) {
		h = h * 31 + *p;
	}

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

	blevel = 0;
}

/*
 * Push new scope (enter block)
 */
void
push_scope(void)
{
	blevel++;
}

/*
 * Pop scope (exit block)
 * Remove all symbols at current level
 */
void
pop_scope(void)
{
	int i;
	SYMTAB *sp, *prev, *next;

	/* Remove all symbols at current level from hash table */
	for (i = 0; i < HASHSIZE; i++) {
		prev = NULL;
		sp = hashtab[i];
		while (sp != NULL) {
			next = sp->snext;
			if (sp->slevel == blevel) {
				/* Remove this symbol */
				if (prev == NULL)
					hashtab[i] = next;
				else
					prev->snext = next;
				/* Free symbol (could warn about unused here) */
				free(sp->sname);
				free(sp);
			} else {
				prev = sp;
			}
			sp = next;
		}
	}

	blevel--;
}

/*
 * Look up symbol (search from current level upward)
 */
SYMTAB *
lookup(char *name)
{
	SYMTAB *sp;
	unsigned int h;
	int maxlevel = -1;
	SYMTAB *found = NULL;

	h = hash(name);
	for (sp = hashtab[h]; sp != NULL; sp = sp->snext) {
		if (strcmp(sp->sname, name) == 0) {
			/* Find the one with highest level (most local) that is <= current level */
			if (sp->slevel <= blevel && sp->slevel > maxlevel) {
				maxlevel = sp->slevel;
				found = sp;
			}
		}
	}

	return found;
}

/*
 * Install new symbol at current level
 */
SYMTAB *
install(char *name, int sclass, TNODE *type)
{
	SYMTAB *sp;
	unsigned int h;

	/* Check for redefinition at current level */
	h = hash(name);
	for (sp = hashtab[h]; sp != NULL; sp = sp->snext) {
		if (sp->slevel == blevel && strcmp(sp->sname, name) == 0) {
			error("redeclaration of '%s'", name);
			return sp;
		}
	}

	/* Create new symbol */
	sp = malloc(sizeof(SYMTAB));
	if (sp == NULL) {
		fatal("out of memory");
	}

	sp->sname = strdup(name);
	sp->sclass = sclass;
	sp->slevel = blevel;
	sp->stype = type;
	sp->soffset = 0;
	sp->sflags = 0;
	sp->sloc = make_loc();

	/* Add to hash table at front of chain */
	sp->snext = hashtab[h];
	hashtab[h] = sp;

	return sp;
}
