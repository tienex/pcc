/*
 * Copyright (c) 2025 PCC BLISS Compiler
 *
 * Symbol table management
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

#define HASHSIZE 211

static SYMTAB *hashtab[HASHSIZE];

/* Hash function */
static unsigned int hash(const char *s) {
	unsigned int hashval;
	for (hashval = 0; *s != '\0'; s++)
		hashval = *s + 31 * hashval;
	return hashval % HASHSIZE;
}

/* Initialize symbol table */
void symtab_init(void) {
	int i;
	for (i = 0; i < HASHSIZE; i++)
		hashtab[i] = NULL;
}

/* Look up a symbol at a specific level */
SYMTAB *lookup(char *name, int level) {
	SYMTAB *sp;
	for (sp = hashtab[hash(name)]; sp != NULL; sp = sp->snext) {
		if (strcmp(name, sp->sname) == 0 && sp->slevel == level)
			return sp;
	}
	return NULL;
}

/* Find a symbol in any visible scope */
SYMTAB *find_symbol(char *name) {
	SYMTAB *sp;
	int lev;

	/* Search from current level down to global level (0) */
	for (lev = blevel; lev >= 0; lev--) {
		sp = lookup(name, lev);
		if (sp != NULL)
			return sp;
	}
	return NULL;
}

/* Install a symbol */
SYMTAB *install(char *name, int class, int level) {
	SYMTAB *sp;
	unsigned int hashval;

	/* Check if already defined at this level */
	sp = lookup(name, level);
	if (sp != NULL) {
		/* Allow forward references to be redefined */
		if (sp->sclass == SFORWARD && class != SFORWARD) {
			sp->sclass = class;
			return sp;
		}
		error("'%s' already defined at this level", name);
		return sp;
	}

	/* Create new entry */
	sp = (SYMTAB *)malloc(sizeof(SYMTAB));
	if (sp == NULL) {
		error("out of memory");
		exit(1);
	}

	sp->sname = strdup(name);
	sp->sclass = class;
	sp->slevel = level;
	sp->stype = NULL;
	sp->soffset = 0;
	sp->sflags = 0;

	/* Insert at beginning of hash chain */
	hashval = hash(name);
	sp->snext = hashtab[hashval];
	hashtab[hashval] = sp;

	return sp;
}

/* Hide all symbols at a given level (when exiting a scope) */
void hide(int level) {
	SYMTAB *sp, *prev;
	int i;

	for (i = 0; i < HASHSIZE; i++) {
		prev = NULL;
		for (sp = hashtab[i]; sp != NULL;) {
			if (sp->slevel == level) {
				/* Remove from chain */
				if (prev == NULL)
					hashtab[i] = sp->snext;
				else
					prev->snext = sp->snext;

				/* Check for unused symbols */
				if (!(sp->sflags & SUSED) &&
				    sp->sclass != SLABEL &&
				    sp->sclass != SFORWARD) {
					warning("'%s' declared but not used", sp->sname);
				}

				/* Free (or keep for later phases) */
				SYMTAB *next = sp->snext;
				/* Don't actually free for now - may need for debugging */
				sp = next;
			} else {
				prev = sp;
				sp = sp->snext;
			}
		}
	}
}
