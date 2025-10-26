/*
 * Copyright (c) 2025 PCC Pascal Compiler
 *
 * Symbol table management
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
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
		char c = *p;
		/* Convert to lowercase if not case sensitive */
		if (!IS_CASE_SENSITIVE() && c >= 'A' && c <= 'Z')
			c = c - 'A' + 'a';
		h = h * 31 + c;
	}

	return h % HASHSIZE;
}

/*
 * Compare symbol names (case sensitive or not depending on dialect)
 */
static int
symcmp(const char *s1, const char *s2)
{
	if (IS_CASE_SENSITIVE())
		return strcmp(s1, s2);
	else
		return strcasecmp(s1, s2);
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
 * Look up symbol at specific level
 */
SYMTAB *
lookup(char *name, int level)
{
	SYMTAB *sp;
	unsigned int h;

	h = hash(name);
	for (sp = hashtab[h]; sp != NULL; sp = sp->snext) {
		if (sp->slevel == level && symcmp(sp->sname, name) == 0)
			return sp;
	}

	return NULL;
}

/*
 * Find symbol in any enclosing scope
 */
SYMTAB *
find_symbol(char *name)
{
	SYMTAB *sp;
	unsigned int h;
	int maxlevel = -1;
	SYMTAB *found = NULL;

	h = hash(name);
	for (sp = hashtab[h]; sp != NULL; sp = sp->snext) {
		if (symcmp(sp->sname, name) == 0) {
			/* Find the one with highest level (most local) */
			if (sp->slevel > maxlevel) {
				maxlevel = sp->slevel;
				found = sp;
			}
		}
	}

	return found;
}

/*
 * Install new symbol
 */
SYMTAB *
install(char *name, int class, int level)
{
	SYMTAB *sp;
	unsigned int h;

	/* Check for redefinition at this level */
	sp = lookup(name, level);
	if (sp != NULL) {
		error("redefinition of '%s'", name);
		note_at(sp->sloc, "previous definition was here");
		return sp;
	}

	/* Create new symbol */
	sp = calloc(1, sizeof(SYMTAB));
	if (sp == NULL)
		fatal("out of memory");

	sp->sname = strdup(name);
	sp->sclass = class;
	sp->slevel = level;
	sp->stype = NULL;
	sp->soffset = 0;
	sp->sflags = 0;
	sp->sloc = make_loc();

	/* Add to hash table */
	h = hash(name);
	sp->snext = hashtab[h];
	hashtab[h] = sp;

	return sp;
}

/*
 * Hide symbols at given level (when exiting scope)
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

			if (sp->slevel == level) {
				/* Check for unused symbols */
				if (warn_unused && !(sp->sflags & SUSED) &&
				    (sp->sclass == AUTO || sp->sclass == PARAM)) {
					warning_at(sp->sloc, "unused variable '%s'",
					    sp->sname);
				}

				/* Remove from hash chain */
				if (prev == NULL)
					hashtab[i] = next;
				else
					prev->snext = next;

				/* Don't free yet - may be referenced in AST */
			} else {
				prev = sp;
			}
		}
	}
}
