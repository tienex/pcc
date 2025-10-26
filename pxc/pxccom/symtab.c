/*
 * Copyright (c) 2025 PCC Xbase++ Compiler
 *
 * Symbol table management
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Hash table size */
#define HASHSIZE 1024

/* Symbol table hash table */
static SYMTAB *hashtab[HASHSIZE];

/* Current scope level */
static int scope_level = 0;

/*
 * Hash function
 */
static unsigned int
hash(const char *s)
{
	unsigned int hashval;

	for (hashval = 0; *s != '\0'; s++)
		hashval = *s + 31 * hashval;
	return hashval % HASHSIZE;
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

	scope_level = 0;
}

/*
 * Lookup symbol in symbol table
 * Returns NULL if not found
 */
SYMTAB *
lookup(char *name, int level)
{
	SYMTAB *sp;
	unsigned int h;

	h = hash(name);
	for (sp = hashtab[h]; sp != NULL; sp = sp->snext) {
		if (strcmp(name, sp->sname) == 0) {
			/* Found - check scope level */
			if (level == -1 || sp->slevel <= level)
				return sp;
		}
	}
	return NULL;
}

/*
 * Install symbol in symbol table
 * Returns pointer to symbol table entry
 */
SYMTAB *
install(char *name, int sclass, TNODE *type, int level)
{
	SYMTAB *sp;
	unsigned int h;

	/* Check if already exists at this level */
	sp = lookup(name, level);
	if (sp != NULL && sp->slevel == level) {
		/* Redefinition at same level */
		error("redefinition of '%s'", name);
		return sp;
	}

	/* Allocate new symbol table entry */
	sp = (SYMTAB *)malloc(sizeof(SYMTAB));
	if (sp == NULL) {
		error("out of memory");
		exit(1);
	}

	/* Fill in fields */
	sp->sname = strdup(name);
	sp->sclass = sclass;
	sp->slevel = level;
	sp->stype = type;
	sp->soffset = 0;
	sp->sflags = 0;
	sp->sline = lineno;

	/* Add to hash table */
	h = hash(name);
	sp->snext = hashtab[h];
	hashtab[h] = sp;

	return sp;
}

/*
 * Enter new scope level
 */
void
enter_scope(void)
{
	scope_level++;
}

/*
 * Exit scope level and remove symbols at current level
 */
void
exit_scope(void)
{
	int i;
	SYMTAB *sp, *prev, *next;

	/* Remove symbols at current scope level */
	for (i = 0; i < HASHSIZE; i++) {
		prev = NULL;
		for (sp = hashtab[i]; sp != NULL; sp = next) {
			next = sp->snext;
			if (sp->slevel == scope_level) {
				/* Remove this entry */
				if (prev == NULL)
					hashtab[i] = next;
				else
					prev->snext = next;

				/* Check for unused symbols */
				if (!(sp->sflags & SUSED) && sp->sclass != S_LABEL) {
					warning("unused variable '%s'", sp->sname);
				}

				/* Free memory */
				free(sp->sname);
				free(sp);
			} else {
				prev = sp;
			}
		}
	}

	scope_level--;
}

/*
 * Get current scope level
 */
int
get_scope_level(void)
{
	return scope_level;
}
