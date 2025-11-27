/*
 * Copyright (c) 2025 PCC Go Compiler
 *
 * Symbol table implementation
 * Hash table with scope levels for nested blocks
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

#define HASHSIZE 1024

/* Hash table */
static SYMTAB *hashtab[HASHSIZE];

/* Current scope level */
int blevel = 0;

/*
 * Simple hash function
 */
static unsigned int
hash(const char *str)
{
	unsigned int h = 0;
	const unsigned char *p = (const unsigned char *)str;

	while (*p)
		h = h * 31 + *p++;

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
 * Look up symbol at specific level
 */
SYMTAB *
lookup(char *name, int level)
{
	unsigned int h;
	SYMTAB *sp;

	h = hash(name);
	for (sp = hashtab[h]; sp != NULL; sp = sp->snext) {
		if (sp->slevel == level && strcmp(sp->sname, name) == 0)
			return sp;
	}

	return NULL;
}

/*
 * Find symbol in current or outer scopes
 */
SYMTAB *
find_symbol(char *name)
{
	int level;
	SYMTAB *sp;

	/* Search from current level outward */
	for (level = blevel; level >= 0; level--) {
		sp = lookup(name, level);
		if (sp != NULL)
			return sp;
	}

	return NULL;
}

/*
 * Install new symbol at current level
 */
SYMTAB *
install(char *name, int class, int level)
{
	unsigned int h;
	SYMTAB *sp;

	/* Check if already exists at this level */
	sp = lookup(name, level);
	if (sp != NULL) {
		error("redefinition of '%s'", name);
		return sp;
	}

	/* Allocate new entry */
	sp = (SYMTAB *)xmalloc(sizeof(SYMTAB));
	sp->sname = str_copy(name);
	sp->sclass = class;
	sp->slevel = level;
	sp->stype = NULL;
	sp->soffset = 0;
	sp->sflags = 0;
	sp->pkg_name = NULL;
	sp->sloc = current_loc();

	/* Check if exported (capitalized first letter) */
	if (is_exported(name))
		sp->sflags |= SEXPORTED;

	/* Check for blank identifier */
	if (strcmp(name, "_") == 0)
		sp->sflags |= SBLANK;

	/* Insert at head of hash chain */
	h = hash(name);
	sp->snext = hashtab[h];
	hashtab[h] = sp;

	return sp;
}

/*
 * Hide all symbols at specified level
 * Called when exiting a scope
 */
void
hide(int level)
{
	int i;
	SYMTAB *sp, *prev, *next;

	/* Remove all symbols at this level */
	for (i = 0; i < HASHSIZE; i++) {
		prev = NULL;
		for (sp = hashtab[i]; sp != NULL; sp = next) {
			next = sp->snext;
			if (sp->slevel == level) {
				/* Remove from chain */
				if (prev == NULL)
					hashtab[i] = next;
				else
					prev->snext = next;
				/* Don't free here - may still need in IR */
			} else {
				prev = sp;
			}
		}
	}
}

/*
 * Check if identifier is exported (capitalized)
 */
int
is_exported(char *name)
{
	if (name == NULL || name[0] == '\0')
		return 0;

	/* In Go, exported names start with uppercase */
	return (name[0] >= 'A' && name[0] <= 'Z');
}

/*
 * Utility: Copy string
 */
char *
str_copy(const char *s)
{
	char *p;

	if (s == NULL)
		return NULL;

	p = (char *)xmalloc(strlen(s) + 1);
	strcpy(p, s);
	return p;
}

/*
 * Utility: Allocate memory with error checking
 */
void *
xmalloc(size_t size)
{
	void *p = malloc(size);
	if (p == NULL) {
		fprintf(stderr, "fatal: out of memory\n");
		exit(1);
	}
	return p;
}

void *
xcalloc(size_t nmemb, size_t size)
{
	void *p = calloc(nmemb, size);
	if (p == NULL) {
		fprintf(stderr, "fatal: out of memory\n");
		exit(1);
	}
	return p;
}
