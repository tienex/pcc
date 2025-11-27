/*
 * Copyright (c) 2025 PCC BASIC Compiler
 *
 * Symbol table management
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "pass1.h"

#define HASH_SIZE 256

/* Symbol table hash buckets */
static SYMTAB *symtab_buckets[HASH_SIZE];

/*
 * Simple hash function
 */
static unsigned int
hash(const char *name)
{
	unsigned int h = 0;
	const char *p = name;

	while (*p) {
		h = (h << 4) + tolower(*p);
		p++;
	}

	return h % HASH_SIZE;
}

/*
 * Initialize symbol table
 */
void
symtab_init(void)
{
	memset(symtab_buckets, 0, sizeof(symtab_buckets));
	blevel = 0;
}

/*
 * Lookup symbol at specific level
 */
SYMTAB *
lookup(char *name, int level)
{
	unsigned int h = hash(name);
	SYMTAB *s;

	for (s = symtab_buckets[h]; s != NULL; s = s->snext) {
		if (strcasecmp(s->sname, name) == 0 && s->slevel == level)
			return s;
	}

	return NULL;
}

/*
 * Find symbol at current or outer levels
 */
SYMTAB *
find_symbol(char *name)
{
	int level;

	/* Search from current level outward */
	for (level = blevel; level >= 0; level--) {
		SYMTAB *s = lookup(name, level);
		if (s != NULL)
			return s;
	}

	return NULL;
}

/*
 * Install new symbol
 */
SYMTAB *
install(char *name, int class, int level)
{
	unsigned int h = hash(name);
	SYMTAB *s;

	/* Check if already exists at this level */
	s = lookup(name, level);
	if (s != NULL)
		return s;  /* Already defined */

	/* Create new symbol */
	s = malloc(sizeof(SYMTAB));
	memset(s, 0, sizeof(SYMTAB));
	s->sname = strdup(name);
	s->sclass = class;
	s->slevel = level;

	/* Add to hash chain */
	s->snext = symtab_buckets[h];
	symtab_buckets[h] = s;

	return s;
}

/*
 * Hide symbols at given level
 */
void
hide(int level)
{
	int i;
	SYMTAB *s, *prev, *next;

	for (i = 0; i < HASH_SIZE; i++) {
		prev = NULL;
		for (s = symtab_buckets[i]; s != NULL; s = next) {
			next = s->snext;
			if (s->slevel == level) {
				/* Remove from chain */
				if (prev == NULL)
					symtab_buckets[i] = next;
				else
					prev->snext = next;
				/* Free symbol (but keep allocated for now) */
			} else {
				prev = s;
			}
		}
	}
}
