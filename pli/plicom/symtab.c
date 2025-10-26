/*
 * Copyright (c) 2025 PCC PL/I Compiler
 *
 * Symbol table management
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

#define HASH_SIZE 512

/* Symbol table hash table */
static SYMTAB *symtab[HASH_SIZE];

/* Current block nesting level */
int blevel = 0;

/* Global variables */
int lineno = 1;
char *ftitle = "<unknown>";
FILE *outfile = NULL;

/* Hash function */
static unsigned int hash(const char *str) {
	unsigned int h = 0;
	while (*str) {
		h = (h << 4) + *str++;
		unsigned int g = h & 0xf0000000;
		if (g) {
			h ^= g >> 24;
			h ^= g;
		}
	}
	return h % HASH_SIZE;
}

/* Initialize symbol table */
void symtab_init(void) {
	memset(symtab, 0, sizeof(symtab));
	blevel = 0;
}

/* Lookup symbol at specific level */
SYMTAB *lookup(char *name, int level) {
	unsigned int h = hash(name);
	SYMTAB *sp;

	for (sp = symtab[h]; sp != NULL; sp = sp->snext) {
		if (strcmp(sp->sname, name) == 0 && sp->slevel == level) {
			return sp;
		}
	}
	return NULL;
}

/* Find symbol (search from current level upwards) */
SYMTAB *find_symbol(char *name) {
	for (int level = blevel; level >= 0; level--) {
		SYMTAB *sp = lookup(name, level);
		if (sp != NULL) {
			return sp;
		}
	}
	return NULL;
}

/* Install symbol in table */
SYMTAB *install(char *name, int class, int level) {
	unsigned int h = hash(name);
	SYMTAB *sp;

	/* Check if already exists at this level */
	sp = lookup(name, level);
	if (sp != NULL) {
		if (sp->sclass != SNULL) {
			error("redeclaration of '%s'", name);
			return sp;
		}
		/* Update existing entry */
		sp->sclass = class;
		return sp;
	}

	/* Create new entry */
	sp = malloc(sizeof(SYMTAB));
	if (sp == NULL) {
		fatal("out of memory");
	}

	sp->sname = strdup(name);
	sp->sclass = class;
	sp->slevel = level;
	sp->stype = NULL;
	sp->soffset = 0;
	sp->sflags = 0;
	sp->sattr = 0;
	sp->sloc = make_loc();

	/* Insert at head of chain */
	sp->snext = symtab[h];
	symtab[h] = sp;

	return sp;
}

/* Hide symbols at given level */
void hide(int level) {
	for (int i = 0; i < HASH_SIZE; i++) {
		SYMTAB **spp = &symtab[i];
		while (*spp != NULL) {
			if ((*spp)->slevel >= level) {
				SYMTAB *sp = *spp;
				*spp = sp->snext;
				/* Could free here, but we keep for debugging */
				/* free(sp->sname); free(sp); */
			} else {
				spp = &(*spp)->snext;
			}
		}
	}
}

/* Enter new scope */
void enter_scope(void) {
	blevel++;
}

/* Exit scope */
void exit_scope(void) {
	hide(blevel);
	blevel--;
}
