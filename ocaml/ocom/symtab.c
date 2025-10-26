/*
 * Copyright (c) 2025 PCC OCaml Compiler
 *
 * Symbol table implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

#define HASH_SIZE 211

/* Symbol table hash table */
static struct symtab *hashtab[HASH_SIZE];

/* Scope management */
#define MAX_SCOPES 100
static struct symtab *scope_stack[MAX_SCOPES];
static int scope_level = 0;

/*
 * Hash function for symbol names
 */
static unsigned int
hash(const char *name)
{
	unsigned int h = 0;
	const char *p;

	for (p = name; *p != '\0'; p++)
		h = h * 31 + *p;

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
		hashtab[i] = NULL;

	scope_level = 0;
}

/*
 * Lookup symbol in symbol table
 */
struct symtab *
lookup(const char *name)
{
	unsigned int h;
	struct symtab *sym;

	h = hash(name);

	for (sym = hashtab[h]; sym != NULL; sym = sym->next) {
		if (strcmp(sym->name, name) == 0)
			return sym;
	}

	return NULL;
}

/*
 * Install symbol in symbol table
 */
struct symtab *
install(const char *name, int type)
{
	unsigned int h;
	struct symtab *sym;

	/* Check if already exists */
	sym = lookup(name);
	if (sym != NULL) {
		warning("redefinition of '%s'", name);
		return sym;
	}

	/* Create new symbol */
	sym = calloc(1, sizeof(struct symtab));
	if (sym == NULL)
		fatal("out of memory");

	sym->name = strdup(name);
	sym->type = type;
	sym->typeinfo = NULL;

	/* Add to hash table */
	h = hash(name);
	sym->next = hashtab[h];
	hashtab[h] = sym;

	return sym;
}

/*
 * Enter a new scope
 */
void
enter_scope(void)
{
	if (scope_level >= MAX_SCOPES)
		fatal("scope nesting too deep");

	scope_stack[scope_level++] = NULL;
}

/*
 * Exit current scope
 */
void
exit_scope(void)
{
	if (scope_level <= 0)
		fatal("scope underflow");

	scope_level--;
}
