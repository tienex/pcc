/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart symbol table management
 */

#include "pass1.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HASH_SIZE 256

typedef struct scope {
	Symbol *symbols[HASH_SIZE];
	struct scope *parent;
	int level;
} Scope;

static Scope *current_scope = NULL;
static int scope_level = 0;

static unsigned int
hash(const char *str)
{
	unsigned int hash = 0;
	while (*str) {
		hash = (hash << 5) + hash + *str++;
	}
	return hash % HASH_SIZE;
}

void
symtab_init(void)
{
	current_scope = malloc(sizeof(Scope));
	memset(current_scope->symbols, 0, sizeof(current_scope->symbols));
	current_scope->parent = NULL;
	current_scope->level = 0;
	scope_level = 0;
}

void
symtab_enter_scope(void)
{
	Scope *new_scope = malloc(sizeof(Scope));
	memset(new_scope->symbols, 0, sizeof(new_scope->symbols));
	new_scope->parent = current_scope;
	new_scope->level = ++scope_level;
	current_scope = new_scope;
}

void
symtab_exit_scope(void)
{
	if (current_scope == NULL || current_scope->parent == NULL) {
		fprintf(stderr, "symtab_exit_scope: no scope to exit\n");
		return;
	}

	Scope *old_scope = current_scope;
	current_scope = current_scope->parent;
	scope_level--;

	/* Free all symbols in the exiting scope */
	for (int i = 0; i < HASH_SIZE; i++) {
		Symbol *sym = old_scope->symbols[i];
		while (sym) {
			Symbol *next = sym->next;
			free(sym->name);
			free(sym);
			sym = next;
		}
	}
	free(old_scope);
}

Symbol *
symtab_lookup(const char *name)
{
	Scope *scope = current_scope;
	unsigned int h = hash(name);

	while (scope) {
		Symbol *sym = scope->symbols[h];
		while (sym) {
			if (strcmp(sym->name, name) == 0) {
				return sym;
			}
			sym = sym->next;
		}
		scope = scope->parent;
	}
	return NULL;
}

Symbol *
symtab_lookup_current(const char *name)
{
	if (current_scope == NULL) {
		return NULL;
	}

	unsigned int h = hash(name);
	Symbol *sym = current_scope->symbols[h];
	while (sym) {
		if (strcmp(sym->name, name) == 0) {
			return sym;
		}
		sym = sym->next;
	}
	return NULL;
}

Symbol *
symtab_insert(const char *name, DartTypeKind type)
{
	if (current_scope == NULL) {
		symtab_init();
	}

	/* Check if symbol already exists in current scope */
	Symbol *existing = symtab_lookup_current(name);
	if (existing) {
		return existing;  /* Already exists */
	}

	unsigned int h = hash(name);
	Symbol *sym = malloc(sizeof(Symbol));
	sym->name = strdup(name);
	sym->type = type;
	sym->scope_level = scope_level;
	sym->type_info = NULL;
	sym->next = current_scope->symbols[h];
	current_scope->symbols[h] = sym;

	return sym;
}

void
symtab_dump(void)
{
	Scope *scope = current_scope;
	int level = scope_level;

	while (scope) {
		printf("Scope level %d:\n", level);
		for (int i = 0; i < HASH_SIZE; i++) {
			Symbol *sym = scope->symbols[i];
			while (sym) {
				printf("  %s: type=%d\n", sym->name, sym->type);
				sym = sym->next;
			}
		}
		scope = scope->parent;
		level--;
	}
}
