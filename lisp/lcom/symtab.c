/*
 * Copyright (c) 2025 PCC Common LISP Compiler
 *
 * Symbol table implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

#define HASH_SIZE 1024

static symbol_t *symbol_table[HASH_SIZE];

/*
 * Simple hash function
 */
static unsigned int
hash(const char *name)
{
	unsigned int h = 0;
	while (*name)
		h = h * 31 + *name++;
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
		symbol_table[i] = NULL;
}

/*
 * Lookup symbol by name
 */
symbol_t *
lookup_symbol(const char *name)
{
	unsigned int h = hash(name);
	symbol_t *sym;

	for (sym = symbol_table[h]; sym != NULL; sym = sym->next) {
		if (strcmp(sym->name, name) == 0)
			return sym;
	}

	return NULL;
}

/*
 * Define a new symbol
 */
symbol_t *
define_symbol(const char *name, lisp_type_t type)
{
	unsigned int h = hash(name);
	symbol_t *sym;

	/* Check if already defined */
	sym = lookup_symbol(name);
	if (sym != NULL) {
		warning_msg("redefinition of '%s'", name);
		return sym;
	}

	/* Create new symbol */
	sym = malloc(sizeof(symbol_t));
	sym->name = strdup(name);
	sym->type = type;
	sym->value = NULL;
	sym->is_function = 0;
	sym->is_global = 1;
	sym->next = symbol_table[h];
	symbol_table[h] = sym;

	return sym;
}

/*
 * Define a function
 */
symbol_t *
define_function(const char *name, lisp_value_t *params, lisp_value_t *body)
{
	symbol_t *sym;

	sym = lookup_symbol(name);
	if (sym != NULL && sym->is_function) {
		warning_msg("redefinition of function '%s'", name);
	} else {
		sym = define_symbol(name, LISP_FUNCTION);
	}

	sym->is_function = 1;
	sym->value = malloc(sizeof(lisp_value_t));
	sym->value->type = LISP_FUNCTION;
	sym->value->value.function.name = strdup(name);
	sym->value->value.function.params = params;
	sym->value->value.function.body = body;

	return sym;
}
