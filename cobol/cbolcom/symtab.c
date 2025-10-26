/*
 * COBOL symbol table management
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

#define HASH_SIZE 256

struct scope {
	struct cobsym *symbols[HASH_SIZE];
	struct scope *parent;
};

static struct scope *current_scope = NULL;

static unsigned int
hash(const char *name)
{
	unsigned int h = 0;
	while (*name) {
		h = (h << 4) + toupper(*name++);
		h ^= (h >> 24);
	}
	return h % HASH_SIZE;
}

void
enter_scope(void)
{
	struct scope *s = xmalloc(sizeof(*s));
	memset(s, 0, sizeof(*s));
	s->parent = current_scope;
	current_scope = s;
}

void
exit_scope(void)
{
	if (current_scope)
		current_scope = current_scope->parent;
}

struct cobsym *
lookup(const char *name)
{
	struct scope *s;
	unsigned int h = hash(name);

	for (s = current_scope; s; s = s->parent) {
		struct cobsym *sym;
		for (sym = s->symbols[h]; sym; sym = sym->next) {
			if (strcasecmp(sym->name, name) == 0)
				return sym;
		}
	}
	return NULL;
}

struct cobsym *
install(const char *name, int level)
{
	struct cobsym *sym;
	unsigned int h = hash(name);

	sym = xmalloc(sizeof(*sym));
	memset(sym, 0, sizeof(*sym));

	sym->name = xstrdup(name);
	sym->level = level;
	sym->next = current_scope->symbols[h];
	current_scope->symbols[h] = sym;

	return sym;
}
