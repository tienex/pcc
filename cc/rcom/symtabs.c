/*	$Id$	*/

/*
 * Ruby symbol table management
 *
 * Implements symbol table for Ruby identifiers
 */

#include "pass1.h"
#include <stdlib.h>
#include <string.h>

#define SYMHASHSZ 256

static struct symtab *symtab_hash[SYMHASHSZ];

/* Hash function for symbol names */
static unsigned int
symhash(const char *name)
{
	unsigned int h = 0;
	while (*name)
		h = (h << 4) + *name++;
	return h % SYMHASHSZ;
}

/* Look up a symbol in the symbol table */
struct symtab *
lookup(char *name, int flags)
{
	struct symtab *sp;
	unsigned int h;

	if (name == NULL)
		return NULL;

	h = symhash(name);

	/* Search for existing symbol */
	for (sp = symtab_hash[h]; sp != NULL; sp = sp->snext) {
		if (strcmp(sp->sname, name) == 0)
			return sp;
	}

	/* Create new symbol if not found */
	if (!(flags & SNOCREAT)) {
		sp = calloc(1, sizeof(struct symtab));
		if (sp == NULL) {
			fprintf(stderr, "out of memory\n");
			exit(1);
		}

		sp->sname = strdup(name);
		sp->stype = 0;
		sp->sclass = SNULL;
		sp->slevel = blevel;
		sp->sflags = flags & SMASK;
		sp->soffset = 0;
		sp->sdf = NULL;
		sp->sap = NULL;
		sp->sblock = NULL;

		/* Link into hash table */
		sp->snext = symtab_hash[h];
		symtab_hash[h] = sp;
	} else {
		sp = NULL;
	}

	return sp;
}

/* Return symbol class name for debugging */
char *
scnames(int class)
{
	static char *names[] = {
		"SNULL",
		"AUTO",
		"EXTERN",
		"STATIC",
		"REGISTER",
		"EXTDEF",
		"THLOCAL",
		"KEYWORD",
		"MOS",
		"PARAM",
		"STNAME",
		"MOU",
		"UNAME",
		"TYPEDEF",
		"",
		"ENAME",
		"MOE",
		"",
		"USTATIC",
		"RUBY_IVAR",
		"RUBY_CVAR",
		"RUBY_GVAR",
	};

	if (class >= 0 && class < sizeof(names)/sizeof(names[0]))
		return names[class];
	return "UNKNOWN";
}

/* Clear symbols at a given scope level */
void
clearsymtab(int level)
{
	struct symtab *sp, *prev;
	int i;

	for (i = 0; i < SYMHASHSZ; i++) {
		prev = NULL;
		sp = symtab_hash[i];
		while (sp != NULL) {
			if (sp->slevel >= level) {
				/* Remove this symbol */
				if (prev == NULL)
					symtab_hash[i] = sp->snext;
				else
					prev->snext = sp->snext;

				/* Free symbol resources */
				if (sp->sname)
					free(sp->sname);
				if (sp->sblock)
					free(sp->sblock);
				free(sp);

				/* Continue with next */
				if (prev == NULL)
					sp = symtab_hash[i];
				else
					sp = prev->snext;
			} else {
				prev = sp;
				sp = sp->snext;
			}
		}
	}
}
