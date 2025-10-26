/*
 * Copyright (c) 2025 PCC ALGOL 60+ Compiler
 *
 * Built-in functions and procedures for ALGOL 60+
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Built-in function names (ALGOL 60 standard transfer functions) */
static const char *builtins[] = {
	/* Arithmetic functions */
	"abs",        /* absolute value */
	"sign",       /* sign of argument */
	"sqrt",       /* square root */
	"sin",        /* sine */
	"cos",        /* cosine */
	"arctan",     /* arctangent */
	"ln",         /* natural logarithm */
	"exp",        /* exponential */
	"entier",     /* floor function (greatest integer <= x) */

	/* I/O procedures (extension) */
	"read",       /* read input */
	"write",      /* write output */
	"print",      /* print (alias for write) */

	NULL
};

/*
 * Initialize built-in functions and procedures
 */
void
init_builtins(void)
{
	const char **p;
	SYMTAB *sp;

	for (p = builtins; *p != NULL; p++) {
		/* Install as built-in function at level 0 */
		sp = install((char *)*p, FUNC, NULL);
		if (sp != NULL) {
			sp->sflags |= SUSED;  /* Don't warn about unused builtins */
		}
	}
}

/*
 * Check if identifier is a built-in
 */
int
is_builtin(char *name)
{
	const char **p;

	for (p = builtins; *p != NULL; p++) {
		if (strcmp(name, *p) == 0)
			return 1;
	}

	return 0;
}

/*
 * Find built-in symbol
 */
SYMTAB *
find_builtin(char *name)
{
	if (is_builtin(name))
		return lookup(name);
	return NULL;
}
