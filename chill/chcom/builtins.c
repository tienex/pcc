/*
 * Copyright (c) 2025 PCC CHILL Compiler
 *
 * Built-in procedures and functions for CHILL
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Built-in function names */
static const char *builtins[] = {
	/* I/O procedures */
	"READ",
	"WRITE",
	"READTEXT",
	"WRITETEXT",

	/* String functions */
	"LENGTH",
	"UPPER",
	"LOWER",
	"SUBSTR",

	/* Mathematical functions */
	"ABS",
	"SIGN",
	"MAX",
	"MIN",
	"SIN",
	"COS",
	"TAN",
	"SQRT",
	"EXP",
	"LN",
	"LOG",

	/* Type conversion */
	"NUM",
	"PRED",
	"SUCC",

	/* Process control */
	"START",
	"STOP",
	"SEND",
	"RECEIVE",
	"DELAY",

	/* Memory management */
	"ALLOCATE",
	"FREE",

	NULL
};

/*
 * Initialize built-in procedures and functions
 */
void
init_builtins(void)
{
	int i;

	/* Install built-ins in global scope */
	for (i = 0; builtins[i] != NULL; i++) {
		SYMTAB *sp = install(builtins[i], SPROC, 0);
		sp->sflags |= SEXTERNAL;
	}
}

/*
 * Check if a name is a built-in function
 */
int
is_builtin(const char *name)
{
	int i;

	for (i = 0; builtins[i] != NULL; i++) {
		if (strcmp(builtins[i], name) == 0)
			return 1;
	}
	return 0;
}
