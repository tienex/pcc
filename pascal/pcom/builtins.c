/*
 * Copyright (c) 2025 PCC Pascal Compiler
 *
 * Built-in functions and procedures for Pascal
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "pass1.h"

/* Built-in function/procedure names */
static const char *builtins[] = {
	/* Standard Pascal I/O */
	"read", "readln", "write", "writeln",
	"reset", "rewrite", "get", "put",
	"eof", "eoln", "page",

	/* Standard Pascal functions */
	"abs", "sqr", "sqrt", "sin", "cos", "exp", "ln", "arctan",
	"odd", "ord", "chr", "succ", "pred",
	"round", "trunc",

	/* String functions */
	"length", "concat", "copy", "pos",

	/* Memory functions */
	"new", "dispose",
	"sizeof",

	/* Type conversion */
	"int", "real", "str", "val",

	/* Extended functions (Turbo/Delphi) */
	"inc", "dec",
	"exit", "break", "continue",
	"fillchar", "move",
	"hi", "lo", "swap",
	"upcase", "lowercase",

	/* File functions */
	"assign", "close", "erase", "rename",
	"seek", "filesize", "filepos",
	"flush",

	/* Delphi specific */
	"setlength", "insert", "delete",
	"trim", "trimleft", "trimright",
	"uppercase", "comparetext",

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
		sp = install((char *)*p, FUNC, 0);
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
		if (IS_CASE_SENSITIVE()) {
			if (strcmp(name, *p) == 0)
				return 1;
		} else {
			if (strcasecmp(name, *p) == 0)
				return 1;
		}
	}

	return 0;
}
