/*
 * Copyright (c) 2025 PCC Go Compiler
 *
 * Built-in functions and types for Go
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Built-in function names */
static const char *builtins[] = {
	/* Memory allocation */
	"make",
	"new",

	/* Container operations */
	"len",
	"cap",
	"append",
	"copy",
	"delete",

	/* Error handling */
	"panic",
	"recover",

	/* I/O */
	"print",
	"println",

	/* Complex numbers */
	"complex",
	"real",
	"imag",

	/* Type conversion */
	"close",

	NULL
};

/* Built-in type names */
static const char *builtin_types[] = {
	"bool",
	"byte",      /* alias for uint8 */
	"rune",      /* alias for int32 */
	"int",
	"int8",
	"int16",
	"int32",
	"int64",
	"uint",
	"uint8",
	"uint16",
	"uint32",
	"uint64",
	"uintptr",
	"float32",
	"float64",
	"complex64",
	"complex128",
	"string",
	"error",     /* built-in interface */
	NULL
};

/*
 * Initialize built-in functions and types
 */
void
init_builtins(void)
{
	int i;
	SYMTAB *sp;

	/* Install built-in functions at level 0 (global) */
	for (i = 0; builtins[i] != NULL; i++) {
		sp = install((char *)builtins[i], FUNC, 0);
		if (sp != NULL) {
			sp->sflags |= SUSED;  /* Built-ins are always "used" */
		}
	}

	/* Install built-in type names */
	for (i = 0; builtin_types[i] != NULL; i++) {
		sp = install((char *)builtin_types[i], TYPENAME, 0);
		if (sp != NULL) {
			sp->sflags |= SUSED;

			/* Set type information */
			if (strcmp(builtin_types[i], "bool") == 0)
				sp->stype = mktype(TBOOL);
			else if (strcmp(builtin_types[i], "byte") == 0)
				sp->stype = mktype(TUINT8);
			else if (strcmp(builtin_types[i], "rune") == 0)
				sp->stype = mktype(TINT32);
			else if (strcmp(builtin_types[i], "int") == 0)
				sp->stype = mktype(TINT);
			else if (strcmp(builtin_types[i], "int8") == 0)
				sp->stype = mktype(TINT8);
			else if (strcmp(builtin_types[i], "int16") == 0)
				sp->stype = mktype(TINT16);
			else if (strcmp(builtin_types[i], "int32") == 0)
				sp->stype = mktype(TINT32);
			else if (strcmp(builtin_types[i], "int64") == 0)
				sp->stype = mktype(TINT64);
			else if (strcmp(builtin_types[i], "uint") == 0)
				sp->stype = mktype(TUINT);
			else if (strcmp(builtin_types[i], "uint8") == 0)
				sp->stype = mktype(TUINT8);
			else if (strcmp(builtin_types[i], "uint16") == 0)
				sp->stype = mktype(TUINT16);
			else if (strcmp(builtin_types[i], "uint32") == 0)
				sp->stype = mktype(TUINT32);
			else if (strcmp(builtin_types[i], "uint64") == 0)
				sp->stype = mktype(TUINT64);
			else if (strcmp(builtin_types[i], "uintptr") == 0)
				sp->stype = mktype(TUINTPTR);
			else if (strcmp(builtin_types[i], "float32") == 0)
				sp->stype = mktype(TFLOAT32);
			else if (strcmp(builtin_types[i], "float64") == 0)
				sp->stype = mktype(TFLOAT64);
			else if (strcmp(builtin_types[i], "complex64") == 0)
				sp->stype = mktype(TCOMPLEX64);
			else if (strcmp(builtin_types[i], "complex128") == 0)
				sp->stype = mktype(TCOMPLEX128);
			else if (strcmp(builtin_types[i], "string") == 0)
				sp->stype = mktype(TSTRING);
		}
	}

	/* Install predeclared constants */
	sp = install("true", CONST, 0);
	if (sp != NULL) {
		sp->stype = mktype(TBOOL);
		sp->sflags |= SUSED;
	}

	sp = install("false", CONST, 0);
	if (sp != NULL) {
		sp->stype = mktype(TBOOL);
		sp->sflags |= SUSED;
	}

	sp = install("nil", CONST, 0);
	if (sp != NULL) {
		sp->sflags |= SUSED;
	}

	sp = install("iota", CONST, 0);
	if (sp != NULL) {
		sp->stype = mktype(TINT);
		sp->sflags |= SUSED;
	}
}

/*
 * Check if name is a built-in function
 */
int
is_builtin(char *name)
{
	int i;

	for (i = 0; builtins[i] != NULL; i++) {
		if (strcmp(name, builtins[i]) == 0)
			return 1;
	}

	return 0;
}
