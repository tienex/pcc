/*
 * Copyright (c) 2025 PCC PL/I Compiler
 *
 * Built-in functions for PL/I
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Built-in function table */
typedef struct builtin {
	const char *name;
	int type;  /* Return type */
} BUILTIN;

static BUILTIN builtins[] = {
	/* Arithmetic functions */
	{"ABS", TFIXED},
	{"CEIL", TFIXED},
	{"FLOOR", TFIXED},
	{"MAX", TFIXED},
	{"MIN", TFIXED},
	{"MOD", TFIXED},
	{"ROUND", TFIXED},
	{"SIGN", TFIXED},
	{"TRUNC", TFIXED},

	/* Floating-point functions */
	{"ACOS", TFLOAT},
	{"ASIN", TFLOAT},
	{"ATAN", TFLOAT},
	{"ATANH", TFLOAT},
	{"COS", TFLOAT},
	{"COSH", TFLOAT},
	{"ERF", TFLOAT},
	{"ERFC", TFLOAT},
	{"EXP", TFLOAT},
	{"LOG", TFLOAT},
	{"LOG10", TFLOAT},
	{"LOG2", TFLOAT},
	{"SIN", TFLOAT},
	{"SINH", TFLOAT},
	{"SQRT", TFLOAT},
	{"TAN", TFLOAT},
	{"TANH", TFLOAT},

	/* String functions */
	{"INDEX", TFIXED},
	{"LENGTH", TFIXED},
	{"REPEAT", TCHAR},
	{"SUBSTR", TCHAR},
	{"TRIM", TCHAR},
	{"VERIFY", TFIXED},

	/* Bit string functions */
	{"BOOL", TBIT},
	{"HIGH", TBIT},
	{"LOW", TBIT},
	{"STRING", TCHAR},

	/* Array functions */
	{"DIM", TFIXED},
	{"HBOUND", TFIXED},
	{"LBOUND", TFIXED},
	{"SUM", TFIXED},
	{"PROD", TFIXED},

	/* Storage functions */
	{"ADDR", TPOINTER},
	{"ALLOCATION", TFIXED},
	{"NULL", TPOINTER},
	{"SIZE", TFIXED},

	/* Conversion functions */
	{"BINARY", TFIXED},
	{"BIT", TBIT},
	{"CHAR", TCHAR},
	{"DECIMAL", TFIXED},
	{"FIXED", TFIXED},
	{"FLOAT", TFLOAT},
	{"UNSPEC", TBIT},

	/* Condition functions */
	{"DATAFIELD", TCHAR},
	{"ONCHAR", TCHAR},
	{"ONCODE", TFIXED},
	{"ONFILE", TFILE},
	{"ONKEY", TCHAR},
	{"ONSOURCE", TCHAR},

	/* Time functions */
	{"DATE", TCHAR},
	{"DATETIME", TCHAR},
	{"TIME", TCHAR},

	/* PL/M specific */
	{"DOUBLE", TDWORD},
	{"LAST", TFIXED},
	{"LENGTH", TFIXED},
	{"MOVE", TNULL},
	{"ROL", TFIXED},
	{"ROR", TFIXED},
	{"SCL", TFIXED},
	{"SCR", TFIXED},
	{"SHL", TFIXED},
	{"SHR", TFIXED},

	{NULL, 0}
};

/* Initialize built-in functions */
void init_builtins(void) {
	for (int i = 0; builtins[i].name != NULL; i++) {
		SYMTAB *sp = install((char *)builtins[i].name, BUILTIN, 0);
		sp->stype = mktype(builtins[i].type);
		sp->sflags |= SUSED;  /* Mark as used so no warnings */
	}
}

/* Check if identifier is a built-in */
int is_builtin(char *name) {
	for (int i = 0; builtins[i].name != NULL; i++) {
		if (strcasecmp(name, builtins[i].name) == 0) {
			return 1;
		}
	}
	return 0;
}
