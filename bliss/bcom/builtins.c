/*
 * Copyright (c) 2025 PCC BLISS Compiler
 *
 * Built-in Functions Support
 * BLISS language built-in functions and primitives
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/*
 * BLISS built-in functions and primitives
 * These are language-level constructs, not library functions
 */

struct builtin {
	const char *name;
	int code;
	const char *description;
};

/* Built-in function codes */
#define BI_CH$PTR           1   /* Character pointer operations */
#define BI_CH$ALLOCATION    2   /* Character allocation */
#define BI_CH$A_RCHAR       3   /* Read character and advance */
#define BI_CH$A_WCHAR       4   /* Write character and advance */
#define BI_CH$RCHAR         5   /* Read character */
#define BI_CH$WCHAR         6   /* Write character */
#define BI_CH$MOVE          7   /* Move characters */
#define BI_CH$FILL          8   /* Fill characters */
#define BI_CH$COMPARE       9   /* Compare characters */
#define BI_CH$EQL           10  /* Character string equality */
#define BI_CH$NEQ           11  /* Character string inequality */
#define BI_CH$LSS           12  /* Character string less than */
#define BI_CH$LEQ           13  /* Character string less or equal */
#define BI_CH$GTR           14  /* Character string greater */
#define BI_CH$GEQ           15  /* Character string greater or equal */
#define BI_CH$TRANSLATE     16  /* Translate characters */

#define BI_NULLPARAMETER    20  /* NULL parameter */
#define BI_ACTUALCOUNT      21  /* Actual parameter count */
#define BI_ACTUALPARAMETER  22  /* Actual parameter */

#define BI_SIGN             30  /* Sign of value */
#define BI_ABS              31  /* Absolute value */
#define BI_MIN              32  /* Minimum */
#define BI_MAX              33  /* Maximum */
#define BI_ROT              34  /* Rotate */
#define BI_FFS              35  /* Find first set */
#define BI_FFC              36  /* Find first clear */

static struct builtin builtins[] = {
	/* Character string primitives */
	{ "CH$PTR", BI_CH$PTR, "Create character pointer" },
	{ "CH$ALLOCATION", BI_CH$ALLOCATION, "Character string allocation size" },
	{ "CH$A_RCHAR", BI_CH$A_RCHAR, "Read character and advance pointer" },
	{ "CH$A_WCHAR", BI_CH$A_WCHAR, "Write character and advance pointer" },
	{ "CH$RCHAR", BI_CH$RCHAR, "Read character" },
	{ "CH$WCHAR", BI_CH$WCHAR, "Write character" },
	{ "CH$MOVE", BI_CH$MOVE, "Move character string" },
	{ "CH$FILL", BI_CH$FILL, "Fill character string" },
	{ "CH$COMPARE", BI_CH$COMPARE, "Compare character strings" },
	{ "CH$EQL", BI_CH$EQL, "Character string equal" },
	{ "CH$NEQ", BI_CH$NEQ, "Character string not equal" },
	{ "CH$LSS", BI_CH$LSS, "Character string less than" },
	{ "CH$LEQ", BI_CH$LEQ, "Character string less or equal" },
	{ "CH$GTR", BI_CH$GTR, "Character string greater than" },
	{ "CH$GEQ", BI_CH$GEQ, "Character string greater or equal" },
	{ "CH$TRANSLATE", BI_CH$TRANSLATE, "Translate character string" },

	/* Parameter manipulation */
	{ "NULLPARAMETER", BI_NULLPARAMETER, "Check for NULL parameter" },
	{ "ACTUALCOUNT", BI_ACTUALCOUNT, "Get actual parameter count" },
	{ "ACTUALPARAMETER", BI_ACTUALPARAMETER, "Get actual parameter" },

	/* Arithmetic built-ins */
	{ "SIGN", BI_SIGN, "Sign of value (-1, 0, +1)" },
	{ "ABS", BI_ABS, "Absolute value" },
	{ "MIN", BI_MIN, "Minimum of values" },
	{ "MAX", BI_MAX, "Maximum of values" },
	{ "ROT", BI_ROT, "Rotate bits" },
	{ "FFS", BI_FFS, "Find first set bit" },
	{ "FFC", BI_FFC, "Find first clear bit" },

	{ NULL, 0, NULL }
};

/*
 * Check if a name is a built-in function
 */
int is_builtin(const char *name)
{
	int i;

	for (i = 0; builtins[i].name != NULL; i++) {
		if (strcmp(name, builtins[i].name) == 0)
			return builtins[i].code;
	}

	return 0;
}

/*
 * Get builtin description
 */
const char *builtin_description(int code)
{
	int i;

	for (i = 0; builtins[i].name != NULL; i++) {
		if (builtins[i].code == code)
			return builtins[i].description;
	}

	return "unknown built-in";
}

/*
 * Print all built-in functions (for help/debugging)
 */
void print_builtins(void)
{
	int i;

	printf("BLISS Built-in Functions:\n");
	printf("========================\n\n");

	printf("Character String Primitives:\n");
	for (i = 0; builtins[i].name != NULL; i++) {
		if (builtins[i].code >= BI_CH$PTR && builtins[i].code <= BI_CH$TRANSLATE) {
			printf("  %-20s - %s\n", builtins[i].name, builtins[i].description);
		}
	}

	printf("\nParameter Manipulation:\n");
	for (i = 0; builtins[i].name != NULL; i++) {
		if (builtins[i].code >= BI_NULLPARAMETER && builtins[i].code <= BI_ACTUALPARAMETER) {
			printf("  %-20s - %s\n", builtins[i].name, builtins[i].description);
		}
	}

	printf("\nArithmetic Operations:\n");
	for (i = 0; builtins[i].name != NULL; i++) {
		if (builtins[i].code >= BI_SIGN && builtins[i].code <= BI_FFC) {
			printf("  %-20s - %s\n", builtins[i].name, builtins[i].description);
		}
	}
}

/*
 * Initialize built-in functions
 * Install them in the symbol table with special class
 */
void init_builtins(void)
{
	int i;

	for (i = 0; builtins[i].name != NULL; i++) {
		/* Built-ins are globally visible */
		SYMTAB *sp = install((char *)builtins[i].name, SROUTINE, 0);
		if (sp) {
			sp->sflags |= SUSED;  /* Don't warn about unused */
			/* Could store builtin code in sp->soffset or similar */
		}
	}
}
