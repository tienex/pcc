/*
 * Copyright (c) 2025 PCC Paradox PAL Compiler
 *
 * Built-in functions and procedures
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "pass1.h"

/* Built-in function/procedure names */
static const char *builtins[] = {
	/* String functions */
	"upper", "lower", "substr", "strlen", "strval", "trim",
	"ltrim", "rtrim", "strpos", "concat", "chr", "asc",
	"format", "fill",

	/* Math functions */
	"abs", "sqrt", "sin", "cos", "tan", "exp", "ln", "log",
	"round", "trunc", "int", "frac", "random", "max", "min",
	"power", "mod",

	/* Date/time functions */
	"today", "now", "year", "month", "day", "hour", "minute",
	"second", "date", "time", "datetime", "datetostr", "strtodate",

	/* Type conversion functions */
	"numval", "strval", "logical",

	/* Table/database functions */
	"moveto", "locate", "scan", "edit", "view", "coedit",
	"resync", "lockrecord", "unlockrecord", "post",

	/* UI functions */
	"msginfo", "msgwarning", "msgerror", "msgquestion",
	"msgok", "msgyesno", "msgokcancel",
	"beep", "sleep", "wait",

	/* System functions */
	"execute", "shell", "getenv", "setenv", "fileexists",
	"filecopy", "filedelete", "filerename",

	/* Array functions (ObjectPAL) */
	"arraysize", "arraydim", "arrayinsert", "arraydelete",

	/* Object methods (ObjectPAL) */
	"setvalue", "getvalue", "setfocus", "refresh", "close",
	"open", "action",

	NULL
};

void init_builtins(void)
{
	int i;
	SYMTAB *sym;

	/* Install all built-in functions in the global symbol table */
	for (i = 0; builtins[i] != NULL; i++) {
		/* Convert to uppercase if not case-sensitive */
		char *name;
		if (!IS_CASE_SENSITIVE()) {
			int j;
			name = strdup(builtins[i]);
			for (j = 0; name[j]; j++) {
				name[j] = toupper(name[j]);
			}
		} else {
			name = strdup(builtins[i]);
		}

		sym = install(name, BUILTIN, 0);
		if (sym) {
			sym->stype = mktype(TPROCEDURE);
			sym->sflags |= SEXTERNAL;
		}
	}
}

int is_builtin(char *name)
{
	int i;
	char *test_name = name;
	char *upper_name = NULL;

	/* Convert to lowercase for comparison if not case-sensitive */
	if (!IS_CASE_SENSITIVE()) {
		int j;
		upper_name = strdup(name);
		for (j = 0; upper_name[j]; j++) {
			upper_name[j] = tolower(upper_name[j]);
		}
		test_name = upper_name;
	}

	for (i = 0; builtins[i] != NULL; i++) {
		if (strcmp(builtins[i], test_name) == 0) {
			if (upper_name) {
				free(upper_name);
			}
			return 1;
		}
	}

	if (upper_name) {
		free(upper_name);
	}
	return 0;
}
