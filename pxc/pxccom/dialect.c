/*
 * Copyright (c) 2025 PCC Xbase++ Compiler
 *
 * Dialect support implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "pass1.h"
#include "dialect.h"

/* Current dialect setting */
DIALECT current_dialect = DIALECT_XBASEPP;
int dialect_strict = 0;

/*
 * Dialect information table
 */
const DIALECT_INFO dialect_info[] = {
	{
		DIALECT_DBASE2,
		"dBASE II",
		"dBASE II (1981) - Original Xbase language",
		FEAT_PROCEDURES | FEAT_PRIVATE | FEAT_PUBLIC,
		1981
	},
	{
		DIALECT_DBASE3,
		"dBASE III",
		"dBASE III (1984) - Enhanced with UDFs",
		FEAT_PROCEDURES | FEAT_PRIVATE | FEAT_PUBLIC | FEAT_FIELD,
		1984
	},
	{
		DIALECT_DBASE3PLUS,
		"dBASE III Plus",
		"dBASE III Plus (1985) - Added compiled code",
		FEAT_PROCEDURES | FEAT_PRIVATE | FEAT_PUBLIC | FEAT_FIELD,
		1985
	},
	{
		DIALECT_DBASE4,
		"dBASE IV",
		"dBASE IV (1988) - Major update with SQL",
		FEAT_PROCEDURES | FEAT_PRIVATE | FEAT_PUBLIC | FEAT_FIELD |
		FEAT_LOCAL,
		1988
	},
	{
		DIALECT_CLIPPER,
		"Clipper",
		"CA-Clipper 5.x (1985-1997) - Industry standard",
		FEAT_PROCEDURES | FEAT_LOCAL | FEAT_STATIC | FEAT_PRIVATE |
		FEAT_PUBLIC | FEAT_FIELD | FEAT_CODEBLOCKS | FEAT_SEQUENCE,
		1987
	},
	{
		DIALECT_FOXBASE,
		"FoxBASE",
		"FoxBASE/FoxPro 1.x (1984) - dBASE alternative",
		FEAT_PROCEDURES | FEAT_PRIVATE | FEAT_PUBLIC | FEAT_LOCAL,
		1984
	},
	{
		DIALECT_FOXPRO,
		"FoxPro",
		"FoxPro 2.x (1991) - Enhanced FoxBASE",
		FEAT_PROCEDURES | FEAT_PRIVATE | FEAT_PUBLIC | FEAT_LOCAL |
		FEAT_FIELD,
		1991
	},
	{
		DIALECT_VFP,
		"Visual FoxPro",
		"Visual FoxPro (1995-2007) - GUI and OOP",
		FEAT_PROCEDURES | FEAT_OOP | FEAT_LOCAL | FEAT_PRIVATE |
		FEAT_PUBLIC | FEAT_FIELD | FEAT_TRY | FEAT_FOREACH |
		FEAT_WITH_OBJECT,
		1995
	},
	{
		DIALECT_HARBOUR,
		"Harbour",
		"Harbour (1999-present) - Open source Clipper",
		FEAT_PROCEDURES | FEAT_OOP | FEAT_LOCAL | FEAT_STATIC |
		FEAT_PRIVATE | FEAT_PUBLIC | FEAT_MEMVAR | FEAT_FIELD |
		FEAT_CODEBLOCKS | FEAT_INLINE | FEAT_SEQUENCE | FEAT_TRY |
		FEAT_FOREACH | FEAT_SWITCH | FEAT_CLASS_VAR | FEAT_THREADS |
		FEAT_ARRAY_ASSIGN | FEAT_HASH_ASSIGN,
		1999
	},
	{
		DIALECT_XHARBOUR,
		"xHarbour",
		"xHarbour (2001-present) - Extended Harbour",
		FEAT_PROCEDURES | FEAT_OOP | FEAT_LOCAL | FEAT_STATIC |
		FEAT_PRIVATE | FEAT_PUBLIC | FEAT_MEMVAR | FEAT_FIELD |
		FEAT_CODEBLOCKS | FEAT_INLINE | FEAT_SEQUENCE | FEAT_TRY |
		FEAT_FOREACH | FEAT_SWITCH | FEAT_CLASS_VAR | FEAT_THREADS |
		FEAT_WITH_OBJECT | FEAT_ARRAY_ASSIGN | FEAT_HASH_ASSIGN,
		2001
	},
	{
		DIALECT_XBASEPP,
		"Xbase++",
		"Xbase++ (1997-present) - Modern OOP Xbase",
		FEAT_PROCEDURES | FEAT_OOP | FEAT_LOCAL | FEAT_STATIC |
		FEAT_PRIVATE | FEAT_PUBLIC | FEAT_MEMVAR | FEAT_FIELD |
		FEAT_CODEBLOCKS | FEAT_INLINE | FEAT_SEQUENCE | FEAT_TRY |
		FEAT_FOREACH | FEAT_SWITCH | FEAT_CLASS_VAR | FEAT_THREADS |
		FEAT_WITH_OBJECT | FEAT_ARRAY_ASSIGN | FEAT_HASH_ASSIGN |
		FEAT_NAMESPACES,
		1997
	},
	{
		DIALECT_AUTO,
		"Auto",
		"Auto-detect dialect from source code",
		0xFFFFFFFF,  /* All features for detection */
		0
	}
};

/*
 * Initialize dialect system
 */
void
dialect_init(void)
{
	current_dialect = DIALECT_XBASEPP;  /* Default to most modern */
	dialect_strict = 0;
}

/*
 * Set current dialect
 */
void
dialect_set(DIALECT d)
{
	extern int debug;

	if (d >= DIALECT_DBASE2 && d <= DIALECT_AUTO) {
		current_dialect = d;
		if (debug) {
			fprintf(stderr, "Dialect set to: %s\n",
				dialect_info[d].name);
		}
	}
}

/*
 * Get current dialect
 */
DIALECT
dialect_get(void)
{
	return current_dialect;
}

/*
 * Get dialect name
 */
const char *
dialect_name(DIALECT d)
{
	if (d >= DIALECT_DBASE2 && d <= DIALECT_AUTO)
		return dialect_info[d].name;
	return "Unknown";
}

/*
 * Check if current dialect has a feature
 */
int
dialect_has_feature(unsigned int feature)
{
	if (current_dialect == DIALECT_AUTO)
		return 1;  /* Auto mode allows everything */

	return (dialect_info[current_dialect].features & feature) != 0;
}

/*
 * Set strict mode
 */
void
dialect_set_strict(int strict)
{
	dialect_strict = strict;
}

/*
 * Auto-detect dialect from source code
 * This is a heuristic approach based on keywords and patterns
 */
DIALECT
dialect_detect(const char *source)
{
	int has_class = 0;
	int has_try = 0;
	int has_foreach = 0;
	int has_inline = 0;
	int has_namespace = 0;
	int has_with_object = 0;
	int has_local = 0;

	/* Simple keyword detection */
	if (strstr(source, "CLASS ") || strstr(source, "ENDCLASS"))
		has_class = 1;
	if (strstr(source, "TRY") || strstr(source, "CATCH"))
		has_try = 1;
	if (strstr(source, "FOR EACH") || strstr(source, "FOREACH"))
		has_foreach = 1;
	if (strstr(source, "INLINE"))
		has_inline = 1;
	if (strstr(source, "NAMESPACE"))
		has_namespace = 1;
	if (strstr(source, "WITH OBJECT"))
		has_with_object = 1;
	if (strstr(source, "LOCAL "))
		has_local = 1;

	/* Detection logic */
	if (has_namespace)
		return DIALECT_XBASEPP;  /* Only Xbase++ has namespaces */

	if (has_class && has_try && has_inline)
		return DIALECT_XHARBOUR;  /* xHarbour or newer */

	if (has_with_object && has_try)
		return DIALECT_VFP;  /* Visual FoxPro */

	if (has_class && has_foreach)
		return DIALECT_HARBOUR;  /* Harbour */

	if (has_class)
		return DIALECT_HARBOUR;  /* Assume Harbour for OOP */

	if (has_local && strstr(source, "{|"))
		return DIALECT_CLIPPER;  /* Clipper has codeblocks */

	if (has_local)
		return DIALECT_DBASE4;  /* dBASE IV has LOCAL */

	/* Default to oldest for maximum compatibility */
	return DIALECT_DBASE3;
}

/*
 * Keyword validation for current dialect
 */
int
dialect_keyword_valid(const char *keyword)
{
	/* Convert to uppercase for comparison */
	char kw[64];
	size_t i;

	for (i = 0; i < sizeof(kw) - 1 && keyword[i]; i++) {
		kw[i] = toupper((unsigned char)keyword[i]);
	}
	kw[i] = '\0';

	/* Check dialect-specific keywords */
	if (strcmp(kw, "CLASS") == 0 || strcmp(kw, "ENDCLASS") == 0) {
		return dialect_has_feature(FEAT_OOP);
	}

	if (strcmp(kw, "METHOD") == 0 || strcmp(kw, "ENDMETHOD") == 0) {
		return dialect_has_feature(FEAT_OOP);
	}

	if (strcmp(kw, "LOCAL") == 0) {
		return dialect_has_feature(FEAT_LOCAL);
	}

	if (strcmp(kw, "STATIC") == 0) {
		return dialect_has_feature(FEAT_STATIC);
	}

	if (strcmp(kw, "PRIVATE") == 0) {
		return dialect_has_feature(FEAT_PRIVATE);
	}

	if (strcmp(kw, "PUBLIC") == 0) {
		return dialect_has_feature(FEAT_PUBLIC);
	}

	if (strcmp(kw, "MEMVAR") == 0) {
		return dialect_has_feature(FEAT_MEMVAR);
	}

	if (strcmp(kw, "FIELD") == 0) {
		return dialect_has_feature(FEAT_FIELD);
	}

	if (strcmp(kw, "INLINE") == 0) {
		return dialect_has_feature(FEAT_INLINE);
	}

	if (strcmp(kw, "SEQUENCE") == 0 || strcmp(kw, "RECOVER") == 0) {
		return dialect_has_feature(FEAT_SEQUENCE);
	}

	if (strcmp(kw, "TRY") == 0 || strcmp(kw, "CATCH") == 0 ||
	    strcmp(kw, "FINALLY") == 0) {
		return dialect_has_feature(FEAT_TRY);
	}

	if (strcmp(kw, "FOREACH") == 0) {
		return dialect_has_feature(FEAT_FOREACH);
	}

	if (strcmp(kw, "SWITCH") == 0) {
		return dialect_has_feature(FEAT_SWITCH);
	}

	if (strcmp(kw, "NAMESPACE") == 0) {
		return dialect_has_feature(FEAT_NAMESPACES);
	}

	/* All other keywords are valid */
	return 1;
}
