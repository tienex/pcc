/*
 * Copyright (c) 2025 PCC Prolog Compiler
 *
 * Prolog Standards Implementation
 * Support for multiple Prolog standards from 1977 to present
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"
#include "standards.h"

/* Global standard configuration */
prolog_standard_t current_standard = PROLOG_ISO_1995;
standard_features_t standard_features;
double_quote_mode_t double_quote_mode = DQ_CODES;

/* Standard names */
static const char *standard_names[] = {
	"Edinburgh Prolog (1977)",
	"DEC-10 Prolog (1970s)",
	"C-Prolog (1980s)",
	"Quintus Prolog (1980s-1990s)",
	"ISO Prolog (1995)",
	"ISO Prolog + Corrigendum 1 (2007)",
	"ISO Prolog + Corrigendum 2 (2012)",
	"SICStus Prolog",
	"SWI-Prolog",
	"YAP Prolog",
	"GNU Prolog",
	"Turbo Prolog",
	"Auto-detect"
};

/* Get standard name */
const char *get_standard_name(prolog_standard_t std) {
	if (std < 0 || std > PROLOG_AUTO)
		return "Unknown";
	return standard_names[std];
}

/* Initialize Edinburgh Prolog (1977-1980s) */
void init_edinburgh_standard(void) {
	memset(&standard_features, 0, sizeof(standard_features));

	/* Edinburgh Prolog features */
	standard_features.edinburgh_syntax = 1;
	standard_features.has_cuts = 1;
	standard_features.has_negation = 1;        /* \+ */
	standard_features.has_if_then_else = 1;    /* -> ; */

	/* Basic I/O */
	standard_features.dec10_io = 1;
	standard_features.dec10_assert_retract = 1;

	/* No ISO features */
	standard_features.iso_syntax = 0;
	standard_features.iso_operators = 0;
	standard_features.iso_builtins = 0;
	standard_features.iso_exceptions = 0;

	/* No modern extensions */
	standard_features.has_constraints = 0;
	standard_features.has_tabling = 0;
	standard_features.has_threads = 0;
	standard_features.has_dicts = 0;
	standard_features.has_strings = 0;

	/* Double quotes produce character codes */
	double_quote_mode = DQ_CODES;
}

/* Initialize DEC-10 Prolog (1970s) */
void init_dec10_standard(void) {
	init_edinburgh_standard();

	/* DEC-10 specific features */
	standard_features.dec10_io = 1;
	standard_features.dec10_assert_retract = 1;

	/* DEC-10 had some additional predicates */
	/* numbervars/3, copy_term/2, etc. */
}

/* Initialize ISO Prolog (1995) - ISO/IEC 13211-1:1995 */
void init_iso_1995_standard(void) {
	memset(&standard_features, 0, sizeof(standard_features));

	/* Core features */
	standard_features.has_cuts = 1;
	standard_features.has_negation = 1;
	standard_features.has_if_then_else = 1;

	/* ISO features */
	standard_features.iso_syntax = 1;
	standard_features.iso_operators = 1;
	standard_features.iso_builtins = 1;
	standard_features.iso_exceptions = 1;
	standard_features.iso_char_conversion = 1;
	standard_features.iso_double_quotes = 1;

	/* Edinburgh compatibility */
	standard_features.edinburgh_syntax = 1;
	standard_features.dec10_io = 1;

	/* No modern extensions yet */
	standard_features.has_constraints = 0;
	standard_features.has_tabling = 0;
	standard_features.has_threads = 0;
	standard_features.has_dicts = 0;
	standard_features.has_strings = 0;
	standard_features.has_unicode = 0;

	/* ISO default: double quotes = character codes */
	double_quote_mode = DQ_CODES;
}

/* Initialize ISO Prolog + Corrigendum 1 (2007) */
void init_iso_cor1_2007_standard(void) {
	init_iso_1995_standard();

	/* Corrigendum 1 fixes */
	standard_features.iso_cor1_2007 = 1;

	/* Clarifications to:
	 * - Character escapes
	 * - Operator precedence
	 * - Error conditions
	 * - Arithmetic functions
	 */
}

/* Initialize ISO Prolog + Corrigendum 2 (2012) */
void init_iso_cor2_2012_standard(void) {
	init_iso_cor1_2007_standard();

	/* Corrigendum 2 fixes */
	standard_features.iso_cor2_2012 = 1;

	/* Additional clarifications and fixes */
	standard_features.has_unicode = 1;  /* Unicode support clarified */
	standard_features.has_cyclic_terms = 1;  /* Cyclic term handling */
}

/* Initialize SWI-Prolog (modern) */
void init_swi_standard(void) {
	init_iso_cor2_2012_standard();

	/* SWI-Prolog extensions */
	standard_features.swi_extensions = 1;
	standard_features.has_dicts = 1;
	standard_features.has_strings = 1;
	standard_features.has_threads = 1;
	standard_features.has_unicode = 1;
	standard_features.has_rational = 1;
	standard_features.has_constraints = 1;  /* CLP(FD), CLP(R), etc. */
	standard_features.has_tabling = 1;
	standard_features.has_cyclic_terms = 1;

	/* SWI default: double quotes = string */
	double_quote_mode = DQ_STRING;
}

/* Initialize SICStus Prolog */
void init_sicstus_standard(void) {
	init_iso_cor2_2012_standard();

	/* SICStus extensions */
	standard_features.sicstus_extensions = 1;
	standard_features.has_constraints = 1;
	standard_features.has_threads = 1;
	standard_features.has_unicode = 1;
	standard_features.has_rational = 1;

	/* SICStus default: double quotes = codes */
	double_quote_mode = DQ_CODES;
}

/* Initialize YAP Prolog */
void init_yap_standard(void) {
	init_iso_cor2_2012_standard();

	/* YAP extensions */
	standard_features.yap_extensions = 1;
	standard_features.has_constraints = 1;
	standard_features.has_tabling = 1;
	standard_features.has_threads = 1;
	standard_features.has_unicode = 1;
	standard_features.has_rational = 1;

	/* YAP default: double quotes = codes */
	double_quote_mode = DQ_CODES;
}

/* Initialize GNU Prolog */
void init_gnu_standard(void) {
	init_iso_1995_standard();

	/* GNU Prolog extensions */
	standard_features.gnu_extensions = 1;
	standard_features.has_constraints = 1;  /* CLP(FD) */

	/* GNU is fairly conservative, close to ISO */
	/* GNU default: double quotes = codes */
	double_quote_mode = DQ_CODES;
}

/* Initialize Turbo Prolog */
void init_turbo_standard(void) {
	memset(&standard_features, 0, sizeof(standard_features));

	/* Turbo Prolog is quite different */
	standard_features.turbo_domains = 1;
	standard_features.has_cuts = 1;
	standard_features.has_negation = 1;
	standard_features.has_if_then_else = 1;

	/* Turbo uses typed predicates */
	/* No ISO compatibility */
	standard_features.iso_syntax = 0;
	standard_features.iso_operators = 0;
	standard_features.iso_builtins = 0;

	/* Turbo default: double quotes = string */
	double_quote_mode = DQ_STRING;
}

/* Initialize standard */
void init_standard(prolog_standard_t std) {
	current_standard = std;

	switch (std) {
	case PROLOG_EDINBURGH:
		init_edinburgh_standard();
		break;
	case PROLOG_DEC10:
		init_dec10_standard();
		break;
	case PROLOG_ISO_1995:
		init_iso_1995_standard();
		break;
	case PROLOG_ISO_COR1_2007:
		init_iso_cor1_2007_standard();
		break;
	case PROLOG_ISO_COR2_2012:
		init_iso_cor2_2012_standard();
		break;
	case PROLOG_SWI:
		init_swi_standard();
		break;
	case PROLOG_SICSTUS:
		init_sicstus_standard();
		break;
	case PROLOG_YAP:
		init_yap_standard();
		break;
	case PROLOG_GNU:
		init_gnu_standard();
		break;
	case PROLOG_TURBO:
		init_turbo_standard();
		break;
	default:
		/* Default to ISO 1995 */
		init_iso_1995_standard();
		break;
	}
}

/* Set standard */
void set_standard(prolog_standard_t std) {
	init_standard(std);
}

/* Auto-detect standard from source code */
prolog_standard_t detect_standard(void) {
	/* This would analyze the source code to detect which standard
	 * is being used based on syntax and features used.
	 * For now, just return ISO 1995 as default.
	 */
	return PROLOG_ISO_1995;
}

/* Check if a feature is enabled */
int feature_enabled(const char *feature) {
	if (strcmp(feature, "cuts") == 0)
		return standard_features.has_cuts;
	if (strcmp(feature, "negation") == 0)
		return standard_features.has_negation;
	if (strcmp(feature, "if_then_else") == 0)
		return standard_features.has_if_then_else;
	if (strcmp(feature, "iso_syntax") == 0)
		return standard_features.iso_syntax;
	if (strcmp(feature, "iso_exceptions") == 0)
		return standard_features.iso_exceptions;
	if (strcmp(feature, "constraints") == 0)
		return standard_features.has_constraints;
	if (strcmp(feature, "tabling") == 0)
		return standard_features.has_tabling;
	if (strcmp(feature, "threads") == 0)
		return standard_features.has_threads;
	if (strcmp(feature, "dicts") == 0)
		return standard_features.has_dicts;
	if (strcmp(feature, "strings") == 0)
		return standard_features.has_strings;
	if (strcmp(feature, "unicode") == 0)
		return standard_features.has_unicode;
	if (strcmp(feature, "turbo_domains") == 0)
		return standard_features.turbo_domains;

	return 0;
}
