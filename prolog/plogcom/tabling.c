/*
 * Copyright (c) 2025 PCC Prolog Compiler
 *
 * Tabling Support (Memoization)
 * SLG Resolution (XSB, SWI-Prolog, YAP)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"
#include "standards.h"

/* Tabling directives and predicates */
struct tabling_builtin {
	char *name;
	int arity;
	char *description;
};

static struct tabling_builtin tabling_builtins[] = {
	/* Tabling directives */
	{"table", 1, "Declare tabled predicate"},
	{"tabled", 1, "Check if predicate is tabled"},

	/* Table inspection */
	{"current_table", 2, "Query tabled predicate"},
	{"abolish_all_tables", 0, "Abolish all tables"},
	{"abolish_table_pred", 1, "Abolish table for predicate"},

	/* Table statistics */
	{"table_statistics", 1, "Get table statistics"},

	/* Tabling modes (SWI-Prolog) */
	{"table_mode", 2, "Set table mode"},

	/* Answer subsumption */
	{"lattice", 1, "Declare lattice for subsumptive tabling"},

	/* Incremental tabling (XSB) */
	{"incremental", 1, "Declare incremental tabling"},

	/* Tabling with answer completion */
	{"tnot", 1, "Tabled negation"},
	{"not_exists", 1, "Tabled negation (alt syntax)"},

	/* Well-founded semantics */
	{"undefined", 0, "Undefined truth value"},
	{"sk_not", 1, "Skeptical negation"},

	/* Terminator */
	{NULL, 0, NULL}
};

/* XSB-style tabling predicates */
static struct tabling_builtin xsb_tabling[] = {
	{"abolish_all_tables", 0, "Abolish all tables"},
	{"abolish_table_pred", 1, "Abolish predicate tables"},
	{"abolish_table_call", 1, "Abolish specific table call"},
	{"tfindall", 3, "Tabled findall"},
	{"get_calls", 2, "Get calls for tabled predicate"},
	{"get_returns", 2, "Get returns for call"},
	{"get_residual", 2, "Get residual for call"},

	{NULL, 0, NULL}
};

/* SWI-Prolog tabling predicates */
static struct tabling_builtin swi_tabling[] = {
	{"tabled", 1, "Check if tabled"},
	{"current_table", 2, "Enumerate tables"},
	{"abolish_all_tables", 0, "Remove all tables"},
	{"abolish_module_tables", 1, "Remove module tables"},
	{"abolish_table_subgoals", 1, "Remove specific table"},

	/* Answer subsumption */
	{"po", 1, "Partial order for subsumption"},
	{"lattice", 1, "Lattice for subsumption"},

	/* Incremental tabling */
	{"incremental", 1, "Incremental tabling directive"},
	{"reeval", 1, "Re-evaluate incremental table"},

	/* Shared tabling */
	{"shared", 1, "Shared tabling directive"},

	{NULL, 0, NULL}
};

/* YAP tabling predicates */
static struct tabling_builtin yap_tabling[] = {
	{"table", 1, "Table directive"},
	{"abolish_all_tables", 0, "Abolish all tables"},
	{"abolish_table", 1, "Abolish specific table"},
	{"show_table", 1, "Display table"},
	{"table_statistics", 0, "Table statistics"},

	{NULL, 0, NULL}
};

/* Initialize tabling built-ins */
void init_tabling_builtins(void) {
	int i;

	if (!standard_features.has_tabling)
		return;

	/* Register common tabling predicates */
	for (i = 0; tabling_builtins[i].name != NULL; i++) {
		struct predicate *pred = define_predicate(
			tabling_builtins[i].name,
			tabling_builtins[i].arity
		);
		pred->is_builtin = 1;
	}

	/* Register XSB-specific if needed */
	if (current_standard == PROLOG_YAP) {
		for (i = 0; xsb_tabling[i].name != NULL; i++) {
			struct predicate *pred = define_predicate(
				xsb_tabling[i].name,
				xsb_tabling[i].arity
			);
			pred->is_builtin = 1;
		}
	}

	/* Register SWI-specific if needed */
	if (current_standard == PROLOG_SWI) {
		for (i = 0; swi_tabling[i].name != NULL; i++) {
			struct predicate *pred = define_predicate(
				swi_tabling[i].name,
				swi_tabling[i].arity
			);
			pred->is_builtin = 1;
		}
	}

	/* Register YAP-specific if needed */
	if (current_standard == PROLOG_YAP) {
		for (i = 0; yap_tabling[i].name != NULL; i++) {
			struct predicate *pred = define_predicate(
				yap_tabling[i].name,
				yap_tabling[i].arity
			);
			pred->is_builtin = 1;
		}
	}
}

/* Check if a predicate should be tabled */
int is_tabled_predicate(char *name, int arity) {
	struct predicate *pred = lookup_predicate(name, arity);
	if (!pred)
		return 0;

	/* Check if predicate has tabling directive */
	/* This would be set during parsing of :- table directive */
	return 0; /* Placeholder */
}
