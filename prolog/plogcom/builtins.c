/*
 * Copyright (c) 2025 PCC Prolog Compiler
 *
 * Built-in predicate definitions
 * Compatible with Turbo Prolog and GNU Prolog
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Built-in predicate list */
struct builtin_info {
	char *name;
	int arity;
	char *description;
};

static struct builtin_info builtins[] = {
	/* Control predicates */
	{"true", 0, "Always succeeds"},
	{"fail", 0, "Always fails"},
	{"!", 0, "Cut (prevent backtracking)"},
	{"call", 1, "Meta-call"},

	/* Type checking */
	{"var", 1, "Test if variable"},
	{"nonvar", 1, "Test if not variable"},
	{"atom", 1, "Test if atom"},
	{"number", 1, "Test if number"},
	{"integer", 1, "Test if integer"},
	{"float", 1, "Test if float"},
	{"compound", 1, "Test if compound term"},
	{"atomic", 1, "Test if atomic"},

	/* Term comparison */
	{"=", 2, "Unification"},
	{"\\=", 2, "Not unifiable"},
	{"==", 2, "Structural equality"},
	{"\\==", 2, "Structural inequality"},
	{"@<", 2, "Term less than"},
	{"@=<", 2, "Term less or equal"},
	{"@>", 2, "Term greater than"},
	{"@>=", 2, "Term greater or equal"},

	/* Arithmetic */
	{"is", 2, "Arithmetic evaluation"},
	{"=:=", 2, "Arithmetic equality"},
	{"=\\=", 2, "Arithmetic inequality"},
	{"<", 2, "Arithmetic less than"},
	{"=<", 2, "Arithmetic less or equal"},
	{">", 2, "Arithmetic greater than"},
	{">=", 2, "Arithmetic greater or equal"},

	/* Term manipulation */
	{"functor", 3, "Get functor and arity"},
	{"arg", 3, "Get argument of compound term"},
	{"copy_term", 2, "Deep copy term"},
	{"=..", 2, "Univ - convert between term and list"},

	/* Database manipulation */
	{"asserta", 1, "Assert at beginning"},
	{"assertz", 1, "Assert at end"},
	{"retract", 1, "Retract clause"},
	{"retractall", 1, "Retract all matching clauses"},
	{"abolish", 1, "Remove all clauses for predicate"},
	{"clause", 2, "Access clause"},

	/* Meta-predicates */
	{"findall", 3, "Find all solutions"},
	{"bagof", 3, "Collect solutions (with duplicates)"},
	{"setof", 3, "Collect solutions (sorted, unique)"},

	/* I/O */
	{"write", 1, "Write term"},
	{"writeln", 1, "Write term with newline"},
	{"read", 1, "Read term"},
	{"get", 1, "Read character"},
	{"put", 1, "Write character"},
	{"nl", 0, "Write newline"},

	/* Control */
	{"halt", 0, "Exit program"},
	{"halt", 1, "Exit program with code"},
	{"abort", 0, "Abort execution"},

	/* List operations (Turbo Prolog) */
	{"append", 3, "List concatenation"},
	{"member", 2, "List membership"},
	{"length", 2, "List length"},
	{"reverse", 2, "Reverse list"},
	{"sort", 2, "Sort list"},

	/* String operations (Turbo Prolog) */
	{"concat", 3, "String concatenation"},
	{"str_len", 2, "String length"},
	{"str_char", 3, "Get character from string"},

	/* Turbo Prolog specific */
	{"consult", 1, "Load Prolog file"},
	{"save", 1, "Save program"},
	{"trace", 0, "Enable trace mode"},
	{"notrace", 0, "Disable trace mode"},

	/* GNU Prolog specific */
	{"see", 1, "Open file for input"},
	{"seen", 0, "Close input file"},
	{"tell", 1, "Open file for output"},
	{"told", 0, "Close output file"},
	{"get0", 1, "Read character (GNU)"},
	{"put0", 1, "Write character (GNU)"},

	/* Terminator */
	{NULL, 0, NULL}
};

/* Initialize built-in predicates */
void init_builtins(void) {
	int i;

	for (i = 0; builtins[i].name != NULL; i++) {
		struct predicate *pred = define_predicate(
			builtins[i].name,
			builtins[i].arity
		);
		pred->is_builtin = 1;
	}
}

/* Check if predicate is built-in */
int is_builtin(char *name, int arity) {
	struct predicate *pred = lookup_predicate(name, arity);
	return pred && pred->is_builtin;
}
