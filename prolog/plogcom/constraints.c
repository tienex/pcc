/*
 * Copyright (c) 2025 PCC Prolog Compiler
 *
 * Constraint Logic Programming (CLP) Support
 * CLP(FD), CLP(R), CLP(Q), CHR
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"
#include "standards.h"

/* CLP(FD) - Constraint Logic Programming over Finite Domains */
struct clp_fd_builtin {
	char *name;
	int arity;
	char *description;
};

static struct clp_fd_builtin clp_fd_builtins[] = {
	/* Domain declaration */
	{"#=", 2, "Constraint: equal"},
	{"#\\=", 2, "Constraint: not equal"},
	{"#<", 2, "Constraint: less than"},
	{"#>", 2, "Constraint: greater than"},
	{"#=<", 2, "Constraint: less or equal"},
	{"#>=", 2, "Constraint: greater or equal"},

	/* Arithmetic constraints */
	{"#=", 2, "Arithmetic constraint equal"},
	{"#\\=", 2, "Arithmetic constraint not equal"},

	/* Domain specification */
	{"in", 2, "Variable in domain"},
	{"ins", 2, "Variables in domain"},
	{"#<=>", 2, "Reification equivalence"},
	{"#=>", 2, "Reification implication"},
	{"#<=", 2, "Reification reverse implication"},
	{"#/\\", 2, "Conjunction of constraints"},
	{"#\\/", 2, "Disjunction of constraints"},
	{"#\\", 1, "Negation of constraint"},

	/* Labeling and search */
	{"label", 1, "Label variables"},
	{"labeling", 2, "Label with options"},
	{"indomain", 1, "Assign value from domain"},

	/* Global constraints */
	{"all_different", 1, "All different constraint"},
	{"all_distinct", 1, "All distinct constraint"},
	{"sum", 3, "Sum constraint"},
	{"scalar_product", 4, "Scalar product constraint"},
	{"lex_chain", 1, "Lexicographic ordering"},
	{"tuples_in", 2, "Tuples constraint"},
	{"serialized", 2, "Serialized tasks constraint"},

	/* Element constraints */
	{"element", 3, "Element constraint"},
	{"nth", 3, "Nth element (deterministic)"},

	/* Counting constraints */
	{"global_cardinality", 2, "Global cardinality constraint"},
	{"count", 4, "Count occurrences"},

	/* Circuit constraints */
	{"circuit", 1, "Hamiltonian circuit constraint"},

	/* Cumulative constraint (scheduling) */
	{"cumulative", 1, "Cumulative resource constraint"},
	{"cumulative", 2, "Cumulative with options"},

	/* Automaton */
	{"automaton", 3, "Automaton constraint"},
	{"automaton", 8, "Automaton with full options"},

	/* Chain constraints */
	{"chain", 2, "Chain constraint"},

	/* Terminator */
	{NULL, 0, NULL}
};

/* CLP(R) - Constraint Logic Programming over Reals */
static struct clp_fd_builtin clp_r_builtins[] = {
	/* Real constraints */
	{"{}", 1, "Constraint goal"},

	/* Operators work on reals */
	{"=", 2, "Real equality"},
	{"<", 2, "Real less than"},
	{">", 2, "Real greater than"},
	{"=<", 2, "Real less or equal"},
	{">=", 2, "Real greater or equal"},

	/* Minimization/maximization */
	{"minimize", 1, "Minimize expression"},
	{"maximize", 1, "Maximize expression"},
	{"inf", 2, "Get infimum of variable"},
	{"sup", 2, "Get supremum of variable"},

	/* Terminator */
	{NULL, 0, NULL}
};

/* CLP(Q) - Constraint Logic Programming over Rationals */
static struct clp_fd_builtin clp_q_builtins[] = {
	/* Same as CLP(R) but over rationals */
	{"{}", 1, "Constraint goal"},
	{"=", 2, "Rational equality"},
	{"<", 2, "Rational less than"},
	{">", 2, "Rational greater than"},
	{"=<", 2, "Rational less or equal"},
	{">=", 2, "Rational greater or equal"},
	{"minimize", 1, "Minimize expression"},
	{"maximize", 1, "Maximize expression"},

	{NULL, 0, NULL}
};

/* CHR - Constraint Handling Rules */
static struct clp_fd_builtin chr_builtins[] = {
	/* CHR syntax support */
	{"<=>", 2, "CHR simplification rule"},
	{"==>", 2, "CHR propagation rule"},
	{"\\", 2, "CHR guard separator"},
	{"|", 2, "CHR body separator"},

	{NULL, 0, NULL}
};

/* Initialize CLP(FD) built-ins */
void init_clp_fd_builtins(void) {
	int i;

	if (!standard_features.has_constraints)
		return;

	for (i = 0; clp_fd_builtins[i].name != NULL; i++) {
		struct predicate *pred = define_predicate(
			clp_fd_builtins[i].name,
			clp_fd_builtins[i].arity
		);
		pred->is_builtin = 1;
	}
}

/* Initialize CLP(R) built-ins */
void init_clp_r_builtins(void) {
	int i;

	if (!standard_features.has_constraints)
		return;

	for (i = 0; clp_r_builtins[i].name != NULL; i++) {
		struct predicate *pred = define_predicate(
			clp_r_builtins[i].name,
			clp_r_builtins[i].arity
		);
		pred->is_builtin = 1;
	}
}

/* Initialize CLP(Q) built-ins */
void init_clp_q_builtins(void) {
	int i;

	if (!standard_features.has_constraints)
		return;

	for (i = 0; clp_q_builtins[i].name != NULL; i++) {
		struct predicate *pred = define_predicate(
			clp_q_builtins[i].name,
			clp_q_builtins[i].arity
		);
		pred->is_builtin = 1;
	}
}

/* Initialize CHR built-ins */
void init_chr_builtins(void) {
	int i;

	if (!standard_features.has_constraints)
		return;

	for (i = 0; chr_builtins[i].name != NULL; i++) {
		struct predicate *pred = define_predicate(
			chr_builtins[i].name,
			chr_builtins[i].arity
		);
		pred->is_builtin = 1;
	}
}

/* Initialize all constraint systems */
void init_constraint_builtins(void) {
	if (!standard_features.has_constraints)
		return;

	init_clp_fd_builtins();
	init_clp_r_builtins();
	init_clp_q_builtins();
	init_chr_builtins();
}
