/*
 * Copyright (c) 2025 PCC Prolog Runtime Library
 *
 * Built-in predicate implementations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "runtime.h"

/* Helper: Register a built-in predicate */
static void register_builtin(prolog_engine_t *eng, const char *name, int arity,
                            int (*func)(prolog_engine_t *, word_t *)) {
	functor_t *f = functor_create(eng, name, arity);
	predicate_t *pred = predicate_define(eng, f);
	pred->builtin = func;
}

/* true/0 - Always succeeds */
int builtin_true(prolog_engine_t *eng, word_t *args) {
	return 1;
}

/* fail/0 - Always fails */
int builtin_fail(prolog_engine_t *eng, word_t *args) {
	return 0;
}

/* !/0 - Cut */
int builtin_cut(prolog_engine_t *eng, word_t *args) {
	/* Discard all choice points created since current clause */
	/* This is simplified - proper cut needs clause-level tracking */
	if (eng->choice) {
		eng->choice = eng->choice->prev;
	}
	return 1;
}

/* =/2 - Unification */
int builtin_unify(prolog_engine_t *eng, word_t *args) {
	return unify(eng, args[0], args[1]);
}

/* \=/2 - Not unifiable */
int builtin_not_unify(prolog_engine_t *eng, word_t *args) {
	return !unify(eng, args[0], args[1]);
}

/* ==/2 - Structural equality */
int builtin_equal(prolog_engine_t *eng, word_t *args) {
	word_t t1 = deref(eng, args[0]);
	word_t t2 = deref(eng, args[1]);
	return t1 == t2 ? 1 : 0;
}

/* \==/2 - Structural inequality */
int builtin_not_equal(prolog_engine_t *eng, word_t *args) {
	return !builtin_equal(eng, args);
}

/* @</2 - Term less than */
int builtin_term_less(prolog_engine_t *eng, word_t *args) {
	return term_compare(eng, args[0], args[1]) < 0 ? 1 : 0;
}

/* @=</2 - Term less or equal */
int builtin_term_lesseq(prolog_engine_t *eng, word_t *args) {
	return term_compare(eng, args[0], args[1]) <= 0 ? 1 : 0;
}

/* @>/2 - Term greater than */
int builtin_term_greater(prolog_engine_t *eng, word_t *args) {
	return term_compare(eng, args[0], args[1]) > 0 ? 1 : 0;
}

/* @>=/2 - Term greater or equal */
int builtin_term_greatereq(prolog_engine_t *eng, word_t *args) {
	return term_compare(eng, args[0], args[1]) >= 0 ? 1 : 0;
}

/* var/1 - Test if variable */
int builtin_var(prolog_engine_t *eng, word_t *args) {
	word_t t = deref(eng, args[0]);
	return IS_REF(t) ? 1 : 0;
}

/* nonvar/1 - Test if not variable */
int builtin_nonvar(prolog_engine_t *eng, word_t *args) {
	return !builtin_var(eng, args);
}

/* atom/1 - Test if atom */
int builtin_atom(prolog_engine_t *eng, word_t *args) {
	word_t t = deref(eng, args[0]);
	return IS_ATOM(t) ? 1 : 0;
}

/* number/1 - Test if number */
int builtin_number(prolog_engine_t *eng, word_t *args) {
	word_t t = deref(eng, args[0]);
	return (IS_INT(t) || IS_FLOAT(t)) ? 1 : 0;
}

/* integer/1 - Test if integer */
int builtin_integer(prolog_engine_t *eng, word_t *args) {
	word_t t = deref(eng, args[0]);
	return IS_INT(t) ? 1 : 0;
}

/* float/1 - Test if float */
int builtin_float_test(prolog_engine_t *eng, word_t *args) {
	word_t t = deref(eng, args[0]);
	return IS_FLOAT(t) ? 1 : 0;
}

/* atomic/1 - Test if atomic */
int builtin_atomic(prolog_engine_t *eng, word_t *args) {
	word_t t = deref(eng, args[0]);
	return (IS_ATOM(t) || IS_INT(t) || IS_FLOAT(t)) ? 1 : 0;
}

/* compound/1 - Test if compound */
int builtin_compound(prolog_engine_t *eng, word_t *args) {
	word_t t = deref(eng, args[0]);
	return (IS_STRUCT(t) || IS_LIST(t)) ? 1 : 0;
}

/* ground/1 - Test if ground */
int builtin_ground(prolog_engine_t *eng, word_t *args) {
	return term_ground(eng, args[0]);
}

/* functor/3 - functor(Term, Functor, Arity) */
int builtin_functor(prolog_engine_t *eng, word_t *args) {
	word_t term = deref(eng, args[0]);
	functor_t *f;

	if (term_functor(eng, term, &f)) {
		word_t name = make_atom(eng, f->name->name);
		word_t arity = make_integer(eng, f->arity);

		return unify(eng, args[1], name) &&
		       unify(eng, args[2], arity);
	}

	return 0;
}

/* arg/3 - arg(N, Term, Arg) */
int builtin_arg(prolog_engine_t *eng, word_t *args) {
	word_t n_term = deref(eng, args[0]);

	if (!IS_INT(n_term))
		return 0;

	int n = GET_INT(n_term);
	word_t arg;

	if (term_arg(eng, n, args[1], &arg)) {
		return unify(eng, args[2], arg);
	}

	return 0;
}

/* =../2 - Univ */
int builtin_univ(prolog_engine_t *eng, word_t *args) {
	word_t term = deref(eng, args[0]);
	word_t list = deref(eng, args[1]);

	if (!IS_REF(term)) {
		/* Term to list */
		word_t result_list;
		if (term_univ(eng, term, &result_list)) {
			return unify(eng, args[1], result_list);
		}
	} else if (!IS_REF(list)) {
		/* List to term */
		word_t result_term;
		if (term_from_univ(eng, list, &result_term)) {
			return unify(eng, args[0], result_term);
		}
	}

	return 0;
}

/* write/1 - Write term */
int builtin_write(prolog_engine_t *eng, word_t *args) {
	print_term(eng, eng->current_output->fp, args[0]);
	return 1;
}

/* writeln/1 - Write term with newline */
int builtin_writeln(prolog_engine_t *eng, word_t *args) {
	print_term(eng, eng->current_output->fp, args[0]);
	fprintf(eng->current_output->fp, "\n");
	return 1;
}

/* nl/0 - Write newline */
int builtin_nl(prolog_engine_t *eng, word_t *args) {
	fprintf(eng->current_output->fp, "\n");
	return 1;
}

/* halt/0 - Exit */
int builtin_halt(prolog_engine_t *eng, word_t *args) {
	exit(0);
	return 1;
}

/* halt/1 - Exit with code */
int builtin_halt1(prolog_engine_t *eng, word_t *args) {
	word_t code = deref(eng, args[0]);

	if (IS_INT(code)) {
		exit(GET_INT(code));
	}

	exit(1);
	return 1;
}

/* Register all built-in predicates */
void register_builtins(prolog_engine_t *eng) {
	/* Control */
	register_builtin(eng, "true", 0, builtin_true);
	register_builtin(eng, "fail", 0, builtin_fail);
	register_builtin(eng, "!", 0, builtin_cut);

	/* Unification */
	register_builtin(eng, "=", 2, builtin_unify);
	register_builtin(eng, "\\=", 2, builtin_not_unify);

	/* Equality */
	register_builtin(eng, "==", 2, builtin_equal);
	register_builtin(eng, "\\==", 2, builtin_not_equal);

	/* Term comparison */
	register_builtin(eng, "@<", 2, builtin_term_less);
	register_builtin(eng, "@=<", 2, builtin_term_lesseq);
	register_builtin(eng, "@>", 2, builtin_term_greater);
	register_builtin(eng, "@>=", 2, builtin_term_greatereq);

	/* Type testing */
	register_builtin(eng, "var", 1, builtin_var);
	register_builtin(eng, "nonvar", 1, builtin_nonvar);
	register_builtin(eng, "atom", 1, builtin_atom);
	register_builtin(eng, "number", 1, builtin_number);
	register_builtin(eng, "integer", 1, builtin_integer);
	register_builtin(eng, "float", 1, builtin_float_test);
	register_builtin(eng, "atomic", 1, builtin_atomic);
	register_builtin(eng, "compound", 1, builtin_compound);
	register_builtin(eng, "ground", 1, builtin_ground);

	/* Term manipulation */
	register_builtin(eng, "functor", 3, builtin_functor);
	register_builtin(eng, "arg", 3, builtin_arg);
	register_builtin(eng, "=..", 2, builtin_univ);

	/* I/O */
	register_builtin(eng, "write", 1, builtin_write);
	register_builtin(eng, "writeln", 1, builtin_writeln);
	register_builtin(eng, "nl", 0, builtin_nl);

	/* System */
	register_builtin(eng, "halt", 0, builtin_halt);
	register_builtin(eng, "halt", 1, builtin_halt1);
}
