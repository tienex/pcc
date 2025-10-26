/*
 * Copyright (c) 2025 PCC Prolog Runtime Library
 *
 * Term construction and manipulation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

/* Create reference (unbound variable) */
word_t make_ref(prolog_engine_t *eng, word_t *addr) {
	return MAKE_TAG(addr, TAG_REF);
}

/* Create atom */
word_t make_atom(prolog_engine_t *eng, const char *name) {
	atom_entry_t *a = atom_intern_cstr(eng, name);
	return MAKE_TAG(a, TAG_ATOM);
}

/* Create integer */
word_t make_integer(prolog_engine_t *eng, intptr_t value) {
	/* Try tagged integer first */
	if (value >= MIN_TAGGED_INT && value <= MAX_TAGGED_INT) {
		return MAKE_INT(value);
	}

	/* Use boxed integer (on heap) */
	word_t *p = heap_alloc(eng, 1);
	if (!p) return 0;

	*p = value;
	return MAKE_TAG(p, TAG_INT);
}

/* Create float */
word_t make_float(prolog_engine_t *eng, double value) {
	word_t *p = heap_alloc(eng, sizeof(float_box_t) / sizeof(word_t) + 1);
	if (!p) return 0;

	float_box_t *f = (float_box_t *)p;
	f->value = value;

	return MAKE_TAG(f, TAG_FLOAT);
}

/* Create string */
word_t make_string(prolog_engine_t *eng, const char *str, size_t len) {
	size_t words = (sizeof(string_t) + len + sizeof(word_t)) / sizeof(word_t);
	word_t *p = heap_alloc(eng, words);
	if (!p) return 0;

	string_t *s = (string_t *)p;
	s->length = len;
	memcpy(s->data, str, len);
	s->data[len] = '\0';

	return MAKE_TAG(s, TAG_STR);
}

/* Create structure */
word_t make_struct(prolog_engine_t *eng, functor_t *f, word_t *args) {
	word_t *p = heap_alloc(eng, f->arity + 1);
	if (!p) return 0;

	p[0] = (word_t)f;
	for (int i = 0; i < f->arity; i++) {
		p[i + 1] = args[i];
	}

	return MAKE_TAG(p, TAG_STRUCT);
}

/* Create list cons cell */
word_t make_list(prolog_engine_t *eng, word_t head, word_t tail) {
	word_t *p = heap_alloc(eng, 2);
	if (!p) return 0;

	p[0] = head;
	p[1] = tail;

	return MAKE_TAG(p, TAG_LIST);
}

/* Create empty list (nil) */
word_t make_nil(prolog_engine_t *eng) {
	return make_atom(eng, "[]");
}

/* Create new unbound variable */
word_t make_var(prolog_engine_t *eng) {
	word_t *p = heap_alloc(eng, 1);
	if (!p) return 0;

	word_t ref = MAKE_TAG(p, TAG_REF);
	*p = ref;  /* Points to itself */

	return ref;
}

/* Copy term (deep copy with variable renaming) */
word_t term_copy(prolog_engine_t *eng, word_t term) {
	/* TODO: Implement proper term copying with variable tracking */
	/* For now, just return the term (shallow copy) */
	return term;
}

/* Get functor from compound term */
int term_functor(prolog_engine_t *eng, word_t term, functor_t **f_out) {
	term = deref(eng, term);

	if (IS_STRUCT(term)) {
		word_t *p = (word_t *)UNTAG(term);
		*f_out = (functor_t *)p[0];
		return 1;
	}

	if (IS_ATOM(term)) {
		atom_entry_t *a = (atom_entry_t *)UNTAG(term);
		functor_t *f = malloc(sizeof(functor_t));
		f->name = a;
		f->arity = 0;
		*f_out = f;
		return 1;
	}

	if (IS_INT(term)) {
		/* Integer has no functor */
		return 0;
	}

	return 0;
}

/* Get nth argument of compound term (1-indexed) */
int term_arg(prolog_engine_t *eng, int n, word_t term, word_t *arg_out) {
	term = deref(eng, term);

	if (!IS_STRUCT(term) && !IS_LIST(term))
		return 0;

	if (IS_STRUCT(term)) {
		word_t *p = (word_t *)UNTAG(term);
		functor_t *f = (functor_t *)p[0];

		if (n < 1 || n > f->arity)
			return 0;

		*arg_out = p[n];
		return 1;
	}

	if (IS_LIST(term)) {
		word_t *p = (word_t *)UNTAG(term);

		if (n == 1) {
			*arg_out = p[0];  /* Head */
			return 1;
		}
		if (n == 2) {
			*arg_out = p[1];  /* Tail */
			return 1;
		}

		return 0;
	}

	return 0;
}

/* Convert term to list (=..) */
int term_univ(prolog_engine_t *eng, word_t term, word_t *list_out) {
	term = deref(eng, term);

	if (IS_ATOM(term)) {
		/* atom =.. [atom] */
		*list_out = make_list(eng, term, make_nil(eng));
		return 1;
	}

	if (IS_STRUCT(term)) {
		word_t *p = (word_t *)UNTAG(term);
		functor_t *f = (functor_t *)p[0];

		/* Build list [functor, arg1, arg2, ...] */
		word_t functor_atom = make_atom(eng, f->name->name);
		word_t result = make_nil(eng);

		/* Add arguments in reverse */
		for (int i = f->arity - 1; i >= 0; i--) {
			result = make_list(eng, p[i + 1], result);
		}

		/* Add functor at front */
		result = make_list(eng, functor_atom, result);

		*list_out = result;
		return 1;
	}

	/* Numbers, variables don't univ */
	return 0;
}

/* Build term from list (=..) */
int term_from_univ(prolog_engine_t *eng, word_t list, word_t *term_out) {
	list = deref(eng, list);

	if (!IS_LIST(list))
		return 0;

	word_t *p = (word_t *)UNTAG(list);
	word_t functor_term = deref(eng, p[0]);

	if (!IS_ATOM(functor_term))
		return 0;

	atom_entry_t *functor_atom = (atom_entry_t *)UNTAG(functor_term);

	/* Get arguments */
	word_t tail = deref(eng, p[1]);

	/* Count arguments */
	int arity = 0;
	word_t t = tail;
	while (IS_LIST(t)) {
		arity++;
		word_t *lp = (word_t *)UNTAG(t);
		t = deref(eng, lp[1]);
	}

	/* Just functor? */
	if (arity == 0) {
		*term_out = make_atom(eng, functor_atom->name);
		return 1;
	}

	/* Collect arguments */
	word_t *args = malloc(arity * sizeof(word_t));
	t = tail;
	for (int i = 0; i < arity; i++) {
		word_t *lp = (word_t *)UNTAG(t);
		args[i] = lp[0];
		t = deref(eng, lp[1]);
	}

	/* Create structure */
	functor_t *f = functor_create(eng, functor_atom->name, arity);
	*term_out = make_struct(eng, f, args);

	free(args);
	return 1;
}

/* Get arity of term */
int term_arity(prolog_engine_t *eng, word_t term) {
	term = deref(eng, term);

	if (IS_STRUCT(term)) {
		word_t *p = (word_t *)UNTAG(term);
		functor_t *f = (functor_t *)p[0];
		return f->arity;
	}

	if (IS_LIST(term))
		return 2;

	return 0;
}
