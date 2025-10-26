/*
 * Copyright (c) 2025 PCC Prolog Runtime Library
 *
 * Predicate management
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

/* Hash functor for predicate table */
static unsigned int hash_functor(functor_t *f) {
	unsigned int hash = f->name->hash;
	hash = (hash * 31) + f->arity;
	return hash;
}

/* Define/lookup predicate */
predicate_t *predicate_define(prolog_engine_t *eng, functor_t *f) {
	unsigned int hash = hash_functor(f);
	int slot = hash % eng->pred_table_size;

	/* Check if exists */
	for (predicate_t *p = eng->pred_table[slot]; p; p = p->next) {
		if (p->functor->name == f->name && p->functor->arity == f->arity)
			return p;
	}

	/* Create new */
	predicate_t *pred = calloc(1, sizeof(predicate_t));
	pred->functor = f;
	pred->builtin = NULL;
	pred->clauses = NULL;
	pred->clause_count = 0;
	pred->is_dynamic = 0;
	pred->is_multifile = 0;

	/* Add to table */
	pred->next = eng->pred_table[slot];
	eng->pred_table[slot] = pred;

	return pred;
}

/* Lookup predicate */
predicate_t *predicate_lookup(prolog_engine_t *eng, functor_t *f) {
	unsigned int hash = hash_functor(f);
	int slot = hash % eng->pred_table_size;

	for (predicate_t *p = eng->pred_table[slot]; p; p = p->next) {
		if (p->functor->name == f->name && p->functor->arity == f->arity)
			return p;
	}

	return NULL;
}

/* Add clause to predicate */
void predicate_add_clause(prolog_engine_t *eng, predicate_t *pred,
                         word_t *head, word_t *body) {
	clause_t *c = malloc(sizeof(clause_t));
	c->pred = pred;
	c->head = head;
	c->body = body;
	c->next = NULL;

	/* Append to end */
	if (!pred->clauses) {
		pred->clauses = (word_t *)c;
	} else {
		clause_t *last = (clause_t *)pred->clauses;
		while (last->next)
			last = last->next;
		last->next = c;
	}

	pred->clause_count++;
}

/* Call predicate with arguments */
int predicate_call(prolog_engine_t *eng, predicate_t *pred, word_t *args) {
	/* Built-in? */
	if (pred->builtin) {
		return pred->builtin(eng, args);
	}

	/* TODO: Execute clauses with backtracking */
	/* This is a placeholder - proper execution requires
	 * WAM-style instruction interpretation or compilation */

	return 0;
}
