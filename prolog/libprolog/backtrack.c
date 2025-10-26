/*
 * Copyright (c) 2025 PCC Prolog Runtime Library
 *
 * Backtracking and choice point management
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

/* Create choice point */
choice_point_t *create_choice_point(prolog_engine_t *eng, int arity) {
	size_t size = sizeof(choice_point_t) + arity * sizeof(word_t);
	choice_point_t *cp = (choice_point_t *)stack_alloc(eng,
	                      (size + sizeof(word_t) - 1) / sizeof(word_t));

	if (!cp)
		return NULL;

	cp->continuation = eng->continuation;
	cp->env = (word_t *)eng->env;
	cp->trail_top = eng->trail_top;
	cp->heap_top = eng->heap_top;
	cp->prev = eng->choice;
	cp->clause = NULL;  /* Set by caller */
	cp->arity = arity;

	eng->choice = cp;
	eng->choice_points++;

	return cp;
}

/* Restore state from choice point (for backtracking) */
void restore_choice_point(prolog_engine_t *eng, choice_point_t *cp) {
	if (!cp)
		return;

	/* Restore registers */
	eng->continuation = cp->continuation;
	eng->env = (environment_t *)cp->env;

	/* Unwind trail */
	unwind_trail(eng, cp->trail_top);

	/* Restore heap top */
	eng->heap_top = cp->heap_top;
}

/* Discard choice point (deterministic success) */
void discard_choice_point(prolog_engine_t *eng) {
	if (!eng->choice)
		return;

	choice_point_t *cp = eng->choice;

	/* Move stack pointer back */
	size_t size = sizeof(choice_point_t) + cp->arity * sizeof(word_t);
	size_t words = (size + sizeof(word_t) - 1) / sizeof(word_t);

	eng->stack_top = (word_t *)cp;
	eng->choice = cp->prev;
}

/* Create environment frame */
environment_t *create_environment(prolog_engine_t *eng, int size) {
	size_t total = sizeof(environment_t) + size * sizeof(word_t);
	environment_t *env = (environment_t *)stack_alloc(eng,
	                      (total + sizeof(word_t) - 1) / sizeof(word_t));

	if (!env)
		return NULL;

	env->continuation = eng->continuation;
	env->prev = eng->env;
	env->size = size;

	/* Initialize slots to unbound variables */
	for (int i = 0; i < size; i++) {
		word_t *slot = &env->slots[i];
		env->slots[i] = MAKE_TAG(slot, TAG_REF);
	}

	eng->env = env;

	return env;
}

/* Discard environment frame */
void discard_environment(prolog_engine_t *eng) {
	if (!eng->env)
		return;

	environment_t *env = eng->env;

	/* Restore continuation and previous environment */
	eng->continuation = env->continuation;
	eng->env = env->prev;

	/* Move stack pointer back */
	eng->stack_top = (word_t *)env;
}

/* Get current choice point depth */
int choice_point_depth(prolog_engine_t *eng) {
	int depth = 0;
	choice_point_t *cp = eng->choice;

	while (cp) {
		depth++;
		cp = cp->prev;
	}

	return depth;
}

/* Trim choice points up to a certain depth (for cut) */
void trim_choice_points(prolog_engine_t *eng, int depth) {
	while (choice_point_depth(eng) > depth) {
		discard_choice_point(eng);
	}
}

/* Save choice point state for exception handling */
typedef struct saved_state {
	choice_point_t *choice;
	environment_t *env;
	word_t *heap_top;
	word_t *trail_top;
	word_t *stack_top;
} saved_state_t;

void save_state(prolog_engine_t *eng, saved_state_t *state) {
	state->choice = eng->choice;
	state->env = eng->env;
	state->heap_top = eng->heap_top;
	state->trail_top = eng->trail_top;
	state->stack_top = eng->stack_top;
}

void restore_state(prolog_engine_t *eng, saved_state_t *state) {
	eng->choice = state->choice;
	eng->env = state->env;
	eng->heap_top = state->heap_top;
	unwind_trail(eng, state->trail_top);
	eng->stack_top = state->stack_top;
}
