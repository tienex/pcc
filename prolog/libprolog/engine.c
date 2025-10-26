/*
 * Copyright (c) 2025 PCC Prolog Runtime Library
 *
 * Engine initialization and memory management
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

/* Global engine instance */
prolog_engine_t *prolog_engine = NULL;

/* Initialize Prolog engine */
prolog_engine_t *prolog_init(void) {
	prolog_engine_t *eng;

	eng = calloc(1, sizeof(prolog_engine_t));
	if (!eng) {
		fprintf(stderr, "Failed to allocate engine\n");
		return NULL;
	}

	/* Allocate memory areas */
	eng->heap = calloc(HEAP_SIZE, sizeof(word_t));
	eng->stack = calloc(STACK_SIZE, sizeof(word_t));
	eng->trail = calloc(TRAIL_SIZE, sizeof(word_t));
	eng->pdl = calloc(PDL_SIZE, sizeof(word_t));

	if (!eng->heap || !eng->stack || !eng->trail || !eng->pdl) {
		fprintf(stderr, "Failed to allocate memory areas\n");
		prolog_cleanup(eng);
		return NULL;
	}

	/* Initialize pointers */
	eng->heap_top = eng->heap;
	eng->heap_limit = eng->heap + HEAP_SIZE;

	eng->stack_top = eng->stack;
	eng->stack_limit = eng->stack + STACK_SIZE;

	eng->trail_top = eng->trail;
	eng->trail_limit = eng->trail + TRAIL_SIZE;

	eng->pdl_top = eng->pdl;
	eng->pdl_limit = eng->pdl + PDL_SIZE;

	/* Initialize atom table */
	eng->atom_table_size = 1024;
	eng->atom_table = calloc(eng->atom_table_size, sizeof(atom_entry_t *));

	/* Initialize predicate table */
	eng->pred_table_size = 1024;
	eng->pred_table = calloc(eng->pred_table_size, sizeof(predicate_t *));

	/* Initialize state */
	eng->env = NULL;
	eng->choice = NULL;
	eng->continuation = NULL;

	/* Initialize streams */
	eng->current_input = NULL;
	eng->current_output = NULL;
	eng->current_error = NULL;
	eng->stream_list = NULL;

	/* Open standard streams */
	stream_t *stdin_stream = calloc(1, sizeof(stream_t));
	stdin_stream->fp = stdin;
	stdin_stream->name = strdup("user_input");
	stdin_stream->flags = STREAM_READ;
	eng->current_input = stdin_stream;

	stream_t *stdout_stream = calloc(1, sizeof(stream_t));
	stdout_stream->fp = stdout;
	stdout_stream->name = strdup("user_output");
	stdout_stream->flags = STREAM_WRITE;
	eng->current_output = stdout_stream;

	stream_t *stderr_stream = calloc(1, sizeof(stream_t));
	stderr_stream->fp = stderr;
	stderr_stream->name = strdup("user_error");
	stderr_stream->flags = STREAM_WRITE;
	eng->current_error = stderr_stream;

	/* Add to stream list */
	stdin_stream->next = stdout_stream;
	stdout_stream->next = stderr_stream;
	stderr_stream->next = NULL;
	eng->stream_list = stdin_stream;

	/* Initialize flags */
	eng->debug = 0;
	eng->trace = 0;
	eng->occurs_check = 0;  /* ISO default: off */

	/* Initialize statistics */
	eng->inferences = 0;
	eng->choice_points = 0;
	eng->unifications = 0;

	/* Set global instance */
	if (!prolog_engine)
		prolog_engine = eng;

	/* Register built-in predicates */
	register_builtins(eng);

	return eng;
}

/* Cleanup engine */
void prolog_cleanup(prolog_engine_t *eng) {
	if (!eng)
		return;

	/* Free memory areas */
	if (eng->heap) free(eng->heap);
	if (eng->stack) free(eng->stack);
	if (eng->trail) free(eng->trail);
	if (eng->pdl) free(eng->pdl);

	/* Free atom table */
	if (eng->atom_table) {
		for (int i = 0; i < eng->atom_table_size; i++) {
			atom_entry_t *a = eng->atom_table[i];
			while (a) {
				atom_entry_t *next = a->next;
				free(a->name);
				free(a);
				a = next;
			}
		}
		free(eng->atom_table);
	}

	/* Free predicate table */
	if (eng->pred_table) {
		for (int i = 0; i < eng->pred_table_size; i++) {
			predicate_t *p = eng->pred_table[i];
			while (p) {
				predicate_t *next = p->next;
				/* Free clauses if dynamic */
				free(p);
				p = next;
			}
		}
		free(eng->pred_table);
	}

	/* Free streams */
	stream_t *s = eng->stream_list;
	while (s) {
		stream_t *next = s->next;
		if (s->name) free(s->name);
		if (s->fp && s->fp != stdin && s->fp != stdout && s->fp != stderr)
			fclose(s->fp);
		free(s);
		s = next;
	}

	free(eng);

	if (prolog_engine == eng)
		prolog_engine = NULL;
}

/* Reset engine to initial state */
void prolog_reset(prolog_engine_t *eng) {
	if (!eng)
		return;

	/* Reset memory pointers */
	eng->heap_top = eng->heap;
	eng->stack_top = eng->stack;
	eng->trail_top = eng->trail;
	eng->pdl_top = eng->pdl;

	/* Reset state */
	eng->env = NULL;
	eng->choice = NULL;
	eng->continuation = NULL;

	/* Reset statistics */
	eng->inferences = 0;
	eng->choice_points = 0;
	eng->unifications = 0;
}

/* Allocate from heap */
word_t *heap_alloc(prolog_engine_t *eng, size_t words) {
	if (eng->heap_top + words > eng->heap_limit) {
		prolog_error(eng, "resource_error", "heap exhausted");
		return NULL;
	}

	word_t *ptr = eng->heap_top;
	eng->heap_top += words;
	return ptr;
}

/* Allocate from stack */
word_t *stack_alloc(prolog_engine_t *eng, size_t words) {
	if (eng->stack_top + words > eng->stack_limit) {
		prolog_error(eng, "resource_error", "stack exhausted");
		return NULL;
	}

	word_t *ptr = eng->stack_top;
	eng->stack_top += words;
	return ptr;
}

/* Check if address is on heap */
int is_heap_addr(prolog_engine_t *eng, word_t *addr) {
	return addr >= eng->heap && addr < eng->heap_top;
}

/* Check if address is on stack */
int is_stack_addr(prolog_engine_t *eng, word_t *addr) {
	return addr >= eng->stack && addr < eng->stack_top;
}

/* Hash function for strings */
static uint32_t hash_string(const char *str, int len) {
	uint32_t hash = 5381;
	for (int i = 0; i < len; i++)
		hash = ((hash << 5) + hash) + str[i];
	return hash;
}

/* Intern an atom */
atom_entry_t *atom_intern(prolog_engine_t *eng, const char *name, int len) {
	uint32_t hash = hash_string(name, len);
	int slot = hash % eng->atom_table_size;

	/* Check if atom exists */
	for (atom_entry_t *a = eng->atom_table[slot]; a; a = a->next) {
		if (a->length == len && a->hash == hash &&
		    memcmp(a->name, name, len) == 0)
			return a;
	}

	/* Create new atom */
	atom_entry_t *a = malloc(sizeof(atom_entry_t));
	a->name = malloc(len + 1);
	memcpy(a->name, name, len);
	a->name[len] = '\0';
	a->length = len;
	a->hash = hash;

	/* Add to table */
	a->next = eng->atom_table[slot];
	eng->atom_table[slot] = a;

	return a;
}

/* Lookup atom (returns NULL if not found) */
atom_entry_t *atom_lookup(prolog_engine_t *eng, const char *name, int len) {
	uint32_t hash = hash_string(name, len);
	int slot = hash % eng->atom_table_size;

	for (atom_entry_t *a = eng->atom_table[slot]; a; a = a->next) {
		if (a->length == len && a->hash == hash &&
		    memcmp(a->name, name, len) == 0)
			return a;
	}

	return NULL;
}

/* Create functor */
functor_t *functor_create(prolog_engine_t *eng, const char *name, int arity) {
	functor_t *f = malloc(sizeof(functor_t));
	f->name = atom_intern_cstr(eng, name);
	f->arity = arity;
	return f;
}

/* Lookup functor */
functor_t *functor_lookup(prolog_engine_t *eng, const char *name, int arity) {
	atom_entry_t *a = atom_lookup_cstr(eng, name);
	if (!a)
		return NULL;

	functor_t *f = malloc(sizeof(functor_t));
	f->name = a;
	f->arity = arity;
	return f;
}

/* Print statistics */
void prolog_statistics(prolog_engine_t *eng, FILE *fp) {
	fprintf(fp, "Prolog Statistics:\n");
	fprintf(fp, "  Inferences:    %llu\n", (unsigned long long)eng->inferences);
	fprintf(fp, "  Unifications:  %llu\n", (unsigned long long)eng->unifications);
	fprintf(fp, "  Choice points: %llu\n", (unsigned long long)eng->choice_points);
	fprintf(fp, "  Heap used:     %ld / %d words\n",
	        (long)(eng->heap_top - eng->heap), HEAP_SIZE);
	fprintf(fp, "  Stack used:    %ld / %d words\n",
	        (long)(eng->stack_top - eng->stack), STACK_SIZE);
	fprintf(fp, "  Trail used:    %ld / %d words\n",
	        (long)(eng->trail_top - eng->trail), TRAIL_SIZE);
}
