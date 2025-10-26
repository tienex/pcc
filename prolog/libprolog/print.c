/*
 * Copyright (c) 2025 PCC Prolog Runtime Library
 *
 * Term printing
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

/* Print term in readable format */
void print_term(prolog_engine_t *eng, FILE *fp, word_t term) {
	term = deref(eng, term);

	if (IS_REF(term)) {
		fprintf(fp, "_G%ld", (long)UNTAG(term));
		return;
	}

	if (IS_INT(term)) {
		fprintf(fp, "%ld", (long)GET_INT(term));
		return;
	}

	if (IS_FLOAT(term)) {
		float_box_t *f = (float_box_t *)UNTAG(term);
		fprintf(fp, "%g", f->value);
		return;
	}

	if (IS_ATOM(term)) {
		atom_entry_t *a = (atom_entry_t *)UNTAG(term);
		/* TODO: Quote atom if needed */
		fprintf(fp, "%s", a->name);
		return;
	}

	if (IS_STR(term)) {
		string_t *s = (string_t *)UNTAG(term);
		fprintf(fp, "\"");
		for (size_t i = 0; i < s->length; i++) {
			char c = s->data[i];
			if (c == '"') fprintf(fp, "\\\"");
			else if (c == '\\') fprintf(fp, "\\\\");
			else if (c == '\n') fprintf(fp, "\\n");
			else if (c == '\t') fprintf(fp, "\\t");
			else fprintf(fp, "%c", c);
		}
		fprintf(fp, "\"");
		return;
	}

	if (IS_STRUCT(term)) {
		word_t *p = (word_t *)UNTAG(term);
		functor_t *f = (functor_t *)p[0];

		fprintf(fp, "%s", f->name->name);

		if (f->arity > 0) {
			fprintf(fp, "(");
			for (int i = 0; i < f->arity; i++) {
				if (i > 0) fprintf(fp, ", ");
				print_term(eng, fp, p[i + 1]);
			}
			fprintf(fp, ")");
		}
		return;
	}

	if (IS_LIST(term)) {
		fprintf(fp, "[");

		int first = 1;
		while (IS_LIST(term)) {
			if (!first) fprintf(fp, ", ");
			first = 0;

			word_t *p = (word_t *)UNTAG(term);
			print_term(eng, fp, p[0]);  /* Head */

			term = deref(eng, p[1]);    /* Tail */
		}

		/* Check for proper list (ends with []) */
		if (IS_ATOM(term)) {
			atom_entry_t *a = (atom_entry_t *)UNTAG(term);
			if (strcmp(a->name, "[]") != 0) {
				/* Improper list */
				fprintf(fp, "|");
				print_term(eng, fp, term);
			}
		} else if (!IS_REF(term)) {
			/* Improper list */
			fprintf(fp, "|");
			print_term(eng, fp, term);
		}

		fprintf(fp, "]");
		return;
	}

	fprintf(fp, "<unknown>");
}

/* Print term in canonical form (quoted, fully parenthesized) */
void print_term_canonical(prolog_engine_t *eng, FILE *fp, word_t term) {
	term = deref(eng, term);

	if (IS_ATOM(term)) {
		atom_entry_t *a = (atom_entry_t *)UNTAG(term);
		/* Always quote atoms in canonical form */
		fprintf(fp, "'%s'", a->name);
		return;
	}

	if (IS_STRUCT(term)) {
		word_t *p = (word_t *)UNTAG(term);
		functor_t *f = (functor_t *)p[0];

		fprintf(fp, "'%s'(", f->name->name);
		for (int i = 0; i < f->arity; i++) {
			if (i > 0) fprintf(fp, ",");
			print_term_canonical(eng, fp, p[i + 1]);
		}
		fprintf(fp, ")");
		return;
	}

	/* For other types, use regular printing */
	print_term(eng, fp, term);
}
