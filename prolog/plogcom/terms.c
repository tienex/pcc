/*
 * Copyright (c) 2025 PCC Prolog Compiler
 *
 * Term creation and manipulation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Create atom term */
struct term *make_atom(char *name) {
	struct term *t = malloc_check(sizeof(struct term));
	t->type = TERM_ATOM;
	t->data.atom = strdup_check(name);
	t->lineno = lineno;
	t->column = current_column;
	t->next = NULL;
	return t;
}

/* Create variable term */
struct term *make_variable(char *name) {
	struct term *t = malloc_check(sizeof(struct term));
	t->type = TERM_VARIABLE;
	t->data.var = strdup_check(name);
	t->lineno = lineno;
	t->column = current_column;
	t->next = NULL;
	return t;
}

/* Create integer term */
struct term *make_integer(long value) {
	struct term *t = malloc_check(sizeof(struct term));
	t->type = TERM_INTEGER;
	t->data.ival = value;
	t->lineno = lineno;
	t->column = current_column;
	t->next = NULL;
	return t;
}

/* Create float term */
struct term *make_float(double value) {
	struct term *t = malloc_check(sizeof(struct term));
	t->type = TERM_FLOAT;
	t->data.fval = value;
	t->lineno = lineno;
	t->column = current_column;
	t->next = NULL;
	return t;
}

/* Create string term */
struct term *make_string(char *value) {
	struct term *t = malloc_check(sizeof(struct term));
	t->type = TERM_STRING;
	t->data.str = strdup_check(value);
	t->lineno = lineno;
	t->column = current_column;
	t->next = NULL;
	return t;
}

/* Create compound term */
struct term *make_compound(char *functor, struct termlist *args) {
	struct term *t = malloc_check(sizeof(struct term));
	t->type = TERM_COMPOUND;
	t->data.compound.functor = strdup_check(functor);
	t->data.compound.args = args;
	t->data.compound.arity = termlist_length(args);
	t->lineno = lineno;
	t->column = current_column;
	t->next = NULL;
	return t;
}

/* Create list from elements */
struct term *make_list(struct termlist *elements) {
	if (!elements)
		return make_nil();

	/* Convert list to nested cons cells */
	struct term *result = make_nil();
	struct termlist *p;

	/* Build list from right to left */
	for (p = elements; p != NULL; p = p->next) {
		if (p->next == NULL) {
			/* Last element */
			result = make_cons(p->term, result);
		}
	}

	/* Now build from left */
	result = make_nil();
	struct termlist *rev = NULL;

	/* Reverse the list */
	for (p = elements; p != NULL; p = p->next) {
		struct termlist *new = malloc_check(sizeof(struct termlist));
		new->term = p->term;
		new->next = rev;
		rev = new;
	}

	/* Build cons cells */
	for (p = rev; p != NULL; p = p->next) {
		result = make_cons(p->term, result);
	}

	/* Free reversed list structure */
	while (rev) {
		struct termlist *tmp = rev;
		rev = rev->next;
		free(tmp);
	}

	return result;
}

/* Create cons cell [Head|Tail] */
struct term *make_cons(struct term *head, struct term *tail) {
	struct term *t = malloc_check(sizeof(struct term));
	t->type = TERM_CONS;
	t->data.cons.head = head;
	t->data.cons.tail = tail;
	t->lineno = lineno;
	t->column = current_column;
	t->next = NULL;
	return t;
}

/* Create empty list [] */
struct term *make_nil(void) {
	struct term *t = malloc_check(sizeof(struct term));
	t->type = TERM_NIL;
	t->lineno = lineno;
	t->column = current_column;
	t->next = NULL;
	return t;
}

/* Create anonymous variable _ */
struct term *make_anonymous(void) {
	struct term *t = malloc_check(sizeof(struct term));
	t->type = TERM_ANON;
	t->lineno = lineno;
	t->column = current_column;
	t->next = NULL;
	return t;
}

/* Create cut ! */
struct term *make_cut(void) {
	struct term *t = malloc_check(sizeof(struct term));
	t->type = TERM_CUT;
	t->lineno = lineno;
	t->column = current_column;
	t->next = NULL;
	return t;
}

/* Create term list */
struct termlist *make_termlist(struct term *term) {
	struct termlist *tl = malloc_check(sizeof(struct termlist));
	tl->term = term;
	tl->next = NULL;
	tl->count = 1;
	return tl;
}

/* Append term to term list */
struct termlist *append_term(struct termlist *list, struct term *term) {
	if (!list)
		return make_termlist(term);

	struct termlist *new = malloc_check(sizeof(struct termlist));
	new->term = term;
	new->next = NULL;

	/* Find end of list */
	struct termlist *p = list;
	while (p->next)
		p = p->next;

	p->next = new;
	list->count++;

	return list;
}

/* Get term list length */
int termlist_length(struct termlist *list) {
	if (!list)
		return 0;
	return list->count;
}

/* Copy term (deep copy) */
struct term *copy_term(struct term *t) {
	if (!t)
		return NULL;

	struct term *copy = malloc_check(sizeof(struct term));
	copy->type = t->type;
	copy->lineno = t->lineno;
	copy->column = t->column;
	copy->next = NULL;

	switch (t->type) {
	case TERM_ATOM:
		copy->data.atom = strdup_check(t->data.atom);
		break;
	case TERM_VARIABLE:
		copy->data.var = strdup_check(t->data.var);
		break;
	case TERM_INTEGER:
		copy->data.ival = t->data.ival;
		break;
	case TERM_FLOAT:
		copy->data.fval = t->data.fval;
		break;
	case TERM_STRING:
		copy->data.str = strdup_check(t->data.str);
		break;
	case TERM_COMPOUND:
		copy->data.compound.functor = strdup_check(t->data.compound.functor);
		copy->data.compound.arity = t->data.compound.arity;
		/* Copy argument list */
		{
			struct termlist *src = t->data.compound.args;
			struct termlist *dst = NULL;
			struct termlist **dst_ptr = &dst;
			while (src) {
				*dst_ptr = malloc_check(sizeof(struct termlist));
				(*dst_ptr)->term = copy_term(src->term);
				(*dst_ptr)->next = NULL;
				dst_ptr = &(*dst_ptr)->next;
				src = src->next;
			}
			copy->data.compound.args = dst;
		}
		break;
	case TERM_CONS:
		copy->data.cons.head = copy_term(t->data.cons.head);
		copy->data.cons.tail = copy_term(t->data.cons.tail);
		break;
	case TERM_NIL:
	case TERM_ANON:
	case TERM_CUT:
		/* No additional data to copy */
		break;
	default:
		break;
	}

	return copy;
}

/* Free term */
void free_term(struct term *t) {
	if (!t)
		return;

	switch (t->type) {
	case TERM_ATOM:
		free(t->data.atom);
		break;
	case TERM_VARIABLE:
		free(t->data.var);
		break;
	case TERM_STRING:
		free(t->data.str);
		break;
	case TERM_COMPOUND:
		free(t->data.compound.functor);
		/* Free argument list */
		{
			struct termlist *p = t->data.compound.args;
			while (p) {
				struct termlist *tmp = p;
				free_term(p->term);
				p = p->next;
				free(tmp);
			}
		}
		break;
	case TERM_CONS:
		free_term(t->data.cons.head);
		free_term(t->data.cons.tail);
		break;
	default:
		break;
	}

	free(t);
}

/* Print term */
void print_term(FILE *fp, struct term *t) {
	if (!t) {
		fprintf(fp, "(null)");
		return;
	}

	switch (t->type) {
	case TERM_ATOM:
		fprintf(fp, "%s", t->data.atom);
		break;
	case TERM_VARIABLE:
		fprintf(fp, "%s", t->data.var);
		break;
	case TERM_INTEGER:
		fprintf(fp, "%ld", t->data.ival);
		break;
	case TERM_FLOAT:
		fprintf(fp, "%g", t->data.fval);
		break;
	case TERM_STRING:
		fprintf(fp, "\"%s\"", t->data.str);
		break;
	case TERM_COMPOUND:
		fprintf(fp, "%s(", t->data.compound.functor);
		{
			struct termlist *p = t->data.compound.args;
			while (p) {
				print_term(fp, p->term);
				if (p->next)
					fprintf(fp, ", ");
				p = p->next;
			}
		}
		fprintf(fp, ")");
		break;
	case TERM_LIST:
		fprintf(fp, "[");
		/* Print list elements */
		fprintf(fp, "]");
		break;
	case TERM_CONS:
		fprintf(fp, "[");
		print_term(fp, t->data.cons.head);
		if (t->data.cons.tail->type != TERM_NIL) {
			fprintf(fp, "|");
			print_term(fp, t->data.cons.tail);
		}
		fprintf(fp, "]");
		break;
	case TERM_NIL:
		fprintf(fp, "[]");
		break;
	case TERM_ANON:
		fprintf(fp, "_");
		break;
	case TERM_CUT:
		fprintf(fp, "!");
		break;
	default:
		fprintf(fp, "<unknown>");
		break;
	}
}
