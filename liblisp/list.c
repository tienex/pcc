/*
 * Copyright (c) 2025 PCC Common LISP Runtime Library
 *
 * List operations implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "liblisp.h"

/*
 * Get CAR (first element) of a cons cell
 */
lisp_object_t *
lisp_car(lisp_object_t *obj)
{
	if (obj->type == LISP_TYPE_NIL)
		return LISP_NIL;

	if (obj->type != LISP_TYPE_CONS) {
		lisp_error("CAR: argument must be a cons cell or NIL");
		return LISP_NIL;
	}

	return obj->value.cons.car;
}

/*
 * Get CDR (rest) of a cons cell
 */
lisp_object_t *
lisp_cdr(lisp_object_t *obj)
{
	if (obj->type == LISP_TYPE_NIL)
		return LISP_NIL;

	if (obj->type != LISP_TYPE_CONS) {
		lisp_error("CDR: argument must be a cons cell or NIL");
		return LISP_NIL;
	}

	return obj->value.cons.cdr;
}

/*
 * Create a cons cell
 */
lisp_object_t *
lisp_cons(lisp_object_t *car, lisp_object_t *cdr)
{
	return lisp_make_cons(car, cdr);
}

/*
 * Create a list from multiple arguments
 */
lisp_object_t *
lisp_list(int n, ...)
{
	va_list ap;
	lisp_object_t *result = LISP_NIL;
	lisp_object_t *tail = NULL;
	int i;

	va_start(ap, n);

	for (i = 0; i < n; i++) {
		lisp_object_t *elem = va_arg(ap, lisp_object_t *);
		lisp_object_t *new_cell = lisp_make_cons(elem, LISP_NIL);

		if (result == LISP_NIL) {
			result = new_cell;
			tail = new_cell;
		} else {
			tail->value.cons.cdr = new_cell;
			tail = new_cell;
		}
	}

	va_end(ap);
	return result;
}

/*
 * Append two lists
 */
lisp_object_t *
lisp_append(lisp_object_t *list1, lisp_object_t *list2)
{
	if (list1->type == LISP_TYPE_NIL)
		return list2;

	lisp_object_t *result = lisp_make_cons(lisp_car(list1), LISP_NIL);
	lisp_object_t *tail = result;
	list1 = lisp_cdr(list1);

	while (list1->type != LISP_TYPE_NIL) {
		lisp_object_t *new_cell = lisp_make_cons(lisp_car(list1), LISP_NIL);
		tail->value.cons.cdr = new_cell;
		tail = new_cell;
		list1 = lisp_cdr(list1);
	}

	tail->value.cons.cdr = list2;
	return result;
}

/*
 * Reverse a list
 */
lisp_object_t *
lisp_reverse(lisp_object_t *list)
{
	lisp_object_t *result = LISP_NIL;

	while (list->type != LISP_TYPE_NIL) {
		result = lisp_make_cons(lisp_car(list), result);
		list = lisp_cdr(list);
	}

	return result;
}

/*
 * Get length of a list
 */
lisp_object_t *
lisp_length(lisp_object_t *list)
{
	long count = 0;

	while (list->type != LISP_TYPE_NIL) {
		count++;
		list = lisp_cdr(list);
	}

	return lisp_make_integer(count);
}

/*
 * Get nth element of a list (0-indexed)
 */
lisp_object_t *
lisp_nth(lisp_object_t *n, lisp_object_t *list)
{
	if (n->type != LISP_TYPE_INTEGER) {
		lisp_error("NTH: first argument must be an integer");
		return LISP_NIL;
	}

	long index = n->value.integer;
	long i;

	for (i = 0; i < index && list->type != LISP_TYPE_NIL; i++) {
		list = lisp_cdr(list);
	}

	if (list->type == LISP_TYPE_NIL)
		return LISP_NIL;

	return lisp_car(list);
}
