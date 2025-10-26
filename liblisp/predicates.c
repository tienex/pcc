/*
 * Copyright (c) 2025 PCC Common LISP Runtime Library
 *
 * Type predicates implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include "liblisp.h"

/*
 * NULL: test if object is NIL
 */
lisp_object_t *
lisp_null(lisp_object_t *obj)
{
	return (obj->type == LISP_TYPE_NIL) ? LISP_T : LISP_NIL;
}

/*
 * ATOM: test if object is an atom (not a cons cell)
 */
lisp_object_t *
lisp_atom(lisp_object_t *obj)
{
	return (obj->type != LISP_TYPE_CONS) ? LISP_T : LISP_NIL;
}

/*
 * CONSP: test if object is a cons cell
 */
lisp_object_t *
lisp_consp(lisp_object_t *obj)
{
	return (obj->type == LISP_TYPE_CONS) ? LISP_T : LISP_NIL;
}

/*
 * LISTP: test if object is a list (cons or NIL)
 */
lisp_object_t *
lisp_listp(lisp_object_t *obj)
{
	return (obj->type == LISP_TYPE_CONS || obj->type == LISP_TYPE_NIL) ?
	       LISP_T : LISP_NIL;
}

/*
 * NUMBERP: test if object is a number
 */
lisp_object_t *
lisp_numberp(lisp_object_t *obj)
{
	return (obj->type == LISP_TYPE_INTEGER || obj->type == LISP_TYPE_FLOAT) ?
	       LISP_T : LISP_NIL;
}

/*
 * INTEGERP: test if object is an integer
 */
lisp_object_t *
lisp_integerp(lisp_object_t *obj)
{
	return (obj->type == LISP_TYPE_INTEGER) ? LISP_T : LISP_NIL;
}

/*
 * FLOATP: test if object is a float
 */
lisp_object_t *
lisp_floatp(lisp_object_t *obj)
{
	return (obj->type == LISP_TYPE_FLOAT) ? LISP_T : LISP_NIL;
}

/*
 * SYMBOLP: test if object is a symbol
 */
lisp_object_t *
lisp_symbolp(lisp_object_t *obj)
{
	return (obj->type == LISP_TYPE_SYMBOL) ? LISP_T : LISP_NIL;
}

/*
 * STRINGP: test if object is a string
 */
lisp_object_t *
lisp_stringp(lisp_object_t *obj)
{
	return (obj->type == LISP_TYPE_STRING) ? LISP_T : LISP_NIL;
}
