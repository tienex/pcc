/*
 * Copyright (c) 2025 PCC Common LISP Compiler
 *
 * Type system implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/*
 * Initialize type system
 */
void
init_types(void)
{
	/* In Common LISP, types are dynamic, so we just initialize
	 * the basic type constants here */
}

/*
 * Create integer value
 */
lisp_value_t *
make_integer(long value)
{
	lisp_value_t *val = malloc(sizeof(lisp_value_t));
	val->type = LISP_INTEGER;
	val->value.integer = value;
	return val;
}

/*
 * Create floating point value
 */
lisp_value_t *
make_float(double value)
{
	lisp_value_t *val = malloc(sizeof(lisp_value_t));
	val->type = LISP_FLOAT;
	val->value.floating = value;
	return val;
}

/*
 * Create string value
 */
lisp_value_t *
make_string(const char *str)
{
	lisp_value_t *val = malloc(sizeof(lisp_value_t));
	val->type = LISP_STRING;
	/* Remove quotes from string */
	size_t len = strlen(str);
	char *s = malloc(len - 1);
	strncpy(s, str + 1, len - 2);
	s[len - 2] = '\0';
	val->value.string = s;
	return val;
}

/*
 * Create symbol value
 */
lisp_value_t *
make_symbol(const char *name)
{
	lisp_value_t *val = malloc(sizeof(lisp_value_t));
	val->type = LISP_SYMBOL;
	val->value.symbol = strdup(name);
	return val;
}

/*
 * Create cons cell (pair)
 */
lisp_value_t *
make_cons(lisp_value_t *car, lisp_value_t *cdr)
{
	lisp_value_t *val = malloc(sizeof(lisp_value_t));
	val->type = LISP_CONS;
	val->value.cons.car = car;
	val->value.cons.cdr = cdr;
	return val;
}

/*
 * Create NIL value
 */
lisp_value_t *
make_nil(void)
{
	lisp_value_t *val = malloc(sizeof(lisp_value_t));
	val->type = LISP_NIL;
	return val;
}

/*
 * Create T (true) value
 */
lisp_value_t *
make_t(void)
{
	lisp_value_t *val = malloc(sizeof(lisp_value_t));
	val->type = LISP_T;
	return val;
}
