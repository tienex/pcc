/*
 * Copyright (c) 2025 PCC Common LISP Runtime Library
 *
 * Arithmetic operations implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "liblisp.h"

/*
 * Add two numbers
 */
lisp_object_t *
lisp_add(lisp_object_t *a, lisp_object_t *b)
{
	if (a->type == LISP_TYPE_INTEGER && b->type == LISP_TYPE_INTEGER)
		return lisp_make_integer(a->value.integer + b->value.integer);

	if (a->type == LISP_TYPE_FLOAT || b->type == LISP_TYPE_FLOAT) {
		double da = (a->type == LISP_TYPE_INTEGER) ? (double)a->value.integer : a->value.floating;
		double db = (b->type == LISP_TYPE_INTEGER) ? (double)b->value.integer : b->value.floating;
		return lisp_make_float(da + db);
	}

	lisp_error("ADD: arguments must be numbers");
	return LISP_NIL;
}

/*
 * Subtract two numbers
 */
lisp_object_t *
lisp_subtract(lisp_object_t *a, lisp_object_t *b)
{
	if (a->type == LISP_TYPE_INTEGER && b->type == LISP_TYPE_INTEGER)
		return lisp_make_integer(a->value.integer - b->value.integer);

	if (a->type == LISP_TYPE_FLOAT || b->type == LISP_TYPE_FLOAT) {
		double da = (a->type == LISP_TYPE_INTEGER) ? (double)a->value.integer : a->value.floating;
		double db = (b->type == LISP_TYPE_INTEGER) ? (double)b->value.integer : b->value.floating;
		return lisp_make_float(da - db);
	}

	lisp_error("SUBTRACT: arguments must be numbers");
	return LISP_NIL;
}

/*
 * Multiply two numbers
 */
lisp_object_t *
lisp_multiply(lisp_object_t *a, lisp_object_t *b)
{
	if (a->type == LISP_TYPE_INTEGER && b->type == LISP_TYPE_INTEGER)
		return lisp_make_integer(a->value.integer * b->value.integer);

	if (a->type == LISP_TYPE_FLOAT || b->type == LISP_TYPE_FLOAT) {
		double da = (a->type == LISP_TYPE_INTEGER) ? (double)a->value.integer : a->value.floating;
		double db = (b->type == LISP_TYPE_INTEGER) ? (double)b->value.integer : b->value.floating;
		return lisp_make_float(da * db);
	}

	lisp_error("MULTIPLY: arguments must be numbers");
	return LISP_NIL;
}

/*
 * Divide two numbers
 */
lisp_object_t *
lisp_divide(lisp_object_t *a, lisp_object_t *b)
{
	if (a->type == LISP_TYPE_INTEGER && b->type == LISP_TYPE_INTEGER) {
		if (b->value.integer == 0) {
			lisp_error("DIVIDE: division by zero");
			return LISP_NIL;
		}
		return lisp_make_integer(a->value.integer / b->value.integer);
	}

	if (a->type == LISP_TYPE_FLOAT || b->type == LISP_TYPE_FLOAT) {
		double da = (a->type == LISP_TYPE_INTEGER) ? (double)a->value.integer : a->value.floating;
		double db = (b->type == LISP_TYPE_INTEGER) ? (double)b->value.integer : b->value.floating;
		if (db == 0.0) {
			lisp_error("DIVIDE: division by zero");
			return LISP_NIL;
		}
		return lisp_make_float(da / db);
	}

	lisp_error("DIVIDE: arguments must be numbers");
	return LISP_NIL;
}

/*
 * Modulo operation
 */
lisp_object_t *
lisp_mod(lisp_object_t *a, lisp_object_t *b)
{
	if (a->type != LISP_TYPE_INTEGER || b->type != LISP_TYPE_INTEGER) {
		lisp_error("MOD: arguments must be integers");
		return LISP_NIL;
	}

	if (b->value.integer == 0) {
		lisp_error("MOD: division by zero");
		return LISP_NIL;
	}

	return lisp_make_integer(a->value.integer % b->value.integer);
}

/*
 * Absolute value
 */
lisp_object_t *
lisp_abs(lisp_object_t *a)
{
	if (a->type == LISP_TYPE_INTEGER)
		return lisp_make_integer(labs(a->value.integer));

	if (a->type == LISP_TYPE_FLOAT)
		return lisp_make_float(fabs(a->value.floating));

	lisp_error("ABS: argument must be a number");
	return LISP_NIL;
}

/*
 * Maximum of two numbers
 */
lisp_object_t *
lisp_max(lisp_object_t *a, lisp_object_t *b)
{
	if (a->type == LISP_TYPE_INTEGER && b->type == LISP_TYPE_INTEGER)
		return lisp_make_integer(a->value.integer > b->value.integer ?
		                         a->value.integer : b->value.integer);

	if (a->type == LISP_TYPE_FLOAT || b->type == LISP_TYPE_FLOAT) {
		double da = (a->type == LISP_TYPE_INTEGER) ? (double)a->value.integer : a->value.floating;
		double db = (b->type == LISP_TYPE_INTEGER) ? (double)b->value.integer : b->value.floating;
		return lisp_make_float(da > db ? da : db);
	}

	lisp_error("MAX: arguments must be numbers");
	return LISP_NIL;
}

/*
 * Minimum of two numbers
 */
lisp_object_t *
lisp_min(lisp_object_t *a, lisp_object_t *b)
{
	if (a->type == LISP_TYPE_INTEGER && b->type == LISP_TYPE_INTEGER)
		return lisp_make_integer(a->value.integer < b->value.integer ?
		                         a->value.integer : b->value.integer);

	if (a->type == LISP_TYPE_FLOAT || b->type == LISP_TYPE_FLOAT) {
		double da = (a->type == LISP_TYPE_INTEGER) ? (double)a->value.integer : a->value.floating;
		double db = (b->type == LISP_TYPE_INTEGER) ? (double)b->value.integer : b->value.floating;
		return lisp_make_float(da < db ? da : db);
	}

	lisp_error("MIN: arguments must be numbers");
	return LISP_NIL;
}

/*
 * Numeric equality
 */
lisp_object_t *
lisp_equal(lisp_object_t *a, lisp_object_t *b)
{
	if (a->type == LISP_TYPE_INTEGER && b->type == LISP_TYPE_INTEGER)
		return (a->value.integer == b->value.integer) ? LISP_T : LISP_NIL;

	if (a->type == LISP_TYPE_FLOAT || b->type == LISP_TYPE_FLOAT) {
		double da = (a->type == LISP_TYPE_INTEGER) ? (double)a->value.integer : a->value.floating;
		double db = (b->type == LISP_TYPE_INTEGER) ? (double)b->value.integer : b->value.floating;
		return (da == db) ? LISP_T : LISP_NIL;
	}

	return LISP_NIL;
}

/*
 * Numeric inequality
 */
lisp_object_t *
lisp_not_equal(lisp_object_t *a, lisp_object_t *b)
{
	return lisp_is_true(lisp_equal(a, b)) ? LISP_NIL : LISP_T;
}

/*
 * Less than
 */
lisp_object_t *
lisp_less_than(lisp_object_t *a, lisp_object_t *b)
{
	if (a->type == LISP_TYPE_INTEGER && b->type == LISP_TYPE_INTEGER)
		return (a->value.integer < b->value.integer) ? LISP_T : LISP_NIL;

	if (a->type == LISP_TYPE_FLOAT || b->type == LISP_TYPE_FLOAT) {
		double da = (a->type == LISP_TYPE_INTEGER) ? (double)a->value.integer : a->value.floating;
		double db = (b->type == LISP_TYPE_INTEGER) ? (double)b->value.integer : b->value.floating;
		return (da < db) ? LISP_T : LISP_NIL;
	}

	lisp_error("LESS-THAN: arguments must be numbers");
	return LISP_NIL;
}

/*
 * Greater than
 */
lisp_object_t *
lisp_greater_than(lisp_object_t *a, lisp_object_t *b)
{
	if (a->type == LISP_TYPE_INTEGER && b->type == LISP_TYPE_INTEGER)
		return (a->value.integer > b->value.integer) ? LISP_T : LISP_NIL;

	if (a->type == LISP_TYPE_FLOAT || b->type == LISP_TYPE_FLOAT) {
		double da = (a->type == LISP_TYPE_INTEGER) ? (double)a->value.integer : a->value.floating;
		double db = (b->type == LISP_TYPE_INTEGER) ? (double)b->value.integer : b->value.floating;
		return (da > db) ? LISP_T : LISP_NIL;
	}

	lisp_error("GREATER-THAN: arguments must be numbers");
	return LISP_NIL;
}

/*
 * Less than or equal
 */
lisp_object_t *
lisp_less_equal(lisp_object_t *a, lisp_object_t *b)
{
	return lisp_is_true(lisp_greater_than(a, b)) ? LISP_NIL : LISP_T;
}

/*
 * Greater than or equal
 */
lisp_object_t *
lisp_greater_equal(lisp_object_t *a, lisp_object_t *b)
{
	return lisp_is_true(lisp_less_than(a, b)) ? LISP_NIL : LISP_T;
}

/*
 * EQ: pointer equality
 */
lisp_object_t *
lisp_eq(lisp_object_t *a, lisp_object_t *b)
{
	return (a == b) ? LISP_T : LISP_NIL;
}

/*
 * EQL: equality for numbers and symbols
 */
lisp_object_t *
lisp_eql(lisp_object_t *a, lisp_object_t *b)
{
	if (a == b)
		return LISP_T;

	if (a->type != b->type)
		return LISP_NIL;

	switch (a->type) {
	case LISP_TYPE_INTEGER:
		return (a->value.integer == b->value.integer) ? LISP_T : LISP_NIL;
	case LISP_TYPE_FLOAT:
		return (a->value.floating == b->value.floating) ? LISP_T : LISP_NIL;
	default:
		return (a == b) ? LISP_T : LISP_NIL;
	}
}

/*
 * Logical AND
 */
lisp_object_t *
lisp_and(lisp_object_t *a, lisp_object_t *b)
{
	return (lisp_is_true(a) && lisp_is_true(b)) ? LISP_T : LISP_NIL;
}

/*
 * Logical OR
 */
lisp_object_t *
lisp_or(lisp_object_t *a, lisp_object_t *b)
{
	return (lisp_is_true(a) || lisp_is_true(b)) ? LISP_T : LISP_NIL;
}

/*
 * Logical NOT
 */
lisp_object_t *
lisp_not(lisp_object_t *a)
{
	return lisp_is_true(a) ? LISP_NIL : LISP_T;
}
