/*
 * Copyright (c) 2025 PCC Common LISP Runtime Library
 *
 * Runtime support library for Common LISP compiler
 */

#ifndef LIBLISP_H
#define LIBLISP_H

#include <stddef.h>
#include <stdint.h>

/* LISP object types */
typedef enum {
	LISP_TYPE_NIL = 0,
	LISP_TYPE_T,
	LISP_TYPE_INTEGER,
	LISP_TYPE_FLOAT,
	LISP_TYPE_STRING,
	LISP_TYPE_SYMBOL,
	LISP_TYPE_CONS,
	LISP_TYPE_FUNCTION,
} lisp_type_t;

/* LISP object representation */
typedef struct lisp_object {
	lisp_type_t type;
	union {
		long integer;
		double floating;
		char *string;
		char *symbol;
		struct {
			struct lisp_object *car;
			struct lisp_object *cdr;
		} cons;
		void *function;
	} value;
} lisp_object_t;

/* Singleton NIL and T objects */
extern lisp_object_t *LISP_NIL;
extern lisp_object_t *LISP_T;

/* Memory management */
lisp_object_t *lisp_alloc(void);
void lisp_free(lisp_object_t *obj);
void lisp_gc_init(void);
void lisp_gc_collect(void);
size_t lisp_gc_get_heap_size(void);

/* Constructors */
lisp_object_t *lisp_make_nil(void);
lisp_object_t *lisp_make_t(void);
lisp_object_t *lisp_make_integer(long value);
lisp_object_t *lisp_make_float(double value);
lisp_object_t *lisp_make_string(const char *str);
lisp_object_t *lisp_make_symbol(const char *name);
lisp_object_t *lisp_make_cons(lisp_object_t *car, lisp_object_t *cdr);

/* List operations */
lisp_object_t *lisp_car(lisp_object_t *obj);
lisp_object_t *lisp_cdr(lisp_object_t *obj);
lisp_object_t *lisp_cons(lisp_object_t *car, lisp_object_t *cdr);
lisp_object_t *lisp_list(int n, ...);
lisp_object_t *lisp_append(lisp_object_t *list1, lisp_object_t *list2);
lisp_object_t *lisp_reverse(lisp_object_t *list);
lisp_object_t *lisp_length(lisp_object_t *list);
lisp_object_t *lisp_nth(lisp_object_t *n, lisp_object_t *list);

/* Arithmetic operations */
lisp_object_t *lisp_add(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_subtract(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_multiply(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_divide(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_mod(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_abs(lisp_object_t *a);
lisp_object_t *lisp_max(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_min(lisp_object_t *a, lisp_object_t *b);

/* Comparison operations */
lisp_object_t *lisp_equal(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_not_equal(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_less_than(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_greater_than(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_less_equal(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_greater_equal(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_eq(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_eql(lisp_object_t *a, lisp_object_t *b);

/* Logical operations */
lisp_object_t *lisp_and(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_or(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_not(lisp_object_t *a);

/* Type predicates */
lisp_object_t *lisp_null(lisp_object_t *obj);
lisp_object_t *lisp_atom(lisp_object_t *obj);
lisp_object_t *lisp_consp(lisp_object_t *obj);
lisp_object_t *lisp_listp(lisp_object_t *obj);
lisp_object_t *lisp_numberp(lisp_object_t *obj);
lisp_object_t *lisp_integerp(lisp_object_t *obj);
lisp_object_t *lisp_floatp(lisp_object_t *obj);
lisp_object_t *lisp_symbolp(lisp_object_t *obj);
lisp_object_t *lisp_stringp(lisp_object_t *obj);

/* I/O operations */
void lisp_print(lisp_object_t *obj);
void lisp_princ(lisp_object_t *obj);
void lisp_prin1(lisp_object_t *obj);
void lisp_write(lisp_object_t *obj);
lisp_object_t *lisp_read(void);
lisp_object_t *lisp_read_line(void);

/* String operations */
lisp_object_t *lisp_string(lisp_object_t *obj);
lisp_object_t *lisp_string_upcase(lisp_object_t *str);
lisp_object_t *lisp_string_downcase(lisp_object_t *str);
lisp_object_t *lisp_string_equal(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_string_less(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_concatenate(int n, ...);

/* Utility functions */
void lisp_error(const char *fmt, ...);
int lisp_is_true(lisp_object_t *obj);
int lisp_is_nil(lisp_object_t *obj);

/* Runtime initialization */
void lisp_runtime_init(void);
void lisp_runtime_cleanup(void);

#endif /* LIBLISP_H */
