/*
 * Copyright (c) 2025 PCC Common LISP Compiler
 *
 * Pass 1 header file
 * Data structures and declarations for LISP compiler frontend
 */

#ifndef PASS1_H
#define PASS1_H

#include <stdio.h>

/* LISP data types */
typedef enum {
	LISP_NIL,           /* NIL - empty list */
	LISP_T,             /* T - true */
	LISP_INTEGER,       /* Integer number */
	LISP_FLOAT,         /* Floating point number */
	LISP_STRING,        /* String literal */
	LISP_SYMBOL,        /* Symbol/identifier */
	LISP_CONS,          /* Cons cell (pair) */
	LISP_FUNCTION,      /* Function */
	LISP_LAMBDA,        /* Lambda expression */
} lisp_type_t;

/* LISP value representation */
typedef struct lisp_value {
	lisp_type_t type;
	union {
		long integer;
		double floating;
		char *string;
		char *symbol;
		struct {
			struct lisp_value *car;
			struct lisp_value *cdr;
		} cons;
		struct {
			char *name;
			struct lisp_value *params;
			struct lisp_value *body;
		} function;
	} value;
} lisp_value_t;

/* Symbol table entry */
typedef struct symbol {
	char *name;
	lisp_type_t type;
	lisp_value_t *value;
	int is_function;
	int is_global;
	struct symbol *next;
} symbol_t;

/* Error handling */
extern int nerrors;
extern int nwarnings;
extern int max_errors;
extern char *current_file;
extern int lineno;
extern char *ftitle;

/* Diagnostic control */
extern int use_color;
extern int show_caret;

/* Output file */
extern FILE *outfile;

/* Symbol table functions */
void symtab_init(void);
symbol_t *lookup_symbol(const char *name);
symbol_t *define_symbol(const char *name, lisp_type_t type);
symbol_t *define_function(const char *name, lisp_value_t *params, lisp_value_t *body);

/* Type functions */
void init_types(void);

/* Built-in functions */
void init_builtins(void);

/* Error reporting */
void error_init(void);
void error_msg(const char *fmt, ...);
void warning_msg(const char *fmt, ...);
void enable_warning(const char *name);
void disable_warning(const char *name);

/* AST construction */
lisp_value_t *make_integer(long value);
lisp_value_t *make_float(double value);
lisp_value_t *make_string(const char *str);
lisp_value_t *make_symbol(const char *name);
lisp_value_t *make_cons(lisp_value_t *car, lisp_value_t *cdr);
lisp_value_t *make_nil(void);
lisp_value_t *make_t(void);

/* Code generation */
void emit_expr(lisp_value_t *expr);
void emit_function(const char *name, lisp_value_t *params, lisp_value_t *body);

#endif /* PASS1_H */
