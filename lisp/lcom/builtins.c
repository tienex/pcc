/*
 * Copyright (c) 2025 PCC Common LISP Compiler
 *
 * Built-in functions for Common LISP
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/*
 * Define a built-in function
 */
static void
define_builtin(const char *name)
{
	symbol_t *sym = define_symbol(name, LISP_FUNCTION);
	sym->is_function = 1;
}

/*
 * Initialize built-in functions
 */
void
init_builtins(void)
{
	/* List operations */
	define_builtin("car");
	define_builtin("cdr");
	define_builtin("cons");
	define_builtin("list");
	define_builtin("append");
	define_builtin("reverse");
	define_builtin("length");
	define_builtin("nth");
	define_builtin("first");
	define_builtin("rest");
	define_builtin("last");

	/* Arithmetic operations */
	define_builtin("+");
	define_builtin("-");
	define_builtin("*");
	define_builtin("/");
	define_builtin("mod");
	define_builtin("rem");
	define_builtin("1+");
	define_builtin("1-");
	define_builtin("abs");
	define_builtin("max");
	define_builtin("min");

	/* Comparison operations */
	define_builtin("=");
	define_builtin("/=");
	define_builtin("<");
	define_builtin(">");
	define_builtin("<=");
	define_builtin(">=");
	define_builtin("eq");
	define_builtin("eql");
	define_builtin("equal");

	/* Logical operations */
	define_builtin("and");
	define_builtin("or");
	define_builtin("not");

	/* Type predicates */
	define_builtin("null");
	define_builtin("atom");
	define_builtin("consp");
	define_builtin("listp");
	define_builtin("numberp");
	define_builtin("integerp");
	define_builtin("floatp");
	define_builtin("symbolp");
	define_builtin("stringp");

	/* I/O operations */
	define_builtin("print");
	define_builtin("princ");
	define_builtin("prin1");
	define_builtin("write");
	define_builtin("format");
	define_builtin("read");
	define_builtin("read-line");

	/* String operations */
	define_builtin("string");
	define_builtin("string-upcase");
	define_builtin("string-downcase");
	define_builtin("string=");
	define_builtin("string<");
	define_builtin("concatenate");

	/* Control flow */
	define_builtin("funcall");
	define_builtin("apply");
	define_builtin("mapcar");
	define_builtin("mapc");
	define_builtin("reduce");
	define_builtin("filter");
}
