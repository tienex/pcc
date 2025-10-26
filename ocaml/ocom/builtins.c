/*
 * Copyright (c) 2025 PCC OCaml Compiler
 *
 * Built-in functions and types
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/*
 * Initialize built-in functions and types
 */
void
init_builtins(void)
{
	/* Install built-in functions */
	install("print_int", TFUNCTION);
	install("print_float", TFUNCTION);
	install("print_char", TFUNCTION);
	install("print_string", TFUNCTION);
	install("print_newline", TFUNCTION);

	/* List operations */
	install("List.length", TFUNCTION);
	install("List.hd", TFUNCTION);
	install("List.tl", TFUNCTION);
	install("List.map", TFUNCTION);
	install("List.fold_left", TFUNCTION);
	install("List.fold_right", TFUNCTION);

	/* Array operations */
	install("Array.length", TFUNCTION);
	install("Array.get", TFUNCTION);
	install("Array.set", TFUNCTION);
	install("Array.make", TFUNCTION);

	/* String operations */
	install("String.length", TFUNCTION);
	install("String.get", TFUNCTION);
	install("String.concat", TFUNCTION);

	/* Math operations */
	install("sqrt", TFUNCTION);
	install("sin", TFUNCTION);
	install("cos", TFUNCTION);
	install("exp", TFUNCTION);
	install("log", TFUNCTION);

	/* Conversion functions */
	install("int_of_float", TFUNCTION);
	install("float_of_int", TFUNCTION);
	install("char_of_int", TFUNCTION);
	install("int_of_char", TFUNCTION);
	install("string_of_int", TFUNCTION);
	install("int_of_string", TFUNCTION);

	/* I/O functions */
	install("read_int", TFUNCTION);
	install("read_float", TFUNCTION);
	install("read_line", TFUNCTION);

	/* Reference operations */
	install("ref", TFUNCTION);
}
