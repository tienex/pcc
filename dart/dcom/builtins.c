/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart Built-in Functions
 */

#include "pass1.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Built-in function table */
typedef struct {
	const char *name;
	void (*handler)(Node *args);
} BuiltinFunction;

static void builtin_print(Node *args);
static void builtin_assert(Node *args);
static void builtin_identical(Node *args);

static BuiltinFunction builtin_functions[] = {
	{ "print", builtin_print },
	{ "assert", builtin_assert },
	{ "identical", builtin_identical },
	{ NULL, NULL }
};

/* Check if name is a built-in function */
int
is_builtin_function(const char *name)
{
	for (int i = 0; builtin_functions[i].name; i++) {
		if (strcmp(builtin_functions[i].name, name) == 0) {
			return 1;
		}
	}
	return 0;
}

/* Get built-in function handler */
void (*get_builtin_handler(const char *name))(Node *)
{
	for (int i = 0; builtin_functions[i].name; i++) {
		if (strcmp(builtin_functions[i].name, name) == 0) {
			return builtin_functions[i].handler;
		}
	}
	return NULL;
}

/* Built-in: print(object) */
static void
builtin_print(Node *args)
{
	/* Code generation for print() is handled in codegen.c */
}

/* Built-in: assert(condition, [message]) */
static void
builtin_assert(Node *args)
{
	/* Code generation for assert() is handled in codegen.c */
}

/* Built-in: identical(a, b) */
static void
builtin_identical(Node *args)
{
	/* Code generation for identical() is handled in codegen.c */
}

/* Dart core library functions */
const char *dart_core_functions[] = {
	/* Type conversion */
	"int.parse",
	"double.parse",
	"num.parse",

	/* Math functions */
	"min",
	"max",
	"pow",
	"sqrt",
	"sin",
	"cos",
	"tan",
	"exp",
	"log",

	/* String functions */
	"String.fromCharCode",
	"String.fromCharCodes",

	NULL
};

/* Check if name is a core library function */
int
is_core_function(const char *name)
{
	for (int i = 0; dart_core_functions[i]; i++) {
		if (strcmp(dart_core_functions[i], name) == 0) {
			return 1;
		}
	}
	return 0;
}
