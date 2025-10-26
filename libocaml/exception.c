/*
 * Copyright (c) 2025 PCC OCaml Runtime Library
 *
 * Exception handling runtime
 */

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include "ocaml_runtime.h"

/* Exception context stack */
#define MAX_EXCEPTION_HANDLERS 256

typedef struct exception_context {
	jmp_buf buf;
	ocaml_exception_t exception;
	int active;
} exception_context_t;

static exception_context_t exception_stack[MAX_EXCEPTION_HANDLERS];
static int exception_stack_top = -1;

/*
 * Raise an exception
 */
void
ocaml_raise(ocaml_value_t exn)
{
	if (exception_stack_top < 0) {
		fprintf(stderr, "Uncaught exception\n");
		exit(1);
	}

	exception_stack[exception_stack_top].exception.tag = exn;
	exception_stack[exception_stack_top].exception.arg = VAL_UNIT;
	exception_stack[exception_stack_top].active = 0;

	longjmp(exception_stack[exception_stack_top].buf, 1);
}

/*
 * Try-catch implementation
 * fn: function to execute
 * arg: argument to pass to fn
 * handler: exception handler function
 */
ocaml_value_t
ocaml_try(ocaml_value_t (*fn)(void *), void *arg,
          ocaml_value_t (*handler)(ocaml_exception_t *))
{
	int old_top;
	ocaml_value_t result;

	/* Push new exception handler */
	exception_stack_top++;
	if (exception_stack_top >= MAX_EXCEPTION_HANDLERS) {
		fprintf(stderr, "ocaml_try: exception stack overflow\n");
		exit(1);
	}

	old_top = exception_stack_top;
	exception_stack[old_top].active = 1;

	if (setjmp(exception_stack[old_top].buf) == 0) {
		/* Normal execution */
		result = fn(arg);
		exception_stack_top = old_top - 1;
		return result;
	} else {
		/* Exception caught */
		exception_stack_top = old_top - 1;
		if (handler) {
			return handler(&exception_stack[old_top].exception);
		} else {
			/* Re-raise if no handler */
			ocaml_raise(exception_stack[old_top].exception.tag);
		}
	}

	/* Not reached */
	return VAL_UNIT;
}
