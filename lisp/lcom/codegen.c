/*
 * Copyright (c) 2025 PCC Common LISP Compiler
 *
 * Code generation for Common LISP
 * Emits intermediate representation that MIP backend can process
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "pass1.h"

static int label_counter = 0;
static int temp_counter = 0;

/*
 * Generate a new label
 */
static int
new_label(void)
{
	return label_counter++;
}

/*
 * Generate a new temporary variable
 */
static int
new_temp(void)
{
	return temp_counter++;
}

/*
 * Emit a simple instruction
 */
static void
emit(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	vfprintf(outfile, fmt, ap);
	va_end(ap);
}

/*
 * Emit expression code
 */
void
emit_expr(lisp_value_t *expr)
{
	if (expr == NULL)
		return;

	switch (expr->type) {
	case LISP_NIL:
		emit("\t; NIL\n");
		emit("\tmov\t$0, %%rax\n");
		break;

	case LISP_T:
		emit("\t; T (true)\n");
		emit("\tmov\t$1, %%rax\n");
		break;

	case LISP_INTEGER:
		emit("\t; Integer: %ld\n", expr->value.integer);
		emit("\tmov\t$%ld, %%rax\n", expr->value.integer);
		break;

	case LISP_FLOAT:
		emit("\t; Float: %f\n", expr->value.floating);
		emit("\tmovsd\t.LC%d(%%rip), %%xmm0\n", new_label());
		break;

	case LISP_STRING:
		emit("\t; String: \"%s\"\n", expr->value.string);
		emit("\tlea\t.LC%d(%%rip), %%rax\n", new_label());
		break;

	case LISP_SYMBOL:
		emit("\t; Symbol: %s\n", expr->value.symbol);
		/* Lookup symbol and load its value */
		emit("\tmov\t%s(%%rip), %%rax\n", expr->value.symbol);
		break;

	case LISP_CONS:
		/* Function call or special form */
		if (expr->value.cons.car->type == LISP_SYMBOL) {
			char *func = expr->value.cons.car->value.symbol;
			lisp_value_t *args = expr->value.cons.cdr;

			emit("\t; Function call: %s\n", func);

			/* Handle built-in operations */
			if (strcmp(func, "+") == 0) {
				/* Addition */
				if (args && args->type == LISP_CONS) {
					emit_expr(args->value.cons.car);
					emit("\tpush\t%%rax\n");
					if (args->value.cons.cdr &&
					    args->value.cons.cdr->type == LISP_CONS) {
						emit_expr(args->value.cons.cdr->value.cons.car);
						emit("\tpop\t%%rbx\n");
						emit("\tadd\t%%rbx, %%rax\n");
					}
				}
			} else if (strcmp(func, "-") == 0) {
				/* Subtraction */
				if (args && args->type == LISP_CONS) {
					emit_expr(args->value.cons.car);
					emit("\tpush\t%%rax\n");
					if (args->value.cons.cdr &&
					    args->value.cons.cdr->type == LISP_CONS) {
						emit_expr(args->value.cons.cdr->value.cons.car);
						emit("\tmov\t%%rax, %%rbx\n");
						emit("\tpop\t%%rax\n");
						emit("\tsub\t%%rbx, %%rax\n");
					}
				}
			} else if (strcmp(func, "print") == 0) {
				/* Print function */
				if (args && args->type == LISP_CONS) {
					emit_expr(args->value.cons.car);
					emit("\tcall\tlisp_print\n");
				}
			} else {
				/* Generic function call */
				emit("\tcall\t%s\n", func);
			}
		}
		break;

	case LISP_FUNCTION:
		/* Function definition handled separately */
		break;

	default:
		error_msg("Unknown expression type");
		break;
	}
}

/*
 * Emit function definition
 */
void
emit_function(const char *name, lisp_value_t *params, lisp_value_t *body)
{
	emit("\n\t.text\n");
	emit("\t.globl\t%s\n", name);
	emit("\t.type\t%s, @function\n", name);
	emit("%s:\n", name);
	emit("\tpush\t%%rbp\n");
	emit("\tmov\t%%rsp, %%rbp\n");

	/* Generate code for function body */
	lisp_value_t *expr = body;
	while (expr != NULL && expr->type == LISP_CONS) {
		emit_expr(expr->value.cons.car);
		expr = expr->value.cons.cdr;
	}

	/* Function epilogue */
	emit("\tpop\t%%rbp\n");
	emit("\tret\n");
	emit("\t.size\t%s, .-%s\n", name, name);
}
