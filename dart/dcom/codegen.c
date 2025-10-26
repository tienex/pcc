/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart code generation - Emits C code using libdart runtime
 */

#include "pass1.h"
#include <stdio.h>
#include <stdlib.h>

static FILE *output = NULL;
static int temp_counter = 0;
static int label_counter = 0;

static void emit(const char *fmt, ...);
static void emit_node(Node *node, const char *result_var);
static void emit_function(Node *node);
static void emit_class(Node *node);
static void emit_statement(Node *node);
static void emit_expression(Node *node, const char *result_var);
static char *new_temp(void);
static char *new_label(void);

static void
emit(const char *fmt, ...)
{
	va_list ap;

	if (!output) {
		output = stdout;
	}

	va_start(ap, fmt);
	vfprintf(output, fmt, ap);
	va_end(ap);
}

static char *
new_temp(void)
{
	static char buf[32];
	snprintf(buf, sizeof(buf), "_t%d", temp_counter++);
	return strdup(buf);
}

static char *
new_label(void)
{
	static char buf[32];
	snprintf(buf, sizeof(buf), "_L%d", label_counter++);
	return strdup(buf);
}

static void
emit_includes(void)
{
	emit("#include <dart.h>\n");
	emit("#include <stdio.h>\n");
	emit("#include <stdlib.h>\n");
	emit("\n");
}

static void
emit_forward_declarations(Node *root)
{
	emit("/* Forward declarations */\n");
	/* TODO: Scan AST for function declarations */
	emit("\n");
}

static void
emit_main_wrapper(void)
{
	emit("int main(int argc, char **argv) {\n");
	emit("    dart_runtime_init();\n");
	emit("    _dart_main();\n");
	emit("    dart_runtime_cleanup();\n");
	emit("    return 0;\n");
	emit("}\n\n");
}

static void
emit_function(Node *node)
{
	if (!node) {
		return;
	}

	/* For now, generate a simple main function wrapper */
	emit("void _dart_main(void) {\n");

	/* Emit function body */
	if (node->data.child) {
		emit_statement(node->data.child);
	}

	emit("}\n\n");
}

static void
emit_statement(Node *node)
{
	if (!node) {
		return;
	}

	switch (node->type) {
	case N_BLOCK:
		emit("    /* Block */\n");
		if (node->data.child) {
			emit_statement(node->data.child);
		}
		break;

	case N_EXPRESSION:
		emit("    /* Expression statement */\n");
		{
			char *temp = new_temp();
			emit("    DartObject *%s = NULL;\n", temp);
			emit_expression(node, temp);
			free(temp);
		}
		break;

	case N_VARIABLE: {
		emit("    /* Variable declaration */\n");
		char *var_name = node->data.strval;
		emit("    DartObject *%s = dart_null();\n", var_name);
		break;
	}

	case N_RETURN:
		emit("    /* Return statement */\n");
		if (node->data.child) {
			char *temp = new_temp();
			emit_expression(node->data.child, temp);
			emit("    return;\n");
			free(temp);
		} else {
			emit("    return;\n");
		}
		break;

	case N_IF: {
		emit("    /* If statement */\n");
		char *cond = new_temp();
		char *label_else = new_label();
		char *label_end = new_label();

		emit("    DartObject *%s = NULL;\n", cond);
		emit_expression(node->data.binary.left, cond);
		emit("    if (!dart_bool_value(%s)) goto %s;\n", cond, label_else);

		if (node->data.binary.right) {
			emit_statement(node->data.binary.right);
		}

		emit("    goto %s;\n", label_end);
		emit("%s:\n", label_else);
		emit("    /* else clause */\n");
		emit("%s:\n", label_end);

		free(cond);
		free(label_else);
		free(label_end);
		break;
	}

	case N_WHILE: {
		emit("    /* While loop */\n");
		char *label_start = new_label();
		char *label_end = new_label();
		char *cond = new_temp();

		emit("%s:\n", label_start);
		emit("    DartObject *%s = NULL;\n", cond);
		emit_expression(node->data.binary.left, cond);
		emit("    if (!dart_bool_value(%s)) goto %s;\n", cond, label_end);

		if (node->data.binary.right) {
			emit_statement(node->data.binary.right);
		}

		emit("    goto %s;\n", label_start);
		emit("%s:\n", label_end);

		free(cond);
		free(label_start);
		free(label_end);
		break;
	}

	case N_FOR:
		emit("    /* For loop */\n");
		/* TODO: Implement for loop */
		break;

	default:
		emit("    /* Unknown statement type %d */\n", node->type);
		break;
	}

	/* Process next statement */
	if (node->next) {
		emit_statement(node->next);
	}
}

static void
emit_expression(Node *node, const char *result_var)
{
	if (!node) {
		emit("    %s = dart_null();\n", result_var);
		return;
	}

	switch (node->type) {
	case N_LITERAL:
		if (node->data.strval) {
			/* String literal */
			emit("    %s = (DartObject *)dart_string_new(\"%s\");\n",
			     result_var, node->data.strval);
		} else {
			/* Integer literal */
			emit("    %s = dart_int_new(%d);\n",
			     result_var, node->data.intval);
		}
		break;

	case N_IDENTIFIER:
		/* Variable reference */
		emit("    %s = %s; /* identifier */\n",
		     result_var, node->data.strval);
		break;

	case N_BINARY_OP: {
		char *left = new_temp();
		char *right = new_temp();

		emit("    DartObject *%s = NULL;\n", left);
		emit("    DartObject *%s = NULL;\n", right);

		emit_expression(node->data.binary.left, left);
		emit_expression(node->data.binary.right, right);

		/* Emit operation */
		emit("    /* Binary operation */\n");
		emit("    %s = dart_int_new(dart_int_value(%s) + dart_int_value(%s));\n",
		     result_var, left, right);

		free(left);
		free(right);
		break;
	}

	case N_CALL: {
		emit("    /* Function call */\n");
		/* TODO: Implement function calls */
		emit("    %s = dart_null();\n", result_var);
		break;
	}

	default:
		emit("    /* Unknown expression type %d */\n", node->type);
		emit("    %s = dart_null();\n", result_var);
		break;
	}
}

void
generate_code(Node *root)
{
	if (root == NULL) {
		return;
	}

	/* Emit header */
	emit("/*\n");
	emit(" * Generated by Dart compiler\n");
	emit(" * Using libdart runtime library\n");
	emit(" */\n\n");

	/* Emit includes */
	emit_includes();

	/* Emit forward declarations */
	emit_forward_declarations(root);

	/* Emit main function */
	emit_function(root);

	/* Emit main wrapper */
	emit_main_wrapper();
}

void
set_output_file(FILE *fp)
{
	output = fp;
}
