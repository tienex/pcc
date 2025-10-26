/*
 * Copyright (c) 2025 PCC Xbase++ Compiler
 *
 * Code generation - Convert AST to C code
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Forward declarations */
static void gen_expr(struct node *xb_node, FILE *out);
static void gen_stmt(struct node *xb_node, FILE *out, int indent);
static void gen_call(struct node *xb_node, FILE *out);

/* Current function being compiled */
static SYMTAB *current_function = NULL;

/* Output file */
static FILE *output_file = NULL;

/* Label counter */
static int label_counter = 0;
static int temp_counter = 0;

/*
 * Generate a new label
 */
static int
new_label(void)
{
	return ++label_counter;
}

/*
 * Print indentation
 */
static void
print_indent(FILE *out, int indent)
{
	for (int i = 0; i < indent; i++)
		fprintf(out, "\t");
}

/*
 * Convert Xbase++ type to C type string
 */
static const char *
xb_type_to_c(TNODE *xb_type)
{
	if (!xb_type)
		return "xb_value_t*";  /* Untyped = variant */

	switch (xb_type->ttype) {
	case TINTEGER:
		return "long long";
	case TFLOAT:
	case TNUMERIC:
		return "double";
	case TCHAR:
	case TMEMO:
		return "char*";
	case TLOGICAL:
		return "int";
	case TDATE:
		return "int32_t";  /* Julian day number */
	case TARRAY:
		return "xb_array_t*";
	case TOBJECT:
		return "xb_object_t*";
	case TCODEBLOCK:
		return "xb_codeblock_t*";
	default:
		return "xb_value_t*";  /* Default to variant */
	}
}

/*
 * Map operator to C string
 */
static const char *
op_to_string(int op)
{
	switch (op) {
	case N_PLUS:   return "+";
	case N_MINUS:  return "-";
	case N_MUL:    return "*";
	case N_DIV:    return "/";
	case N_MOD:    return "%";
	case N_EQ:     return "==";
	case N_NE:     return "!=";
	case N_LT:     return "<";
	case N_LE:     return "<=";
	case N_GT:     return ">";
	case N_GE:     return ">=";
	case N_AND:    return "&&";
	case N_OR:     return "||";
	default:       return "+";
	}
}

/*
 * Generate code for expression
 */
static void
gen_expr(struct node *xb_node, FILE *out)
{
	if (!xb_node)
		return;

	switch (xb_node->op) {
	case N_ICON:
		fprintf(out, "%lld", xb_node->n.ival.val);
		break;

	case N_FCON:
		fprintf(out, "%.15g", xb_node->n.fval.val);
		break;

	case N_SCON:
		/* Escape string and output */
		fprintf(out, "\"");
		for (char *p = xb_node->n.sval.str; *p; p++) {
			switch (*p) {
			case '\n': fprintf(out, "\\n"); break;
			case '\t': fprintf(out, "\\t"); break;
			case '\r': fprintf(out, "\\r"); break;
			case '\\': fprintf(out, "\\\\"); break;
			case '"':  fprintf(out, "\\\""); break;
			default:   fprintf(out, "%c", *p); break;
			}
		}
		fprintf(out, "\"");
		break;

	case N_NAME:
		if (xb_node->n.sym.sym) {
			xb_node->n.sym.sym->sflags |= SUSED;
			fprintf(out, "%s", xb_node->n.sym.sym->sname);
		}
		break;

	case N_ASSIGN:
		if (xb_node->n.bn.left->op == N_NAME) {
			xb_node->n.bn.left->n.sym.sym->sflags |= SSET;
		}
		gen_expr(xb_node->n.bn.left, out);
		fprintf(out, " = ");
		gen_expr(xb_node->n.bn.right, out);
		break;

	case N_PLUS:
	case N_MINUS:
	case N_MUL:
	case N_DIV:
	case N_MOD:
	case N_EQ:
	case N_NE:
	case N_LT:
	case N_LE:
	case N_GT:
	case N_GE:
	case N_AND:
	case N_OR:
		fprintf(out, "(");
		gen_expr(xb_node->n.bn.left, out);
		fprintf(out, " %s ", op_to_string(xb_node->op));
		gen_expr(xb_node->n.bn.right, out);
		fprintf(out, ")");
		break;

	case N_UMINUS:
		fprintf(out, "-(");
		gen_expr(xb_node->n.bn.left, out);
		fprintf(out, ")");
		break;

	case N_NOT:
		fprintf(out, "!(");
		gen_expr(xb_node->n.bn.left, out);
		fprintf(out, ")");
		break;

	case N_CALL:
		gen_call(xb_node, out);
		break;

	case N_SUBSCR:
		/* Array access: arr[idx] */
		fprintf(out, "xb_array_get(");
		gen_expr(xb_node->n.bn.left, out);
		fprintf(out, ", ");
		gen_expr(xb_node->n.bn.right, out);
		fprintf(out, ")");
		break;

	case N_FIELD:
		/* Object field access */
		fprintf(out, "xb_object_get_field(");
		gen_expr(xb_node->n.bn.left, out);
		fprintf(out, ", \"%s\")",
			xb_node->n.bn.right->n.sym.sym ?
			xb_node->n.bn.right->n.sym.sym->sname : "unknown");
		break;

	default:
		warning("code generation for op %d not implemented", xb_node->op);
		fprintf(out, "0");
		break;
	}
}

/*
 * Generate code for function call
 */
static void
gen_call(struct node *xb_node, FILE *out)
{
	/* Function name */
	if (xb_node->n.bn.left && xb_node->n.bn.left->op == N_NAME) {
		fprintf(out, "xb_%s(", xb_node->n.bn.left->n.sym.sym->sname);
	} else {
		fprintf(out, "(");
		gen_expr(xb_node->n.bn.left, out);
		fprintf(out, ")(");
	}

	/* Arguments */
	if (xb_node->n.bn.right) {
		struct node *arg = xb_node->n.bn.right;
		int first = 1;

		/* Handle argument list */
		while (arg) {
			if (!first)
				fprintf(out, ", ");
			first = 0;

			if (arg->op == N_ARGLIST && arg->n.bn.left) {
				gen_expr(arg->n.bn.left, out);
				arg = arg->n.bn.right;
			} else {
				gen_expr(arg, out);
				break;
			}
		}
	}

	fprintf(out, ")");
}

/*
 * Generate code for IF statement
 */
static void
gen_if(struct node *xb_node, FILE *out, int indent)
{
	print_indent(out, indent);
	fprintf(out, "if (");
	gen_expr(xb_node->n.bn.left, out);
	fprintf(out, ") {\n");

	/* Generate then block */
	if (xb_node->n.bn.right) {
		gen_stmt(xb_node->n.bn.right, out, indent + 1);
	}

	print_indent(out, indent);
	fprintf(out, "}\n");

	/* TODO: Handle ELSE clause */
}

/*
 * Generate code for WHILE loop
 */
static void
gen_while(struct node *xb_node, FILE *out, int indent)
{
	print_indent(out, indent);
	fprintf(out, "while (");
	gen_expr(xb_node->n.bn.left, out);
	fprintf(out, ") {\n");

	/* Body */
	if (xb_node->n.bn.right) {
		gen_stmt(xb_node->n.bn.right, out, indent + 1);
	}

	print_indent(out, indent);
	fprintf(out, "}\n");
}

/*
 * Generate code for FOR loop
 */
static void
gen_for(struct node *xb_node, FILE *out, int indent)
{
	print_indent(out, indent);
	fprintf(out, "for (");

	/* Init */
	if (xb_node->n.bn.left && xb_node->n.bn.left->n.bn.left) {
		gen_expr(xb_node->n.bn.left->n.bn.left, out);
	}
	fprintf(out, "; ");

	/* Condition */
	if (xb_node->n.bn.left && xb_node->n.bn.left->n.bn.right) {
		gen_expr(xb_node->n.bn.left->n.bn.right, out);
	}
	fprintf(out, "; ");

	/* Increment - TODO: extract from somewhere */
	fprintf(out, ") {\n");

	/* Body */
	if (xb_node->n.bn.right) {
		gen_stmt(xb_node->n.bn.right, out, indent + 1);
	}

	print_indent(out, indent);
	fprintf(out, "}\n");
}

/*
 * Generate code for RETURN statement
 */
static void
gen_return(struct node *xb_node, FILE *out, int indent)
{
	print_indent(out, indent);
	fprintf(out, "return");

	if (xb_node->n.bn.left) {
		fprintf(out, " ");
		gen_expr(xb_node->n.bn.left, out);
	}

	fprintf(out, ";\n");
}

/*
 * Generate code for statement
 */
static void
gen_stmt(struct node *xb_node, FILE *out, int indent)
{
	if (!xb_node)
		return;

	switch (xb_node->op) {
	case N_IF:
		gen_if(xb_node, out, indent);
		break;

	case N_WHILE:
		gen_while(xb_node, out, indent);
		break;

	case N_FOR:
		gen_for(xb_node, out, indent);
		break;

	case N_RETURN:
		gen_return(xb_node, out, indent);
		break;

	case N_BLOCK:
		/* Generate left then right */
		gen_stmt(xb_node->n.bn.left, out, indent);
		gen_stmt(xb_node->n.bn.right, out, indent);
		break;

	default:
		/* Expression statement */
		print_indent(out, indent);
		gen_expr(xb_node, out);
		fprintf(out, ";\n");
		break;
	}
}

/*
 * Generate code for function definition
 */
void
codegen_function(SYMTAB *func, struct node *body)
{
	FILE *out = output_file;
	if (!out)
		return;

	current_function = func;
	label_counter = 0;

	/* Function signature */
	fprintf(out, "\n%s %s(",
		xb_type_to_c(func->stype),
		func->sname);

	/* Parameters - TODO: extract from function */
	fprintf(out, "void");

	fprintf(out, ")\n{\n");

	/* Generate body */
	if (body) {
		gen_stmt(body, out, 1);
	}

	/* Function epilogue */
	fprintf(out, "}\n");

	current_function = NULL;
}

/*
 * Set output file for code generation
 */
void
codegen_set_output(FILE *fp)
{
	output_file = fp;
}

/*
 * Generate C file header
 */
void
codegen_header(FILE *out)
{
	fprintf(out, "/* Generated by PXC (Xbase++ Compiler) */\n\n");
	fprintf(out, "#include <stdio.h>\n");
	fprintf(out, "#include <stdlib.h>\n");
	fprintf(out, "#include <string.h>\n");
	fprintf(out, "#include <stdint.h>\n");
	fprintf(out, "#include <pxc/xbrt.h>\n\n");
}

/*
 * Initialize code generation
 */
void
codegen_init(void)
{
	label_counter = 0;
	temp_counter = 0;
	output_file = NULL;
}
