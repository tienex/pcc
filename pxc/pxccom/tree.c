/*
 * Copyright (c) 2025 PCC Xbase++ Compiler
 *
 * AST (Abstract Syntax Tree) operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/*
 * Create a new AST node
 */
NODE *
make_node(int op, NODE *left, NODE *right)
{
	NODE *np;

	np = (NODE *)malloc(sizeof(NODE));
	if (np == NULL) {
		error("out of memory");
		exit(1);
	}

	memset(np, 0, sizeof(NODE));
	np->op = op;
	np->n.bn.left = left;
	np->n.bn.right = right;

	/* Type inference - simplified */
	if (left != NULL && right != NULL) {
		/* Binary operation - result type is common type */
		if (left->n_type != NULL && right->n_type != NULL) {
			if (type_compatible(left->n_type, right->n_type))
				np->n_type = left->n_type;
			else
				np->n_type = make_type(TVARIANT);
		}
	} else if (left != NULL) {
		/* Unary operation - result type is operand type */
		np->n_type = left->n_type;
	}

	return np;
}

/*
 * Create integer constant node
 */
NODE *
make_icon(long long val)
{
	NODE *np;

	np = (NODE *)malloc(sizeof(NODE));
	if (np == NULL) {
		error("out of memory");
		exit(1);
	}

	memset(np, 0, sizeof(NODE));
	np->op = N_ICON;
	np->n.ival.val = val;
	np->n_type = make_type(TINTEGER);

	return np;
}

/*
 * Create float constant node
 */
NODE *
make_fcon(double val)
{
	NODE *np;

	np = (NODE *)malloc(sizeof(NODE));
	if (np == NULL) {
		error("out of memory");
		exit(1);
	}

	memset(np, 0, sizeof(NODE));
	np->op = N_FCON;
	np->n.fval.val = val;
	np->n_type = make_type(TFLOAT);

	return np;
}

/*
 * Create string constant node
 */
NODE *
make_scon(char *str)
{
	NODE *np;

	np = (NODE *)malloc(sizeof(NODE));
	if (np == NULL) {
		error("out of memory");
		exit(1);
	}

	memset(np, 0, sizeof(NODE));
	np->op = N_SCON;
	np->n.sval.str = strdup(str);
	np->n.sval.len = strlen(str);
	np->n_type = make_type(TCHAR);

	return np;
}

/*
 * Create name (variable reference) node
 */
NODE *
make_name(SYMTAB *sym)
{
	NODE *np;

	np = (NODE *)malloc(sizeof(NODE));
	if (np == NULL) {
		error("out of memory");
		exit(1);
	}

	memset(np, 0, sizeof(NODE));
	np->op = N_NAME;
	np->n.sym.sym = sym;
	np->n_type = sym->stype;

	/* Mark symbol as used */
	sym->sflags |= SUSED;

	return np;
}

/*
 * Walk the AST and perform semantic analysis / code generation
 */
void
walk_tree(NODE *np)
{
	if (np == NULL)
		return;

	switch (np->op) {
	case N_ICON:
	case N_FCON:
	case N_SCON:
		/* Constants - nothing to do */
		break;

	case N_NAME:
		/* Variable reference */
		break;

	case N_PLUS:
	case N_MINUS:
	case N_MUL:
	case N_DIV:
	case N_MOD:
		/* Binary arithmetic operations */
		walk_tree(np->n.bn.left);
		walk_tree(np->n.bn.right);
		break;

	case N_ASSIGN:
		/* Assignment */
		walk_tree(np->n.bn.left);
		walk_tree(np->n.bn.right);
		/* Mark left side as set */
		if (np->n.bn.left->op == N_NAME) {
			np->n.bn.left->n.sym.sym->sflags |= SSET;
		}
		break;

	case N_EQ:
	case N_NE:
	case N_LT:
	case N_LE:
	case N_GT:
	case N_GE:
		/* Comparison operations */
		walk_tree(np->n.bn.left);
		walk_tree(np->n.bn.right);
		break;

	case N_AND:
	case N_OR:
		/* Logical operations */
		walk_tree(np->n.bn.left);
		walk_tree(np->n.bn.right);
		break;

	case N_NOT:
	case N_UMINUS:
		/* Unary operations */
		walk_tree(np->n.bn.left);
		break;

	case N_CALL:
		/* Function call */
		walk_tree(np->n.bn.left);   /* Function expression */
		walk_tree(np->n.bn.right);  /* Argument list */
		break;

	case N_SUBSCR:
		/* Array subscript */
		walk_tree(np->n.bn.left);   /* Array expression */
		walk_tree(np->n.bn.right);  /* Index expression */
		break;

	case N_FIELD:
		/* Field/member access */
		walk_tree(np->n.bn.left);   /* Object expression */
		walk_tree(np->n.bn.right);  /* Field name */
		break;

	case N_IF:
		/* IF statement */
		walk_tree(np->n.bn.left);   /* Condition */
		walk_tree(np->n.bn.right);  /* Then/else branches */
		break;

	case N_WHILE:
		/* WHILE loop */
		walk_tree(np->n.bn.left);   /* Condition */
		walk_tree(np->n.bn.right);  /* Body */
		break;

	case N_FOR:
		/* FOR loop */
		walk_tree(np->n.bn.left);
		walk_tree(np->n.bn.right);
		break;

	case N_RETURN:
		/* RETURN statement */
		walk_tree(np->n.bn.left);   /* Return value */
		break;

	case N_BLOCK:
		/* Statement block */
		walk_tree(np->n.bn.left);
		walk_tree(np->n.bn.right);
		break;

	default:
		/* Unknown node type */
		warning("unknown AST node type: %d", np->op);
		break;
	}
}

/*
 * Print AST for debugging
 */
void
print_tree(NODE *np, int indent)
{
	int i;

	if (np == NULL)
		return;

	/* Print indentation */
	for (i = 0; i < indent; i++)
		printf("  ");

	switch (np->op) {
	case N_ICON:
		printf("ICON: %lld\n", np->n.ival.val);
		break;

	case N_FCON:
		printf("FCON: %f\n", np->n.fval.val);
		break;

	case N_SCON:
		printf("SCON: \"%s\"\n", np->n.sval.str);
		break;

	case N_NAME:
		printf("NAME: %s\n", np->n.sym.sym->sname);
		break;

	case N_PLUS:
		printf("PLUS\n");
		print_tree(np->n.bn.left, indent + 1);
		print_tree(np->n.bn.right, indent + 1);
		break;

	case N_ASSIGN:
		printf("ASSIGN\n");
		print_tree(np->n.bn.left, indent + 1);
		print_tree(np->n.bn.right, indent + 1);
		break;

	case N_CALL:
		printf("CALL\n");
		print_tree(np->n.bn.left, indent + 1);
		print_tree(np->n.bn.right, indent + 1);
		break;

	default:
		printf("OP: %d\n", np->op);
		print_tree(np->n.bn.left, indent + 1);
		print_tree(np->n.bn.right, indent + 1);
		break;
	}
}
