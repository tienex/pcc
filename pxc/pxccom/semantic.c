/*
 * Copyright (c) 2025 PCC Xbase++ Compiler
 *
 * Semantic analysis - Type checking and validation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Current scope context */
static int current_level = 0;
static SYMTAB *current_func = NULL;

/*
 * Enter a new scope
 */
void
semantic_enter_scope(void)
{
	current_level++;
	enter_scope();
}

/*
 * Exit current scope
 */
void
semantic_exit_scope(void)
{
	exit_scope();
	current_level--;
}

/*
 * Check if expression is an lvalue (can be assigned to)
 */
static int
is_lvalue(struct node *n)
{
	if (!n)
		return 0;

	switch (n->op) {
	case N_NAME:     /* Variable */
	case N_SUBSCR:   /* Array element */
	case N_FIELD:    /* Object field */
		return 1;
	default:
		return 0;
	}
}

/*
 * Infer type of expression
 */
static TNODE *
infer_type(struct node *n)
{
	if (!n)
		return NULL;

	/* If type already set, return it */
	if (n->n_type)
		return n->n_type;

	switch (n->op) {
	case N_ICON:
		n->n_type = make_type(TINTEGER);
		break;

	case N_FCON:
		n->n_type = make_type(TFLOAT);
		break;

	case N_SCON:
		n->n_type = make_type(TCHAR);
		break;

	case N_NAME:
		if (n->n.sym.sym && n->n.sym.sym->stype)
			n->n_type = n->n.sym.sym->stype;
		else
			n->n_type = make_type(TVARIANT);
		break;

	case N_PLUS:
	case N_MINUS:
	case N_MUL:
	case N_DIV:
	case N_MOD: {
		TNODE *lt = infer_type(n->n.bn.left);
		TNODE *rt = infer_type(n->n.bn.right);

		/* Numeric operations preserve type */
		if (lt && (lt->ttype == TFLOAT || lt->ttype == TNUMERIC))
			n->n_type = lt;
		else if (rt && (rt->ttype == TFLOAT || rt->ttype == TNUMERIC))
			n->n_type = rt;
		else
			n->n_type = make_type(TNUMERIC);
		break;
	}

	case N_EQ:
	case N_NE:
	case N_LT:
	case N_LE:
	case N_GT:
	case N_GE:
	case N_AND:
	case N_OR:
	case N_NOT:
		n->n_type = make_type(TLOGICAL);
		break;

	case N_ASSIGN:
		n->n_type = infer_type(n->n.bn.left);
		break;

	case N_CALL:
		/* Function calls - check if builtin or user function */
		if (n->n.bn.left && n->n.bn.left->op == N_NAME) {
			SYMTAB *func = n->n.bn.left->n.sym.sym;
			if (func && func->stype)
				n->n_type = func->stype;
			else
				n->n_type = make_type(TVARIANT);
		} else {
			n->n_type = make_type(TVARIANT);
		}
		break;

	case N_SUBSCR:
		/* Array subscript - element type */
		infer_type(n->n.bn.left);
		if (n->n.bn.left->n_type && n->n.bn.left->n_type->ttype == TARRAY) {
			n->n_type = n->n.bn.left->n_type->tattr.array.elem_type;
		} else {
			n->n_type = make_type(TVARIANT);
		}
		break;

	case N_FIELD:
		/* Object field access */
		n->n_type = make_type(TVARIANT);
		break;

	default:
		n->n_type = make_type(TVARIANT);
		break;
	}

	return n->n_type;
}

/*
 * Analyze expression
 */
static void
analyze_expr(struct node *n)
{
	if (!n)
		return;

	switch (n->op) {
	case N_ICON:
	case N_FCON:
	case N_SCON:
		/* Constants - nothing to check */
		break;

	case N_NAME:
		/* Check if variable is declared */
		if (!n->n.sym.sym) {
			error("undeclared variable");
		}
		break;

	case N_ASSIGN:
		/* Check lvalue */
		if (!is_lvalue(n->n.bn.left)) {
			error("invalid lvalue in assignment");
		}
		analyze_expr(n->n.bn.left);
		analyze_expr(n->n.bn.right);

		/* Check type compatibility */
		infer_type(n->n.bn.left);
		infer_type(n->n.bn.right);
		if (!type_compatible(n->n.bn.left->n_type, n->n.bn.right->n_type)) {
			warning("type mismatch in assignment");
		}
		break;

	case N_CALL:
		/* Analyze function and arguments */
		analyze_expr(n->n.bn.left);
		analyze_expr(n->n.bn.right);

		/* Check argument count for built-ins */
		if (n->n.bn.left && n->n.bn.left->op == N_NAME) {
			SYMTAB *func = n->n.bn.left->n.sym.sym;
			if (func) {
				/* TODO: Check argument count and types */
			}
		}
		break;

	case N_SUBSCR:
		analyze_expr(n->n.bn.left);
		analyze_expr(n->n.bn.right);

		/* Check that left side is array */
		infer_type(n->n.bn.left);
		if (n->n.bn.left->n_type &&
		    n->n.bn.left->n_type->ttype != TARRAY &&
		    n->n.bn.left->n_type->ttype != TVARIANT) {
			error("subscript on non-array type");
		}

		/* Check that index is numeric */
		infer_type(n->n.bn.right);
		if (n->n.bn.right->n_type &&
		    n->n.bn.right->n_type->ttype != TINTEGER &&
		    n->n.bn.right->n_type->ttype != TNUMERIC &&
		    n->n.bn.right->n_type->ttype != TVARIANT) {
			error("array index must be numeric");
		}
		break;

	default:
		/* Recursively analyze children */
		if (n->n.bn.left)
			analyze_expr(n->n.bn.left);
		if (n->n.bn.right)
			analyze_expr(n->n.bn.right);
		break;
	}

	/* Infer type for this expression */
	infer_type(n);
}

/*
 * Analyze statement
 */
static void
analyze_stmt(struct node *n)
{
	if (!n)
		return;

	switch (n->op) {
	case N_IF:
		/* Analyze condition and body */
		analyze_expr(n->n.bn.left);
		analyze_stmt(n->n.bn.right);
		break;

	case N_WHILE:
		/* Analyze condition and body */
		analyze_expr(n->n.bn.left);
		analyze_stmt(n->n.bn.right);
		break;

	case N_FOR:
		/* TODO: Analyze FOR loop parts */
		analyze_stmt(n->n.bn.right);
		break;

	case N_RETURN:
		/* Analyze return value */
		if (n->n.bn.left) {
			analyze_expr(n->n.bn.left);
		}
		break;

	case N_BLOCK:
		/* Analyze both statements */
		analyze_stmt(n->n.bn.left);
		analyze_stmt(n->n.bn.right);
		break;

	default:
		/* Expression statement */
		analyze_expr(n);
		break;
	}
}

/*
 * Analyze function definition
 */
void
semantic_analyze_function(SYMTAB *func, struct node *body)
{
	current_func = func;

	/* Analyze function body */
	if (body) {
		analyze_stmt(body);
	}

	current_func = NULL;
}

/*
 * Initialize semantic analysis
 */
void
semantic_init(void)
{
	current_level = 0;
	current_func = NULL;
}
