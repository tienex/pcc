/*
 * Copyright (c) 2025 PCC Xbase++ Compiler
 *
 * Code generation - Convert AST to PCC IR
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"
#include "../../mip/manifest.h"

/* Forward declarations */
static NODE *gen_expr(struct node *xb_node);
static void gen_stmt(struct node *xb_node);
static NODE *gen_call(struct node *xb_node);
static NODE *gen_assign(struct node *xb_node);

/* Current function being compiled */
static SYMTAB *current_function = NULL;

/* Label counter */
static int label_counter = 0;

/*
 * Generate a new label
 */
static int
new_label(void)
{
	return ++label_counter;
}

/*
 * Convert Xbase++ type to PCC type
 */
static TWORD
xb_type_to_pcc(TNODE *xb_type)
{
	if (!xb_type)
		return PTR | VOID;  /* Untyped = void* */

	switch (xb_type->ttype) {
	case TINTEGER:
		return LONGLONG;
	case TFLOAT:
	case TNUMERIC:
		return DOUBLE;
	case TCHAR:
	case TMEMO:
		return PTR | CHAR;  /* char* */
	case TLOGICAL:
		return INT;
	case TDATE:
		return INT;  /* Julian day number */
	case TARRAY:
	case TOBJECT:
	case TCODEBLOCK:
		return PTR | VOID;  /* Generic pointer */
	default:
		return PTR | VOID;  /* Default to void* */
	}
}

/*
 * Generate code for integer constant
 */
static NODE *
gen_icon(long long val)
{
	NODE *p = talloc();
	p->n_op = ICON;
	p->n_type = LONGLONG;
	p->n_lval = val;
	p->n_rval = 0;
	return p;
}

/*
 * Generate code for float constant
 */
static NODE *
gen_fcon(double val)
{
	NODE *p = talloc();
	p->n_op = FCON;
	p->n_type = DOUBLE;
	p->n_dcon = val;
	return p;
}

/*
 * Generate code for string constant
 */
static NODE *
gen_scon(char *str)
{
	NODE *p = talloc();
	p->n_op = NAME;
	p->n_type = PTR | CHAR;
	/* TODO: Add string to string table and get label */
	p->n_lval = 0;
	p->n_rval = 0;
	return p;
}

/*
 * Generate code for variable reference
 */
static NODE *
gen_name(SYMTAB *sym)
{
	NODE *p = talloc();

	sym->sflags |= SUSED;

	p->n_op = NAME;
	p->n_type = xb_type_to_pcc(sym->stype);
	p->n_sp = sym;
	p->n_lval = 0;
	p->n_rval = 0;

	return p;
}

/*
 * Generate code for binary operation
 */
static NODE *
gen_binop(int op, struct node *left, struct node *right)
{
	NODE *l = gen_expr(left);
	NODE *r = gen_expr(right);
	NODE *p = talloc();

	/* Map Xbase++ operators to PCC operators */
	switch (op) {
	case N_PLUS:   p->n_op = PLUS; break;
	case N_MINUS:  p->n_op = MINUS; break;
	case N_MUL:    p->n_op = MUL; break;
	case N_DIV:    p->n_op = DIV; break;
	case N_MOD:    p->n_op = MOD; break;
	case N_EQ:     p->n_op = EQ; break;
	case N_NE:     p->n_op = NE; break;
	case N_LT:     p->n_op = LT; break;
	case N_LE:     p->n_op = LE; break;
	case N_GT:     p->n_op = GT; break;
	case N_GE:     p->n_op = GE; break;
	case N_AND:    p->n_op = AND; break;
	case N_OR:     p->n_op = OR; break;
	default:       p->n_op = PLUS; break;
	}

	p->n_left = l;
	p->n_right = r;
	p->n_type = l->n_type;  /* Result type */

	return p;
}

/*
 * Generate code for unary operation
 */
static NODE *
gen_unop(int op, struct node *operand)
{
	NODE *p = talloc();
	NODE *o = gen_expr(operand);

	switch (op) {
	case N_UMINUS: p->n_op = UMINUS; break;
	case N_NOT:    p->n_op = NOT; break;
	default:       p->n_op = UMINUS; break;
	}

	p->n_left = o;
	p->n_type = o->n_type;

	return p;
}

/*
 * Generate code for assignment
 */
static NODE *
gen_assign(struct node *xb_node)
{
	NODE *p = talloc();
	NODE *left = gen_expr(xb_node->n.bn.left);
	NODE *right = gen_expr(xb_node->n.bn.right);

	p->n_op = ASSIGN;
	p->n_left = left;
	p->n_right = right;
	p->n_type = left->n_type;

	/* Mark left side as set */
	if (xb_node->n.bn.left->op == N_NAME) {
		xb_node->n.bn.left->n.sym.sym->sflags |= SSET;
	}

	return p;
}

/*
 * Generate code for function call
 */
static NODE *
gen_call(struct node *xb_node)
{
	NODE *p = talloc();
	NODE *func = gen_expr(xb_node->n.bn.left);
	NODE *args = NULL;

	if (xb_node->n.bn.right) {
		args = gen_expr(xb_node->n.bn.right);
	}

	p->n_op = CALL;
	p->n_left = func;
	p->n_right = args;
	p->n_type = PTR | VOID;  /* Generic return type */

	return p;
}

/*
 * Generate code for array subscript
 */
static NODE *
gen_subscript(struct node *xb_node)
{
	NODE *p = talloc();
	NODE *arr = gen_expr(xb_node->n.bn.left);
	NODE *idx = gen_expr(xb_node->n.bn.right);

	p->n_op = PLUS;  /* Array access is base + index */
	p->n_left = arr;
	p->n_right = idx;
	p->n_type = PTR | VOID;

	/* Dereference the result */
	NODE *deref = talloc();
	deref->n_op = UMUL;
	deref->n_left = p;
	deref->n_type = PTR | VOID;

	return deref;
}

/*
 * Generate code for field access
 */
static NODE *
gen_field(struct node *xb_node)
{
	NODE *p = talloc();
	NODE *obj = gen_expr(xb_node->n.bn.left);

	/* Field access - for now treat as function call to accessor */
	p->n_op = CALL;
	p->n_left = obj;
	p->n_right = NULL;
	p->n_type = PTR | VOID;

	return p;
}

/*
 * Generate code for expression
 */
static NODE *
gen_expr(struct node *xb_node)
{
	if (!xb_node)
		return NULL;

	switch (xb_node->op) {
	case N_ICON:
		return gen_icon(xb_node->n.ival.val);

	case N_FCON:
		return gen_fcon(xb_node->n.fval.val);

	case N_SCON:
		return gen_scon(xb_node->n.sval.str);

	case N_NAME:
		return gen_name(xb_node->n.sym.sym);

	case N_ASSIGN:
		return gen_assign(xb_node);

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
		return gen_binop(xb_node->op, xb_node->n.bn.left, xb_node->n.bn.right);

	case N_UMINUS:
	case N_NOT:
		return gen_unop(xb_node->op, xb_node->n.bn.left);

	case N_CALL:
		return gen_call(xb_node);

	case N_SUBSCR:
		return gen_subscript(xb_node);

	case N_FIELD:
		return gen_field(xb_node);

	default:
		warning("code generation for op %d not implemented", xb_node->op);
		return gen_icon(0);
	}
}

/*
 * Generate code for IF statement
 */
static void
gen_if(struct node *xb_node)
{
	int else_label = new_label();
	int end_label = new_label();

	NODE *cond = gen_expr(xb_node->n.bn.left);

	/* Generate: if (!cond) goto else_label */
	ecomp(buildtree(NOT, cond, NULL));
	cbranch(buildtree(NOT, cond, NULL), bcon(else_label));

	/* Generate then block */
	if (xb_node->n.bn.right) {
		gen_stmt(xb_node->n.bn.right);
	}

	/* Jump to end */
	branch(end_label);

	/* Else label */
	plabel(else_label);

	/* TODO: Generate else block if present */

	/* End label */
	plabel(end_label);
}

/*
 * Generate code for WHILE loop
 */
static void
gen_while(struct node *xb_node)
{
	int loop_label = new_label();
	int end_label = new_label();

	/* Loop label */
	plabel(loop_label);

	/* Condition */
	NODE *cond = gen_expr(xb_node->n.bn.left);
	cbranch(buildtree(NOT, cond, NULL), bcon(end_label));

	/* Body */
	if (xb_node->n.bn.right) {
		gen_stmt(xb_node->n.bn.right);
	}

	/* Loop back */
	branch(loop_label);

	/* End label */
	plabel(end_label);
}

/*
 * Generate code for RETURN statement
 */
static void
gen_return(struct node *xb_node)
{
	NODE *retval = NULL;

	if (xb_node->n.bn.left) {
		retval = gen_expr(xb_node->n.bn.left);
	}

	/* Generate return */
	ecomp(buildtree(RETURN, retval, NULL));
}

/*
 * Generate code for statement
 */
static void
gen_stmt(struct node *xb_node)
{
	if (!xb_node)
		return;

	switch (xb_node->op) {
	case N_IF:
		gen_if(xb_node);
		break;

	case N_WHILE:
		gen_while(xb_node);
		break;

	case N_RETURN:
		gen_return(xb_node);
		break;

	case N_BLOCK:
		/* Generate left then right */
		gen_stmt(xb_node->n.bn.left);
		gen_stmt(xb_node->n.bn.right);
		break;

	default:
		/* Expression statement */
		if (xb_node->op >= N_PLUS && xb_node->op <= N_FIELD) {
			NODE *expr = gen_expr(xb_node);
			ecomp(expr);
		}
		break;
	}
}

/*
 * Generate code for function definition
 */
void
codegen_function(SYMTAB *func, struct node *body)
{
	current_function = func;
	label_counter = 0;

	/* Function prologue */
	printf("; Function: %s\n", func->sname);

	/* Generate body */
	if (body) {
		gen_stmt(body);
	}

	/* Function epilogue */
	current_function = NULL;
}

/*
 * Initialize code generation
 */
void
codegen_init(void)
{
	label_counter = 0;
}
