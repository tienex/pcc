/*
 * Copyright (c) 2025 PCC BLISS Compiler
 *
 * Tree/IR generation - Bridge to MIP backend
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Build a tree node (stub - will use MIP functions) */
NODE *buildtree(int op, NODE *left, NODE *right) {
	/* This would call the real MIP buildtree function */
	/* For now, just a placeholder */
	return NULL;
}

/* Create integer constant node */
NODE *mkintconst(int value) {
	/* This would create a MIP ICON node */
	return NULL;
}

/* Create name reference node */
NODE *mkname(char *name) {
	/* Look up symbol and create NAME node */
	SYMTAB *sp = find_symbol(name);
	if (sp == NULL) {
		error("undefined identifier '%s'", name);
		return NULL;
	}
	sp->sflags |= SUSED;
	/* Create NAME node */
	return NULL;
}

/* Create function call node */
NODE *mkcall(NODE *func, NODE *args) {
	/* Create CALL node with function and arguments */
	return NULL;
}

/* Create assignment node */
NODE *mkassign(NODE *left, NODE *right) {
	/* Create ASSIGN node */
	return NULL;
}

/* Create if expression node */
NODE *mkif(NODE *cond, NODE *then_part, NODE *else_part) {
	/* Create conditional node (CBRANCH or similar) */
	return NULL;
}

/* Create loop node */
NODE *mkloop(int type, NODE *cond, NODE *body) {
	/* Create loop construct */
	return NULL;
}

/* Create block node */
NODE *mkblock(NODE *decls, NODE *stmts) {
	/* Create block with declarations and statements */
	return NULL;
}

/* Check if node is a constant */
int is_constant(NODE *p) {
	/* Check if p is a constant expression */
	return 0;
}

/* Evaluate constant expression */
int eval_constant(NODE *p) {
	/* Evaluate compile-time constant */
	return 0;
}

/* Emit completed expression tree (to MIP backend) */
void ecomp(NODE *p) {
	/* This would emit the tree to the backend */
	/* For now, just a stub */
}

/* Emit local comments */
void lcommprint(void) {
	/* Print local comments if any */
}

/* End of function code */
void efcode(void) {
	/* Emit function epilogue code */
}

/* Beginning of function code */
void bfcode(void) {
	/* Emit function prologue code */
}

/* Get a new label */
int getlab(void) {
	static int labelno = 1;
	return labelno++;
}
