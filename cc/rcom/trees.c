/*	$Id$	*/

/*
 * Ruby tree manipulation functions
 *
 * Implements tree building and manipulation for Ruby AST
 */

#include "pass1.h"
#include <stdlib.h>
#include <string.h>

/* Build a tree node with the given operator and children */
struct p1node *
buildtree(int op, struct p1node *l, struct p1node *r)
{
	struct p1node *p;

	p = talloc();
	p->n_op = op;
	p->n_left = l;
	p->n_right = r;
	p->n_type = INT;  /* Default type */

	/* Type inference would go here */
	if (l) {
		p->n_type = l->n_type;
	}

	return p;
}

/* Create a block/compound statement node */
struct p1node *
block(int op, struct p1node *l, struct p1node *r)
{
	return buildtree(op, l, r);
}

/* Create a name tree from a symbol table entry */
struct p1node *
nametree(struct symtab *sp)
{
	struct p1node *p;

	p = talloc();
	p->n_op = NAME;
	p->n_sp = sp;
	p->n_type = sp->stype;
	p->n_qual = sp->squal;
	p->n_df = sp->sdf;
	p->n_ap = sp->sap;

	return p;
}

/* Create an integer constant node */
struct p1node *
bcon(CONSZ val)
{
	struct p1node *p;

	p = talloc();
	p->n_op = ICON;
	p->n_type = INT;
	p->n_lval = val;
	p->n_sp = NULL;

	return p;
}

/* Create a floating-point constant node */
struct p1node *
fcon(char *str)
{
	struct p1node *p;

	p = talloc();
	p->n_op = FCON;
	p->n_type = DOUBLE;
	p->n_sp = NULL;

	/* Parse and store float value */
	/* This is simplified - real implementation would handle softfloat */

	return p;
}

/* Create a string constant node */
struct p1node *
string(char *str)
{
	struct p1node *p;
	struct symtab *sp;

	/* Create symbol for string constant */
	sp = lookup(str, 0);
	if (!sp->stype) {
		sp->stype = PTR | CHAR;
		sp->sclass = STATIC;
	}

	p = talloc();
	p->n_op = STRING;
	p->n_type = PTR | CHAR;
	p->n_sp = sp;

	/* Store string in data segment */
	locctr(STRNG, sp);

	return p;
}

/* Emit code for an expression */
void
ecomp(struct p1node *p)
{
	if (p == NULL)
		return;

	/* Generate code for this tree */
	/* This calls the myp2tree function to lower to Pass 2 IR */
	send_passt(IP_NODE, p);
}

/* Define an identifier */
void
defid(struct symtab *sp, int class)
{
	sp->sclass = class;
	/* Additional symbol table setup */
}

/*
 * Ruby-specific tree manipulation functions
 */

/* Create a Ruby block node */
struct p1node *
ruby_block_node(int nparams, char **params, struct p1node *body)
{
	struct p1node *p;

	p = talloc();
	p->n_op = CALL;  /* Treat as function call for now */
	p->n_type = INT;
	p->n_left = body;
	p->n_right = NULL;

	/* Store block parameters in symbol table */
	/* This is simplified */

	return p;
}

/* Create a Ruby method call node */
struct p1node *
ruby_method_call(struct p1node *receiver, char *method, struct p1node *args)
{
	struct symtab *sp;
	struct p1node *fn, *all_args;

	/* Look up method name */
	sp = lookup(method, 0);
	fn = nametree(sp);

	/* Combine receiver with arguments */
	if (receiver) {
		if (args) {
			all_args = block(CM, receiver, args);
		} else {
			all_args = receiver;
		}
	} else {
		all_args = args;
	}

	return buildtree(CALL, fn, all_args);
}

/* Create a Ruby class definition node */
struct p1node *
ruby_class_node(char *name, struct p1node *superclass, struct p1node *body)
{
	struct symtab *sp;

	/* Create symbol for class */
	sp = lookup(name, 0);
	sp->stype = STRTY;
	sp->sclass = STNAME;

	/* Process class body */
	return body;
}

/* Create a Ruby string interpolation node */
struct p1node *
ruby_string_interp(struct p1node *parts)
{
	/* Convert string interpolation to concatenation */
	/* This is simplified */
	return parts;
}
