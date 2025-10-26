/*
 * COBOL expression handling
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

NODE *
buildtree(int op, NODE *left, NODE *right)
{
	NODE *p;

	/* Allocate node */
	p = talloc();
	p->n_op = op;
	p->n_left = left;
	p->n_right = right;

	/* Set type based on operands */
	if (left)
		p->n_type = left->n_type;
	else
		p->n_type = INT;

	return p;
}

NODE *
make_icon(CONSZ val)
{
	NODE *p = talloc();
	p->n_op = ICON;
	p->n_lval = val;
	p->n_type = INT;
	return p;
}

NODE *
make_name(const char *name)
{
	NODE *p;
	struct cobsym *sym;

	/* Look up symbol */
	sym = lookup(name);
	if (!sym) {
		error_at(lineno, "Undefined symbol: %s", name);
		return make_icon(0);
	}

	/* Create NAME node */
	p = talloc();
	p->n_op = NAME;
	p->n_name = xstrdup(name);

	/* Set type from symbol's PICTURE clause */
	if (sym->pic)
		p->n_type = picture_to_type(sym->pic);
	else
		p->n_type = INT;

	return p;
}
