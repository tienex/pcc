/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "pass1.h"
#include "unicode.h"

#ifdef LANG_CXX
#define P1ND NODE
#define	p1alloc talloc
#define	p1nfree nfree
#define	p1fwalk fwalk
#define	p1tcopy ccopy
#endif

/*
 * Local optimizations and transformations for WebAssembly
 */
P1ND *
clocal(P1ND *p)
{
	struct symtab *q;
	P1ND *r, *l;
	TWORD t;
	int o;

#ifdef PCC_DEBUG
	if (xdebug) {
		printf("clocal\n");
		p1fwalk(p, eprint, 0);
	}
#endif

	switch (o = p->n_op) {
	case NAME:
		/* handle variables */
		if ((q = p->n_sp) == NULL)
			return p;

		switch (q->sclass) {
		case PARAM:
		case AUTO:
			/* WebAssembly locals - convert to OREG */
			r = block(REG, NULL, NULL, PTR+STRTY, 0, 0);
			slval(r, 0);
			r->n_rval = FPREG;
			p = stref(block(STREF, r, p, 0, 0, 0));
			break;
		default:
			break;
		}
		break;

	case PMCONV:
	case PVCONV:
		if (p->n_right->n_op != ICON)
			cerror("bad conversion");
		r = buildtree(o == PMCONV ? MUL : DIV, p->n_left, p->n_right);
		p1nfree(p);
		p = r;
		break;

	case PCONV:
		t = p->n_type;
		l = p->n_left;

		/* Simplify pointer conversions */
		if (p->n_type > BTMASK && l->n_type > BTMASK)
			goto delp;
		if (l->n_op == ICON && l->n_sp == NULL)
			goto delp;
		break;

delp:
		l->n_type = p->n_type;
		l->n_qual = p->n_qual;
		l->n_df = p->n_df;
		l->n_ap = p->n_ap;
		p = p1nfree(p);
		break;

	case FORCE:
		/* put return value in return reg */
		p->n_op = ASSIGN;
		p->n_right = p->n_left;
		p->n_left = block(REG, NULL, NULL, p->n_type, 0, 0);
		p->n_left->n_rval = RETREG(p->n_type);
		break;
	}

#ifdef PCC_DEBUG
	if (xdebug) {
		printf("clocal end\n");
		p1fwalk(p, eprint, 0);
	}
#endif

	return p;
}

/*
 * Additional tree transformations for pass 2
 */
void
myp2tree(P1ND *p)
{
	struct symtab *sp;

	switch (p->n_op) {
	case FCON:
		/* Handle floating point constants */
		sp = tmpalloc(sizeof(struct symtab));
		sp->sclass = STATIC;
		sp->sap = 0;
		sp->slevel = 1;
		sp->soffset = getlab();
		sp->sflags = 0;
		sp->stype = p->n_type;
		sp->squal = (CON >> TSHIFT);

		defloc(sp);
		ninval(0, tsize(sp->stype, sp->sdf, sp->sap), p);

		p->n_op = NAME;
		slval(p, 0);
		p->n_sp = sp;
		break;
	}
}

/*
 * Can this symbol have its address taken?
 */
int
andable(P1ND *p)
{
	return 1; /* all names can have & taken on them */
}

/*
 * Return 1 if a variable of type t is OK to put in register.
 */
int
cisreg(TWORD t)
{
	/* WebAssembly locals can hold any type */
	return 1;
}

/*
 * return a node, for structure references, which is suitable for
 * being added to a pointer of type t, in order to be off bits offset
 * into a structure
 */
P1ND *
offcon(OFFSZ off, TWORD t, union dimfun *d, struct attr *ap)
{
	P1ND *p;

	if (xdebug)
		printf("offcon: OFFSZ %lld type %x dim %p siz %d\n",
		    off, t, d, (int)tsize(t, d, ap));

	p = bcon(0);
	p->n_lval = off / SZCHAR; /* byte offset */
	return p;
}

/*
 * Allocate off bits on the stack. off is in bytes.
 * return a node that computes the address of the allocated space.
 */
P1ND *
stackaddr(P1ND *p)
{
	P1ND *r;

	r = block(REG, NULL, NULL, p->n_type, p->n_df, p->n_ap);
	r->n_lval = off;
	r->n_rval = STKREG;
	return r;
}

/*
 * Allocate bits on the stack.
 */
void
inoff(OFFSZ off, struct symtab *p)
{
	/* WebAssembly manages locals automatically */
}

/*
 * Special handling for long long operations
 */
P1ND *
special(P1ND *p, int shape)
{
	return p;
}

/*
 * Return a node which will compare the value of the node p to NULL
 */
P1ND *
comperr(P1ND *p)
{
	return buildtree(NE, p, bcon(0));
}

/*
 * Return nonzero if the register assignment for a node is acceptable.
 */
int
acceptable(struct optab *op)
{
	return 1;
}
