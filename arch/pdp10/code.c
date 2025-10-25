/*	$Id$	*/
/*
 * Copyright (c) 2003 Anders Magnusson (ragge@ludd.luth.se).
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission
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


# include "pass1.h"

#ifdef LANG_CXX
#define p1listf listf
#define p1tfree tfree
#else
#define NODE P1ND
#define talloc p1alloc
#endif


/*
 * Print out assembler segment name.
 */
void
setseg(int seg, char *name)
{
	switch (seg) {
	case PROG: name = ".text"; break;
	case DATA:
	case LDATA: name = ".data"; break;
	case STRNG:
	case RDATA: name = ".section .rodata"; break;
	case UDATA: break;
	case CTORS: name = ".section .ctors,\"aw\",@progbits"; break;
	case DTORS: name = ".section .dtors,\"aw\",@progbits"; break;
	case NMSEG:
		printf("\t.section %s\n", name);
		return;
	default:
		cerror("setseg: unknown segment %d", seg);
	}
	if (name != NULL)
		printf("\t%s\n", name);
}

/*
 * Define everything needed to print out some data (or text).
 * This means segment, alignment, visibility, etc.
 */
void
defloc(struct symtab *sp)
{
	char *nextsect = NULL;	/* notyet */
	static char *loctbl[] = { "text", "data", "section .rodata" };
	static int lastloc = -1;
	TWORD t;
	int s;

	if (sp == NULL) {
		lastloc = -1;
		return;
	}
	t = sp->stype;
	s = ISFTN(t) ? PROG : ISCON(cqual(t, sp->squal)) ? RDATA : DATA;
	if (nextsect) {
		printf("	.section %s\n", nextsect);
		nextsect = NULL;
		s = -1;
	} else if (s != lastloc)
		printf("	.%s\n", loctbl[s]);
	lastloc = s;
	if (sp->sclass == EXTDEF)
		printf("	.globl %s\n", getexname(sp));
	if (sp->slevel == 0)
		printf("%s:\n", getexname(sp));
	else
		printf(LABFMT ":\n", sp->soffset);
}

int structrettemp;  /* temp for struct return pointer */

/*
 * code for the end of a function
 * For struct returns, copy the local struct to the address
 * passed in the hidden first argument on the stack.
 */
void
efcode(void)
{
	NODE *p, *q, *r;
	int sz;

	/* Only handle struct/union returns */
	if (cftnsp->stype != STRTY+FTN && cftnsp->stype != UNIONTY+FTN)
		return;

	/* Get size of return struct */
	sz = (int)tsize(BTYPE(cftnsp->stype), cftnsp->sdf, cftnsp->sap);

	/* Load the hidden return pointer from stack into a temp */
	/* The hidden arg is stored at offset 0 from frame pointer */
	p = tempnode(structrettemp, PTR+STRTY, cftnsp->sdf, cftnsp->sap);

	/* Create node for the auto struct being returned */
	q = block(NAME, NIL, NIL, cftnsp->stype, cftnsp->sdf, cftnsp->sap);
	q = buildtree(ADDROF, q, NIL);

	/* Generate memcpy to copy struct to return location */
	/* This will be handled by structure assignment code */
	r = block(ICON, NIL, NIL, INT, 0, 0);
	slval(r, sz / SZCHAR);  /* size in bytes */

	/* Build the copy: *retptr = struct_value */
	p = buildtree(UMUL, p, NIL);
	q = buildtree(UMUL, q, NIL);
	ecomp(buildtree(ASSIGN, p, q));
}

/*
 * code for the beginning of a function; a is an array of
 * indices in stab for the arguments; n is the number
 */
void
bfcode(struct symtab **sp, int cnt)
{
	NODE *p, *q;
	int i, n;
	int stackoffset = 0;

	/* Handle struct/union return: hidden pointer is first arg on stack */
	if (cftnsp->stype == STRTY+FTN || cftnsp->stype == UNIONTY+FTN) {
		/* Create a temp to hold the return pointer */
		p = tempnode(0, PTR+STRTY, cftnsp->sdf, cftnsp->sap);
		structrettemp = regno(p);

		/* Load from stack: the hidden pointer is at offset 0 from FP */
		/* Stack layout: [saved FP][return addr][hidden ptr][args...] */
		q = block(OREG, NIL, NIL, PTR+STRTY, cftnsp->sdf, cftnsp->sap);
		q->n_rval = FPREG;  /* R16 = frame pointer */
		slval(q, 2 * SZINT / SZCHAR);  /* skip saved FP and return addr */

		/* Assign: temp = *(FP + offset) */
		ecomp(buildtree(ASSIGN, p, q));

		/* Hidden arg consumed one stack slot */
		stackoffset = SZINT;
	}

	/* recalculate the arg offset and create TEMP moves */
	for (n = 1, i = 0; i < cnt; i++) {
		if (n < 8) {
			p = tempnode(0, sp[i]->stype, sp[i]->sdf, sp[i]->sap);
			q = block(REG, NIL, NIL,
			    sp[i]->stype, sp[i]->sdf, sp[i]->sap);
			q->n_rval = n;
			p = buildtree(ASSIGN, p, q);
			sp[i]->soffset = regno(p->n_left);
			sp[i]->sflags |= STNODE;
			ecomp(p);
		} else {
			sp[i]->soffset += SZINT * n + stackoffset;
			if (xtemps) {
				/* put stack args in temps if optimizing */
				p = tempnode(0, sp[i]->stype,
				    sp[i]->sdf, sp[i]->sap);
				p = buildtree(ASSIGN, p, nametree(sp[i]));
				sp[i]->soffset = regno(p->n_left);
				sp[i]->sflags |= STNODE;
				ecomp(p);
			}
		}
		n += szty(sp[i]->stype);
	}
}


void
bjobcode(void)
{
}

/* called just before final exit */
/* flag is 1 if errors, 0 if none */
void
ejobcode(int flag)
{
}

/*
 * Make a register node, helper for funcode.
 */
static NODE *
mkreg(NODE *p, int n)
{
	NODE *r;

	r = block(REG, NIL, NIL, p->n_type, p->n_df, p->n_ap);
	if (szty(p->n_type) == 2)
		n += 16;
	r->n_rval = n;
	return r;
}

static int regnum;
/*
 * Move args to registers and emit expressions bottom-up.
 */
static void
fixargs(NODE *p)
{
	NODE *r;

	if (p->n_op == CM) {
		fixargs(p->n_left);
		r = p->n_right;
		if (r->n_op == STARG)
			regnum = 9; /* end of register list */
		else if (regnum + szty(r->n_type) > 8)
			p->n_right = block(FUNARG, r, NIL, r->n_type,
			    r->n_df, r->n_ap);
		else
			p->n_right = buildtree(ASSIGN, mkreg(r, regnum), r);
	} else {
		if (p->n_op == STARG) {
			regnum = 9; /* end of register list */
		} else {
			r = talloc();
			*r = *p;
			r = buildtree(ASSIGN, mkreg(r, regnum), r);
			*p = *r;
			p1nfree(r);
		}
		r = p;
	}
	regnum += szty(r->n_type);
}


/*
 * Called with a function call with arguments as argument.
 * This is done early in buildtree() and only done once.
 * Handle struct returns by adding a hidden pointer argument.
 */
NODE *
funcode(NODE *p)
{
	NODE *r, *l;
	TWORD t;

	regnum = 1;

	/* Check if this function returns a struct/union */
	t = p->n_type;
	if (p->n_op == CALL && (ISSOU(BTYPE(t)))) {
		/* Allocate temp space for the returned struct */
		r = tempnode(0, t, p->n_df, p->n_ap);
		structrettemp = regno(r);

		/* Create address of temp */
		r = buildtree(ADDROF, r, NIL);

		/* Add as hidden first argument on stack (FUNARG) */
		/* This goes on stack, not in registers */
		l = talloc();
		l->n_op = FUNARG;
		l->n_left = r;
		l->n_type = r->n_type;
		l->n_df = r->n_df;
		l->n_ap = r->n_ap;
		l->n_right = NIL;

		/* Prepend to argument list */
		if (p->n_right->n_op != NIL) {
			r = talloc();
			r->n_op = CM;
			r->n_left = l;
			r->n_right = p->n_right;
			r->n_type = INT;  /* doesn't matter */
			p->n_right = r;
		} else {
			p->n_right = l;
		}
	}

	fixargs(p->n_right);
	return p;
}

/* fix up type of field p */
void
fldty(struct symtab *p)
{
}

/*
 * Variadic argument support for PDP-10.
 * Since all variadic arguments are on the stack, implementation is simple:
 * va_list is just a pointer to the stack location.
 */

/* __builtin_va_start / __builtin_stdarg_start */
NODE *
pdp10_builtin_stdarg_start(const struct bitable *bt, NODE *a)
{
	NODE *p, *q;
	int off;

	/* a->n_left is the va_list, a->n_right is the last fixed arg (unused) */
	p = a->n_left;
	nfree(a->n_right);  /* Don't need the last arg reference */

	/*
	 * Calculate offset to first variadic argument.
	 * Fixed args in registers R1-R7 have been saved to stack if needed.
	 * Variadic args start after all fixed args.
	 * Stack offset calculation: account for saved FP, return addr, and fixed args.
	 */
	off = (2 * SZINT) / SZCHAR;  /* Skip saved FP and return addr */

	/* Create stack reference for first variadic arg location */
	q = block(OREG, NIL, NIL, PTR+CHAR, 0, 0);
	q->n_rval = FPREG;  /* R16 = frame pointer */
	slval(q, off);

	/* Assign: va_list = &first_vararg */
	return buildtree(ASSIGN, p, q);
}

/* __builtin_va_arg */
NODE *
pdp10_builtin_va_arg(const struct bitable *bt, NODE *a)
{
	NODE *p, *q, *r;
	int sz, tmpnr;
	TWORD type;

	/* a->n_left is va_list, a->n_right is type node */
	p = a->n_left;
	q = a->n_right;

	/* Get type and size */
	type = q->n_type;
	sz = (int)tsize(type, q->n_df, q->n_ap);

	/* Dereference va_list to get current arg pointer */
	r = buildtree(UMUL, ccopy(p), NIL);

	/* Cast to correct type */
	r = cast(r, type, 0);

	/* Round up size to SZINT for stack alignment */
	if (sz < SZINT)
		sz = SZINT;

	/* Increment va_list pointer: va_list += sizeof(type) */
	q = buildtree(PLUS, p, bcon(sz / SZCHAR));
	p = a->n_left;  /* Get va_list again */
	ecomp(buildtree(ASSIGN, p, q));

	tfree(a);
	return r;
}

/* __builtin_va_end - no-op on PDP-10 */
NODE *
pdp10_builtin_va_end(const struct bitable *bt, NODE *a)
{
	tfree(a);
	return bcon(0);  /* Return dummy value */
}

/* __builtin_va_copy */
NODE *
pdp10_builtin_va_copy(const struct bitable *bt, NODE *a)
{
	/* a->n_left is dest, a->n_right is src */
	/* Simply: dest = src */
	return buildtree(ASSIGN, a->n_left, a->n_right);
}

/*
 * Return 0 to use default if-else chain for switch statements.
 * Could be optimized with jump tables for dense switches, but
 * if-else chains work correctly for all cases.
 */
int
mygenswitch(int num, TWORD type, struct swents **p, int n)
{
	return 0;
}

/*
 * __builtin_return_address(level)
 * Returns the return address at the given frame level.
 * Level 0 is the current function's return address.
 */
NODE *
builtin_return_address(const struct bitable *bt, NODE *a)
{
	int nframes;
	NODE *f;

	if (a->n_op != ICON) {
		uerror("__builtin_return_address requires constant argument");
		return bcon(0);
	}

	nframes = (int)glval(a);
	tfree(a);

	/* Start with frame pointer (R16) */
	f = block(REG, NIL, NIL, PTR+VOID, 0, 0);
	regno(f) = FPREG;

	/* Walk up the frame chain */
	while (nframes--)
		f = block(UMUL, f, NIL, PTR+VOID, 0, 0);

	/* Return address is at offset 1 from frame pointer */
	f = block(PLUS, f, bcon(1), INCREF(PTR+VOID), 0, 0);
	f = buildtree(UMUL, f, NIL);

	return f;
}

/*
 * __builtin_frame_address(level)
 * Returns the frame pointer at the given frame level.
 * Level 0 is the current function's frame pointer.
 */
NODE *
builtin_frame_address(const struct bitable *bt, NODE *a)
{
	int nframes;
	NODE *f;

	if (a->n_op != ICON) {
		uerror("__builtin_frame_address requires constant argument");
		return bcon(0);
	}

	nframes = (int)glval(a);
	tfree(a);

	/* Start with frame pointer (R16) */
	f = block(REG, NIL, NIL, PTR+VOID, 0, 0);
	regno(f) = FPREG;

	/* Walk up the frame chain */
	while (nframes--)
		f = block(UMUL, f, NIL, PTR+VOID, 0, 0);

	return f;
}

/*
 * __builtin_dwarf_cfa()
 * Returns the Canonical Frame Address (CFA) - the value of the stack pointer
 * in the calling function. This is used for DWARF debugging information.
 * For PDP-10, the CFA is the frame pointer plus 2 (skipping saved FP and return addr).
 */
NODE *
builtin_cfa(const struct bitable *bt, NODE *a)
{
	NODE *f;

	tfree(a);  /* CFA takes no arguments */

	/* Start with frame pointer (R16) */
	f = block(REG, NIL, NIL, PTR+VOID, 0, 0);
	regno(f) = FPREG;

	/* CFA = FP + 2 (2 words: saved FP + return address) */
	return block(PLUS, f, bcon(2), INCREF(PTR+VOID), 0, 0);
}
