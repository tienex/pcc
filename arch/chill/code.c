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

#ifdef LANG_CXX
#define P1ND NODE
#define p1alloc talloc
#define p1nfree nfree
#define p1fwalk fwalk
#define p1tcopy ccopy
#endif

/*
 * cause the alignment to become a multiple of n
 */
void
defalign(int n)
{
	n /= SZCHAR;
	if (n == 1)
		return;
	printf("	.align	%d\n", n);
}

/*
 * Print out assembler segment name.
 */
void
setseg(int seg, char *name)
{
	switch (seg) {
	case PROG:
		printf("	.text\n");
		break;
	case DATA:
	case LDATA:
		printf("	.data\n");
		break;
	case STRNG:
	case RDATA:
		printf("	.section .rodata\n");
		break;
	case UDATA:
		printf("	.bss\n");
		break;
	default:
		cerror((char *)__func__);
	}
}

/*
 * Define everything needed to print out some data (or text).
 * This means segment, alignment, visibility, etc.
 */
void
defloc(struct symtab *sp)
{
	char *name;

	name = getexname(sp);

	if (ISFTN(sp->stype)) {
		/* Function declaration */
		if (sp->sclass == EXTDEF)
			printf("	.globl	%s\n", name);
		printf("	.type	%s,@function\n", name);
		printf("%s:\n", name);
		return;
	}

	if (sp->sclass == EXTDEF)
		printf("	.globl	%s\n", name);

	if (sp->slevel == 0)
		printf("%s:\n", name);
	else
		printf(LABFMT ":\n", sp->soffset);
}

/*
 * make a common declaration for id, if reasonable
 */
void
defzero(struct symtab *sp)
{
	int off;
	char *name;

	name = getexname(sp);
	off = tsize(sp->stype, sp->sdf, sp->sap);
	SETOFF(off, SZCHAR);
	off /= SZCHAR;

	if (sp->sclass == STATIC) {
		if (sp->slevel == 0) {
			printf("	.local	%s\n", name);
		} else
			printf("	.local	" LABFMT "\n", sp->soffset);
	} else
		printf("	.comm	%s,%d\n", name, off);
}

/*
 * code for the end of a function
 */
void
efcode(void)
{
	P1ND *p, *q;

	if (cftnsp->stype != STRTY+FTN && cftnsp->stype != UNIONTY+FTN) {
		/* Normal function return */
		printf("	ret\n");
		return;
	}

	/* Handle struct return */
	p = block(REG, NULL, NULL, CHAR+PTR, 0, 0);
	regno(p) = R0;
	q = block(OREG, NULL, NULL, CHAR+PTR, 0, 0);
	regno(q) = FPREG;
	slval(q, 8);
	p = block(CM, q, p, INT, 0, 0);
	p->n_right->n_name = "";
	p = block(CALL, bcon(0), p, CHAR+PTR, 0, 0);
	p->n_left->n_name = "memcpy";
	p = clocal(p);
	send_passt(IP_NODE, p);

	printf("	ret\n");
}

/*
 * code for the beginning of a function
 */
void
bfcode(struct symtab **a, int n)
{
	int i, off;

	/* Save frame pointer and set up new frame */
	printf("	addi	sp,sp,-16\n");
	printf("	sd	ra,8(sp)\n");
	printf("	sd	s0,0(sp)\n");
	printf("	addi	s0,sp,16\n");

	if (cftnsp->stype != STRTY+FTN && cftnsp->stype != UNIONTY+FTN)
		return;

	/* Function returns struct, adjust arg offset */
	for (i = 0; i < n; i++)
		a[i]->soffset += SZPOINT(INT);
}

/*
 * called just before final exit
 */
void
ejobcode(int flag)
{
	printf("	.ident	\"PCC: CHILL compiler\"\n");
}

void
bjobcode(void)
{
	/* Nothing needed */
}

/*
 * Return the field type for the given symtab entry
 */
TWORD
fldty(struct symtab *p)
{
	return (TWORD)DEUNSIGN(p->stype);
}

/*
 * Generate code for switch statement
 */
void
mygenswitch(int num, TWORD type, struct swents **p, int n)
{
	/* Use default jump table implementation */
}

/*
 * Called with a function call with arguments as argument.
 * Generate code for the function call.
 */
P1ND *
funcode(P1ND *p)
{
	return p;
}

/*
 * Builtins support
 */
P1ND *
builtin_return_address(const struct bitable *bt, P1ND *a)
{
	int nframes;

	if (a->n_op != ICON)
		goto bad;
	nframes = (int)glval(a);

	p1nfree(a);

	if (nframes > 0)
		goto bad;

	/* Return address is at 8(sp) */
	a = block(REG, NULL, NULL, PTR+VOID, 0, 0);
	regno(a) = STKREG;
	a = block(PLUS, a, bcon(8), PTR+VOID, 0, 0);
	a = block(UMUL, a, NULL, PTR+VOID, 0, 0);

	return a;

bad:
	uerror("bad argument to __builtin_return_address");
	return bcon(0);
}

P1ND *
builtin_frame_address(const struct bitable *bt, P1ND *a)
{
	int nframes;

	if (a->n_op != ICON)
		goto bad;
	nframes = (int)glval(a);

	p1nfree(a);

	if (nframes > 0)
		goto bad;

	/* Return frame pointer */
	a = block(REG, NULL, NULL, PTR+VOID, 0, 0);
	regno(a) = FPREG;

	return a;

bad:
	uerror("bad argument to __builtin_frame_address");
	return bcon(0);
}

P1ND *
builtin_cfa(const struct bitable *bt, P1ND *a)
{
	/* Canonical frame address - same as frame pointer for CHILL */
	return builtin_frame_address(bt, a);
}
