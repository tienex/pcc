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
 * WebAssembly handles alignment automatically
 */
void
defalign(int n)
{
	/* WebAssembly memory alignment is handled by load/store instructions */
}

/*
 * Print out assembler segment name.
 * WebAssembly uses modules with different sections
 */
void
setseg(int seg, char *name)
{
	switch (seg) {
	case PROG:
		/* Functions are defined with (func ...) in WAT */
		break;
	case DATA:
	case LDATA:
		printf("  ;; .data section\n");
		break;
	case STRNG:
	case RDATA:
		printf("  ;; .rodata section\n");
		break;
	case UDATA:
		printf("  ;; .bss section\n");
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
		printf("  (func $%s", name);
		if (sp->sclass == EXTDEF)
			printf(" (export \"%s\")", name);
		return;
	}

	if (sp->sclass == EXTDEF)
		printf("  ;; .globl %s\n", name);

	if (sp->slevel == 0)
		printf("  ;; %s:\n", name);
	else
		printf("  ;; " LABFMT ":\n", sp->soffset);
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
			printf("  ;; .local %s\n", name);
		} else
			printf("  ;; .local " LABFMT "\n", sp->soffset);
	}

	/* WebAssembly uses data section for initialized data */
	if (sp->slevel == 0) {
		printf("  (data $%s (i32.const 0) \"", name);
		/* Zero-filled data */
		for (int i = 0; i < off; i++)
			printf("\\00");
		printf("\")\n");
	}
}

/*
 * code for the end of a function
 */
void
efcode(void)
{
	P1ND *p, *q;

	if (cftnsp->stype != STRTY+FTN && cftnsp->stype != UNIONTY+FTN) {
		printf("  ) ;; end func\n");
		return;
	}

	/* Handle struct return - similar to other architectures */
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

	printf("  ) ;; end func\n");
}

/*
 * code for the beginning of a function
 */
void
bfcode(struct symtab **a, int n)
{
	int i;

	/* Print function parameters */
	for (i = 0; i < n; i++) {
		printf(" (param $arg%d ", i);

		/* Map types to WebAssembly types */
		switch (a[i]->stype) {
		case CHAR:
		case UCHAR:
		case SHORT:
		case USHORT:
		case INT:
		case UNSIGNED:
			printf("i32");
			break;
		case LONG:
		case ULONG:
		case LONGLONG:
		case ULONGLONG:
			printf("i64");
			break;
		case FLOAT:
			printf("f32");
			break;
		case DOUBLE:
		case LDOUBLE:
			printf("f64");
			break;
		default:
			if (ISPTR(a[i]->stype))
				printf("i32"); /* pointers are i32 in wasm32 */
			else
				printf("i32"); /* default to i32 */
			break;
		}
		printf(")");
	}

	/* Print return type */
	printf(" (result ");
	if (cftnsp->stype == VOID+FTN) {
		/* No return type for void functions - actually, omit (result) */
		printf("i32"); /* placeholder, will be handled properly in full implementation */
	} else if ((cftnsp->stype & ~BTMASK) == (FTN << BTSHIFT)) {
		TWORD t = BTYPE(cftnsp->stype);
		switch (t) {
		case CHAR:
		case UCHAR:
		case SHORT:
		case USHORT:
		case INT:
		case UNSIGNED:
			printf("i32");
			break;
		case LONG:
		case ULONG:
		case LONGLONG:
		case ULONGLONG:
			printf("i64");
			break;
		case FLOAT:
			printf("f32");
			break;
		case DOUBLE:
		case LDOUBLE:
			printf("f64");
			break;
		default:
			printf("i32");
			break;
		}
	} else {
		printf("i32");
	}
	printf(")\n");

	if (cftnsp->stype != STRTY+FTN && cftnsp->stype != UNIONTY+FTN)
		return;

	/* Function returns struct, adjust arg offset */
	for (i = 0; i < n; i++)
		a[i]->soffset += SZPOINT(INT);
}

/* called just before final exit */
void
ejobcode(int flag)
{
	printf(") ;; end module\n");
}

void
bjobcode(void)
{
	printf("(module\n");
	printf("  (memory (export \"memory\") 1)\n");
}

/* fix up type of field p */
void
fldty(struct symtab *p)
{
}

/*
 * XXX - fix genswitch.
 */
int
mygenswitch(int num, TWORD type, struct swents **p, int n)
{
	return 0;
}

/*
 * Called with a function call with arguments as argument.
 * This is done early in buildtree() and only done once.
 */
P1ND *
funcode(P1ND *p)
{
	P1ND *r, *l;

	/* Fix function call arguments */
	for (r = p->n_right; r->n_op == CM; r = r->n_left) {
		if (r->n_right->n_op != STARG) {
			r->n_right = block(FUNARG, r->n_right, NULL,
			    r->n_right->n_type, r->n_right->n_df,
			    r->n_right->n_ap);
		}
	}
	if (r->n_op != STARG) {
		l = p1alloc();
		*l = *r;
		r->n_op = FUNARG;
		r->n_left = l;
		r->n_type = l->n_type;
	}

	return p;
}

/*
 * Return return address as given by a.
 */
P1ND *
builtin_return_address(const struct bitable *bt, P1ND *a)
{
	cerror((char *)__func__);
	return 0;
}

/*
 * Return frame as given by a.
 */
P1ND *
builtin_frame_address(const struct bitable *bt, P1ND *a)
{
	cerror((char *)__func__);
	return 0;
}

/*
 * Return "canonical frame address".
 */
P1ND *
builtin_cfa(const struct bitable *bt, P1ND *a)
{
	cerror((char *)__func__);
	return 0;
}
