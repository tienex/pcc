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

#include "pass2.h"
#include <ctype.h>
#include <string.h>

void acon(NODE *p);
int argsize(NODE *p);

static int maxargsz;

const char *rnames[] = {
	/* Integer registers */
	"$r0", "$r1", "$r2", "$r3", "$r4", "$r5", "$r6", "$r7",
	/* Float registers */
	"$f0", "$f1", "$f2", "$f3", "$f4", "$f5", "$f6", "$f7",
};

void
deflab(int label)
{
	printf(".L%d:\n", label);
}

/*
 * Print function prologue
 */
void
prologue(struct interpass_prolog *ipp)
{
	int i, nlocals = 0;

	maxargsz = 0;

	/* Function already started in defloc(), just declare locals */
	printf("    ;; locals\n");

	/* Calculate number of local variables needed */
	nlocals = p2maxautooff / (SZINT/SZCHAR);

	for (i = 0; i < nlocals; i++) {
		printf("    (local $local%d i32)\n", i);
	}
}

/*
 * Print function epilogue
 */
void
eoftn(struct interpass_prolog *ipp)
{
	if (ipp->ipp_ip.ip_lbl == 0)
		return;

	/* Function end is printed by efcode() in code.c */
}

/*
 * Print opcode for binary operations
 */
void
hopcode(int f, int o)
{
	char *str = NULL;

	switch (o) {
	case PLUS:
		str = "add";
		break;
	case MINUS:
		str = "sub";
		break;
	case AND:
		str = "and";
		break;
	case OR:
		str = "or";
		break;
	case ER:
		str = "xor";
		break;
	default:
		comperr("hopcode: %d", o);
		str = "unknown";
	}
	printf("%s", str);
}

/*
 * Return type size in bytes.
 */
int
tlen(NODE *p)
{
	switch (p->n_type) {
	case CHAR:
	case UCHAR:
		return 1;
	case SHORT:
	case USHORT:
		return SZSHORT/SZCHAR;
	case DOUBLE:
	case LDOUBLE:
		return SZDOUBLE/SZCHAR;
	case INT:
	case UNSIGNED:
	case LONG:
	case ULONG:
	default:
		return SZINT/SZCHAR;
	}
}

/*
 * Assign to a register
 */
void
zzzcode(NODE *p, int c)
{
	/* Handle special cases in assembly output */
	switch (c) {
	default:
		comperr("zzzcode: unknown code %c", c);
	}
}

/*
 * Print out a constant node
 */
void
adrcon(CONSZ val)
{
	printf("%lld", val);
}

/*
 * Print out a name/address
 */
void
conput(FILE *fp, NODE *p)
{
	int off = p->n_lval;
	char *n = p->n_name;

	switch (p->n_op) {
	case ICON:
		if (n && n[0] != '\0') {
			fprintf(fp, "%s", n);
			if (off)
				fprintf(fp, "+%d", off);
		} else
			fprintf(fp, "%d", off);
		return;

	default:
		comperr("illegal conput: %p", p);
	}
}

/*
 * Print out an address (name)
 */
void
insput(NODE *p)
{
	comperr("insput");
}

/*
 * Write out the upper address, like the upper register of a 2-register
 * reference, or the next memory location.
 */
void
upput(NODE *p, int size)
{
	size /= SZCHAR;
	switch (p->n_op) {
	case REG:
		fprintf(stdout, "%s", rnames[p->n_rval + 1]);
		break;

	case ICON:
	case NAME:
		p->n_lval += size;
		adrput(stdout, p);
		p->n_lval -= size;
		break;

	case OREG:
		p->n_lval += size;
		adrput(stdout, p);
		p->n_lval -= size;
		break;

	default:
		comperr("upput: bad op %d", p->n_op);
	}
}

/*
 * Print out an address, suitable for use in an instruction
 */
void
adrput(FILE *io, NODE *p)
{
	switch (p->n_op) {
	case REG:
		fprintf(io, "%s", rnames[p->n_rval]);
		break;

	case ICON:
	case NAME:
		if (p->n_name && p->n_name[0]) {
			fputs(p->n_name, io);
			if (p->n_lval != 0)
				fprintf(io, "+%lld", p->n_lval);
		} else
			fprintf(io, "%lld", p->n_lval);
		break;

	case OREG:
		fprintf(io, "%lld(%s)", p->n_lval, rnames[p->n_rval]);
		break;

	default:
		comperr("adrput: illegal address op %d", p->n_op);
		return;
	}
}

/*
 * Print out a string (for output)
 */
void
cbgen(int op, int lab)
{
	if (op == 0)
		printf("	br .L%d\n", lab);
	else
		comperr("cbgen: op %d", op);
}

/*
 * Perform any target-specific conversions on OREG nodes
 */
void
myreader(struct interpass *ipole)
{
}

/*
 * Remove some passes that are not needed for this target
 */
void
mycanon(NODE *p)
{
}

/*
 * Called just before register allocation
 */
void
myoptim(struct interpass *ip)
{
}

/*
 * Move between registers
 */
void
rmove(int s, int d, TWORD t)
{
	switch (t) {
	case FLOAT:
	case DOUBLE:
	case LDOUBLE:
		printf("	local.get %s\n", rnames[s]);
		printf("	local.set %s\n", rnames[d]);
		break;
	default:
		printf("	local.get %s\n", rnames[s]);
		printf("	local.set %s\n", rnames[d]);
		break;
	}
}

/*
 * Number of scratch registers to reserve for special purposes
 */
int
COLORMAP(int c, int *r)
{
	/* All registers are available */
	return c < 8 ? c : c - 8;
}
