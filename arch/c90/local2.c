/*	$Id$	*/
/*
 * Copyright (c) 2025 C90 Backend Generator
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

/* Register names for C90 backend (map to C variables) */
char *rnames[] = {
	"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
	"r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
	"f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",
};

/*
 * Print a label
 */
void
deflab(int label)
{
	printf("L%d:\n", label);
}

/*
 * Generate function prologue
 */
void
prologue(struct interpass_prolog *ipp)
{
	char *name = ipp->ipp_name;
	int i;

	/* Emit function signature */
	printf("\n");
	if (ipp->ipp_vis)
		printf("/* extern */ ");

	printf("void %s(", name);

	/* For simplicity, use void args - full implementation would
	 * need to track actual parameter types */
	printf("void)\n");
	printf("{\n");

	/* Declare local register variables */
	printf("\t/* Register variables */\n");
	for (i = 0; i < MAXREGS; i++) {
		if (TESTBIT(p2env.p_regs, i)) {
			if (i < 16)
				printf("\tlong %s;\n", rnames[i]);
			else
				printf("\tdouble %s;\n", rnames[i]);
		}
	}
	printf("\n");
}

/*
 * Generate function epilogue
 */
void
eoftn(struct interpass_prolog *ipp)
{
	printf("}\n\n");
}

/*
 * Convert opcode to C operator string
 */
static char *
opcode_to_c(int op)
{
	switch (op) {
	case PLUS: return "+";
	case MINUS: return "-";
	case MUL: return "*";
	case DIV: return "/";
	case MOD: return "%";
	case AND: return "&";
	case OR: return "|";
	case ER: return "^";
	case LS: return "<<";
	case RS: return ">>";
	case EQ: return "==";
	case NE: return "!=";
	case LT: return "<";
	case LE: return "<=";
	case GT: return ">";
	case GE: return ">=";
	default: return "?";
	}
}

/*
 * Print an address/operand
 */
void
adrput(FILE *io, NODE *p)
{
	int r;
	char *name;

	switch (p->n_op) {
	case REG:
		r = p->n_rval;
		if (r >= 0 && r < MAXREGS)
			fprintf(io, "%s", rnames[r]);
		else
			fprintf(io, "r_BAD_%d", r);
		break;

	case ICON:
		if (p->n_name && p->n_name[0]) {
			fprintf(io, "%s", p->n_name);
			if (getlval(p) != 0)
				fprintf(io, "+%lld", (long long)getlval(p));
		} else {
			fprintf(io, "%lld", (long long)getlval(p));
		}
		break;

	case OREG:
		r = p->n_rval;
		if (getlval(p) != 0)
			fprintf(io, "*(%s + %lld)", rnames[r], (long long)getlval(p));
		else
			fprintf(io, "*%s", rnames[r]);
		break;

	case NAME:
		name = p->n_name;
		if (name && name[0])
			fprintf(io, "%s", name);
		else
			fprintf(io, "NAME_%lld", (long long)getlval(p));
		break;

	default:
		fprintf(io, "OP%d", p->n_op);
		break;
	}
}

/*
 * Generate comparison code
 */
void
cbgen(int op, int label)
{
	printf("\tif (CC %s 0) goto L%d;\n", opcode_to_c(op), label);
}

/*
 * Emit a string constant
 */
void
instring(struct symtab *sp)
{
	char *s = sp->sname;
	printf("static char str_%d[] = \"", (int)sp->soffset);
	for (; *s; s++) {
		if (*s == '"' || *s == '\\')
			printf("\\");
		printf("%c", *s);
	}
	printf("\";\n");
}

/*
 * Print out a constant
 */
void
ninval(CONSZ off, int fsz, NODE *p)
{
	/* Emit initialization value */
	printf("\t/* init */\n");
}

/*
 * Print a common declaration
 */
void
commdec(struct symtab *sp)
{
	char *name = getexname(sp);
	printf("/* common */ int %s;\n", name);
}

/*
 * Print a local declaration
 */
void
lcommdec(struct symtab *sp)
{
	char *name = getexname(sp);
	printf("static int %s;\n", name);
}

/*
 * Called before generating code for a function
 */
void
myreader(struct interpass *ip)
{
	/* Nothing needed */
}

/*
 * Remove redundant operations
 */
int
findops(NODE *p, int cookie)
{
	return 0;
}

/*
 * Generate special code for a node
 */
void
zzzcode(NODE *p, int c)
{
	/* Handle special code generation directives */
}

/*
 * Rewrite operations for optimal C code generation
 */
int
rewfld(NODE *p)
{
	return 1;
}

/*
 * Called after register allocation
 */
void
rmove(int s, int d, TWORD t)
{
	printf("\t%s = %s; /* move */\n", rnames[d], rnames[s]);
}

/*
 * Variable used for return value
 */
int
retreg(int t)
{
	return R0;  /* Use r0 for return values */
}

/*
 * Branch to a label
 */
void
cbranch(NODE *p, int lbl)
{
	printf("\tif (");
	adrput(stdout, p);
	printf(") goto L%d;\n", lbl);
}

/*
 * Unconditional jump
 */
void
branch(int lbl)
{
	printf("\tgoto L%d;\n", lbl);
}

/*
 * Evaluate an expression
 */
void
mygenregs(struct interpass *ip)
{
	/* Register allocation is simplified for C90 */
}

/*
 * Can this subtree be accessed as an address?
 */
int
fldexpand(NODE *p, int cookie, char **cp)
{
	return 0;
}

/*
 * Special handling for specific architectures
 */
void
lastcall(NODE *p)
{
	/* Nothing special needed for C90 */
}

/*
 * Setup for structure return
 */
NODE *
builtin_return_address(NODE *f, NODE *a, TWORD t)
{
	return block(ICON, NIL, NIL, t, 0, 0);
}

/*
 * Setup for frame address
 */
NODE *
builtin_frame_address(NODE *f, NODE *a, TWORD t)
{
	return block(ICON, NIL, NIL, t, 0, 0);
}
