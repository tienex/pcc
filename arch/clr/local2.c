/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC CLR Backend Contributors.
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

/*
 * CLR backend - pass 2 optimizations and stack management
 */

#include "pass2.h"
#include <ctype.h>
#include <string.h>

/* CLR doesn't need external name prefix (it's in namespace) */
#define EXPREFIX	""

static int max_stack_depth = 0;
static int current_stack_depth = 0;
static TWORD ftype;

/* Register names for CLR "virtual stack" */
char *rnames[] = {
	/* Evaluation stack positions */
	"eval0", "eval1", "eval2", "eval3",
	"eval4", "eval5", "eval6", "eval7",
	/* Long long pairs */
	"eval01", "eval23", "eval45", "eval67",
	/* Floating-point stack */
	"fp0", "fp1", "fp2", "fp3",
};

/*
 * Print out a label.
 */
void
deflab(int label)
{
	printf(LABFMT ":\n", label);
}

/*
 * Calculate "register" (stack slot) usage and local variable offsets.
 * For CLR, this means tracking stack depth and locals.
 */
static int
offcalc(struct interpass_prolog *ipp)
{
	int local_size = p2maxautooff;

	/* Convert from bits to bytes */
	if (local_size >= AUTOINIT/SZCHAR)
		local_size -= AUTOINIT/SZCHAR;

	return local_size;
}

/*
 * Emit function prologue.
 * For CLR, this is handled by bfcode in code.c.
 * This function just calculates metadata.
 */
void
prologue(struct interpass_prolog *ipp)
{
	ftype = ipp->ipp_type;

	/* Calculate local variable space needed */
	offcalc(ipp);

	/* Stack depth tracking is handled by instruction selection */
	max_stack_depth = 8;  /* Conservative default */
	current_stack_depth = 0;
}

/*
 * End of function.
 * For CLR, this is handled by efcode in code.c.
 */
void
eoftn(struct interpass_prolog *ipp)
{
	if (ipp->ipp_ip.ip_lbl == 0)
		return; /* no code needs to be generated */

	/* Nothing special needed - CLR manages stack automatically */
}

/*
 * Return the register class for a given type.
 */
int
COLORMAP(int c, int *r)
{
	int num = 0;

	switch (c) {
	case CLASSA:  /* Integer/pointer registers */
		num = 8;  /* EVAL_0 through EVAL_7 */
		for (int i = 0; i < num; i++)
			r[i] = i;
		break;

	case CLASSC:  /* Long long registers */
		num = 4;  /* EVAL_01, EVAL_23, EVAL_45, EVAL_67 */
		for (int i = 0; i < num; i++)
			r[i] = 8 + i;
		break;

	case CLASSD:  /* Floating-point registers */
		num = 4;  /* FP_0 through FP_3 */
		for (int i = 0; i < num; i++)
			r[i] = 12 + i;
		break;
	}
	return num;
}

/*
 * Convert register number to register class.
 */
char *
clr_rclass_name(int regno)
{
	if (regno < 8)
		return "int32";
	else if (regno < 12)
		return "int64";
	else
		return "float64";
}

/*
 * Emit move from one register to another.
 * For CLR stack machine, this might involve dup or local stores.
 */
void
rmove(int s, int d, TWORD t)
{
	/* If source and dest are the same, nothing to do */
	if (s == d)
		return;

	/* In CLR, "moving" between stack positions means:
	 * - Store source to a local
	 * - Load from that local to destination
	 * Or simply duplicate if positions are adjacent
	 */

	printf("\t\t\t// move %s to %s\n", rnames[s], rnames[d]);

	/* For now, emit as comment - actual implementation depends on context */
}

/*
 * Check if two registers overlap.
 */
int
roverlapcheck(int r1, int r2)
{
	/* Long long registers overlap with their component halves */
	if (r1 == EVAL_01 && (r2 == EVAL_0 || r2 == EVAL_1))
		return 1;
	if (r1 == EVAL_23 && (r2 == EVAL_2 || r2 == EVAL_3))
		return 1;
	if (r1 == EVAL_45 && (r2 == EVAL_4 || r2 == EVAL_5))
		return 1;
	if (r1 == EVAL_67 && (r2 == EVAL_6 || r2 == EVAL_7))
		return 1;

	/* Check reverse */
	if (r2 == EVAL_01 && (r1 == EVAL_0 || r1 == EVAL_1))
		return 1;
	if (r2 == EVAL_23 && (r1 == EVAL_2 || r1 == EVAL_3))
		return 1;
	if (r2 == EVAL_45 && (r1 == EVAL_4 || r1 == EVAL_5))
		return 1;
	if (r2 == EVAL_67 && (r1 == EVAL_6 || r1 == EVAL_7))
		return 1;

	return 0;
}

/*
 * Convert addressing mode to CLR IL.
 */
void
adrput(FILE *io, NODE *p)
{
	int r;
	char *s;

	switch (p->n_op) {
	case NAME:
		/* Global variable */
		if (p->n_name && p->n_name[0] != '\0')
			printf("%s", p->n_name);
		else
			printf("_G%d", (int)p->n_lval);
		break;

	case OREG:
		/* Local variable or parameter */
		r = p->n_rval;
		if (r == FPREG) {
			/* Local variable */
			printf("loc_%d", (int)(p->n_lval / (SZINT/SZCHAR)));
		} else {
			/* Parameter */
			printf("arg_%d", r);
		}
		break;

	case REG:
		/* Register (evaluation stack position) */
		r = p->n_rval;
		printf("%s", rnames[r]);
		break;

	case ICON:
		/* Integer constant */
		if (p->n_name && p->n_name[0] != '\0')
			printf("%s", p->n_name);
		else
			printf("%lld", (long long)p->n_lval);
		break;

	default:
		comperr("adrput: bad op %d", p->n_op);
	}
}

/*
 * Emit special addressing modes for CLR IL.
 */
void
zzzcode(NODE *p, int c)
{
	NODE *l, *r;
	int pr;

	switch (c) {
	case 'A':  /* Emit left address */
		adrput(stdout, p->n_left);
		break;

	case 'B':  /* Emit right address */
		adrput(stdout, p->n_right);
		break;

	case 'C':  /* Emit constant */
		printf("%lld", (long long)p->n_lval);
		break;

	case 'D':  /* Emit double constant */
		printf("%lld", (long long)p->n_lval);
		break;

	case 'E':  /* Emit element type for arrays */
		/* Determine element type from pointer */
		l = p->n_left;
		if (l && ISPTR(l->n_type)) {
			TWORD base = DECREF(l->n_type);
			printf("%s", clr_type_name(base, NULL, NULL));
		} else {
			printf("int32");
		}
		break;

	case 'F':  /* Emit field name */
		if (p->n_name)
			printf("%s", p->n_name);
		break;

	case 'G':  /* Emit global field reference */
		printf("PCC.GeneratedCode::%s", p->n_name ? p->n_name : "_global");
		break;

	case 'L':  /* Emit label */
		printf(LABFMT, (int)p->n_lval);
		break;

	case 'M':  /* Emit method call */
		adrput(stdout, p->n_left);
		break;

	case 'N':  /* Negate constant */
		printf("%lld", -(long long)p->n_lval);
		break;

	case 'O':  /* Emit offset */
		printf("%lld", (long long)p->n_lval);
		break;

	case 'P':  /* Push constant onto evaluation stack */
		printf("\t\t\tldc.i4\t");
		adrput(stdout, p);
		printf("\n");
		current_stack_depth++;
		if (current_stack_depth > max_stack_depth)
			max_stack_depth = current_stack_depth;
		break;

	case 'Q':  /* Pop from evaluation stack */
		printf("\t\t\tpop\n");
		if (current_stack_depth > 0)
			current_stack_depth--;
		break;

	case 'R':  /* Emit return type */
		printf("%s", clr_type_name(p->n_type, NULL, NULL));
		break;

	case 'S':  /* Emit store to local */
		printf("\t\t\tstloc.s\t");
		adrput(stdout, p);
		printf("\n");
		if (current_stack_depth > 0)
			current_stack_depth--;
		break;

	case 'T':  /* Emit type name */
		printf("%s", clr_type_name(p->n_type, NULL, NULL));
		break;

	case 'U':  /* Emit load from local */
		printf("\t\t\tldloc.s\t");
		adrput(stdout, p);
		printf("\n");
		current_stack_depth++;
		if (current_stack_depth > max_stack_depth)
			max_stack_depth = current_stack_depth;
		break;

	case 'V':  /* Emit load address */
		printf("\t\t\tldloca.s\t");
		adrput(stdout, p);
		printf("\n");
		current_stack_depth++;
		if (current_stack_depth > max_stack_depth)
			max_stack_depth = current_stack_depth;
		break;

	case 'W':  /* Emit array length */
		printf("\t\t\tldlen\n");
		break;

	case 'X':  /* Emit box instruction */
		printf("\t\t\tbox\t%s\n", clr_type_name(p->n_type, NULL, NULL));
		break;

	case 'Y':  /* Emit unbox instruction */
		printf("\t\t\tunbox.any\t%s\n", clr_type_name(p->n_type, NULL, NULL));
		break;

	case 'Z':  /* Emit duplicate top of stack */
		printf("\t\t\tdup\n");
		current_stack_depth++;
		if (current_stack_depth > max_stack_depth)
			max_stack_depth = current_stack_depth;
		break;

	default:
		comperr("zzzcode: unknown code %c", c);
	}
}

/*
 * Generate function call sequence.
 */
void
gencall(NODE *p, NODE *prevp)
{
	/* Emit call instruction */
	/* Arguments are already on stack from argument evaluation */

	printf("\t\t\tcall\t");

	/* Determine call target */
	NODE *l = p->n_left;
	if (l->n_op == ICON && l->n_name) {
		/* Direct call */
		printf("%s ", clr_type_name(p->n_type, NULL, NULL));
		printf("PCC.GeneratedCode::%s(", l->n_name);

		/* TODO: Emit parameter types */

		printf(")\n");
	} else {
		/* Indirect call via function pointer/delegate */
		printf("%s ", clr_type_name(p->n_type, NULL, NULL));
		printf("*( ");
		adrput(stdout, l);
		printf(" )\n");
	}

	/* Adjust stack depth */
	/* Arguments are popped, return value pushed (if not void) */
	if (p->n_type != VOID) {
		current_stack_depth++;
		if (current_stack_depth > max_stack_depth)
			max_stack_depth = current_stack_depth;
	}
}

/*
 * Assign to a location.
 */
void
conput(FILE *fp, NODE *p)
{
	switch (p->n_op) {
	case ICON:
		if (p->n_name && p->n_name[0])
			fprintf(fp, "%s", p->n_name);
		else
			fprintf(fp, "%lld", (long long)p->n_lval);
		return;

	default:
		comperr("conput: bad op %d", p->n_op);
	}
}

/*
 * Print out the epilogue.
 */
void
myexit(int i)
{
	printf("\n");
}

/*
 * Print out a constant node.
 */
void
myinit(FILE *fp, NODE *p)
{
	/* Emit initialization value */
	conput(fp, p);
}

/*
 * Print a long long constant.
 */
void
ninval(CONSZ off, int fsz, NODE *p)
{
	printf("\t\t\tldc.i8\t%lld\n", (long long)p->n_lval);
}

/*
 * Give target the opportunity to handle pragmas.
 */
void
mypragma(char *str)
{
	/* Could implement #pragma directives for CLR-specific features */
}

/*
 * Rewrite operations before code generation.
 */
void
myoptim(struct interpass *ip)
{
	/* Perform target-specific optimizations */
}

/*
 * Mark variables as "used" to prevent optimization removal.
 */
void
myused(struct symtab *sp)
{
	/* Mark symbol as used */
}

/*
 * Determine if a symbol needs GOT handling (not applicable for CLR).
 */
int
mypcc_gotref(struct symtab *sp)
{
	return 0;  /* CLR doesn't use GOT */
}

/*
 * Check if initializer is all zeros.
 */
int
zeroinit(struct symtab *sp)
{
	/* Check if initial value is zero */
	return 0;  /* Default: not zero */
}
