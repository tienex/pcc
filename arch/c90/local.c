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

#include "pass1.h"
#include <string.h>

/*
 * Memory model selection (default: 32-bit)
 */
int mcc90model = MCC90_32BIT;

/*
 * Local optimizations for C90 backend
 */

NODE *
clocal(NODE *p)
{
	struct symtab *sp;
	NODE *l, *r;
	int o = p->n_op;
	TWORD t = p->n_type;

	switch (o) {
	case NAME:
		/* Handle special variable names */
		if ((sp = p->n_sp) != NULL) {
			/* Preserve symbol attributes */
		}
		break;

	case CALL:
	case UCALL:
		/* Map builtin functions to C90 equivalents */
		l = p->n_left;
		if (l->n_op == NAME && l->n_sp != NULL) {
			char *name = l->n_sp->sname;

			/* Map __builtin_* functions */
			if (strncmp(name, "__builtin_", 10) == 0) {
				/* These will be handled in code generation */
			}
		}
		break;

	case STASG:
		/* Structure assignment - ensure proper handling */
		break;
	}

	return p;
}

void
myp2tree(NODE *p)
{
	/* Convert tree to pass2 form */
}

int
andable(NODE *p)
{
	/* All C variables are addressable */
	return 1;
}

void
cendarg(void)
{
	/* End of argument processing */
}

int
cisreg(TWORD t)
{
	/* All basic types can be in registers (C variables) */
	return 1;
}

void
mygenswitch(int num, TWORD type, struct swents **p, int n)
{
	/* Generate switch statement in C */
	int i;

	printf("\tswitch (");
	/* switch expression already evaluated */
	printf(") {\n");

	for (i = 0; i < n; i++) {
		printf("\tcase %lld: goto L%d;\n",
		    (long long)p[i]->sval, p[i]->slab);
	}

	printf("\tdefault: goto L%d;\n", p[n-1]->slab);
	printf("\t}\n");
}

NODE *
offcon(OFFSZ off, TWORD t, union dimfun *d, struct attr *ap)
{
	/* Create constant offset node */
	NODE *p = block(ICON, NIL, NIL, t, d, ap);
	setlval(p, off);
	return p;
}

void
ninval(CONSZ off, int size, NODE *p)
{
	/* Initialize data */
	printf("\t/* init: offset=%lld size=%d */\n", (long long)off, size);
}

int
fldal(unsigned int t)
{
	/* Field alignment */
	return ALINT;
}

void
mflags(char *str)
{
	/* Process machine-specific flags */

	/* Memory model selection */
	if (strncmp(str, "-mcmodel=", 9) == 0) {
		char *model = str + 9;

		if (strcmp(model, "16bit") == 0 || strcmp(model, "small16") == 0) {
			mcc90model = MCC90_16BIT;
		} else if (strcmp(model, "32bit") == 0 || strcmp(model, "small") == 0) {
			mcc90model = MCC90_32BIT;
		} else if (strcmp(model, "64bit") == 0 || strcmp(model, "medium") == 0) {
			mcc90model = MCC90_64BIT;
		} else if (strcmp(model, "segmented") == 0) {
			mcc90model = MCC90_SEGMENTED;
		} else {
			werror("unknown memory model: %s", model);
		}
	}

	/* C dialect selection */
	if (strcmp(str, "-std=c90") == 0 || strcmp(str, "-std=c89") == 0) {
		/* Strict C90 mode - already default */
	} else if (strcmp(str, "-std=c99") == 0) {
		/* Enable C99 features in output */
		/* TODO: add C99 mode flag */
	}
}

int
pragma_allpacked(void)
{
	return 0;
}

void
fixdef(struct symtab *sp)
{
	/* Fix up definitions */
}

void
pass1_lastchance(struct symtab *sp)
{
	/* Last chance for pass1 modifications */
}
