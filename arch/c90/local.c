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

/*
 * Local optimizations for C90 backend
 */

NODE *
clocal(NODE *p)
{
	/* Minimal local optimizations */
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
