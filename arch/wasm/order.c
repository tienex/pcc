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
#include <string.h>

/*
 * Should the assignment op be stored,
 * given that it lies as the right operand to an operator of type o?
 */
int
stoasg(NODE *p, int o)
{
	/* WebAssembly has simple stack-based evaluation, no special cases */
	return 0;
}

/*
 * Should this operation be stored,
 * given that it lies as the right operand to an operator of type o?
 */
int
storeop(int o, int r)
{
	/* WebAssembly stack machine handles this naturally */
	return 0;
}

/*
 * Return 1 if it is a good idea to make an OREG out of p
 */
int
oregok(NODE *p, int sharp)
{
	/* WebAssembly supports OREG for memory operations */
	return 1;
}

/*
 * Should the assignment op p be expanded into two ops
 */
int
setasg(NODE *p, int cookie)
{
	return 0;
}

/*
 * Should ops be printed out immediately?
 */
int
setorder(NODE *p)
{
	/* WebAssembly instructions are in order */
	return 0;
}

/*
 * Is it legal to make a OREG or NAME node that has an
 * offset of off, given the register r?
 */
int
notoff(TWORD t, int r, CONSZ off, char *cp)
{
	/* WebAssembly memory access can have any offset */
	return 0;
}

/*
 * Turn a UMUL node into an OREG node
 */
void
offstar(NODE *p, int shape)
{
	if (isreg(p))
		return;

	if (p->n_op == PLUS || p->n_op == MINUS) {
		if (p->n_right->n_op == ICON) {
			if (isreg(p->n_left)) {
				NODE *l = p->n_left;
				p->n_op = OREG;
				p->n_lval = p->n_right->n_lval;
				if (p->n_op == MINUS)
					p->n_lval = -p->n_lval;
				p->n_rval = l->n_rval;
				nfree(p->n_right);
				nfree(l);
				return;
			}
		}
	}

	comperr("offstar: cannot convert to OREG");
}

/*
 * Do the actual conversion of offstar-found OREGs into real OREGs
 */
void
myormake(NODE *p)
{
}

/*
 * Check if a constant is suitable for a specific operation
 */
int
shumul(NODE *p, int shape)
{
	/* WebAssembly can handle any constant */
	return 0;
}

/*
 * Special handling for function arguments
 */
void
setregs(void)
{
	/* WebAssembly handles arguments via function parameters */
}

/*
 * Check if constant value is acceptable for this operation
 */
int
acceptable(struct optab *op)
{
	return 1;
}
