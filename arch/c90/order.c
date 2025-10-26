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
#include <string.h>

/*
 * Check if offset is legal for this type and register
 */
int
notoff(TWORD t, int r, CONSZ off, char *cp)
{
	return 0;  /* All offsets are acceptable in C */
}

/*
 * Turn a UMUL-referenced node into OREG
 */
void
offstar(NODE *p, int shape)
{
	if (isreg(p))
		return;

	(void)geninsn(p, INAREG);
}

/*
 * Do the actual conversion of offstar-found OREGs
 */
void
myormake(NODE *q)
{
	/* Simple implementation for C90 */
}

/*
 * Shape matches for UMUL
 */
int
shumul(NODE *p, int shape)
{
	if (shape & SOREG)
		return SROREG;
	return SRNOPE;
}

/*
 * Rewrite operations on binary operators
 */
int
setbin(NODE *p)
{
	return 0;
}

/*
 * Setup for assignment operator
 */
int
setasg(NODE *p, int cookie)
{
	return 0;
}

/*
 * Setup for unary operator
 */
int
setuni(NODE *p, int cookie)
{
	return 0;
}

/*
 * Set evaluation order of a binary node
 */
int
setorder(NODE *p)
{
	return 0;
}

/*
 * Set registers in calling conventions live
 */
int *
livecall(NODE *p)
{
	static int r[] = { -1 };
	return r;
}

/*
 * Signal whether the instruction is acceptable for this target
 */
int
acceptable(struct optab *op)
{
	return 1;
}
