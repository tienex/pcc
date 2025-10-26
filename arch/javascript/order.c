/*	$Id$	*/
/*
 * Copyright (c) 2025 JavaScript Backend
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

/*
 * JavaScript instruction ordering and scheduling decisions
 * JavaScript evaluation is left-to-right, so ordering is straightforward
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
	/*
	 * JavaScript evaluates expressions left-to-right
	 * Assignment operators can be used in expressions
	 * Generally don't need to force storage
	 */
	return 0;
}

/*
 * Should this operation be stored,
 * given that it lies as the right operand to an operator of type o?
 */
int
storeop(int o, int r)
{
	/*
	 * JavaScript handles operator precedence correctly
	 * No special storage requirements
	 */
	return 0;
}

/*
 * Return 1 if it is a good idea to make an OREG out of p
 */
int
oregok(NODE *p, int sharp)
{
	/*
	 * JavaScript supports property access and array indexing
	 * OREG can be used for:
	 * - Array access: array[index]
	 * - Object property: obj.prop or obj["prop"]
	 * - Offset access: base + offset
	 */

	/* Always allow OREG for JavaScript */
	return 1;
}

/*
 * Should the assignment op p be expanded into two ops
 */
int
setasg(NODE *p, int cookie)
{
	/*
	 * JavaScript supports compound assignment operators
	 * (+=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=, >>>=)
	 * Usually don't need to expand
	 */
	return 0;
}

/*
 * Should ops be printed out immediately?
 */
int
setorder(NODE *p)
{
	/*
	 * JavaScript expressions are evaluated left-to-right
	 * No need to reorder operations
	 */
	return 0;
}

/*
 * Is it legal to make a OREG or NAME node that has an
 * offset of off, given the register r?
 */
int
notoff(TWORD t, int r, CONSZ off, char *cp)
{
	/*
	 * JavaScript can handle any offset in array access
	 * and object property access
	 * Always return 0 (legal)
	 */
	return 0;
}

/*
 * Turn a UMUL node into an OREG node
 */
void
offstar(NODE *p, int shape)
{
	NODE *l;

	if (isreg(p))
		return;

	l = p->n_left;

	/*
	 * For JavaScript, UMUL (dereference) becomes:
	 * - Array access if base+offset: array[offset]
	 * - Property access if named: obj.prop
	 * - Direct access if register: reg
	 */

	if (l->n_op == PLUS || l->n_op == MINUS) {
		NODE *r = l->n_right;

		if (r->n_op == ICON) {
			/* base + constant -> OREG with offset */
			if (l->n_left->n_op == REG) {
				/* Perfect for OREG */
				p->n_op = OREG;
				setlval(p, l->n_op == PLUS ? getlval(r) : -getlval(r));
				regno(p) = regno(l->n_left);
				tfree(l);
				return;
			}
		}
	}

	if (l->n_op == REG) {
		/* Simple dereference of register */
		p->n_op = OREG;
		setlval(p, 0);
		regno(p) = regno(l);
		tfree(l);
		return;
	}

	/* Can't optimize - leave as UMUL */
}

/*
 * Find the next instruction to be expanded
 * JavaScript uses natural left-to-right evaluation order
 */
int
findops(NODE *p, int cookie, int *cp)
{
	/*
	 * JavaScript doesn't need special instruction ordering
	 * Return 0 to use default behavior
	 */
	return 0;
}

/*
 * Check if shape is valid for the given type
 */
int
shpr(int shape, int type)
{
	/*
	 * Shape checking for JavaScript
	 * All shapes are valid since JavaScript is dynamically typed
	 */
	return 1;
}

/*
 * Find the cheapest way to satisfy a shape requirement
 */
int
findcheap(int cookie)
{
	/*
	 * JavaScript doesn't have different costs for different shapes
	 * Return 0 to use default
	 */
	return 0;
}

/*
 * Check if we should use a different addressing mode
 */
int
myreader(struct interpass *ipole)
{
	/*
	 * JavaScript doesn't have multiple addressing modes
	 * Return 0 to use default behavior
	 */
	return 0;
}

/*
 * Called to check if a node can be generated in-line
 */
int
acceptable(struct optab *op)
{
	/*
	 * JavaScript can generate most operations inline
	 * Return 1 to accept all patterns
	 */
	return 1;
}

/*
 * Called before printing out an instruction
 * Can modify the instruction tree
 */
void
mycanon(NODE *p)
{
	/*
	 * JavaScript doesn't need instruction canonicalization
	 * Leave the tree as-is
	 */
}

/*
 * Called to check if operation should be done in a specific register class
 */
int
special(NODE *p, int shape)
{
	/*
	 * JavaScript doesn't have register class restrictions
	 * Return SRNOPE to indicate no special handling
	 */
	return SRNOPE;
}

/*
 * Called to check for target-specific optimizations
 */
int
myoptim(struct interpass *ip)
{
	/*
	 * JavaScript-specific optimizations could go here
	 * For now, return 0 to use default optimizations
	 */
	return 0;
}

/*
 * Check if a constant fits in a specific field
 */
int
shumul(NODE *p, int shape)
{
	/*
	 * JavaScript handles all integer sizes natively
	 * (up to 2^53-1 for Number, arbitrary for BigInt)
	 * Return 1 to indicate constant fits
	 */
	return 1;
}

/*
 * Check if index scaling is allowed
 */
int
scalindex(NODE *p)
{
	/*
	 * JavaScript array access doesn't support scaling
	 * (unlike x86 where you can do [base + index*scale])
	 * Must calculate offset explicitly: array[index]
	 * Return 0 to indicate no scaling support
	 */
	return 0;
}

/*
 * Generate code for a node that couldn't be matched
 */
int
geninsn(NODE *p, int cookie)
{
	/*
	 * This is called when no table entry matches
	 * For JavaScript, we should handle common cases here
	 * Return 0 to indicate we didn't handle it (use default)
	 */
	return 0;
}

/*
 * Called to set up for generating a function call
 */
int
argsiz(NODE *p)
{
	/*
	 * JavaScript doesn't need to calculate argument stack size
	 * Arguments are passed as function parameters
	 * Return 0
	 */
	return 0;
}

/*
 * Check if a value should be promoted
 */
int
promote(NODE *p)
{
	/*
	 * JavaScript automatically promotes values as needed
	 * No explicit promotion required
	 * Return 0
	 */
	return 0;
}

/*
 * Check if operation needs a temporary
 */
int
needtemp(NODE *p)
{
	/*
	 * Check if operation needs a temporary variable
	 * Depends on complexity of expression
	 */

	switch (p->n_op) {
	case CALL:
	case UCALL:
	case STCALL:
		/* Function calls may need temporaries for args */
		return 1;

	case STASG:
		/* Structure assignment may need temporary */
		return 1;

	case QUEST:
		/* Ternary operator may need temporary */
		return 1;

	case ANDAND:
	case OROR:
		/* Short-circuit operators may need temporary */
		return 1;

	default:
		/* Simple operations don't need temporaries */
		return 0;
	}
}

/*
 * Check if value should be stored in memory vs register
 */
int
freetemp(NODE *p)
{
	/*
	 * JavaScript doesn't distinguish between memory and registers
	 * All variables are in JavaScript's variable space
	 * Return 0
	 */
	return 0;
}

/*
 * Check if operation can be done with a memory operand
 */
int
rmove(int s, int d, TWORD t)
{
	/*
	 * JavaScript variable assignment is always possible
	 * s = source register, d = destination register
	 * Return 1 to indicate move is possible
	 */
	return 1;
}

/*
 * Rewrite tree for better code generation
 */
NODE *
funarg(NODE *p)
{
	/*
	 * Function arguments don't need special handling in JavaScript
	 * Return the node as-is
	 */
	return p;
}

/*
 * Check if we can generate code for structure copy
 */
int
genstruct(NODE *p)
{
	/*
	 * JavaScript object copying:
	 * - Shallow copy: obj2 = obj1 (assigns reference)
	 * - Deep copy: obj2 = {...obj1} (ES6) or Object.assign({}, obj1) (ES5)
	 *
	 * For now, return 0 to use default handling
	 */
	return 0;
}

/*
 * Check alignment requirements
 */
int
chkalign(int alignment)
{
	/*
	 * JavaScript has no alignment requirements
	 * Return 1 to indicate alignment is satisfied
	 */
	return 1;
}
