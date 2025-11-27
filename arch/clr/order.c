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
 * CLR backend - expression evaluation order and cost metrics
 *
 * The CLR uses a stack-based architecture, so evaluation order is
 * critical for minimizing stack depth and avoiding unnecessary spills.
 */

#include "pass2.h"
#include <string.h>

/*
 * Sethi-Ullman numbers for CLR stack machine.
 *
 * These determine evaluation order for expression trees.
 * For a stack machine:
 * - Leaf nodes (constants, variables) have cost 0 (just push)
 * - Unary operators have cost of their operand
 * - Binary operators need careful ordering to minimize stack depth
 *
 * The bits in the return value encode:
 * - Bit 0: Does this node need a "register" (stack slot)?
 * - Bit 1: Should left child be evaluated first?
 * - Bit 2: Should right child be evaluated first?
 * - Bit 3+: Additional flags
 */
int
suflags(int op)
{
	switch (op) {
	/* Leaf nodes - no evaluation needed beyond a push */
	case NAME:
	case ICON:
	case FCON:
	case REG:
		return 0;

	/* Memory reference - slightly more expensive than constant */
	case OREG:
		return 01;

	/* Unary operators - inherit from operand */
	case UMINUS:
	case COMPL:
	case NOT:
		return 01;

	/* Address-of operator */
	case ADDROF:
		return 01;

	/* Indirection (dereference) */
	case UMUL:
		return 01;

	/* Type conversions */
	case SCONV:
	case PCONV:
		return 01;

	/* Binary arithmetic operators */
	/* These consume two stack slots and produce one */
	/* Evaluate in left-to-right order (bit 01 = left first) */
	case PLUS:
	case MINUS:
	case MUL:
	case DIV:
	case MOD:
		return 02;

	/* Bitwise operators */
	case AND:
	case OR:
	case ER:	/* XOR */
	case LS:	/* Left shift */
	case RS:	/* Right shift */
		return 02;

	/* Comparison operators */
	case EQ:
	case NE:
	case LT:
	case LE:
	case GT:
	case GE:
	case ULT:
	case ULE:
	case UGT:
	case UGE:
		return 02;

	/* Assignment operators */
	/* Right side evaluated first, then stored to left */
	case ASSIGN:
		return 04;  /* Bit 04 = right first */

	/* Compound assignment operators */
	case ASG_PLUS:
	case ASG_MINUS:
	case ASG_MUL:
	case ASG_DIV:
	case ASG_MOD:
	case ASG_AND:
	case ASG_OR:
	case ASG_ER:
	case ASG_LS:
	case ASG_RS:
		return 04;

	/* Function call - very expensive */
	case CALL:
	case UCALL:
	case STCALL:
	case USTCALL:
		return 010;  /* High cost */

	/* Conditional expression */
	case QUEST:
	case COLON:
		return 02;

	/* Array subscripting */
	case LB:
		return 02;

	/* Struct/union member access */
	case DOT:
	case STREF:
		return 01;

	/* Comma operator */
	case COMOP:
		return 02;  /* Evaluate left, discard, then right */

	/* Branch and control flow */
	case CBRANCH:
		return 02;

	case GOTO:
		return 01;

	case RETURN:
		return 01;

	/* Inline assembly */
	case XASM:
		return 010;

	/* Default */
	default:
		return 01;
	}
}

/*
 * Calculate cost of storing a temporary for this node.
 *
 * For CLR stack machine, "cost" represents:
 * - Stack depth consumed
 * - Complexity of operation
 * - Likelihood of needing a local variable
 */
int
cost(NODE *p)
{
	int op = p->n_op;
	TWORD type = p->n_type;

	/* Leaf nodes are cheap */
	switch (op) {
	case ICON:
		/* Small constants are very cheap (ldc.i4.0, ldc.i4.1, etc.) */
		if (p->n_lval >= -1 && p->n_lval <= 8)
			return 1;
		/* Larger constants need full ldc.i4 */
		if (p->n_lval >= -128 && p->n_lval <= 127)
			return 2;
		return 3;

	case FCON:
		/* Floating-point constants are more expensive */
		return 4;

	case NAME:
		/* Global field access */
		return 3;

	case OREG:
		/* Local variable or parameter access */
		return 2;

	case REG:
		/* Already in "register" (on stack) */
		return 1;

	/* Unary operators */
	case UMINUS:
	case COMPL:
	case NOT:
		return 2;

	/* Binary arithmetic */
	case PLUS:
	case MINUS:
		return 3;

	case MUL:
		/* Multiplication is moderate cost in CLR */
		return 4;

	case DIV:
	case MOD:
		/* Division is expensive */
		return 6;

	/* Bitwise operations are cheap */
	case AND:
	case OR:
	case ER:
		return 2;

	case LS:
	case RS:
		return 3;

	/* Comparisons */
	case EQ:
	case NE:
	case LT:
	case LE:
	case GT:
	case GE:
	case ULT:
	case ULE:
	case UGT:
	case UGE:
		return 3;

	/* Memory access */
	case UMUL:  /* Dereference */
		return 4;

	case ASSIGN:
		/* Store operation */
		return 3;

	/* Type conversions */
	case SCONV:
	case PCONV:
		/* CLR has explicit conversion instructions */
		if (type == CHAR || type == UCHAR)
			return 2;  /* conv.i1 / conv.u1 */
		if (type == SHORT || type == USHORT)
			return 2;  /* conv.i2 / conv.u2 */
		if (type == LONGLONG || type == ULONGLONG)
			return 3;  /* conv.i8 / conv.u8 */
		if (type == FLOAT)
			return 3;  /* conv.r4 */
		if (type == DOUBLE || type == LDOUBLE)
			return 3;  /* conv.r8 */
		return 2;

	/* Function calls are very expensive */
	case CALL:
	case UCALL:
	case STCALL:
	case USTCALL:
		return 20;

	/* Array operations */
	case LB:
		return 5;

	/* Default cost */
	default:
		return 4;
	}
}

/*
 * Determine if a node should be evaluated into a temporary.
 *
 * For CLR, this means deciding whether to store the result
 * in a local variable to avoid re-computation.
 */
int
stoarg(NODE *p, int arg)
{
	/* For function arguments, evaluate directly onto stack */
	/* CLR calling convention: args pushed left-to-right */
	return 0;  /* Don't force temporary */
}

/*
 * Determine if we need to store a floating-point argument.
 */
int
stofarg(NODE *p, int arg)
{
	/* CLR handles float args the same as int args */
	return 0;
}

/*
 * Determine if we need to store a struct argument.
 */
int
stostarg(NODE *p, int arg)
{
	/* Structs might need special handling */
	/* For now, evaluate normally */
	return 0;
}

/*
 * Check if this node can be generated using a memory operand.
 * For CLR, all operations work on the evaluation stack.
 */
int
acceptable(NODE *p)
{
	/* Most patterns are acceptable for CLR IL */
	return 1;
}

/*
 * Reorder a tree for optimal evaluation on stack machine.
 *
 * For CLR, we want to:
 * - Minimize stack depth
 * - Avoid unnecessary local variable spills
 * - Evaluate in left-to-right order when possible
 */
NODE *
clr_reorder_tree(NODE *p)
{
	/* No reordering needed - rely on table-driven code generation */
	return p;
}

/*
 * Calculate the maximum stack depth needed for an expression.
 * This is critical for CLR's .maxstack directive.
 */
int
clr_calc_stack_depth(NODE *p)
{
	int left_depth, right_depth, max_depth;

	if (p == NULL)
		return 0;

	switch (optype(p->n_op)) {
	case LTYPE:
		/* Leaf node - pushes one value */
		return 1;

	case UTYPE:
		/* Unary operator - same depth as operand */
		return clr_calc_stack_depth(p->n_left);

	case BITYPE:
		/* Binary operator */
		left_depth = clr_calc_stack_depth(p->n_left);
		right_depth = clr_calc_stack_depth(p->n_right);

		/* Both operands evaluated, then combined */
		if (left_depth == right_depth) {
			/* Equal depth - need one extra slot */
			max_depth = left_depth + 1;
		} else {
			/* Unequal - use maximum */
			max_depth = (left_depth > right_depth) ? left_depth : right_depth;
		}

		/* Some operators consume both and produce one */
		switch (p->n_op) {
		case PLUS:
		case MINUS:
		case MUL:
		case DIV:
		case AND:
		case OR:
		case ER:
			/* Result is one value on stack */
			return max_depth;

		case ASSIGN:
			/* Assignment may need extra slot for address */
			return max_depth + 1;

		default:
			return max_depth;
		}

	default:
		return 1;
	}
}

/*
 * Estimate register (stack slot) pressure for scheduling.
 */
int
clr_reg_pressure(NODE *p)
{
	/* For CLR, this is equivalent to stack depth */
	return clr_calc_stack_depth(p);
}
