/*
 * Copyright (c) 2025 PCC Prolog Runtime Library
 *
 * Arithmetic expression evaluation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "runtime.h"

/* Evaluate arithmetic expression to integer */
int eval_arith(prolog_engine_t *eng, word_t expr, intptr_t *result) {
	expr = deref(eng, expr);

	/* Integer constant */
	if (IS_INT(expr)) {
		*result = GET_INT(expr);
		return 1;
	}

	/* Float constant - convert to int */
	if (IS_FLOAT(expr)) {
		float_box_t *f = (float_box_t *)UNTAG(expr);
		*result = (intptr_t)f->value;
		return 1;
	}

	/* Compound expression */
	if (IS_STRUCT(expr)) {
		word_t *p = (word_t *)UNTAG(expr);
		functor_t *f = (functor_t *)p[0];
		const char *op = f->name->name;

		/* Unary operators */
		if (f->arity == 1) {
			intptr_t arg;
			if (!eval_arith(eng, p[1], &arg))
				return 0;

			if (strcmp(op, "+") == 0) {
				*result = arg;
				return 1;
			}
			if (strcmp(op, "-") == 0) {
				*result = -arg;
				return 1;
			}
			if (strcmp(op, "abs") == 0) {
				*result = abs(arg);
				return 1;
			}
			if (strcmp(op, "sign") == 0) {
				*result = (arg > 0) ? 1 : (arg < 0) ? -1 : 0;
				return 1;
			}
			if (strcmp(op, "\\") == 0) {  /* Bitwise NOT */
				*result = ~arg;
				return 1;
			}
		}

		/* Binary operators */
		if (f->arity == 2) {
			intptr_t left, right;
			if (!eval_arith(eng, p[1], &left))
				return 0;
			if (!eval_arith(eng, p[2], &right))
				return 0;

			if (strcmp(op, "+") == 0) {
				*result = left + right;
				return 1;
			}
			if (strcmp(op, "-") == 0) {
				*result = left - right;
				return 1;
			}
			if (strcmp(op, "*") == 0) {
				*result = left * right;
				return 1;
			}
			if (strcmp(op, "//") == 0 || strcmp(op, "div") == 0) {
				if (right == 0) return 0;  /* Division by zero */
				*result = left / right;
				return 1;
			}
			if (strcmp(op, "mod") == 0) {
				if (right == 0) return 0;
				*result = left % right;
				return 1;
			}
			if (strcmp(op, "rem") == 0) {
				if (right == 0) return 0;
				*result = left % right;
				return 1;
			}
			if (strcmp(op, "**") == 0 || strcmp(op, "^") == 0) {
				*result = (intptr_t)pow((double)left, (double)right);
				return 1;
			}
			if (strcmp(op, "<<") == 0) {
				*result = left << right;
				return 1;
			}
			if (strcmp(op, ">>") == 0) {
				*result = left >> right;
				return 1;
			}
			if (strcmp(op, "/\\") == 0) {  /* Bitwise AND */
				*result = left & right;
				return 1;
			}
			if (strcmp(op, "\\/") == 0) {  /* Bitwise OR */
				*result = left | right;
				return 1;
			}
			if (strcmp(op, "xor") == 0) {
				*result = left ^ right;
				return 1;
			}
		}
	}

	return 0;  /* Not an arithmetic expression */
}

/* Evaluate arithmetic expression to float */
int eval_arith_float(prolog_engine_t *eng, word_t expr, double *result) {
	expr = deref(eng, expr);

	/* Float constant */
	if (IS_FLOAT(expr)) {
		float_box_t *f = (float_box_t *)UNTAG(expr);
		*result = f->value;
		return 1;
	}

	/* Integer constant - convert to float */
	if (IS_INT(expr)) {
		*result = (double)GET_INT(expr);
		return 1;
	}

	/* Compound expression */
	if (IS_STRUCT(expr)) {
		word_t *p = (word_t *)UNTAG(expr);
		functor_t *f = (functor_t *)p[0];
		const char *op = f->name->name;

		/* Unary operators */
		if (f->arity == 1) {
			double arg;
			if (!eval_arith_float(eng, p[1], &arg))
				return 0;

			if (strcmp(op, "+") == 0) {
				*result = arg;
				return 1;
			}
			if (strcmp(op, "-") == 0) {
				*result = -arg;
				return 1;
			}
			if (strcmp(op, "abs") == 0) {
				*result = fabs(arg);
				return 1;
			}
			if (strcmp(op, "sqrt") == 0) {
				*result = sqrt(arg);
				return 1;
			}
			if (strcmp(op, "sin") == 0) {
				*result = sin(arg);
				return 1;
			}
			if (strcmp(op, "cos") == 0) {
				*result = cos(arg);
				return 1;
			}
			if (strcmp(op, "tan") == 0) {
				*result = tan(arg);
				return 1;
			}
			if (strcmp(op, "exp") == 0) {
				*result = exp(arg);
				return 1;
			}
			if (strcmp(op, "log") == 0) {
				*result = log(arg);
				return 1;
			}
			if (strcmp(op, "floor") == 0) {
				*result = floor(arg);
				return 1;
			}
			if (strcmp(op, "ceiling") == 0) {
				*result = ceil(arg);
				return 1;
			}
			if (strcmp(op, "round") == 0) {
				*result = round(arg);
				return 1;
			}
		}

		/* Binary operators */
		if (f->arity == 2) {
			double left, right;
			if (!eval_arith_float(eng, p[1], &left))
				return 0;
			if (!eval_arith_float(eng, p[2], &right))
				return 0;

			if (strcmp(op, "+") == 0) {
				*result = left + right;
				return 1;
			}
			if (strcmp(op, "-") == 0) {
				*result = left - right;
				return 1;
			}
			if (strcmp(op, "*") == 0) {
				*result = left * right;
				return 1;
			}
			if (strcmp(op, "/") == 0) {
				if (right == 0.0) return 0;
				*result = left / right;
				return 1;
			}
			if (strcmp(op, "**") == 0 || strcmp(op, "^") == 0) {
				*result = pow(left, right);
				return 1;
			}
			if (strcmp(op, "atan2") == 0) {
				*result = atan2(left, right);
				return 1;
			}
		}
	}

	return 0;
}

/* is/2 built-in */
int builtin_is(prolog_engine_t *eng, word_t *args) {
	word_t result_term = deref(eng, args[0]);
	word_t expr = args[1];

	/* Try float evaluation first */
	double fval;
	if (eval_arith_float(eng, expr, &fval)) {
		/* Check if result is integer */
		if (floor(fval) == fval) {
			intptr_t ival = (intptr_t)fval;
			word_t int_term = make_integer(eng, ival);
			return unify(eng, result_term, int_term);
		} else {
			word_t float_term = make_float(eng, fval);
			return unify(eng, result_term, float_term);
		}
	}

	/* Try integer evaluation */
	intptr_t ival;
	if (eval_arith(eng, expr, &ival)) {
		word_t int_term = make_integer(eng, ival);
		return unify(eng, result_term, int_term);
	}

	return 0;
}

/* Arithmetic comparison helpers */
static int compare_arith(prolog_engine_t *eng, word_t t1, word_t t2, int *cmp) {
	/* Try float comparison */
	double f1, f2;
	if (eval_arith_float(eng, t1, &f1) && eval_arith_float(eng, t2, &f2)) {
		if (f1 < f2) *cmp = -1;
		else if (f1 > f2) *cmp = 1;
		else *cmp = 0;
		return 1;
	}

	/* Try integer comparison */
	intptr_t i1, i2;
	if (eval_arith(eng, t1, &i1) && eval_arith(eng, t2, &i2)) {
		if (i1 < i2) *cmp = -1;
		else if (i1 > i2) *cmp = 1;
		else *cmp = 0;
		return 1;
	}

	return 0;
}

/* =:=/2 - Arithmetic equal */
int builtin_arith_equal(prolog_engine_t *eng, word_t *args) {
	int cmp;
	return compare_arith(eng, args[0], args[1], &cmp) && cmp == 0;
}

/* =\=/2 - Arithmetic not equal */
int builtin_arith_noteq(prolog_engine_t *eng, word_t *args) {
	int cmp;
	return compare_arith(eng, args[0], args[1], &cmp) && cmp != 0;
}

/* </2 - Arithmetic less */
int builtin_arith_less(prolog_engine_t *eng, word_t *args) {
	int cmp;
	return compare_arith(eng, args[0], args[1], &cmp) && cmp < 0;
}

/* =</2 - Arithmetic less or equal */
int builtin_arith_lesseq(prolog_engine_t *eng, word_t *args) {
	int cmp;
	return compare_arith(eng, args[0], args[1], &cmp) && cmp <= 0;
}

/* >/2 - Arithmetic greater */
int builtin_arith_greater(prolog_engine_t *eng, word_t *args) {
	int cmp;
	return compare_arith(eng, args[0], args[1], &cmp) && cmp > 0;
}

/* >=/2 - Arithmetic greater or equal */
int builtin_arith_greatereq(prolog_engine_t *eng, word_t *args) {
	int cmp;
	return compare_arith(eng, args[0], args[1], &cmp) && cmp >= 0;
}
