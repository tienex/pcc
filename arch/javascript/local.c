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
 * JavaScript-specific local optimizations and transformations
 */

#include "pass1.h"
#include "unicode.h"

#ifdef LANG_CXX
#define P1ND NODE
#define	p1alloc talloc
#define	p1nfree nfree
#define	p1fwalk fwalk
#define	p1tcopy ccopy
#endif

extern int mjesmodel;

/*
 * JavaScript-specific local optimizations
 * Transform C operations into JavaScript-friendly equivalents
 */
P1ND *
clocal(P1ND *p)
{
	struct symtab *q;
	P1ND *r, *l;
	TWORD t;
	int o;

#ifdef PCC_DEBUG
	if (xdebug) {
		printf("clocal (JavaScript)\n");
		p1fwalk(p, eprint, 0);
	}
#endif

	switch (o = p->n_op) {
	case NAME:
		/* Handle variable references */
		if ((q = p->n_sp) == NULL)
			return p;

		switch (q->sclass) {
		case PARAM:
		case AUTO:
			/* JavaScript local variables - no special conversion needed */
			break;
		case EXTERN:
		case EXTDEF:
			/* Global variables */
			break;
		default:
			break;
		}
		break;

	case PMCONV:
	case PVCONV:
		/* Pointer arithmetic conversions */
		if (p->n_right->n_op != ICON)
			cerror("bad conversion");
		r = buildtree(o == PMCONV ? MUL : DIV, p->n_left, p->n_right);
		p1nfree(p);
		p = r;
		break;

	case PCONV:
		/* Pointer conversions */
		t = p->n_type;
		l = p->n_left;

		/* JavaScript doesn't distinguish pointer types */
		if (p->n_type > BTMASK && l->n_type > BTMASK)
			goto delp;
		if (l->n_op == ICON && l->n_sp == NULL)
			goto delp;
		break;

delp:
		/* Delete unnecessary conversion */
		l->n_type = p->n_type;
		l->n_qual = p->n_qual;
		l->n_df = p->n_df;
		l->n_ap = p->n_ap;
		p = p1nfree(p);
		break;

	case SCONV:
		/* Type conversions */
		l = p->n_left;
		t = p->n_type;

		/* JavaScript automatic type conversion */
		if (DEUNSIGN(t) == DEUNSIGN(l->n_type)) {
			/* No-op conversion between signed/unsigned */
			goto delp;
		}

		/* Integer to BigInt conversion for ES2020+ */
		if ((t == LONGLONG || t == ULONGLONG) && (mjesmodel & MJES_ES2020)) {
			/* Will be handled by code generator to emit BigInt() call */
			break;
		}

		/* Float/Double conversions are no-op in JavaScript */
		if (ISFLOAT(t) && ISFLOAT(l->n_type))
			goto delp;

		break;

	case MOD:
		/* Modulo operation */
		/* JavaScript % operator works for both int and float */
		/* But we may want to use Math.fmod for floating point */
		if (ISFLOAT(p->n_type)) {
			/* For floating point, % works but may need fmod */
			/* Leave as-is for now, table.c will handle emission */
		}
		break;

	case DIV:
		/* Division operation */
		l = p->n_left;
		r = p->n_right;

		/* JavaScript / is always floating-point division */
		/* For integer division, we need Math.trunc or ~~  */
		if (!ISFLOAT(p->n_type)) {
			/* Integer division - will be handled by table.c */
			/* to emit Math.trunc(a / b) or ~~(a / b) */
		}
		break;

	case LS:
	case RS:
		/* Left/right shift operations */
		/* JavaScript << and >> work for 32-bit integers */
		/* >>> is unsigned right shift */
		if (p->n_type == UNSIGNED || p->n_type == ULONG) {
			/* Use >>> for unsigned right shift */
			if (o == RS) {
				/* Mark for unsigned right shift in table.c */
			}
		}
		break;

	case AND:
	case OR:
	case ER:
		/* Bitwise operations */
		/* JavaScript bitwise operators convert to 32-bit integers */
		/* For 64-bit (BigInt), need special handling in ES2020+ */
		if ((p->n_type == LONGLONG || p->n_type == ULONGLONG) &&
		    (mjesmodel & MJES_ES2020)) {
			/* Will use BigInt operators in table.c */
		}
		break;

	case COMPL:
		/* Bitwise NOT */
		/* JavaScript ~ operator */
		break;

	case UMINUS:
		/* Unary minus */
		/* JavaScript unary - */
		break;

	case NOT:
		/* Logical NOT */
		/* JavaScript ! operator */
		break;

	case QUEST:
		/* Ternary operator */
		/* JavaScript ?: operator works the same */
		break;

	case ASSIGN:
		/* Assignment operations */
		l = p->n_left;
		r = p->n_right;

		/* JavaScript uses = for assignment */
		/* Compound assignments (+=, -=, etc.) handled by table.c */
		break;

	case CALL:
		/* Function calls */
		/* JavaScript function call syntax is the same */
		/* But we may need to handle 'this' binding for methods */
		break;

	case STCALL:
		/* Structure return call */
		/* JavaScript objects are returned by reference */
		break;

	case STASG:
		/* Structure assignment */
		/* JavaScript object assignment (shallow copy) */
		/* For deep copy, would need Object.assign() or spread */
		if (mjesmodel & MJES_ES6) {
			/* Can use spread operator: {...obj} */
		} else {
			/* Use loop or Object.assign() polyfill */
		}
		break;

	case FORCE:
		/* Force value into return register */
		p->n_op = ASSIGN;
		p->n_right = p->n_left;
		p->n_left = block(REG, NULL, NULL, p->n_type, 0, 0);
		p->n_left->n_rval = RETREG(p->n_type);
		break;

	case CBRANCH:
		/* Conditional branch */
		/* JavaScript if/else works the same */
		break;

	case GOTO:
		/* Goto statement */
		/* JavaScript supports labels and goto-like behavior */
		break;

	case RETURN:
		/* Return statement */
		/* JavaScript return works the same */
		break;

	case FUNARG:
		/* Function argument */
		/* JavaScript passes arguments by value (primitives) or reference (objects) */
		break;

	default:
		break;
	}

#ifdef PCC_DEBUG
	if (xdebug) {
		printf("clocal end\n");
		p1fwalk(p, eprint, 0);
	}
#endif

	return p;
}

/*
 * Additional tree transformations for pass 2
 * Handle JavaScript-specific constructs
 */
void
myp2tree(P1ND *p)
{
	struct symtab *sp;

	switch (p->n_op) {
	case FCON:
		/* Floating point constants */
		/* JavaScript handles these directly */
		/* But we may want to use special syntax for Infinity, NaN, etc. */

		/* Check for special values */
		if (isinf(p->n_dcon)) {
			/* Infinity or -Infinity */
			p->n_op = NAME;
			sp = tmpalloc(sizeof(struct symtab));
			sp->sclass = EXTERN;
			sp->sname = p->n_dcon > 0 ? "Infinity" : "-Infinity";
			p->n_sp = sp;
			return;
		}

		if (isnan(p->n_dcon)) {
			/* NaN */
			p->n_op = NAME;
			sp = tmpalloc(sizeof(struct symtab));
			sp->sclass = EXTERN;
			sp->sname = "NaN";
			p->n_sp = sp;
			return;
		}

		/* Regular float constant - store as string literal */
		sp = tmpalloc(sizeof(struct symtab));
		sp->sclass = STATIC;
		sp->sap = 0;
		sp->slevel = 1;
		sp->soffset = getlab();
		sp->sflags = 0;
		sp->stype = p->n_type;
		sp->squal = (CON >> TSHIFT);

		defloc(sp);
		ninval(0, tsize(sp->stype, sp->sdf, sp->sap), p);

		p->n_op = NAME;
		slval(p, 0);
		p->n_sp = sp;
		break;

	case ICON:
		/* Integer constants */
		/* JavaScript Number.MAX_SAFE_INTEGER = 2^53 - 1 */
		/* For larger integers, use BigInt (ES2020+) */
		if ((p->n_type == LONGLONG || p->n_type == ULONGLONG) &&
		    (mjesmodel & MJES_ES2020)) {
			CONSZ val = glval(p);
			if (val > JS_MAX_SAFE_INTEGER || val < JS_MIN_SAFE_INTEGER) {
				/* Will emit BigInt literal in table.c */
				p->n_flags |= 0x100; /* Custom flag for BigInt */
			}
		}
		break;

	case NAME:
		/* Variable names */
		/* JavaScript variable naming is handled by code.c */
		break;

	default:
		break;
	}
}

/*
 * Optimize multiply/divide by powers of 2
 * JavaScript JIT engines optimize these, but we can help
 */
P1ND *
optim2(P1ND *p)
{
	int o = p->n_op;
	int val;
	P1ND *r;

	/* Only optimize integer operations */
	if (ISFLOAT(p->n_type))
		return p;

	switch (o) {
	case MUL:
		/* Multiply by power of 2 -> left shift */
		r = p->n_right;
		if (r->n_op == ICON && (val = ispow2(glval(r))) > 0) {
			/* Replace MUL with LS (left shift) */
			p->n_op = LS;
			slval(r, val);
		}
		break;

	case DIV:
		/* Divide by power of 2 -> right shift (for unsigned) */
		r = p->n_right;
		if ((p->n_type == UNSIGNED || p->n_type == ULONG) &&
		    r->n_op == ICON && (val = ispow2(glval(r))) > 0) {
			/* Replace DIV with RS (right shift) */
			p->n_op = RS;
			slval(r, val);
		}
		break;

	case MOD:
		/* Modulo by power of 2 -> bitwise AND (for unsigned) */
		r = p->n_right;
		if ((p->n_type == UNSIGNED || p->n_type == ULONG) &&
		    r->n_op == ICON && ispow2(glval(r)) > 0) {
			/* Replace MOD with AND */
			p->n_op = AND;
			slval(r, glval(r) - 1);
		}
		break;
	}

	return p;
}

/*
 * Check if value is a power of 2
 * Returns log2(n) if n is power of 2, else -1
 */
int
ispow2(CONSZ n)
{
	int log = 0;

	if (n <= 0 || (n & (n - 1)) != 0)
		return -1; /* Not a power of 2 */

	while (n > 1) {
		n >>= 1;
		log++;
	}

	return log;
}

/*
 * JavaScript-specific function to check if identifier is reserved word
 * Returns 1 if name is a JavaScript reserved word, 0 otherwise
 */
int
js_is_reserved(char *name)
{
	static char *reserved[] = {
		/* ES3 reserved words */
		"break", "case", "catch", "continue", "default", "delete",
		"do", "else", "finally", "for", "function", "if", "in",
		"instanceof", "new", "return", "switch", "this", "throw",
		"try", "typeof", "var", "void", "while", "with",

		/* ES5 strict mode reserved words */
		"implements", "interface", "let", "package", "private",
		"protected", "public", "static", "yield",

		/* ES6 reserved words */
		"class", "const", "enum", "export", "extends", "import", "super",

		/* ES2015+ reserved words */
		"await", "async",

		/* Literals */
		"null", "true", "false",

		/* Special values */
		"undefined", "NaN", "Infinity",

		/* TypeScript reserved words */
		"type", "namespace", "declare", "module", "as", "from",

		NULL
	};

	for (int i = 0; reserved[i] != NULL; i++) {
		if (strcmp(name, reserved[i]) == 0)
			return 1;
	}

	return 0;
}

/*
 * Mangle C identifier to valid JavaScript identifier if needed
 * Adds '_' prefix to reserved words
 */
char *
js_mangle_name(char *name)
{
	static char buf[256];

	if (js_is_reserved(name)) {
		snprintf(buf, sizeof(buf), "_%s", name);
		return buf;
	}

	return name;
}

/*
 * Check if operation can be done inline (JavaScript optimization)
 */
int
js_can_inline(P1ND *p)
{
	switch (p->n_op) {
	case PLUS:
	case MINUS:
	case MUL:
	case AND:
	case OR:
	case ER:
		return 1; /* Simple operations can be inlined */
	case DIV:
	case MOD:
		return 0; /* Division may need special handling */
	case CALL:
		/* Small functions could be inlined */
		/* For now, don't inline */
		return 0;
	default:
		return 0;
	}
}

/*
 * Optimize boolean expressions for JavaScript
 * JavaScript has truthy/falsy values
 */
P1ND *
js_optimize_bool(P1ND *p)
{
	P1ND *l, *r;

	switch (p->n_op) {
	case EQ:
		/* Use === in ES5+ for strict equality */
		if (mjesmodel & MJES_ES5) {
			/* Mark for strict equality */
			p->n_flags |= 0x200;
		}
		break;

	case NE:
		/* Use !== in ES5+ for strict inequality */
		if (mjesmodel & MJES_ES5) {
			/* Mark for strict inequality */
			p->n_flags |= 0x200;
		}
		break;

	case ANDAND:
	case OROR:
		/* JavaScript short-circuit evaluation */
		/* Already works correctly */
		break;

	case NOT:
		/* JavaScript ! operator */
		/* Can use !! for boolean coercion */
		l = p->n_left;
		if (l->n_op == NOT) {
			/* Double negation - boolean coercion */
			/* !!x converts to boolean */
		}
		break;
	}

	return p;
}

/*
 * Handle array access for JavaScript
 * JavaScript arrays are 0-indexed like C
 */
P1ND *
js_array_access(P1ND *p)
{
	/* JavaScript array access is the same as C: array[index] */
	/* No special transformation needed */
	return p;
}

/*
 * Handle string operations for JavaScript
 * JavaScript strings are immutable and have methods
 */
P1ND *
js_string_op(P1ND *p, int op)
{
	/* String concatenation uses + operator */
	/* String methods like charAt, substring, etc. */
	/* For now, leave as-is */
	return p;
}
