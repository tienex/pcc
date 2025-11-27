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
 * JavaScript code emission (local2.c)
 * Handles instruction emission, function prologue/epilogue
 */

#include "pass2.h"
#include <ctype.h>
#include <string.h>

void acon(NODE *p);
int argsize(NODE *p);

extern int mjesmodel;
extern int mjesmodule;

static int maxargsz;

/* Register names map to JavaScript variables */
const char *rnames[] = {
	/* General purpose registers -> JavaScript variables */
	"_r0", "_r1", "_r2", "_r3", "_r4", "_r5", "_r6", "_r7",
	"_r8", "_r9", "_r10", "_r11", "_r12", "_r13", "_r14", "_r15",
	"_r16", "_r17", "_r18", "_r19", "_r20", "_r21", "_r22", "_r23",
	"_r24", "_r25", "_r26", "_r27", "_r28", "_r29", "_r30", "_r31",
	/* Float registers -> JavaScript variables */
	"_f0", "_f1", "_f2", "_f3", "_f4", "_f5", "_f6", "_f7",
	"_f8", "_f9", "_f10", "_f11", "_f12", "_f13", "_f14", "_f15",
};

/*
 * Define a label
 */
void
deflab(int label)
{
	printf("_L%d:\n", label);
}

/*
 * Get variable declaration keyword
 */
static char *
js_vardecl(void)
{
	if (mjesmodel & MJES_ES6)
		return "let";
	else
		return "var";
}

/*
 * Print function prologue
 * Declare local variables (register variables)
 */
void
prologue(struct interpass_prolog *ipp)
{
	int i, nlocals = 0;
	int nregs_used = 0;

	maxargsz = 0;

	/* Declare register variables used in this function */
	/* The register allocator will determine which registers are used */

	printf("  // Function prologue\n");

	/* Declare local variables for register allocation */
	/* We'll use a simple scheme: declare all possible registers */
	/* The optimizer will remove unused ones in production */

	if (p2maxautooff > 0) {
		nlocals = (p2maxautooff + SZINT/SZCHAR - 1) / (SZINT/SZCHAR);

		if (nlocals > 0) {
			printf("  %s ", js_vardecl());
			for (i = 0; i < nlocals; i++) {
				if (i > 0)
					printf(", ");
				printf("_local%d", i);
			}
			printf(";\n");
		}
	}

	/* Declare register variables that will be used */
	/* For simplicity, declare first 16 general purpose registers */
	printf("  %s _r0, _r1, _r2, _r3, _r4, _r5, _r6, _r7, _r8, _r9, _r10, _r11, _r12, _r13, _r14, _r15;\n",
	       js_vardecl());

	/* Declare float registers if needed */
	if (xtemps[CLASSB] > 0) {
		printf("  %s _f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7;\n",
		       js_vardecl());
	}
}

/*
 * Print function epilogue
 */
void
eoftn(struct interpass_prolog *ipp)
{
	if (ipp->ipp_ip.ip_lbl == 0)
		return;

	/* Function end is printed by efcode() in code.c */
	printf("  // Function epilogue\n");
}

/*
 * Get JavaScript operator string
 */
static char *
js_opcode(int o, TWORD t)
{
	int is_bigint = (t == LONGLONG || t == ULONGLONG) && (mjesmodel & MJES_ES2020);

	switch (o) {
	case PLUS:
		return "+";
	case MINUS:
		return "-";
	case MUL:
		return "*";
	case DIV:
		/* JavaScript / is float division */
		/* For integer division, use Math.trunc or ~~ */
		if (!ISFLOAT(t)) {
			if (mjesmodel & MJES_ES6)
				return "INTDIV"; /* Special marker */
			else
				return "INTDIV5"; /* ES5 version */
		}
		return "/";
	case MOD:
		return "%";
	case AND:
		return "&";
	case OR:
		return "|";
	case ER:
		return "^";
	case LS:
		return "<<";
	case RS:
		if (t == UNSIGNED || t == ULONG || t == ULONGLONG)
			return ">>>"; /* Unsigned right shift */
		return ">>";
	case EQ:
		if (mjesmodel & MJES_ES5)
			return "==="; /* Strict equality */
		return "==";
	case NE:
		if (mjesmodel & MJES_ES5)
			return "!=="; /* Strict inequality */
		return "!=";
	case LT:
		return "<";
	case LE:
		return "<=";
	case GT:
		return ">";
	case GE:
		return ">=";
	case ANDAND:
		return "&&";
	case OROR:
		return "||";
	case NOT:
		return "!";
	case COMPL:
		return "~";
	case UMINUS:
		return "-";
	default:
		return "???";
	}
}

/*
 * Print opcode for binary operations
 */
void
hopcode(int f, int o)
{
	char *str = js_opcode(o, f);
	printf("%s", str);
}

/*
 * Return type size in bytes
 */
int
tlen(NODE *p)
{
	switch (p->n_type) {
	case CHAR:
	case UCHAR:
	case BOOL:
		return 1;
	case SHORT:
	case USHORT:
		return SZSHORT/SZCHAR;
	case DOUBLE:
	case LDOUBLE:
	case FLOAT:
		return SZDOUBLE/SZCHAR;
	case LONGLONG:
	case ULONGLONG:
		return SZLONGLONG/SZCHAR;
	case INT:
	case UNSIGNED:
	case LONG:
	case ULONG:
	default:
		return SZINT/SZCHAR;
	}
}

/*
 * Handle special code generation cases
 */
void
zzzcode(NODE *p, int c)
{
	NODE *l, *r;
	int pr, lr, rr;

	switch (c) {
	case 'A': /* Emit function call */
		l = p->n_left;
		printf("(");
		adrput(stdout, l);
		printf(")");
		break;

	case 'B': /* Emit BigInt literal (ES2020+) */
		if (mjesmodel & MJES_ES2020) {
			printf("%lldn", (long long)getlval(p));
		} else {
			printf("%lld", (long long)getlval(p));
		}
		break;

	case 'C': /* Emit integer division */
		l = p->n_left;
		r = p->n_right;
		if (mjesmodel & MJES_ES6) {
			printf("Math.trunc(");
			adrput(stdout, l);
			printf(" / ");
			adrput(stdout, r);
			printf(")");
		} else {
			/* ES5: use ~~ for integer conversion */
			printf("~~(");
			adrput(stdout, l);
			printf(" / ");
			adrput(stdout, r);
			printf(")");
		}
		break;

	case 'D': /* Emit typeof operator */
		printf("typeof ");
		break;

	case 'E': /* Emit optional chaining (ES2020+) */
		if (mjesmodel & MJES_ES2020) {
			printf("?.");
		} else {
			printf("."); /* Fallback */
		}
		break;

	case 'F': /* Emit nullish coalescing (ES2020+) */
		if (mjesmodel & MJES_ES2020) {
			printf(" ?? ");
		} else {
			/* Not directly supported in older ES */
			printf(" || ");
		}
		break;

	case 'G': /* Emit strict equality */
		if (mjesmodel & MJES_ES5) {
			printf("===");
		} else {
			printf("==");
		}
		break;

	case 'H': /* Emit strict inequality */
		if (mjesmodel & MJES_ES5) {
			printf("!==");
		} else {
			printf("!=");
		}
		break;

	case 'I': /* Emit instanceof operator */
		printf(" instanceof ");
		break;

	case 'J': /* Emit async keyword (ES2017+) */
		if (mjesmodel & MJES_ES2017) {
			printf("async ");
		}
		break;

	case 'K': /* Emit await keyword (ES2017+) */
		if (mjesmodel & MJES_ES2017) {
			printf("await ");
		}
		break;

	case 'L': /* Emit arrow function syntax (ES6+) */
		if (mjesmodel & MJES_ES6) {
			printf(" => ");
		} else {
			printf("function ");
		}
		break;

	case 'M': /* Emit spread operator (ES6+) */
		if (mjesmodel & MJES_ES6) {
			printf("...");
		}
		break;

	case 'N': /* Emit new operator */
		printf("new ");
		break;

	case 'O': /* Emit delete operator */
		printf("delete ");
		break;

	case 'P': /* Emit in operator */
		printf(" in ");
		break;

	case 'Q': /* Emit void operator */
		printf("void ");
		break;

	case 'R': /* Emit return statement */
		printf("return");
		break;

	case 'S': /* Emit this keyword */
		printf("this");
		break;

	case 'T': /* Emit TypeScript type annotation */
		if (mjesmodel & MJES_TYPESCRIPT) {
			/* Type annotation handled separately */
		}
		break;

	case 'U': /* Emit undefined */
		printf("undefined");
		break;

	case 'V': /* Emit null */
		printf("null");
		break;

	case 'W': /* Emit NaN */
		printf("NaN");
		break;

	case 'X': /* Emit Infinity */
		printf("Infinity");
		break;

	case 'Y': /* Emit yield (ES6+) */
		if (mjesmodel & MJES_ES6) {
			printf("yield ");
		}
		break;

	case 'Z': /* Emit super (ES6+) */
		if (mjesmodel & MJES_ES6) {
			printf("super");
		}
		break;

	default:
		comperr("zzzcode: unknown code '%c' (0x%x)", c, c);
	}
}

/*
 * Print out a constant value
 */
void
adrcon(CONSZ val)
{
	printf("%lld", (long long)val);
}

/*
 * Print out a constant or name
 */
void
conput(FILE *fp, NODE *p)
{
	CONSZ off = getlval(p);
	char *n = p->n_name;

	switch (p->n_op) {
	case ICON:
		if (n && n[0] != '\0') {
			fprintf(fp, "%s", n);
			if (off)
				fprintf(fp, " + %lld", (long long)off);
		} else {
			fprintf(fp, "%lld", (long long)off);
		}
		return;

	case FCON:
		fprintf(fp, "%Lf", (long double)p->n_dcon);
		return;

	default:
		comperr("illegal conput: op=%d", p->n_op);
	}
}

/*
 * Print out an instruction
 */
void
insput(NODE *p)
{
	comperr("insput not implemented for JavaScript backend");
}

/*
 * Write out the upper address (for multi-register values)
 */
void
upput(NODE *p, int size)
{
	size /= SZCHAR;

	switch (p->n_op) {
	case REG:
		/* JavaScript doesn't split registers */
		fprintf(stdout, "%s", rnames[regno(p) + 1]);
		break;

	case ICON:
	case NAME:
		slval(p, getlval(p) + size);
		adrput(stdout, p);
		slval(p, getlval(p) - size);
		break;

	case OREG:
		slval(p, getlval(p) + size);
		adrput(stdout, p);
		slval(p, getlval(p) - size);
		break;

	default:
		comperr("upput: illegal op %d", p->n_op);
	}
}

/*
 * Print out an address (operand)
 */
void
adrput(FILE *io, NODE *p)
{
	CONSZ off;
	char *n;

	switch (p->n_op) {
	case REG:
		/* Register -> JavaScript variable */
		fprintf(io, "%s", rnames[regno(p)]);
		break;

	case NAME:
		/* Variable name */
		n = p->n_name;
		if (n && n[0] != '\0') {
			fprintf(io, "%s", n);
		} else {
			fprintf(io, "_L%lld", (long long)getlval(p));
		}
		break;

	case ICON:
		/* Constant */
		off = getlval(p);
		n = p->n_name;
		if (n && n[0] != '\0') {
			fprintf(io, "%s", n);
			if (off)
				fprintf(io, " + %lld", (long long)off);
		} else {
			fprintf(io, "%lld", (long long)off);
		}
		break;

	case FCON:
		/* Floating point constant */
		fprintf(io, "%Lf", (long double)p->n_dcon);
		break;

	case OREG:
		/* Offset from register (array access, struct member, etc.) */
		off = getlval(p);
		if (off == 0) {
			fprintf(io, "%s", rnames[regno(p)]);
		} else {
			fprintf(io, "%s[%lld]", rnames[regno(p)], (long long)off);
		}
		break;

	case UMUL:
		/* Dereference */
		fprintf(io, "*");
		adrput(io, p->n_left);
		break;

	default:
		comperr("adrput: illegal op %d", p->n_op);
	}
}

/*
 * Calculate argument size for function calls
 */
int
argsize(NODE *p)
{
	/* JavaScript doesn't need explicit argument size calculation */
	return 0;
}

/*
 * Print out address constant (used for labels, addresses)
 */
void
acon(NODE *p)
{
	if (p->n_name[0] != '\0') {
		printf("%s", p->n_name);
		if (getlval(p) != 0)
			printf("+%lld", (long long)getlval(p));
	} else {
		printf("_L%lld", (long long)getlval(p));
	}
}

/*
 * Generate initialization code
 */
void
ninval(CONSZ off, int size, NODE *p)
{
	/* Initialize data with value */
	/* JavaScript: emit assignment */
	/* This is called for static initializers */
}

/*
 * Print out a string constant
 */
void
instring(struct symtab *sp)
{
	char *s, *str;

	str = sp->sname;

	/* JavaScript string literal */
	if (mjesmodel & MJES_ES6) {
		/* Use template literals for ES6+ (handles multiline, etc.) */
		printf("`");
		for (s = str; *s != '\0'; ) {
			if (*s == '`' || *s == '\\' || *s == '$') {
				printf("\\%c", *s++);
			} else if (*s == '\n') {
				printf("\\n");
				s++;
			} else if (*s == '\r') {
				printf("\\r");
				s++;
			} else if (*s == '\t') {
				printf("\\t");
				s++;
			} else {
				printf("%c", *s++);
			}
		}
		printf("`");
	} else {
		/* ES5/ES3 regular string literal */
		printf("\"");
		for (s = str; *s != '\0'; ) {
			if (*s == '"' || *s == '\\') {
				printf("\\%c", *s++);
			} else if (*s == '\n') {
				printf("\\n");
				s++;
			} else if (*s == '\r') {
				printf("\\r");
				s++;
			} else if (*s == '\t') {
				printf("\\t");
				s++;
			} else {
				printf("%c", *s++);
			}
		}
		printf("\"");
	}
}

/*
 * Print out a wide string constant
 */
void
inwstring(struct symtab *sp)
{
	/* JavaScript strings are UTF-16 by default */
	/* Just use regular string handling */
	instring(sp);
}

/*
 * Emit code for XASM (inline assembly)
 * JavaScript doesn't support inline assembly
 */
void
targarg(NODE *p, void *arg)
{
	/* Not supported for JavaScript */
	comperr("inline assembly not supported for JavaScript backend");
}

/*
 * Return the character set for this target
 */
char *
exname(char *p)
{
	/* JavaScript identifiers: alphanumeric, $, _ */
	/* Cannot start with digit */
	/* Return mangled name if necessary */
	return p;
}

/*
 * Return the alignment of field 'n'
 */
int
fldal(unsigned int t)
{
	/* JavaScript has no alignment requirements */
	return ALCHAR;
}

/*
 * Return the size of field 'n'
 */
CONSZ
fldoff(struct symtab *p)
{
	/* Field offset calculation */
	return p->soffset;
}

/*
 * Fix up type for function parameter
 */
void
fixdef(struct symtab *sp)
{
	/* JavaScript doesn't need type fixing for parameters */
}

/*
 * Do something target-dependent on struct declaration
 */
void
fixarg(struct symtab *sp, int class, TWORD type)
{
	/* No special handling needed for JavaScript */
}
