/*	$Id$	*/
/*
 * Copyright (c) 2003 Anders Magnusson (ragge@ludd.luth.se).
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


# include "pass2.h"
# include <ctype.h>

void acon(FILE *, NODE *p);
int argsize(NODE *p);
void genargs(NODE *p);

static int offlab;
int offarg;
static int addto;
static int regoff[16];

void
deflab(int label)
{
	printf(LABFMT ":\n", label);
}

void
prologue(struct interpass_prolog *ipp)
{
	int i, j;

	if (ipp->ipp_vis)
		printf("	.globl %s\n", ipp->ipp_name);
	printf("%s:\n", ipp->ipp_name);
	addto = p2maxautooff;
	if (addto >= AUTOINIT/SZCHAR)
		addto -= AUTOINIT/SZCHAR;
	addto /= SZINT/SZCHAR;	/* use words here */
	printf("	push %s,%s\n",rnames[STKREG], rnames[FPREG]);
	printf("	move %s,%s\n", rnames[FPREG],rnames[STKREG]);

	for (i = p2env.p_regs[0], j = 0; i ; i >>= 1, j++) {
		if (i & 1)
			regoff[j] = addto++;
	}
	if (addto)
		printf("	addi %s,0%o\n", rnames[STKREG], addto);

	for (i = p2env.p_regs[0], j = 0; i ; i >>= 1, j++) {
		if (i & 1)
			printf("	movem %s,%d(%s)\n",
			    rnames[j], regoff[j], rnames[STKREG]);
	}
}

void
eoftn(struct interpass_prolog *ipp)
{
	int i, j;

	if (ipp->ipp_ip.ip_lbl == 0)
		return; /* no code needs to be generated */
	for (i = p2env.p_regs[0], j = 0; i ; i >>= 1, j++) {
		if (i & 1)
			printf("	move %s,%d(%s)\n",
			    rnames[j], regoff[j], rnames[STKREG]);
	}
	printf("	move %s,%s\n", rnames[STKREG], rnames[FPREG]);
	printf("	pop %s,%s\n", rnames[STKREG], rnames[FPREG]);
	printf("	popj %s,\n", rnames[STKREG]);
}

#if 0
void
prologue(int regs, int autos)
{
	int i, addto;

	offlab = getlab2();
	if (regs < 0 || autos < 0) {
		/*
		 * non-optimized code, jump to epilogue for code generation.
		 */
		ftlab1 = getlab2();
		ftlab2 = getlab2();
		printf("	jrst L%d\n", ftlab1);
		printf("L%d:\n", ftlab2);
	} else {
		/*
		 * We here know what register to save and how much to 
		 * add to the stack.
		 */
		autos = autos + (SZINT-1);
		addto = (autos - AUTOINIT)/SZINT + (MAXRVAR-regs);
		if (addto || gflag) {
			printf("	push %s,%s\n",rnames[017], rnames[016]);
			printf("	move %s,%s\n", rnames[016],rnames[017]);
			for (i = regs; i < MAXRVAR; i++) {
				int db = ((i+1) < MAXRVAR);
				printf("	%smovem %s,0%o(%s)\n",
				    db ? "d" : "",
				    rnames[i+1], i+1-regs, rnames[016]);
				if (db)
					i++;
			}
			if (addto)
				printf("	addi %s,0%o\n", rnames[017], addto);
		} else
			offarg = 1;
	}
}

/*
 * End of block.
 */
void
eoftn(int regs, int autos, int retlab)
{
	register OFFSZ spoff;	/* offset from stack pointer */
	int i;

	spoff = autos + (SZINT-1);
	if (spoff >= AUTOINIT)
		spoff -= AUTOINIT;
	spoff /= SZINT;
	/* return from function code */
	printf("L%d:\n", retlab);
	if (gflag || isoptim == 0 || autos != AUTOINIT || regs != MAXRVAR) {
		for (i = regs; i < MAXRVAR; i++) {
			int db = ((i+1) < MAXRVAR);
			printf("	%smove %s,0%o(%s)\n", db ? "d" : "",
			    rnames[i+1], i+1-regs, rnames[016]);
			if (db)
				i++;
		}
		printf("	move %s,%s\n", rnames[017], rnames[016]);
		printf("	pop %s,%s\n", rnames[017], rnames[016]);
	}
	printf("	popj %s,\n", rnames[017]);

	/* Prolog code */
	if (isoptim == 0) {
		printf("L%d:\n", ftlab1);
		printf("	push %s,%s\n", rnames[017], rnames[016]);
		printf("	move %s,%s\n", rnames[016], rnames[017]);
		for (i = regs; i < MAXRVAR; i++) {
			int db = ((i+1) < MAXRVAR);
			printf("	%smovem %s,0%o(%s)\n", db ? "d" : "",
			    rnames[i+1], i+1-regs, rnames[016]);
			spoff++;
			if (db)
				i++, spoff++;
		}
		if (spoff)
			printf("	addi %s,0%llo\n", rnames[017], spoff);
		printf("	jrst L%d\n", ftlab2);
	}
	printf("	.set " LABFMT ",0%o\n", offlab, MAXRVAR-regs);
	offarg = isoptim = 0;
}
#endif

/*
 * add/sub/...
 *
 * Param given:
 *	R - Register
 *	M - Memory
 *	C - Constant
 */
void
hopcode(int f, int o)
{
	char *str;

	switch (o) {
	case PLUS:
		str = "add";
		break;
	case MINUS:
		str = "sub";
		break;
	case AND:
		str = "and";
		break;
	case OR:
		str = "ior";
		break;
	case ER:
		str = "xor";
		break;
	default:
		comperr("hopcode: unknown op %d", o);
		str = "UNKNOWN";
		break;
	}

	/* Output instruction with format suffix */
	if (f == 'C')
		printf("%si", str);  /* Immediate forms: addi, subi, etc. */
	else
		printf("%s", str);   /* Register forms: add, sub, etc. */
}

char *
rnames[] = {  /* keyed to register number tokens */
	"%0", "%1", "%2", "%3", "%4", "%5", "%6", "%7",
	"%10", "%11", "%12", "%13", "%14", "%15", "%16", "%17",
	"%0", "%1", "%2", "%3", "%4", "%5", "%6", "%7",
	"%10", "%11", "%12", "%13", "%14", "%15",
};

int
tlen(p) NODE *p;
{
	switch(p->n_type) {
		case CHAR:
		case UCHAR:
			return(1);

		case SHORT:
		case USHORT:
			return(SZSHORT/SZCHAR);

		case DOUBLE:
			return(SZDOUBLE/SZCHAR);

		case INT:
		case UNSIGNED:
		case LONG:
		case ULONG:
			return(SZINT/SZCHAR);

		case LONGLONG:
		case ULONGLONG:
			return SZLONGLONG/SZCHAR;

		default:
			if (!ISPTR(p->n_type))
				cerror("tlen type %d not pointer");
			return SZPOINT(0)/SZCHAR;
		}
}

static char *
binskip[] = {
	"e",	/* jumpe */
	"n",	/* jumpn */
	"le",	/* jumple */
	"l",	/* jumpl */
	"ge",	/* jumpge */
	"g",	/* jumpg */
};

/*
 * Extract the higher 36 bits from a longlong.
 */
static CONSZ
gethval(CONSZ lval)
{
	CONSZ hval = (lval >> 35) & 03777777777LL;

	if ((hval & 03000000000LL) == 03000000000LL) {
		hval |= 0777000000000LL;
	} else if ((hval & 03000000000LL) == 02000000000LL) {
		hval &= 01777777777LL;
		hval |= 0400000000000LL;
	}
	return hval;
}

/*
 * Do a binary comparision, and jump accordingly.
 */
static void
twocomp(NODE *p)
{
	int o = p->n_op;
	extern int negrel[];
	int isscon = 0, iscon = p->n_right->n_op == ICON;

	if (o < EQ || o > GT)
		cerror("bad binary conditional branch: %s", opst[o]);

	if (iscon && p->n_right->n_name[0] != 0) {
		printf("	cam%s ", binskip[negrel[o-EQ]-EQ]);
		adrput(stdout, getlr(p, 'L'));
		putchar(',');
		printf("[ .long ");
		adrput(stdout, getlr(p, 'R'));
		putchar(']');
		printf("\n	jrst L%d\n", p->n_label);
		return;
	}
	if (iscon)
		isscon = getlval(p->n_right) >= 0 &&
		    getlval(p->n_right) < 01000000;

	printf("	ca%c%s ", iscon && isscon ? 'i' : 'm',
	    binskip[negrel[o-EQ]-EQ]);
	adrput(stdout, getlr(p, 'L'));
	putchar(',');
	if (iscon && (isscon == 0)) {
		printf("[ .long ");
		adrput(stdout, getlr(p, 'R'));
		putchar(']');
	} else
		adrput(stdout, getlr(p, 'R'));
	printf("\n	jrst L%d\n", p->n_label);
}

/*
 * Compare byte/word pointers.
 * Note: Does not handle highest bit set in address
 */
static void
ptrcomp(NODE *p)
{
	printf("	rot "); adrput(stdout, getlr(p, 'L')); printf(",6\n");
	printf("	rot "); adrput(stdout, getlr(p, 'R')); printf(",6\n");
	twocomp(p);
}

/*
 * Do a binary comparision of two long long, and jump accordingly.
 * Note: Can be optimized for constant operands
 */
static void     
twollcomp(NODE *p)
{       
	int o = p->n_op;
	int iscon = p->n_right->n_op == ICON;
	int m = 0; /* Initialize to avoid uninitialized warning */

	if (o < EQ || o > GT)
		cerror("bad long long conditional branch: %s", opst[o]);

	/* Special strategy for equal/not equal */
	if (o == EQ || o == NE) {
		if (o == EQ)
			m = getlab2();
		printf("	came ");
		upput(getlr(p, 'L'), SZLONG);
		putchar(',');
		if (iscon)
			printf("[ .long ");
		upput(getlr(p, 'R'), SZLONG);
		if (iscon)
			putchar(']');
		printf("\n	jrst L%d\n", o == EQ ? m : p->n_label);
		printf("	cam%c ", o == EQ ? 'n' : 'e');
		adrput(stdout, getlr(p, 'L'));
		putchar(',');
		if (iscon)
			printf("[ .long ");
		adrput(stdout, getlr(p, 'R'));
		if (iscon)
			putchar(']');
		printf("\n	jrst L%d\n", p->n_label);
		if (o == EQ)
			printf("L%d:\n", m);
		return;
	}
	/* First test highword */
	printf("	cam%ce ", o == GT || o == GE ? 'l' : 'g');
	adrput(stdout, getlr(p, 'L'));
	putchar(',');
	if (iscon)
		printf("[ .long ");
	adrput(stdout, getlr(p, 'R'));
	if (iscon)
		putchar(']');
	printf("\n	jrst L%d\n", p->n_label);

	/* Test equality */
	printf("	came ");
	adrput(stdout, getlr(p, 'L'));
	putchar(',');
	if (iscon)
		printf("[ .long ");
	adrput(stdout, getlr(p, 'R'));
	if (iscon)
		putchar(']');
	printf("\n	jrst L%d\n", m = getlab2());

	/* Test lowword. Only works with pdp10 format for longlongs */
	printf("	cam%c%c ", o == GT || o == GE ? 'l' : 'g',
	    o == LT || o == GT ? 'e' : ' ');
	upput(getlr(p, 'L'), SZLONG);
	putchar(',');
	if (iscon)  
		printf("[ .long ");
	upput(getlr(p, 'R'), SZLONG);
	if (iscon)
		putchar(']');
	printf("\n	jrst L%d\n", p->n_label);
	printf("L%d:\n", m);
}

/*
 * Print the correct instruction for constants.
 */
static void
constput(NODE *p)
{
	CONSZ val = getlval(p->n_right);
	int reg = p->n_left->n_rval;

	/* Only numeric constant */
	if (p->n_right->n_name[0] == '\0') {
		if (val == 0) {
			printf("movei %s,0", rnames[reg]);
		} else if ((val & 0777777000000LL) == 0) {
			printf("movei %s,0%llo", rnames[reg], val);
		} else if ((val & 0777777) == 0) {
			printf("hrlzi %s,0%llo", rnames[reg], val >> 18);
		} else {
			printf("move %s,[ .long 0%llo]", rnames[reg],
			    szty(p->n_right->n_type) > 1 ? val :
			    val & 0777777777777LL);
		}
		/* Can have more tests here, hrloi etc */
		return;
	} else {
		printf("xmovei %s,%s", rnames[reg], p->n_right->n_name);
		if (val)
			printf("+" CONFMT, val);
	}
}

/*
 * Return true if the constant can be bundled in an instruction (immediate).
 */
static int
oneinstr(NODE *p)
{
	if (p->n_name[0] != '\0')
		return 0;
	if ((getlval(p) & 0777777000000ULL) != 0)
		return 0;
	return 1;
}

/*
 * Emit a halfword or byte instruction, from OREG to REG.
 * Sign extension must also be done here.
 */
static void
emitshort(NODE *p)
{
	CONSZ off = getlval(p);
	TWORD type = p->n_type;
	int reg = p->n_rval;
	int issigned = !ISUNSIGNED(type);
	int ischar = type == CHAR || type == UCHAR;
	int reg1 = getlr(p, '1')->n_rval;

	if (off < 0) { /* argument, use move instead */
		printf("	move ");
	} else if (off == 0 && p->n_name[0] == 0) {
		printf("	ldb %s,%s\n", rnames[reg1], rnames[reg]);
		/* Sign extend to handle negative values correctly */
		switch (type) {
		case CHAR:
			printf("	lsh %s,033\n", rnames[reg1]);
			printf("	ash %s,-033\n", rnames[reg1]);
			break;
		case SHORT:
			printf("	hrre %s,%s\n",
			    rnames[reg1], rnames[reg1]);
			break;
		}
		return;
	} else if (ischar) {
		if (off >= 0700000000000LL && p->n_name[0] != '\0') {
			cerror("emitsh");
			/* reg contains index integer */
//			if (!istreg(reg))
//				cerror("emitshort !istreg");
			printf("	adjbp %s,[ .long 0%llo+%s ]\n",
			    rnames[reg], off, p->n_name);
			printf("	ldb ");
			adrput(stdout, getlr(p, '1'));
			printf(",%s\n", rnames[reg]);
			goto signe;
		}
		printf("	ldb ");
		adrput(stdout, getlr(p, '1'));
		if (off)
			printf(",[ .long 0%02o11%02o%06o ]\n",
			    (int)(27-(9*(off&3))), reg, (int)off/4);
		else
			printf(",%s\n", rnames[reg]);
signe:		if (issigned) {
			printf("	lsh ");
			adrput(stdout, getlr(p, '1'));
			printf(",033\n	ash ");
			adrput(stdout, getlr(p, '1'));
			printf(",-033\n");
		}
		return;
	} else {
		printf("	h%cr%c ", off & 1 ? 'r' : 'l',
		    issigned ? 'e' : 'z');
	}
	setlval(p, getlval(p) / (ischar ? 4 : 2));
	adrput(stdout, getlr(p, '1'));
	putchar(',');
	adrput(stdout, getlr(p, 'L'));
	putchar('\n');
}

/*
 * Store a short from a register. Destination is a OREG.
 */
static void
storeshort(NODE *p)
{
	NODE *l = p->n_left;
	CONSZ off = getlval(l);
	int reg = l->n_rval;
	int ischar = BTYPE(p->n_type) == CHAR || BTYPE(p->n_type) == UCHAR;

	if (l->n_op == NAME) {
		if (ischar) {
			printf("	dpb ");
			adrput(stdout, getlr(p, 'R'));
			printf(",[ .long 0%02o%010o+%s ]\n",
			    070+((int)off&3), (int)(off/4), l->n_name);
			return;
		}
		printf("	hr%cm ", off & 1 ? 'r' : 'l');
		setlval(l, getlval(l) / 2);
		adrput(stdout, getlr(p, 'R'));
		putchar(',');   
		adrput(stdout, getlr(p, 'L'));
		putchar('\n');
		return;
	}

	if (off || reg == FPREG) { /* Can emit halfword instructions */
		if (off < 0) { /* argument, use move instead */
			printf("	movem ");
		} else if (ischar) {
			printf("	dpb ");
			adrput(stdout, getlr(p, '1'));
			printf(",[ .long 0%02o11%02o%06o ]\n",
			    (int)(27-(9*(off&3))), reg, (int)off/4);
			return;
		} else {
			printf("	hr%cm ", off & 1 ? 'r' : 'l');
		}
		setlval(l, getlval(l) / 2);
		adrput(stdout, getlr(p, 'R'));
		putchar(',');
		adrput(stdout, getlr(p, 'L'));
	} else {
		printf("	dpb ");
		adrput(stdout, getlr(p, 'R'));
		putchar(',');
		l = getlr(p, 'L');
		l->n_op = REG;
		adrput(stdout, l);
		l->n_op = OREG;
	}
	putchar('\n');
}

/*
 * Multiply a register with a constant.
 */
static void     
imuli(NODE *p)
{
	NODE *r = p->n_right;

	if (getlval(r) >= 0 && getlval(r) <= 0777777) {
		printf("	imuli ");
		adrput(stdout, getlr(p, 'L'));
		printf(",0%llo\n", getlval(r));
	} else {
		printf("	imul ");
		adrput(stdout, getlr(p, 'L'));
		printf(",[ .long 0%llo ]\n", getlval(r) & 0777777777777LL);
	}
}

/*
 * Divide a register with a constant.
 */
static void     
idivi(NODE *p)
{
	NODE *r = p->n_right;

	if (getlval(r) >= 0 && getlval(r) <= 0777777) {
		printf("	idivi ");
		adrput(stdout, getlr(p, '1'));
		printf(",0%llo\n", getlval(r));
	} else {
		printf("	idiv ");
		adrput(stdout, getlr(p, '1'));
		printf(",[ .long 0%llo ]\n", getlval(r) & 0777777777777LL);
	}
}

/*
 * move a constant into a register.
 */
static void
xmovei(NODE *p)
{
	/*
	 * Trick: If this is an unnamed constant, just move it directly,
	 * otherwise use xmovei to get section number.
	 */
	if (p->n_name[0] == '\0' || getlval(p) > 0777777) {
		printf("	");
		zzzcode(p, 'D');
		putchar(' ');
		adrput(stdout, getlr(p, '1'));
		putchar(',');
		zzzcode(p, 'E');
	} else {
		printf("	xmovei ");
		adrput(stdout, getlr(p, '1'));
		printf(",%s", p->n_name);
		if (getlval(p) != 0)
			printf("+0%llo", getlval(p));
	}
	putchar('\n');
}

static void
printcon(NODE *p) 
{
	CONSZ cz;

	p = p->n_left;
	if (getlval(p) >= 0700000000000LL) {
		/* converted to pointer in clocal() */
		conput(stdout, p);
		return;
	}
	if (getlval(p) == 0 && p->n_name[0] == '\0') {
		putchar('0');
		return;
	}
	if (BTYPE(p->n_type) == CHAR || BTYPE(p->n_type) == UCHAR)
		cz = (getlval(p)/4) | ((getlval(p) & 3) << 30);
	else
		cz = (getlval(p)/2) | (((getlval(p) & 1) + 5) << 30);
	cz |= 0700000000000LL;
	printf("0%llo", cz);
	if (p->n_name[0] != '\0')
		printf("+%s", p->n_name);
}

static void
putcond(NODE *p)
{               
	char *c = 0; /* Initialize to avoid uninitialized warning */

	switch (p->n_op) {
	case EQ: c = "e"; break;
	case NE: c = "n"; break;
	case LE: c = "le"; break;
	case LT: c = "l"; break;
	case GT: c = "g"; break;
	case GE: c = "ge"; break;
	default:
		cerror("putcond");
	}
	printf("%s", c);
}

void
zzzcode(NODE *p, int c)
{
	NODE *l;
	CONSZ hval;

	switch (c) {
	case 'A': /* ildb right arg */
		adrput(stdout, p->n_left->n_left);
		break;

	case 'B': /* remove from stack after subroutine call */
		if (p->n_qual)
			printf("	subi %%17,0%o\n", p->n_qual);
		break;

	case 'C':
		constput(p);
		break;

	case 'D': /* Find out which type of const load insn to use */
		if (p->n_op != ICON)
			cerror("zzzcode not ICON");
		if (p->n_name[0] == '\0') {
			if ((getlval(p) <= 0777777) && (getlval(p) > 0))
				printf("movei");
			else if ((getlval(p) & 0777777) == 0)
				printf("hrlzi");
			else
				printf("move");
		} else
			printf("move");
		break;

	case 'E': /* Print correct constant expression */
		if (p->n_name[0] == '\0') {
			if ((getlval(p) <= 0777777) && (getlval(p) > 0)){
				printf("0%llo", getlval(p));
			} else if ((getlval(p) & 0777777) == 0) {
				printf("0%llo", getlval(p) >> 18);
			} else {
				if (getlval(p) < 0)
					printf("[ .long -0%llo]", -getlval(p));
				else
					printf("[ .long 0%llo]", getlval(p));
			}
		} else {
			if (getlval(p) == 0)
				printf("[ .long %s]", p->n_name);
			else
				printf("[ .long %s+0%llo]",
				    p->n_name, getlval(p));
		}
		break;

	case 'G': /* structure argument */
		printf("	addl %%17,0%o\n",
		    attr_find(p->n_ap, ATTR_P2STRUCT)->iarg(0)/(SZINT/SZCHAR));
		printf("	foo...\n");
		break;

	case 'P':
		p = getlr(p, 'R');
		/* FALLTHROUGH */
	case 'O':
		/*
		 * Print long long expression.
		 */
		hval = gethval(getlval(p));
		printf("[ .long 0%llo,0%llo", hval,
		    (getlval(p) & 0377777777777LL) | (hval & 0400000000000LL));
		if (p->n_name[0] != '\0')
			printf("+%s", p->n_name);
		printf(" ]");
		break;

	case 'F': /* Print an "opsimp" instruction based on its const type */
		hopcode(oneinstr(p->n_right) ? 'C' : 'R', p->n_op);
		break;

	case 'H': /* Print a small constant */
		p = p->n_right;
		printf("0%llo", getlval(p) & 0777777);
		break;

	case 'Q': /* two-param long long comparisions */
		twollcomp(p);
		break;

	case 'R': /* two-param conditionals */
		twocomp(p);
		break;

	case 'U':
		emitshort(p);
		break;
		
	case 'V':
		storeshort(p);
		break;

	case 'Z':
		ptrcomp(p);
		break;

	case 'a':
		imuli(p);
		break;

	case 'b':
		idivi(p);
		break;

	case 'c':
		xmovei(p);
		break;

	case 'd':
		printcon(p);
		break;

	case 'e':
		putcond(p);
		break;

	case 'g':
		if (p->n_right->n_op != OREG || getlval(p->n_right) != 0)
			comperr("bad Zg oreg");
		printf("%s", rnames[p->n_right->n_rval]);
		break;

#if 0
	case '1': /* double upput */
		p = getlr(p, '1');
		p->n_rval += 2;
		adrput(stdout, p);
		p->n_rval -= 2;
		break;
#endif

	case 'i': /* Write instruction for short load from name */
		l = getlr(p, 'L');
		printf("	h%cr%c %s,%s+" CONFMT "\n",
		    getlval(l) & 1 ? 'r' : 'l',
		    ISUNSIGNED(p->n_type) ? 'z' : 'e',
		    rnames[getlr(p, '1')->n_rval],
		    l->n_name, getlval(l) >> 1);
		break;

	default:
		cerror("zzzcode %c", c);
	}
}

#if 0
/* set up temporary registers */
void
setregs(void)
{
	fregs = 7;	/* 7 free regs on PDP10 (1-7) */
}
#endif

/*ARGSUSED*/
int
rewfld(NODE *p)
{
	return(1);
}

int
fldexpand(NODE *p, int cookie, char **cp)
{
	return 0;
}

int
flshape(NODE *p)
{
	register int o = p->n_op;

	return (o == REG || o == NAME || o == ICON ||
		(o == OREG && (!R2TEST(p->n_rval) || tlen(p) == 1)));
}

/* INTEMP shapes must not contain any temporary registers */
int
shtemp(NODE *p)
{
	return(0);
}

int
shumul(NODE *p, int order)
{
	register int o;

	if (x2debug) {
		printf("shumul(%p)\n", p);
//		eprint(p, 0, &val, &val);
	}

	o = p->n_op;
#if 0
	if (o == NAME || (o == OREG && !R2TEST(p->n_rval)) || o == ICON)
		return(STARNM);
#endif

#if 0
	if ((o == INCR) &&
	    (p->n_left->n_op == REG && p->n_right->n_op == ICON) &&
	    p->n_right->n_name[0] == '\0') {
		switch (p->n_type) {
			case CHAR|PTR:
			case UCHAR|PTR:
				o = 1;
				break;

			case SHORT|PTR:
			case USHORT|PTR:
				o = 2;
				break;

			case INT|PTR:
			case UNSIGNED|PTR:
			case LONG|PTR:
			case ULONG|PTR:
			case FLOAT|PTR:
				o = 4;
				break;

			case DOUBLE|PTR:
			case LONGLONG|PTR:
			case ULONGLONG|PTR:
				o = 8;
				break;

			default:
				if (ISPTR(p->n_type) &&
				     ISPTR(DECREF(p->n_type))) {
					o = 4;
					break;
				} else
					return(0);
		}
		return( 0);
	}
#endif
	return( SRNOPE );
}

void
adrcon(CONSZ val)
{
	cerror("adrcon: val %llo\n", val);
}

void
conput(FILE *fp, NODE *p)
{
	switch (p->n_op) {
	case ICON:
		if (getlval(p) != 0) {
			acon(fp, p);
			if (p->n_name[0] != '\0')
				fputc('+', fp);
		}
		if (p->n_name[0] != '\0')
			fprintf(fp, "%s", p->n_name);
		if (p->n_name[0] == '\0' && getlval(p) == 0)
			fputc('0', fp);
		return;

	case REG:
		fprintf(fp, "%s", rnames[p->n_rval]);
		return;

	default:
		cerror("illegal conput");
	}
}

/*ARGSUSED*/
void
insput(NODE *p)
{
	cerror("insput");
}

/*
 * Write out the upper address, like the upper register of a 2-register
 * reference, or the next memory location.
 */
void
upput(NODE *p, int size)
{

	size /= SZLONG;
	switch (p->n_op) {
	case REG:
		printf("%s", rnames[p->n_rval + size]);
		break;

	case NAME:
	case OREG:
		setlval(p, getlval(p) + size);
		adrput(stdout, p);
		setlval(p, getlval(p) - size);
		break;
	case ICON:
		printf(CONFMT, getlval(p) >> (36 * size));
		break;
	default:
		cerror("upput bad op %d size %d", p->n_op, size);
	}
}

void
adrput(FILE *fp, NODE *p)
{
	int r;
	/* output an address, with offsets, from p */

	if (p->n_op == FLD)
		p = p->n_left;

	switch (p->n_op) {

	case NAME:
		if (p->n_name[0] != '\0')
			fputs(p->n_name, fp);
		if (getlval(p) != 0)
			fprintf(fp, "+" CONFMT, getlval(p) & 0777777777777LL);
		return;

	case OREG:
		r = p->n_rval;
#if 0
		if (R2TEST(r)) { /* double indexing */
			register int flags;

			flags = R2UPK3(r);
			if (flags & 1)
				putc('*', fp);
			if (flags & 4)
				putc('-', fp);
			if (p->n_lval != 0 || p->n_name[0] != '\0')
				acon(p);
			if (R2UPK1(r) != 100)
				printf("(%s)", rnames[R2UPK1(r)]);
			if (flags & 2)
				putchar('+');
			printf("[%s]", rnames[R2UPK2(r)]);
			return;
		}
#endif
		if (R2TEST(r))
			cerror("adrput: unwanted double indexing: r %o", r);
		if (getlval(p) != FPREG && getlval(p) < 0 && p->n_name[0]) {
			fprintf(fp, "%s", p->n_name);
			acon(fp, p);
			fprintf(fp, "(%s)", rnames[p->n_rval]);
			return;
		}
		if (getlval(p) < 0 && p->n_rval == FPREG && offarg) {
			setlval(p, getlval(p) - (offarg-2));
			acon(fp, p);
			setlval(p, getlval(p) + (offarg-2));
		} else if (getlval(p) != 0)
			acon(fp, p);
		if (p->n_name[0] != '\0')
			fprintf(fp, "%s%s", getlval(p) ? "+" : "", p->n_name);
		if (getlval(p) > 0 && p->n_rval == FPREG && offlab)
			fprintf(fp, "+" LABFMT, offlab);
		if (getlval(p) < 0 && p->n_rval == FPREG && offarg)
			fprintf(fp, "(017)");
		else
			fprintf(fp, "(%s)", rnames[p->n_rval]);
		return;
	case ICON:
		/* addressable value of the constant */
		if (getlval(p) > 0) {
			acon(fp, p);
			if (p->n_name[0] != '\0')
				putc('+', fp);
		}
		if (p->n_name[0] != '\0')
			fprintf(fp, "%s", p->n_name);
		if (getlval(p) < 0) 
			acon(fp, p);
		if (p->n_name[0] == '\0' && getlval(p) == 0)
			putc('0', fp);
		return;

	case REG:
		fputs(rnames[p->n_rval], fp);
		return;

	default:
		cerror("illegal address, op %d", p->n_op);
		return;

	}
}

/*
 * print out a constant
 */
void
acon(FILE *fp, NODE *p)
{
	if (getlval(p) < 0 && getlval(p) > -0777777777777LL)
		fprintf(fp, "-" CONFMT, -getlval(p));
	else
		fprintf(fp, CONFMT, getlval(p));
}

/*   printf conditional and unconditional branches */
void
cbgen(int o,int lab)
{
}

/*
 * Do some local optimizations that must be done after optim is called.
 */
static void
optim2(NODE *p, void *arg)
{
	int op = p->n_op;
	int m, ml;
	NODE *l;

	/* Remove redundant PCONV's */
	if (op == PCONV) {
		l = p->n_left;
		m = BTYPE(p->n_type);
		ml = BTYPE(l->n_type);
		if ((m == INT || m == LONG || m == LONGLONG || m == FLOAT ||
		    m == DOUBLE || m == STRTY || m == UNIONTY ||
		    m == UNSIGNED || m == ULONG || m == ULONGLONG) &&
		    (ml == INT || ml == LONG || ml == LONGLONG || ml == FLOAT ||
		    ml == DOUBLE || ml == STRTY || ml == UNIONTY || 
		    ml == UNSIGNED || ml == ULONG ||
		    ml == ULONGLONG) && ISPTR(l->n_type)) {
			*p = *l;
			nfree(l);
			op = p->n_op;
		} else
		if (ISPTR(DECREF(p->n_type)) &&
		    (l->n_type == INCREF(STRTY))) {
			*p = *l;
			nfree(l);
			op = p->n_op;
		} else
		if (ISPTR(DECREF(l->n_type)) &&
		    (p->n_type == INCREF(INT) ||
		    p->n_type == INCREF(STRTY) ||
		    p->n_type == INCREF(UNSIGNED))) {
			*p = *l;
			nfree(l);
			op = p->n_op;
		}

	}
	/* Add constands, similar to the one in optim() */
	if (op == PLUS && p->n_right->n_op == ICON) {
		l = p->n_left;
		if (l->n_op == PLUS && l->n_right->n_op == ICON &&
		    (p->n_right->n_name[0] == '\0' ||
		     l->n_right->n_name[0] == '\0')) {
			setlval(l, getlval(l) + getlval(p->n_right));
			if (l->n_right->n_name[0] == '\0')
				l->n_right->n_name = p->n_right->n_name;
			nfree(p->n_right);
			*p = *l;
			nfree(l);
		}
	}

	/* Convert "PTR undef" (void *) to "PTR uchar" */
	/* TODO: This logic should ideally be in machine-independent code */
	if (BTYPE(p->n_type) == VOID)
		p->n_type = (p->n_type & ~BTMASK) | UCHAR;
	if (op == ICON) {
		if ((p->n_type == (PTR|CHAR) || p->n_type == (PTR|UCHAR))
		    && getlval(p) == 0 && p->n_name[0] != '\0')
			setlval(p, 0700000000000LL);
		if ((p->n_type == (PTR|SHORT) || p->n_type == (PTR|USHORT))
		    && getlval(p) == 0 && p->n_name[0] != '\0')
			setlval(p, 0750000000000LL);
	}
	if (op == MINUS) {
		if ((p->n_left->n_type == (PTR|CHAR) ||
		    p->n_left->n_type == (PTR|UCHAR)) &&
		    (p->n_right->n_type == (PTR|CHAR) ||
		    p->n_right->n_type == (PTR|UCHAR))) {
			l = talloc();
			l->n_op = SCONV;
			l->n_type = INT;
			l->n_left = p->n_right;
			p->n_right = l;
			l = talloc();
			l->n_op = SCONV;
			l->n_type = INT;
			l->n_left = p->n_left;
			p->n_left = l;
		}
	}
}

void
myreader(struct interpass *ipole)
{
	struct interpass *ip;

	DLIST_FOREACH(ip, ipole, qelem) {
		if (ip->type != IP_NODE)
			continue;
		walkf(ip->ip_node, optim2, 0);
	}

	if (x2debug) {
		printf("myreader final tree:\n");
		printip(ipole);
	}
}

/*
 * Remove some PCONVs after OREGs are created.
 */
static void
pconv2(NODE *p, void *arg)
{
	NODE *q;

	if (p->n_op == PLUS) {
		if (p->n_type == (PTR|SHORT) || p->n_type == (PTR|USHORT)) {
			if (p->n_right->n_op != ICON)
				return;
			if (p->n_left->n_op != PCONV)
				return;
			if (p->n_left->n_left->n_op != OREG)
				return;
			q = p->n_left->n_left;
			nfree(p->n_left);
			p->n_left = q;
			/*
			 * This will be converted to another OREG later.
			 */
		}
	}
}

void
mycanon(NODE *p)
{
	walkf(p, pconv2, 0);
}

/*
 * Remove last goto.
 */
void
myoptim(struct interpass *ip)
{
}

/*
 * Return a class suitable for a specific type.
 */
int
gclass(TWORD t)
{
	return (szty(t) == 2 ? CLASSB : CLASSA);
}

static int
argsiz(NODE *p)
{
	TWORD t = p->n_type;

	if (t == STRTY || t == UNIONTY)
		return attr_find(p->n_ap, ATTR_P2STRUCT)->iarg(0)/(SZINT/SZCHAR);
	return szty(t);
}

/*
 * Calculate argument sizes.
 */
void
lastcall(NODE *p)
{
        NODE *op = p;
        int size = 0;

        p->n_qual = 0;
        if (p->n_op != CALL && p->n_op != FORTCALL && p->n_op != STCALL)
                return;
        for (p = p->n_right; p->n_op == CM; p = p->n_left)
		if (p->n_right->n_op != ASSIGN)
                	size += argsiz(p->n_right);
	if (p->n_op != ASSIGN)
        	size += argsiz(p);
        op->n_qual = size; /* Store size in n_qual for later use */
}

void
rmove(int s, int d, TWORD t)
{
	printf("	%smove %s,%s\n",
	    (s > 017 ? "d" : ""), rnames[d], rnames[s]);
}

/*
 * For class c, find worst-case displacement of the number of
 * registers in the array r[] indexed by class.
 */
int
COLORMAP(int c, int *r)
{
	int num;

	switch (c) {
	case CLASSA:
		/* there are 13 classa, so min 6 classb are needed to block */
		num = r[CLASSB] * 2;
		num += r[CLASSA];
		return num < 13;
	case CLASSB:
		/* 7 classa may block all classb */
		num = r[CLASSB] + r[CLASSA];
		return num < 7;
	}
	comperr("COLORMAP");
	return 0; /* Return value not used */
}

/*
 * Target-dependent command-line options.
 */
/*
 * Handle machine-specific command-line flags.
 * Supports:
 *   -masm=<format>   - Set assembly syntax (gnu, midas)
 *   -mabi=<format>   - Set ABI/object format (elf, macho, pecoff, none)
 *   -m64             - Enable power-of-2 type mode (8/16/32/64-bit types)
 *   -m36             - Use native PDP-10 types (9/18/36-bit, default)
 */
void
mflags(char *str)
{
	if (strncmp(str, "asm=", 4) == 0) {
		str += 4;
		if (strcmp(str, "gnu") == 0)
			pdp10_asmfmt = PDP10_ASM_GNU;
		else if (strcmp(str, "midas") == 0)
			pdp10_asmfmt = PDP10_ASM_MIDAS;
		else
			fprintf(stderr, "pcc: unknown assembly format '%s' (use 'gnu' or 'midas')\n", str);
	} else if (strncmp(str, "abi=", 4) == 0) {
		str += 4;
		if (strcmp(str, "elf") == 0)
			pdp10_abi = PDP10_ABI_ELF;
		else if (strcmp(str, "macho") == 0)
			pdp10_abi = PDP10_ABI_MACHO;
		else if (strcmp(str, "pecoff") == 0 || strcmp(str, "pe") == 0)
			pdp10_abi = PDP10_ABI_PECOFF;
		else if (strcmp(str, "none") == 0)
			pdp10_abi = PDP10_ABI_NONE;
		else
			fprintf(stderr, "pcc: unknown ABI '%s' (use 'elf', 'macho', 'pecoff', or 'none')\n", str);
	} else if (strcmp(str, "64") == 0 || strcmp(str, "pow2") == 0) {
		/* -m64 enables power-of-2 types (pow2 kept for compatibility) */
		pdp10_pow2 = 1;

		/* Initialize runtime type system with POW2 sizes */
		pdp10_init_runtime_types();

#ifdef PDP10_POW2
		/* Compiler was built with POW2 support */
		fprintf(stderr, "pcc: -m64 mode enabled\n");
		fprintf(stderr, "pcc: Using power-of-2 types (8/16/32/64 bit) with VAX FP format\n");
		fprintf(stderr, "pcc: Compiler built with -DPDP10_POW2: Full support enabled\n");
#else
		/* Compiler was built for native mode - FP format mismatch! */
		fprintf(stderr, "pcc: -m64 mode enabled (RUNTIME mode)\n");
		fprintf(stderr, "pcc: Using power-of-2 types (8/16/32/64 bit)\n");
		fprintf(stderr, "pcc: WARNING: Floating-point format limitation:\n");
		fprintf(stderr, "pcc:   - Compiler built WITHOUT -DPDP10_POW2\n");
		fprintf(stderr, "pcc:   - Will use PDP-10 FP format (36/72-bit) instead of VAX!\n");
		fprintf(stderr, "pcc:   - Avoid floating-point operations in this mode\n");
		fprintf(stderr, "pcc: Type sizes, struct layouts, and array indexing: FULLY WORKING\n");
		fprintf(stderr, "pcc: For full POW2 support with VAX FP: Recompile with -DPDP10_POW2\n");
#endif
	} else if (strcmp(str, "36") == 0) {
		/* -m36 explicitly selects native mode (already the default) */
		pdp10_pow2 = 0;
		pdp10_init_runtime_types();
		fprintf(stderr, "pcc: -m36 mode enabled (native PDP-10 types)\n");
		fprintf(stderr, "pcc: Using native 9/18/36-bit types with PDP-10 FP format\n");
	} else {
		fprintf(stderr, "pcc: unknown PDP-10 option '%s'\n", str);
	}
}

/*
 * Check for other names of the xasm constraints registers.
 * Map register names to register numbers for inline assembly constraints.
 */
int
xasmconstregs(char *s)
{
	int i;
	char buf[4];

	/* Match register names: r0-r17 or just 0-17 */
	for (i = 0; i < 18; i++) {
		if (strcmp(&rnames[i][1], s) == 0)  /* Skip '%' prefix */
			return i;
		/* Also allow just the number */
		snprintf(buf, sizeof(buf), "%d", i);
		if (strcmp(buf, s) == 0)
			return i;
	}
	return -1;
}

/*
 * Do something target-dependent for xasm arguments.
 * Supposed to find target-specific constraints and rewrite them.
 */
int
myxasm(struct interpass *ip, NODE *p)
{
	return 0;
}
