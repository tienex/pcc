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


# include "pass1.h"
# include "x86asm.h"

#ifdef LANG_CXX
#define	p1listf	listf
#define	p1tfree tfree
#else
#define	NODE P1ND
#define	talloc p1alloc
#endif

x86asm_ctx_t *asm_ctx = NULL;

/*
 * Convert ASM_FORMAT string to x86asm_format_t enum
 */
static x86asm_format_t
get_asm_format(void)
{
#ifdef ASM_FORMAT
	if (strcmp(ASM_FORMAT, "gnu-as") == 0)
		return ASM_FMT_GNU_AS;
	if (strcmp(ASM_FORMAT, "apple-as") == 0)
		return ASM_FMT_APPLE_AS;
	if (strcmp(ASM_FORMAT, "nasm") == 0)
		return ASM_FMT_NASM;
	if (strcmp(ASM_FORMAT, "yasm") == 0)
		return ASM_FMT_YASM;
	if (strcmp(ASM_FORMAT, "fasm") == 0)
		return ASM_FMT_FASM;
	if (strcmp(ASM_FORMAT, "masm") == 0)
		return ASM_FMT_MASM;
	if (strcmp(ASM_FORMAT, "jwasm") == 0)
		return ASM_FMT_JWASM;
	if (strcmp(ASM_FORMAT, "uasm") == 0)
		return ASM_FMT_UASM;
	if (strcmp(ASM_FORMAT, "tasm") == 0)
		return ASM_FMT_TASM;
	if (strcmp(ASM_FORMAT, "wasm") == 0)
		return ASM_FMT_WASM;
#endif
	/* Default based on ABI if ASM_FORMAT not set */
#if defined(MACHOABI)
	return ASM_FMT_APPLE_AS;
#else
	return ASM_FMT_GNU_AS;
#endif
}

/*
 * Print out assembler segment name.
 */
void
setseg(int seg, char *name)
{
	x86asm_segment_t segment;

	if (!asm_ctx) return;

	switch (seg) {
	case PROG: segment = SEG_TEXT; break;
	case DATA:
	case LDATA: segment = SEG_DATA; break;
	case UDATA: return; /* BSS handled separately */
#ifdef MACHOABI
	case PICLDATA:
	case PICDATA: segment = SEG_PIC_DATA; break;
	case PICRDATA: segment = SEG_PIC_RODATA; break;
	case STRNG: segment = SEG_CSTRING; break;
	case RDATA: segment = SEG_CONST; break;
#else
	case PICLDATA: segment = SEG_PIC_LOCAL; break;
	case PICDATA: segment = SEG_PIC_DATA; break;
	case PICRDATA: segment = SEG_PIC_RODATA; break;
	case STRNG:
#ifdef AOUTABI
	case RDATA: segment = SEG_DATA; break;
#else
	case RDATA: segment = SEG_RODATA; break;
#endif
#endif
	case TLSDATA: segment = SEG_TDATA; break;
	case TLSUDATA: segment = SEG_TBSS; break;
#ifdef MACHOABI
	case CTORS:
		segment = SEG_MOD_INIT_FUNC;
		x86asm_segment(asm_ctx, segment, NULL);
		x86asm_align(asm_ctx, 2);
		return;
	case DTORS:
		segment = SEG_MOD_TERM_FUNC;
		x86asm_segment(asm_ctx, segment, NULL);
		x86asm_align(asm_ctx, 2);
		return;
#else
	case CTORS: segment = SEG_CTORS; break;
	case DTORS: segment = SEG_DTORS; break;
#endif
	case NMSEG:
		/* Custom section - use the name parameter */
		x86asm_segment(asm_ctx, SEG_CUSTOM, name);
		return;
	default:
		return;
	}
	x86asm_segment(asm_ctx, segment, NULL);
}

#ifdef MACHOABI
void
defalign(int al)
{
	if (asm_ctx)
		x86asm_align(asm_ctx, ispow2(al/ALCHAR));
}
#endif

/*
 * Define everything needed to print out some data (or text).
 * This means segment, alignment, visibility, etc.
 */
void
defloc(struct symtab *sp)
{
	char *name;
	char labelbuf[64];

	if (!asm_ctx) return;

	name = getexname(sp);

	/* Emit label */
	if (sp->slevel == 0) {
		x86asm_label(asm_ctx, name, sp->sclass == EXTDEF);
	} else {
		snprintf(labelbuf, sizeof(labelbuf), LABFMT, sp->soffset);
		x86asm_label(asm_ctx, labelbuf, 0);
	}

#if defined(ELFABI)
	/* Emit type directive */
	if (sp->sclass == EXTDEF) {
		x86asm_symbol_type(asm_ctx, name,
		    ISFTN(sp->stype) ? SYMBOL_TYPE_FUNCTION : SYMBOL_TYPE_OBJECT);
	}

	/* Emit size directive for data objects */
	if (!ISFTN(sp->stype)) {
		size_t sz = (size_t)tsize(sp->stype, sp->sdf, sp->sap)/SZCHAR;
		if (sp->slevel == 0) {
			x86asm_symbol_size(asm_ctx, name, sz);
		} else {
			snprintf(labelbuf, sizeof(labelbuf), LABFMT, sp->soffset);
			x86asm_symbol_size(asm_ctx, labelbuf, sz);
		}
	}
#endif
}

int structrettemp;

/*
 * code for the end of a function
 * deals with struct return here
 */
void
efcode(void)
{
	extern int gotnr;
	NODE *p, *q;
	int sz;

	gotnr = 0;	/* new number for next fun */
	if (cftnsp->stype != STRTY+FTN && cftnsp->stype != UNIONTY+FTN)
		return;

	/* struct return for small structs */
	sz = (int)tsize(BTYPE(cftnsp->stype), cftnsp->sdf, cftnsp->sap);
#if defined(os_openbsd)
	if (sz == SZCHAR || sz == SZSHORT || sz == SZINT || sz == SZLONGLONG) {
#else
	if (sz == SZLONGLONG && attr_find(cftnsp->sap, ATTR_COMPLEX)) {
#endif
		/* Pointer to struct in eax */
		if (sz == SZLONGLONG) {
			q = block(OREG, NIL, NIL, INT, 0, 0);
			slval(q, 4);
			p = block(REG, NIL, NIL, INT, 0, 0);
			p->n_rval = EDX;
			ecomp(buildtree(ASSIGN, p, q));
		}
		if (sz < SZSHORT) sz = CHAR;
		else if (sz > SZSHORT) sz = INT;
		else sz = SHORT;
		q = block(OREG, NIL, NIL, sz, 0, 0);
		if (sz < SZINT)
			q = cast(q, INT, 0);
		p = block(REG, NIL, NIL, INT, 0, 0);
		p = (buildtree(ASSIGN, p, q));
		ecomp(p);
		return;
	}

	/* Create struct assignment */
	q = tempnode(structrettemp, PTR+STRTY, 0, cftnsp->sap);
	q = buildtree(UMUL, q, NIL);
	p = block(REG, NIL, NIL, PTR+STRTY, 0, cftnsp->sap);
	p = buildtree(UMUL, p, NIL);
	p = buildtree(ASSIGN, q, p);
	ecomp(p);

	/* put hidden arg in eax on return */
	q = tempnode(structrettemp, INT, 0, 0);
	p = block(REG, NIL, NIL, INT, 0, 0);
	regno(p) = EAX;
	ecomp(buildtree(ASSIGN, p, q));
}

#ifdef GCC_COMPAT
static TWORD reparegs[] = { EAX, EDX, ECX };
static TWORD fastregs[] = { ECX, EDX };
#endif
static TWORD longregs[] = { EAXEDX, EDXECX };
#ifdef NOBREGS
static TWORD charregs[] = { EAX, EDX, ECX };
#else
static TWORD charregs[] = { AL, DL, CL };
#endif
static TWORD *regpregs;

/*
 * code for the beginning of a function; a is an array of
 * indices in symtab for the arguments; n is the number
 *
 * Classifying args on i386; not simple:
 * - Args may be on stack or in registers (regparm)
 * - There may be a hidden first arg, unless OpenBSD struct return.
 * - Regparm syntax is not well documented.
 * - There may be stdcall functions, where the called function pops stack
 * - ...probably more
 */
void
bfcode(struct symtab **sp, int cnt)
{
	extern int argstacksize;
#ifdef GCC_COMPAT
	struct attr *ap;
#endif
	struct symtab *sp2;
	extern int gotnr;
	NODE *n, *p;
	int i, regparmarg;
	int argbase, nrarg, sz;

	/* Take care of PIC stuff first */
        if (kflag) {
#define STL     200
                char *str = xmalloc(STL);
#if !defined(MACHOABI)
                int l = getlab();
#else
                char *name;
#endif

                /* Generate extended assembler for PIC prolog */
                p = tempnode(0, INT, 0, 0);
                gotnr = regno(p);
                p = block(XARG, p, NIL, INT, 0, 0);
                p->n_name = "=g";
                p = block(XASM, p, bcon(0), INT, 0, 0);

#if defined(MACHOABI)
                /* Get SO name from attribute or fall back to sname */
                {
                        struct attr *ap;
                        name = (ap = attr_find(cftnsp->sap, ATTR_SONAME)) ?
                            ap->sarg(0) : cftnsp->sname;
                }
                if (snprintf(str, STL, "call L%s$pb\nL%s$pb:\n\tpopl %%0\n",
                    name, name) >= STL)
                        cerror("bfcode");
#else
                if (snprintf(str, STL,
                    "call " LABFMT ";" LABFMT ":;\tpopl %%0;"
                    "\taddl $_GLOBAL_OFFSET_TABLE_+[.-" LABFMT "], %%0;",
                    l, l, l) >= STL)
                        cerror("bfcode");
#endif
                p->n_name = addstring(str);
                p->n_right->n_type = STRTY;
		free(str);
                ecomp(p);
        }

	argbase = ARGINIT;
	nrarg = regparmarg = 0;
	argstacksize = 0;

#ifdef GCC_COMPAT
	regpregs = reparegs;
        if ((ap = attr_find(cftnsp->sap, GCC_ATYP_REGPARM)))
                regparmarg = ap->iarg(0);
        if ((ap = attr_find(cftnsp->sap, GCC_ATYP_FASTCALL)))
                regparmarg = 2, regpregs = fastregs;
#endif

	/* Function returns struct, create return arg node */
	if (cftnsp->stype == STRTY+FTN || cftnsp->stype == UNIONTY+FTN) {
		sz = (int)tsize(BTYPE(cftnsp->stype), cftnsp->sdf, cftnsp->sap);
#if defined(os_openbsd)
		/* OpenBSD uses non-standard return for small structs */
		if (sz > SZLONGLONG)
#else
		if (sz != SZLONGLONG ||
		    attr_find(cftnsp->sap, ATTR_COMPLEX) == 0)
#endif
		{
			if (regparmarg) {
				n = block(REG, 0, 0, INT, 0, 0);
				regno(n) = regpregs[nrarg++];
			} else {
				n = block(OREG, 0, 0, INT, 0, 0);
				slval(n, argbase/SZCHAR);
				argbase += SZINT;
				regno(n) = FPREG;
				argstacksize += 4; /* popped by callee */
			}
			p = tempnode(0, INT, 0, 0);
			structrettemp = regno(p);
			p = buildtree(ASSIGN, p, n);
			ecomp(p);
		}
	}

	/*
	 * Find where all params are so that they end up at the right place.
	 * At the same time recalculate their arg offset on stack.
	 * We also get the "pop size" for stdcall.
	 */
	for (i = 0; i < cnt; i++) {
		sp2 = sp[i];
		sz = (int)tsize(sp2->stype, sp2->sdf, sp2->sap);

		SETOFF(sz, SZINT);

		if (cisreg(sp2->stype) == 0 ||
		    ((regparmarg - nrarg) * SZINT < sz)) {	/* not in reg */
			sp2->soffset = argbase;
			argbase += sz;
			nrarg = regparmarg;	/* no more in reg either */
		} else {					/* in reg */
			sp2->soffset = regpregs[nrarg];
			nrarg += sz/SZINT;
			sp2->sclass = REGISTER;
		}
	}

	/*
	 * Now (argbase - ARGINIT) is used space on stack.
	 * Move (if necessary) the args to something new.
	 */
	for (i = 0; i < cnt; i++) {
		int reg, j;

		sp2 = sp[i];

		if ((ISSOU(sp2->stype) && sp2->sclass == REGISTER) ||
		    (sp2->sclass == REGISTER && xtemps == 0)) {
			/* must move to stack */
			sz = (int)tsize(sp2->stype, sp2->sdf, sp2->sap);
			SETOFF(sz, SZINT);
			SETOFF(autooff, SZINT);
			reg = sp2->soffset;
			sp2->sclass = AUTO;
			sp2->soffset = NOOFFSET;
			oalloc(sp2, &autooff);
                        for (j = 0; j < sz/SZCHAR; j += 4) {
                                p = block(OREG, 0, 0, INT, 0, 0);
                                slval(p, sp2->soffset/SZCHAR + j);
                                regno(p) = FPREG;
                                n = block(REG, 0, 0, INT, 0, 0);
                                regno(n) = regpregs[reg++];
                                p = block(ASSIGN, p, n, INT, 0, 0);
                                ecomp(p);
                        }
		} else if (cisreg(sp2->stype) && !ISSOU(sp2->stype) &&
		    ((cqual(sp2->stype, sp2->squal) & VOL) == 0) && xtemps) {
			/* just put rest in temps */
			if (sp2->sclass == REGISTER) {
				n = block(REG, 0, 0, sp2->stype,
				    sp2->sdf, sp2->sap);
				if (ISLONGLONG(sp2->stype))
					regno(n) = longregs[sp2->soffset];
				else if (DEUNSIGN(sp2->stype) == CHAR ||
				    sp2->stype == BOOL)
					regno(n) = charregs[sp2->soffset];
				else
					regno(n) = regpregs[sp2->soffset];
			} else {
                                n = block(OREG, 0, 0, sp2->stype,
				    sp2->sdf, sp2->sap);
                                slval(n, sp2->soffset/SZCHAR);
                                regno(n) = FPREG;
			}
			p = tempnode(0, sp2->stype, sp2->sdf, sp2->sap);
			sp2->soffset = regno(p);
			sp2->sflags |= STNODE;
			n = buildtree(ASSIGN, p, n);
			ecomp(n);
		}
	}

        if (attr_find(cftnsp->sap, GCC_ATYP_STDCALL)) {
#ifdef PECOFFABI
                char buf[256];
                char *name;
#endif
		/* XXX interaction STDCALL and struct return? */
		argstacksize += (argbase - ARGINIT)/SZCHAR;
#ifdef PECOFFABI
                /*
                 * mangle name in symbol table as a callee.
                 */
                {
                        struct attr *ap;
                        /* Get existing soname or use exname(sname) */
                        if ((ap = attr_find(cftnsp->sap, ATTR_SONAME)) != NULL)
                                name = ap->sarg(0);
                        else
                                name = exname(cftnsp->sname);
                        /* Create mangled name */
                        snprintf(buf, 256, "%s@%d", name, argstacksize);
                        /* Store as ATTR_SONAME */
                        if (ap == NULL) {
                                cftnsp->sap = attr_add(cftnsp->sap,
                                    attr_new(ATTR_SONAME, 1));
                                ap = attr_find(cftnsp->sap, ATTR_SONAME);
                        }
                        ap->sarg(0) = addname(buf);
                }
#endif
        }

}

#if defined(MACHOABI)
struct stub stublist;
struct stub nlplist;
#endif

/* called just before final exit */
/* flag is 1 if errors, 0 if none */
void
ejobcode(int flag)
{
#if defined(MACHOABI)
	/*
	 * iterate over the stublist and output the PIC stubs
`	 */
	if (kflag && asm_ctx) {
		struct stub *p;
		char labelbuf[256];

		DLIST_FOREACH(p, &stublist, link) {
			x86asm_directive(asm_ctx, ".section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5", NULL);
			snprintf(labelbuf, sizeof(labelbuf), "L%s$stub", p->name);
			x86asm_label(asm_ctx, labelbuf, 0);
			x86asm_indirect_symbol(asm_ctx, p->name);
			x86asm_directive(asm_ctx, "hlt ; hlt ; hlt ; hlt ; hlt", NULL);
			x86asm_directive(asm_ctx, ".subsections_via_symbols", NULL);
		}

		x86asm_directive(asm_ctx, ".section __IMPORT,__pointers,non_lazy_symbol_pointers", NULL);
		DLIST_FOREACH(p, &nlplist, link) {
			snprintf(labelbuf, sizeof(labelbuf), "L%s$non_lazy_ptr", p->name);
			x86asm_label(asm_ctx, labelbuf, 0);
			x86asm_indirect_symbol(asm_ctx, p->name);
			x86asm_directive(asm_ctx, ".long 0", NULL);
	        }

	}
#endif

	if (asm_ctx) {
		char ident_str[256];
		snprintf(ident_str, sizeof(ident_str), "PCC: %s", VERSSTR);
		x86asm_ident(asm_ctx, ident_str);
		x86asm_destroy(asm_ctx);
		asm_ctx = NULL;
	}
}

void
bjobcode(void)
{
#ifdef os_sunos
	astypnames[SHORT] = astypnames[USHORT] = "\t.2byte";
#endif
	astypnames[INT] = astypnames[UNSIGNED] = "\t.long";
#if defined(MACHOABI)
	DLIST_INIT(&stublist, link);
	DLIST_INIT(&nlplist, link);
#endif
#if defined(__GNUC__) || defined(__PCC__)
	/* Be sure that the compiler uses full x87 */
	/* XXX cross-compiling will fail here */
	int fcw;
	__asm("fstcw (%0)" : : "r"(&fcw));
	fcw |= 0x300;
	__asm("fldcw (%0)" : : "r"(&fcw));
#endif

	/* Initialize x86asm context for 32-bit mode (i386) */
	asm_ctx = x86asm_create(get_asm_format(), stdout, 32);
}

/*
 * Convert FUNARG to assign in case of regparm.
 */
static int regcvt, rparg, fcall;
static void
addreg(NODE *p)
{
	TWORD t;
	NODE *q;
	int sz, r;

	sz = (int)tsize(p->n_type, p->n_df, p->n_ap)/SZCHAR;
	sz = (sz + 3) >> 2;	/* sz in regs */
	if ((regcvt+sz) > rparg) {
		regcvt = rparg;
		return;
	}
	if (sz > 2)
		uerror("cannot put struct in 3 regs (yet)");

	if (sz == 2)
		r = regcvt == 0 ? EAXEDX : EDXECX;
	else if (fcall)
		r = regcvt == 0 ? ECX : EDX;
	else
		r = regcvt == 0 ? EAX : regcvt == 1 ? EDX : ECX;

	if (p->n_op == FUNARG) {
		/* at most 2 regs */
		if (p->n_type < INT) {
			p->n_left = ccast(p->n_left, INT, 0, 0, 0);
			p->n_type = INT;
		}

		p->n_op = ASSIGN;
		p->n_right = p->n_left;
	} else if (p->n_op == STARG) {
		/* convert to ptr, put in reg */
		q = p->n_left;
		t = sz == 2 ? LONGLONG : INT;
		q = cast(q, INCREF(t), 0);
		q = buildtree(UMUL, q, NIL);
		p->n_op = ASSIGN;
		p->n_type = t;
		p->n_right = q;
	} else
		cerror("addreg");
	p->n_left = block(REG, 0, 0, p->n_type, 0, 0);
	regno(p->n_left) = r;
	regcvt += sz;
}

/*
 * Called with a function call with arguments as argument.
 * This is done early in buildtree() and only done once.
 * Returns p.
 */
NODE *
funcode(NODE *p)
{
	extern int gotnr;
	struct attr *ap;
	NODE *r, *l;
	TWORD t = DECREF(DECREF(p->n_left->n_type));
	int stcall;

	stcall = ISSOU(t);
	/*
	 * We may have to prepend:
	 * - Hidden arg0 for struct return (in reg or on stack).
	 * - ebx in case of PIC code.
	 */

	/* Fix function call arguments. On x86, just add funarg */
	for (r = p->n_right; r->n_op == CM; r = r->n_left) {
		if (r->n_right->n_op != STARG)
			r->n_right = block(FUNARG, r->n_right, NIL,
			    r->n_right->n_type, r->n_right->n_df,
			    r->n_right->n_ap);
	}
	if (r->n_op != STARG) {
		l = talloc();
		*l = *r;
		r->n_op = FUNARG;
		r->n_left = l;
		r->n_type = l->n_type;
	}
#ifdef os_openbsd
	if (stcall && (ap = strattr(p->n_left->n_ap)) &&
	    ap->amsize != SZCHAR && ap->amsize != SZSHORT &&
	    ap->amsize != SZINT && ap->amsize != SZLONGLONG)
#else
	if (stcall &&
	    (attr_find(p->n_left->n_ap, ATTR_COMPLEX) == 0 ||
	     ((ap = strattr(p->n_left->n_ap)) && ap->amsize > SZLONGLONG)))
#endif
	{
		/* Prepend a placeholder for struct address. */
		/* Use EBP, can never show up under normal circumstances */
		l = talloc();
		*l = *r;
		r->n_op = CM;
		r->n_right = l;
		r->n_type = INT;
		l = block(REG, 0, 0, INCREF(VOID), 0, 0);
		regno(l) = EBP;
		l = block(FUNARG, l, 0, INCREF(VOID), 0, 0);
		r->n_left = l;
	}

#ifdef GCC_COMPAT
	fcall = 0;
	if ((ap = attr_find(p->n_left->n_ap, GCC_ATYP_REGPARM)))
		rparg = ap->iarg(0);
	else if ((ap = attr_find(p->n_left->n_ap, GCC_ATYP_FASTCALL)))
		fcall = rparg = 2;
	else
#endif
		rparg = 0;

	regcvt = 0;
	if (rparg)
		p1listf(p->n_right, addreg);

	if (kflag == 0)
		return p;

#if defined(ELFABI)
	/* Create an ASSIGN node for ebx */
	l = block(REG, NIL, NIL, INT, 0, 0);
	l->n_rval = EBX;
	l = buildtree(ASSIGN, l, tempnode(gotnr, INT, 0, 0));
	if (p->n_right->n_op != CM) {
		p->n_right = block(CM, l, p->n_right, INT, 0, 0);
	} else {
		for (r = p->n_right; r->n_left->n_op == CM; r = r->n_left)
			;
		r->n_left = block(CM, l, r->n_left, INT, 0, 0);
	}
#endif
	return p;
}

/* fix up type of field p */
void
fldty(struct symtab *p)
{
}

/*
 * XXX - fix genswitch.
 */
int
mygenswitch(int num, TWORD type, struct swents **p, int n)
{
	return 0;
}

NODE *	
builtin_return_address(const struct bitable *bt, NODE *a)
{	
	int nframes;
	NODE *f; 
	
	if (a->n_op != ICON)
		goto bad;

	nframes = (int)glval(a);
  
	p1tfree(a);	
			
	f = block(REG, NIL, NIL, PTR+VOID, 0, 0);
	regno(f) = FPREG;
 
	while (nframes--)
		f = block(UMUL, f, NIL, PTR+VOID, 0, 0);
				    
	f = block(PLUS, f, bcon(4), INCREF(PTR+VOID), 0, 0);
	f = buildtree(UMUL, f, NIL);	
   
	return f;
bad:						
	uerror("bad argument to __builtin_return_address");
	return bcon(0);
}

NODE *
builtin_frame_address(const struct bitable *bt, NODE *a)
{
	int nframes;
	NODE *f;

	if (a->n_op != ICON)
		goto bad;

	nframes = (int)glval(a);

	p1tfree(a);

	f = block(REG, NIL, NIL, PTR+VOID, 0, 0);
	regno(f) = FPREG;

	while (nframes--)
		f = block(UMUL, f, NIL, PTR+VOID, 0, 0);

	return f;
bad:
	uerror("bad argument to __builtin_frame_address");
	return bcon(0);
}

/*
 * Return "canonical frame address".
 */
NODE *
builtin_cfa(const struct bitable *bt, NODE *a)
{
	uerror("missing builtin_cfa");
	return bcon(0);
}

