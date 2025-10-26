/*	$Id$	*/
/*
 * Copyright (c) 2011 Anders Magnusson (ragge@ludd.luth.se).
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

# include "pass1.h"
# include "../../common/abi/abi.h"
# include "seh.h"

struct symtab spole0 = { 0, 0, 0, 0, 0, 0, 0, "base", "base", };
struct symtab *spole = &spole0;
struct symtab *nscur = &spole0;
int elnk, nsptr;
int cxxcuraccess = ACCESS_PUBLIC; /* default access in structs */

/* C++ standard and ABI selection */
int cxx_standard = CXX_STD_98;          /* Default to C++98 for compatibility */
int cxx_abi = CXX_ABI_ITANIUM;          /* Default to Itanium ABI (GCC/Clang) */
static abi_context_t *abi_ctx = NULL;   /* ABI library context */

/* RAII: Track objects needing destruction at scope exit */
struct dtor_entry {
	struct symtab *obj;        /* Object to destroy */
	struct symtab *dtor;       /* Destructor function */
	int level;                 /* Block level where created */
	struct dtor_entry *next;   /* Next in list */
};

static struct dtor_entry *dtor_stack = NULL;  /* Stack of objects needing destruction */

static struct symtab *sfind(char *n, struct symtab *sp);

/*
 * Set current access level for class members.
 * Called when 'public:', 'private:', or 'protected:' appears in class body.
 */
void
cxxaccess(char *name)
{
	if (strcmp(name, "public") == 0) {
		cxxcuraccess = ACCESS_PUBLIC;
		if (cppdebug)printf("access: public\n");
	} else if (strcmp(name, "private") == 0) {
		cxxcuraccess = ACCESS_PRIVATE;
		if (cppdebug)printf("access: private\n");
	} else if (strcmp(name, "protected") == 0) {
		cxxcuraccess = ACCESS_PROTECTED;
		if (cppdebug)printf("access: protected\n");
	} else {
		/* Labels in C++, e.g., "label:" */
		if (cppdebug)printf("label or unknown access: %s\n", name);
	}
}

/*
 * Declare a namespace.
 */
void
dclns(NODE *attr, char *n)
{
	struct symtab *sp;
#ifdef GCC_COMPAT
	struct attr *ap = gcc_attr_parse(attr);
#else
	struct attr *ap = NULL;
#endif

	if (cppdebug)printf("declaring namespace %s\n", n);
	n = addname(n);

	sp = sfind(n, nscur->sup);
	while (sp != NULL) {
		if (sp->sname == n && sp->sclass == NSPACE)
			break;
		sp = sfind(n, sp->snext);
	}
	if (sp == NULL) {
		/* New namespace */
		sp = getsymtab(n, 0);
		sp->sclass = NSPACE;
		INSSYM(sp);
	}
	nscur = sp;
	if (cppdebug)printf("declaring namespace2 %s\n", nscur->sname);
	sp->sap = attr_add(sp->sap, ap); /* XXX check attributes */
}

/*
 * Generate a call tree to function named n.
 */
static NODE *
callftn(char *n, ...)
{
	struct symtab *sp = getsymtab(n, 0);
	NODE *p, *a, *b;
	va_list ap;

	sp->stype = (FTN|VOID) | (PTR << TSHIFT);
	va_start(ap, n);

	a = va_arg(ap, NODE *);
	if (a != NULL) {
		do {
			b = va_arg(ap, NODE *);
			if (b != NULL)
				a = buildtree(CM, a, b);
		} while (b != NULL);
	}
	
	p = doacall(sp, nametree(sp), a, 0);
	va_end(ap);
	return p;
}

/*
 * Sanitycheck "new" keyword.
 */
NODE *
cxx_new(NODE *p)
{
	NODE *q = p;
	NODE *t1 = bcon(1);
	int nw = NM_NEW;

	while (p->n_op == LB) {
		nw = NM_NWA;
		t1 = buildtree(MUL, t1, eve(p->n_right));
		p->n_right = bcon(0);
		p = p->n_left;
	}
	if (p->n_op != TYPE)
		uerror("new used illegally");
	t1 = buildtree(MUL, t1, 
	    xbcon(tsize(p->n_type, p->n_df, p->n_ap)/SZCHAR, NULL, INTPTR));
	tfree(q);
	return callftn(decoratename(NULL, nw), t1, NULL);
}

/*
 * Handle "delete" keyword.
 */
NODE *
cxx_delete(NODE *p, int del)
{
	return callftn(decoratename(NULL, del), p, NULL);
}

/*
  <operator-name> ::= nw	# new           
		  ::= na	# new[]
		  ::= dl	# delete        
		  ::= da	# delete[]      
		  ::= ps        # + (unary)
		  ::= ng	# - (unary)     
		  ::= ad	# & (unary)     
		  ::= de	# * (unary)     
		  ::= co	# ~             
		  ::= pl	# +             
		  ::= mi	# -             
		  ::= ml	# *             
		  ::= dv	# /             
		  ::= rm	# %             
		  ::= an	# &             
		  ::= or	# |             
		  ::= eo	# ^             
		  ::= aS	# =             
		  ::= pL	# +=            
		  ::= mI	# -=            
		  ::= mL	# *=            
		  ::= dV	# /=            
		  ::= rM	# %=            
		  ::= aN	# &=            
		  ::= oR	# |=            
		  ::= eO	# ^=            
		  ::= ls	# <<            
		  ::= rs	# >>            
		  ::= lS	# <<=           
		  ::= rS	# >>=           
		  ::= eq	# ==            
		  ::= ne	# !=            
		  ::= lt	# <             
		  ::= gt	# >             
		  ::= le	# <=            
		  ::= ge	# >=            
		  ::= nt	# !             
		  ::= aa	# &&            
		  ::= oo	# ||            
		  ::= pp	# ++ (postfix in <expression> context)
		  ::= mm	# -- (postfix in <expression> context)           
		  ::= cm	# ,             
		  ::= pm	# ->*           
		  ::= pt	# ->            
		  ::= cl	# ()            
		  ::= ix	# []            
		  ::= qu	# ?             
		  ::= st	# sizeof (a type)
		  ::= sz	# sizeof (an expression)
                  ::= at        # alignof (a type)
                  ::= az        # alignof (an expression)
		  ::= cv <type>	# (cast)        
		  ::= v <digit> <source-name>	# vendor extended operator
*/

/*
  <builtin-type> ::= v	# void
		 ::= w	# wchar_t
		 ::= b	# bool
		 ::= c	# char
		 ::= a	# signed char
		 ::= h	# unsigned char
		 ::= s	# short
		 ::= t	# unsigned short
		 ::= i	# int
		 ::= j	# unsigned int
		 ::= l	# long
		 ::= m	# unsigned long
		 ::= x	# long long, __int64
		 ::= y	# unsigned long long, __int64
		 ::= n	# __int128
		 ::= o	# unsigned __int128
		 ::= f	# float
		 ::= d	# double
		 ::= e	# long double, __float80
		 ::= g	# __float128
		 ::= z	# ellipsis
                 ::= Dd # IEEE 754r decimal floating point (64 bits)
                 ::= De # IEEE 754r decimal floating point (128 bits)
                 ::= Df # IEEE 754r decimal floating point (32 bits)
                 ::= Dh # IEEE 754r half-precision floating point (16 bits)
                 ::= Di # char32_t
                 ::= Ds # char16_t
                 ::= Da # auto (in dependent new-expressions)
                 ::= Dn # std::nullptr_t (i.e., decltype(nullptr))
		 ::= u <source-name>	# vendor extended type
*/

/* matches type numbering in manifest.h */
static char chmap[] = { 'v', 'b', 'c', 'h', 's', 't', 'i', 'j', 'l', 'm',
	'x', 'y', 'f', 'd', 'e' };

static int
typch(int typ)
{
	int c = BTYPE(typ);
	if (c == VOID)
		c = 0;
	return chmap[c];
}

#define	MAXNM	255	/* max length of mangled name */
static char nmblk[MAXNM];
static int nmptr, subptr;

/* push character */
static void
nmch(int c)
{
	if (nmptr >= MAXNM)
		cerror("Too long mangled name");
	nmblk[nmptr++] = c;
}

/* Push length and string */
static void
pshsln(char *c)
{
	int i, j, ln = (int)strlen(c);

#define cnt(v,n) for (v = 0; ln >= n; v++, ln -= n)
	cnt(i,100);
	cnt(j,10);
	if (i) nmch(i+'0');
	if (j || i) nmch(j+'0');
	nmch(ln+'0');
	for (; *c; c++)
		nmch(*c);
}

/* Recurse to push namespace names */
static void
recnpsh(struct symtab *sp)
{
	if (sp == spole)
		return;
	if (sp == sp->sdown)
		cerror("sp == sp->sdown");
	if (sp->sdown)
		recnpsh(sp->sdown);
	pshsln(sp->sname);
}

static void
pshargs(union arglist *al)
{
	TWORD t;
	

	for (; al->type != TNULL; al++) {
		t = al->type;
		if (t == TELLIPSIS) {
			nmch('z');
			continue;
		}
		while (t > BTMASK) {
			if (ISPTR(t))
				nmch('P');
			else
				uerror("pshargs2: %lx\n", t);
			t = DECREF(t);
		}
		if (t > LDOUBLE)
			uerror("pshargs: %lx\n", t);
		/* XXX - cannot emit const/volatile */
		nmch(typch(t));
	}
}

/*
 * Do name mangling of a symbol table entry.
 * The resulting name is saved in soname.
 */
char *
decoratename(struct symtab *sp, int type)
{
	char *n;

#define	QNM(m,s) case m: n = s; break
	switch (type) {
	QNM(NM_NEW,"_Znwm");
	QNM(NM_NWA,"_Znam");
	QNM(NM_DEL,"_ZdlPv");
	QNM(NM_DLA,"_ZdaPv");
	case NM_NORMAL: /* Defined in defid() */
		break;
	default:
		uerror("missed mangling %d\n", type);
		return "";
	}
	if (type != NM_NORMAL)
		return addname(n);

	/* special non-mangled cases:
	 * "C" linkage
	 * main() function
	 * variables outside namespaces and classes
	 */
	if (elnk == LINK_C || strcmp(sp->sname, "main") == 0 ||
	    (sp->sdown == spole && !ISFTN(sp->stype))) {
		n = exname(sp->sname);
		return addname(n);
	}

	/* Use ABI library for function mangling if available */
	if (ISFTN(sp->stype) && abi_ctx != NULL) {
		char *abi_mangled = cxxabi_mangle_function(sp);
		if (abi_mangled != NULL) {
			if (cppdebug)
				printf("ABI mangled %s -> %s\n", sp->sname, abi_mangled);
			return abi_mangled;
		}
		/* Fall through to manual mangling if ABI mangling fails */
		if (cppdebug)
			printf("ABI mangling failed for %s, using manual mangling\n", sp->sname);
	}

	/* Compute the mangled name for other symbols using manual Itanium-style mangling */
	nmptr = 0;
	subptr = 0;
	nmch('_'); nmch('Z');
	if (sp->sdown != NULL) {
		nmch('N');
		recnpsh(sp->sdown);
	}
	pshsln(sp->sname);
	if (sp->sdown != NULL)
		nmch('E');
	if (ISFTN(sp->stype) && sp->sdf->dfun)
		pshargs(sp->sdf->dfun);
	nmch(0);
	return addname(nmblk);
}

/*
 * find a symtab entry in the given link.
 */
static struct symtab *
sfind(char *n, struct symtab *sp)
{
	while (sp) {
	if (cppdebug)printf("sfind: checking %s against %s\n", n, sp->sname);
		if (sp->sname == n)
			return sp;
		sp = sp->snext;
	}
	return NULL;
}

/* class or namespace? */
#define	CLORNS(sp) (sp->sclass == STNAME || sp->sclass == CLNAME || \
	sp->sclass == UNAME || sp->sclass == NSPACE)

/*
 * find a symtab path entry in the given path.
 * p is expected to be a link of NMNAMEs.
 * It is supposed to return a sup value of the last found class.
 */
static struct symtab *
pfind(NODE *p, struct symtab *sp)
{
	char *n;

	if (cppdebug)printf("pfind: op %d searching %s\n", p->n_op, p->n_op == NAME ?
(char *)p->n_sp:(char *)p->n_right->n_sp);

	if (p->n_op == NAME) {
		n = (char *)p->n_sp;
		if ((sp = sfind(n, sp)) == NULL)
			return NULL;
	if (cppdebug)printf("pfind: NAME class %d name %s\n", sp->sclass, sp->sname);
		while (!CLORNS(sp)) {
			if ((sp = sfind(n, sp->snext)) == NULL)
				return NULL;
		}
	if (cppdebug)printf("pfind: FOUND %s\n", sp->sname);
		sp = sp->sup;
	} else {
		n = (char *)p->n_right->n_sp;
		if ((sp = sfind(n, sp)) == NULL)
			return NULL;
	if (cppdebug)printf("pfind: NMLIST class %d name %s\n", sp->sclass, sp->sname);
		while (!CLORNS(sp)) {
			if ((sp = sfind(n, sp->snext)) == NULL)
				return NULL;
		}
		sp = pfind(p->n_left, sp->sup);
	}
	return sp;
}

/*
 * Declare a variable.
 */
struct symtab *
cxxdeclvar(NODE *p)
{
	struct symtab *sp;

	if (blevel && p->n_op == NAME) {
		sp = p->n_sp = lookup((char *)p->n_sp, 0);
	} else {
		sp = cxxlookup(p, SNORMAL);
	}
	return sp;
}

/*
 * class is MOS if variable is member of a CLASS, NORMAL otherwise.
 * A CLASS as member of a class has symbol type CLASS.
 */
char *symclass[] = { "NORMAL", "CLASS", "LABEL", "MOS", "STRING" };

/*
 * Do a name lookup.  p can be either just a NAME or NMLIST.
 * The first symbol instance on its level is returned, which may or
 * may not be correct.
 * If no symbol is found, return a new symtab entry.
 * p should be a NAME after this with n_sp filled in accordingly.
 * It's the responsibility of the declaration routine to add it to 
 * the symbol table.
 * nfree() will be called on p after this function.
 */
struct symtab *
cxxlookup(NODE *p, int flags)
{
	struct symtab *sp, *ns;
	int ftyp = flags & SMASK;
	NODE *q;
	char *n, *s;

#define SPNAME(p) ((char *)(p->n_op == NAME ? p->n_sp : p->n_right->n_sp))
#ifdef PCC_DEBUG
	if (cppdebug){ printf("cxxlookup %s\n", SPNAME(p)); symtree(); }
#endif

	q = p;
	if (p->n_op == NAME) {
		s = (char *)p->n_sp;
		if (blevel) {
			sp = lookup(s, SNOCREAT); /* check if auto var */
			if (sp == NULL) {
				/* check if in classes */
				for (ns = nscur; ns != spole; ns = ns->sdown)
					if ((sp = sfind(s, ns->sup)))
						break;
				if (sp == NULL)
					sp = sfind(s, spole->sup);
			}
			if (sp == NULL)
				sp = lookup(s, 0); /* fallback */
		} else {
			ns = nscur;
			sp = sfind(s, ns);
			while (sp != NULL) {
				if ((sp->sflags & SMASK) == ftyp)
					break;
				sp = sfind(s, sp->snext);
			}
			if (sp == NULL) {
				sp = getsymtab(s, ftyp);
				if ((flags & SNOCREAT) == 0) {
#ifdef PCC_DEBUG
	if (cppdebug)printf("cxxlookup: adding %s %s %s at %s\n", symclass[ftyp], s, sp->soname, nscur ? nscur->sname : "base");
#endif
					INSSYM(sp);
					cxxsetname(sp);
				}
			}
		}
	} else {
		/* Search through namespaces/classes for it */
		n = SPNAME(p);
		ns = pfind(p->n_left, spole->sup);
		if (ns == NULL) {
			uerror("undeclared class in chain");
			return getsymtab(n, ftyp);
		}
		if ((sp = sfind(n, ns)) == NULL) {
			sp = getsymtab(n, ftyp);
			if ((flags & SNOCREAT) == 0) {
				sp->snext = ns->snext;
				ns->snext = sp;
			}
		}
	}
	
	/* make top node a NAME */
	if (q->n_op != NAME) {
		tfree(q->n_left);
		p = q->n_right;
		*q = *q->n_right;
		nfree(p);
	}
	q->n_sp = sp;
	return sp;
}

void
cxxsetname(struct symtab *sp)
{
	if (elnk == LINK_C)
		return; /* leave to target */
	sp->soname = decoratename(sp, NM_NORMAL);
}

/*
 * Create a symbol out of a struct.
 * We call the symbol "__%THIS" to avoid interference.
 */
struct symtab *
cxxstrvar(struct symtab *so)
{
	struct symtab *sp;
	NODE *p;

	sp = lookup("__%THIS", 0);
	p = block(NAME, 0, 0, INCREF(so->stype), so->sdf, so->sap);
	p->n_sp = sp;
	defid(p, PARAM);
	nfree(p);
	return sp;
}

/*
 * Declare a struct (class) based on its name n.
 * Assumed that nmcur is correctly pointing to either:
 * - nothing (class at level 0)
 * - current namespace
 * - parent class
 */
struct symtab *
cxxdclstr(char *n)
{
	struct symtab *sp;

	sp = sfind(n, nscur->sup);
	while (sp && !CLORNS(sp))
		sp = sfind(n, sp->snext);
	if (sp == 0) {
		sp = getsymtab(n, STAGNAME);
		INSSYM(sp);
	}
	/* Allow forward declarations and reopening for definitions */
	nscur = sp;

if (cppdebug)printf("declaring struct %s %p nscur %s\n", n, sp, nscur->sname);
	return sp;
}

#ifdef PCC_DEBUG
static void
symwalk(struct symtab *sp, int indent)
{
	int i; 

	while (sp) {
		for (i = 0; i < indent; i++)
			printf("  ");
		printf("%s (%p) %s\n", sp->sname, sp, scnames(sp->sclass));
		if (sp->sup)
			symwalk(sp->sup, indent+1);
		sp = sp->snext;
	}
}

void
symtree(void)
{
	symwalk(spole, 0);
}
#endif

/*
 * Compare a matching prototype for a function.
 */
static int
cxxpcmp(struct symtab *sp, NODE *p)
{
	union arglist *a1, *a2;
	int i;

	if (!ISFTN(sp->stype) || p->n_df == NULL || sp->sdf == NULL)
		return 0; /* no dimfun */
	if ((a1 = sp->sdf->dfun) == NULL || (a2 = p->n_df->dfun) == NULL)
		return 0; /* no argument */

	for (i = 0; ; i++) {
		if (a1[i].type == TNULL && a2[i].type == TNULL)
			return 1; /* equal prototypes */
		if (a1[i].type != a2[i].type)
			return 1; /* unequal prototypes */
	}
}

struct ckstr {
	int rv;
	union arglist *al;
};

static void
cxxckproto(NODE *p, void *arg)
{
	struct ckstr *cp = arg;

	if (cp->rv == -1)
		return;

	if (cp->al[0].type != p->n_type)
		goto fail;
	if (BTYPE(cp->al[0].type) > LDOUBLE)
		uerror("cxxckproto");
	cp->al++;
	return;
fail:
	cp->rv = -1;
}

/*
 * Compare a matching prototype for an argument tree.
 * Here we can expand to also do inexact matches.
 * Return 0 if equal, -1 if failed.
 */
static int
cxxptreecmp(struct symtab *sp, NODE *p)
{
	struct ckstr ckstr;
	union arglist *a1;

	if (!ISFTN(sp->stype) || sp->sdf == NULL ||
	    (a1 = sp->sdf->dfun) == NULL)
		return 0; /* no dimfun */

	if (p == NULL && a1[0].type == TNULL)
		return 1; /* arg-less */

	ckstr.rv = 0;
	ckstr.al = a1;
	flist(p, cxxckproto, &ckstr);

	if (ckstr.al[0].type != TNULL)
		return -1; /* arg number error */
	return ckstr.rv;
}

/*
 * Search for (and declare) a function.
 */
struct symtab *
cxxftnfind(NODE *p, int flags)
{
	struct symtab *sp, *ns;
	char *s;

	if (p->n_op == NAME) {
		s = (char *)p->n_sp;
		/* Search for equally named functions */
		sp = sfind(s, nscur->sup);
		while (sp != NULL) {
			if (cxxpcmp(sp, p)) {
				if (sp->sclass != NSPACE ||
				    sp->sclass == EXTDEF) {
					uerror("%s redefined", s);
					return sp;
				} else
					break;
			}
			sp = sfind(s, sp->snext);
		}
		if (sp == NULL) {
			sp = getsymtab(s, SNORMAL);
			sp->stype = p->n_type;
			sp->squal = p->n_qual;
			sp->sdf = p->n_df;
			sp->sap = p->n_ap;
			INSSYM(sp);
			if (nscur->sclass != NSPACE && nscur != &spole0)
				uerror("inside struct");
		}
		sp->sclass = EXTDEF;
		if (sp->soname == 0)
			sp->soname = decoratename(sp, NM_NORMAL);
	} else {
		/*
		 * declared outside class, tree-style reference
		 * Must have been defined already
		 * This will be an external declaration (not spooled).
		 */
		s = SPNAME(p);
		if ((ns = pfind(p->n_left, spole->sup)) == NULL) {
			uerror("undeclared class in chain");
			goto undecl;
		}
		/* Search for an EXTERN or EXTDEF declaration within */
		/* EXTDEF causes redeclaration. */
		sp = sfind(s, ns);
		while (sp != NULL) {
			if (sp->sclass == EXTERN || sp->sclass == EXTDEF) {
				if (cxxpcmp(sp, p->n_right)) {
					if (sp->sclass == EXTDEF)
						uerror("%s redefined", s);
					break;
				}
			}
			sp = sfind(s, sp->snext);
		}
		if (sp == NULL) {
			uerror("%s undeclared", s);
			goto undecl;
		}
		sp->sclass = EXTDEF;
	}
	return sp;

undecl:
	return getsymtab(s, SNORMAL);
}

/*
 * Reference to a struct as a :: name.
 */
NODE *
cxxrstruct(int soru, NODE *attr, NODE *t, char *n)
{
	struct symtab *ns, *sp;

	ns = pfind(t, spole->sup);
	if (ns == NULL)
		goto undecl;

	tfree(t);
	sp = sfind(n, ns);
	while (sp != NULL) {
		if (sp->sclass == soru)
			return mkty(sp->stype, 0, sp->sap);
		sp = sfind(n, sp->snext);
	}
undecl:
	uerror("%s undeclared", n);
	return mkty(INT, 0, 0);
}

/*
 * Search for correct matching function in a struct depending on 
 * argument list a.  Return a call node for this function.
 * Do not touch neither f nor a.
 * return a name tree suitable for a function call.
 * We know here that f is a struct reference.
 */
NODE *
cxxmatchftn(NODE *f, NODE *a)
{
	struct attr *ap;
	struct symtab *sp;
	char *n = (char *)f->n_right->n_sp;

	f = f->n_left;

	if ((ap = attr_find(f->n_ap, ATTR_STRUCT)) == NULL) {
		uerror("undefined class");
		sp = getsymtab(n, 0);
	} else
		sp = ap->amlist;
	sp = sfind(n, sp);
	while (sp != NULL) {
		if (ISFTN(sp->stype) && cxxptreecmp(sp, a) == 0)
			break;
		sp = sfind(n, sp->snext);
	}
	if (sp == NULL)
		uerror("undefined class member");
	return nametree(sp);
}

/*
 * Add hidden argument f first in node list a. Return resulting a.
 */
NODE *
cxxaddhidden(NODE *a, NODE *f)
{
	NODE *q;

	if (a == NULL)
		return f;
	if (a->n_op != CM)
		return block(CM, f, a, INT, 0, 0);
	for (q = a; q->n_left->n_op == CM; q = q->n_left)
		;
	q->n_left = block(CM, f, q->n_left, INT, 0, 0);
	return a;
}

/*
 * Watch out for references to static members.
 */
NODE *
cxxstructref(NODE *p, int f, char *n)
{
	struct symtab *sp = strmemb(p->n_ap);

	if (sp == NULL)
		cerror("ref to unknown struct");
	sp = sfind(n, sp);
	while (sp != NULL) {
		if (!ISFTN(sp->stype)) {
			if (sp->sclass == STATIC || sp->sclass == USTATIC) {
				tfree(p);
				return nametree(sp);
			}
			break;
		}
		sp = sfind(n, sp->snext);
	}
	return structref(p, f, n);
}

/*
 * Check if function name fname is a constructor for classsym.
 * Constructor: function name == class name
 */
int
cxxisctor(char *fname, struct symtab *classsym)
{
	if (classsym == NULL || fname == NULL)
		return 0;
	return (strcmp(fname, classsym->sname) == 0);
}

/*
 * Check if function name fname is a destructor for classsym.
 * Destructor: function name == ~ClassName
 */
int
cxxisdtor(char *fname, struct symtab *classsym)
{
	if (classsym == NULL || fname == NULL)
		return 0;
	if (fname[0] != '~')
		return 0;
	return (strcmp(fname + 1, classsym->sname) == 0);
}

/*
 * Mark function sp as a constructor.
 */
void
cxxmarkctor(struct symtab *sp)
{
	if (sp == NULL)
		return;
	sp->sflags |= SCTOR;
	if (cppdebug)
		printf("Marked %s as constructor\n", sp->sname);
}

/*
 * Mark function sp as a destructor.
 */
void
cxxmarkdtor(struct symtab *sp)
{
	if (sp == NULL)
		return;
	sp->sflags |= SDTOR;
	if (cppdebug)
		printf("Marked %s as destructor\n", sp->sname);
}

/*
 * Check if a type is a class type (struct in C++ terms).
 * Returns 1 if type is a class, 0 otherwise.
 */
int
cxxisclass(TWORD type)
{
	TWORD t = BTYPE(type);
	return (t == STRTY);
}

/*
 * Find a constructor for a given class symbol.
 * Returns the constructor symtab entry, or NULL if not found.
 */
struct symtab *
cxxfindctor(struct symtab *classsym)
{
	struct symtab *sp;

	if (classsym == NULL || classsym->sclass != STNAME)
		return NULL;

	/* Look for a constructor in the class's symbol list */
	for (sp = classsym->sup; sp != NULL; sp = sp->snext) {
		if (sp->sflags & SCTOR)
			return sp;
	}

	return NULL;
}

/*
 * Find a destructor for a given class symbol.
 * Returns the destructor symtab entry, or NULL if not found.
 */
struct symtab *
cxxfinddtor(struct symtab *classsym)
{
	struct symtab *sp;

	if (classsym == NULL || classsym->sclass != STNAME)
		return NULL;

	/* Look for a destructor in the class's symbol list */
	for (sp = classsym->sup; sp != NULL; sp = sp->snext) {
		if (sp->sflags & SDTOR)
			return sp;
	}

	return NULL;
}

/*
 * Generate a call to a constructor or destructor.
 * sp: the object being constructed/destructed
 * fnsym: the constructor/destructor function symbol
 * Returns a function call node.
 */
NODE *
cxxgencall(struct symtab *sp, struct symtab *fnsym)
{
	NODE *fn, *obj, *call;

	if (sp == NULL || fnsym == NULL)
		return NULL;

	/* Create function name node */
	fn = nametree(fnsym);

	/* Create object reference node */
	obj = nametree(sp);

	/* For member functions, we need to pass the object address as first argument */
	/* Build a call: ctor(&obj) or dtor(&obj) */
	obj = buildtree(ADDROF, obj, NIL);

	/* Build function call */
	call = buildtree(CALL, fn, obj);

	if (cppdebug)
		printf("Generated call to %s for object %s\n", fnsym->sname, sp->sname);

	return call;
}

/*
 * Register an object for destruction at scope exit (RAII).
 * Called when an automatic object with a destructor is created.
 */
void
cxxregister_dtor(struct symtab *obj, struct symtab *dtor, int level)
{
	struct dtor_entry *entry;

	if (obj == NULL || dtor == NULL)
		return;

	/* Create new destructor entry */
	entry = malloc(sizeof(struct dtor_entry));
	if (entry == NULL)
		return;

	entry->obj = obj;
	entry->dtor = dtor;
	entry->level = level;
	entry->next = dtor_stack;
	dtor_stack = entry;

	if (cppdebug)
		printf("Registered destructor for %s at level %d\n", obj->sname, level);
}

/*
 * Call destructors for all objects at or above the given level.
 * Called at scope exit to implement RAII.
 * Destructors are called in reverse order of construction (LIFO).
 */
void
cxxcall_dtors(int level)
{
	struct dtor_entry *entry, *prev, *next;
	NODE *call;
	int count = 0;

	if (cppdebug)
		printf("cxxcall_dtors: level %d\n", level);

	/* First pass: emit destructor calls for matching level */
	entry = dtor_stack;
	while (entry != NULL) {
		if (entry->level >= level) {
			/* Generate destructor call */
			call = cxxgencall(entry->obj, entry->dtor);
			if (call != NULL) {
				ecomp(call);
				count++;
				if (cppdebug)
					printf("Called destructor for %s\n", entry->obj->sname);
			}
		}
		entry = entry->next;
	}

	/* Second pass: remove destroyed objects from stack */
	prev = NULL;
	entry = dtor_stack;
	while (entry != NULL) {
		next = entry->next;
		if (entry->level >= level) {
			/* Remove from stack */
			if (prev == NULL)
				dtor_stack = next;
			else
				prev->next = next;
			free(entry);
		} else {
			prev = entry;
		}
		entry = next;
	}

	if (cppdebug && count > 0)
		printf("Called %d destructors at level %d\n", count, level);
}

/*
 * Convert our CXX_ABI_* enum to ABI library's abi_kind_t.
 */
static abi_kind_t
cxx_to_abi_kind(int cxx_abi_type)
{
	switch (cxx_abi_type) {
	case CXX_ABI_ITANIUM:  return ABI_ITANIUM;
	case CXX_ABI_MSVC:     return ABI_MSVC;
	case CXX_ABI_WATCOM:   return ABI_WATCOM;
	case CXX_ABI_BORLAND:  return ABI_BORLAND;
	case CXX_ABI_GNU_OLD:  return ABI_GNU_OLD;
	case CXX_ABI_DMC:      return ABI_DMC;
	case CXX_ABI_ARM:      return ABI_ARM;
	default:               return ABI_ITANIUM; /* Safe default */
	}
}

/*
 * Initialize the ABI library context.
 * Should be called once during compiler initialization.
 */
void
cxxabi_init(void)
{
	abi_kind_t abi_kind;

	/* Convert C++ ABI type to ABI library type */
	abi_kind = cxx_to_abi_kind(cxx_abi);

	/* Initialize ABI context */
	if (abi_ctx != NULL)
		abi_destroy(abi_ctx);

	abi_ctx = abi_init(abi_kind);

	if (abi_ctx == NULL)
		cerror("Failed to initialize ABI library");

	if (cppdebug)
		printf("Initialized %s ABI (C++%s)\n",
		       cxx_abi == CXX_ABI_ITANIUM ? "Itanium" :
		       cxx_abi == CXX_ABI_MSVC ? "MSVC" :
		       cxx_abi == CXX_ABI_WATCOM ? "Watcom" :
		       cxx_abi == CXX_ABI_BORLAND ? "Borland" : "Unknown",
		       cxx_standard == CXX_STD_98 ? "98" :
		       cxx_standard == CXX_STD_03 ? "03" :
		       cxx_standard == CXX_STD_11 ? "11" :
		       cxx_standard == CXX_STD_14 ? "14" :
		       cxx_standard == CXX_STD_17 ? "17" :
		       cxx_standard == CXX_STD_20 ? "20" :
		       cxx_standard == CXX_STD_23 ? "23" : "Unknown");
}

/*
 * Get the current ABI context.
 */
abi_context_t *
cxxabi_get_context(void)
{
	if (abi_ctx == NULL)
		cxxabi_init();
	return abi_ctx;
}

/*
 * Convert PCC TWORD type to ABI type kind.
 */
static abi_type_kind_t
pcc_to_abi_type(TWORD t)
{
	switch (BTYPE(t)) {
	case VOID:      return ABI_TYPE_VOID;
	case BOOL:      return ABI_TYPE_BOOL;
	case CHAR:      return ABI_TYPE_CHAR;
	case UCHAR:     return ABI_TYPE_UCHAR;
	case SHORT:     return ABI_TYPE_SHORT;
	case USHORT:    return ABI_TYPE_USHORT;
	case INT:       return ABI_TYPE_INT;
	case UNSIGNED:  return ABI_TYPE_UINT;
	case LONG:      return ABI_TYPE_LONG;
	case ULONG:     return ABI_TYPE_ULONG;
	case LONGLONG:  return ABI_TYPE_LONGLONG;
	case ULONGLONG: return ABI_TYPE_ULONGLONG;
	case FLOAT:     return ABI_TYPE_FLOAT;
	case DOUBLE:    return ABI_TYPE_DOUBLE;
	case LDOUBLE:   return ABI_TYPE_LONGDOUBLE;
	case STRTY:     return ABI_TYPE_CLASS;
	case UNIONTY:   return ABI_TYPE_UNION;
	case ENUMTY:    return ABI_TYPE_ENUM;
	default:        return ABI_TYPE_INT; /* fallback */
	}
}

/*
 * Create an ABI type descriptor from PCC type.
 */
static abi_type_t *
create_abi_type(TWORD type, union dimfun *df, struct attr *ap)
{
	abi_type_t *atype;

	atype = abi_create_type(pcc_to_abi_type(type));
	if (atype == NULL)
		return NULL;

	/* Handle type qualifiers */
	if (type & CON)
		atype->is_const = 1;
	if (type & VOL)
		atype->is_volatile = 1;

	/* Handle pointers and references */
	if (ISPTR(type)) {
		TWORD pointee_type = DECREF(type);
		atype->kind = ABI_TYPE_POINTER;
		atype->u.pointee = create_abi_type(pointee_type, df, ap);
	}

	return atype;
}

/*
 * Create an ABI function descriptor from PCC symbol table entry.
 */
static abi_function_t *
create_abi_function(struct symtab *sp)
{
	abi_function_t *func;
	abi_param_t *param, *last_param = NULL;

	func = calloc(1, sizeof(abi_function_t));
	if (func == NULL)
		return NULL;

	func->name = sp->sname;

	/* Set return type */
	func->return_type = create_abi_type(DECREF(sp->stype), sp->sdf, sp->sap);

	/* Check if constructor or destructor */
	if (sp->sflags & SCTOR)
		func->is_constructor = 1;
	if (sp->sflags & SDTOR)
		func->is_destructor = 1;

	/* Check if member function */
	if (sp->sdown != NULL && sp->sdown != spole && sp->sdown->sclass != NSPACE) {
		/* This is a member function - we'll set parent_class later if needed */
		func->parent_class = NULL; /* For now */
	}

	/* Add parameters if this is a function with prototype */
	if (ISFTN(sp->stype) && sp->sdf && sp->sdf->dfun) {
		union arglist *al;
		int param_num = 0;

		for (al = sp->sdf->dfun; al->type != TNULL; al++, param_num++) {
			/* Skip ellipsis */
			if (al->type == TELLIPSIS)
				continue;

			param = calloc(1, sizeof(abi_param_t));
			if (param == NULL)
				break;

			/* Parameters don't have names in arglist, use generic name */
			/* In practice, the ABI library mainly needs the types */
			param->name = NULL;
			param->type = create_abi_type(al->type, al->df, al->sap);
			param->next = NULL;

			if (last_param == NULL)
				func->params = param;
			else
				last_param->next = param;
			last_param = param;
		}
	}

	return func;
}

/*
 * Free ABI function descriptor and associated memory.
 */
static void
free_abi_function(abi_function_t *func)
{
	abi_param_t *param, *next;

	if (func == NULL)
		return;

	/* Free parameters */
	param = func->params;
	while (param != NULL) {
		next = param->next;
		if (param->type)
			abi_destroy_type(param->type);
		free(param);
		param = next;
	}

	/* Free return type */
	if (func->return_type)
		abi_destroy_type(func->return_type);

	free(func);
}

/*
 * Mangle a function or method name using the ABI library.
 * Returns a statically allocated string (via addname).
 */
char *
cxxabi_mangle_function(struct symtab *sp)
{
	abi_context_t *ctx;
	abi_function_t *func;
	char *mangled;
	char *result;

	/* Get ABI context */
	ctx = cxxabi_get_context();
	if (ctx == NULL)
		return NULL;

	/* Create ABI function descriptor */
	func = create_abi_function(sp);
	if (func == NULL)
		return NULL;

	/* Mangle using ABI library */
	mangled = abi_mangle_function(ctx, func);

	/* Copy to permanent storage via addname */
	result = mangled ? addname(mangled) : NULL;

	/* Clean up */
	if (mangled)
		free(mangled);
	free_abi_function(func);

	return result;
}

/*
 * C++ Exception Handling Code Generation
 *
 * These functions generate code for try/catch/throw statements.
 * Currently using stub implementations - full implementation will
 * require integration with libseh (SEH runtime library).
 *
 * See SEH_LIBRARY_STATUS.md for details on library integration status.
 */

/*
 * Generate code for try-catch block
 *
 * try { ... } catch (Type e) { ... }
 *
 * Architecture limitations:
 * PCC's current code generation architecture does not support the complex
 * control flow needed for proper exception handling. A full implementation
 * requires:
 *
 * 1. Stack frame modification to allocate _seh_registration structure
 * 2. Prolog code to call _seh_register() before try block
 * 3. setjmp() call to establish exception handler entry point
 * 4. Exception type matching code for each catch block
 * 5. Epilog code to call _seh_unregister() on all exit paths
 * 6. Integration with RAII destructor unwinding
 *
 * This would require significant changes to:
 * - Function prologue/epilogue generation (local.c, local2.c)
 * - Stack frame layout (order.c)
 * - Control flow graph management (optim.c)
 * - Statement emission (ecomp() in pftn.c)
 *
 * Current implementation: Minimal stub with SEH headers included.
 */
NODE *
cxxtry(NODE *try_body, NODE *handler_seq)
{
	static int warned = 0;

	if (!warned) {
		werror("try/catch blocks not yet fully implemented");
		werror("Exception handling requires architectural changes to code generator");
		werror("See cxxcode.c:cxxtry() for details");
		warned = 1;
	}

	/* For now, execute try body normally (no exception handling) */
	if (try_body)
		ecomp(try_body);

	if (handler_seq)
		tfree(handler_seq);

	/* TODO: Full implementation requires:
	 *
	 * struct _seh_registration reg;
	 * _seh_register(&reg, handler_func, NULL);
	 * if (setjmp(reg.jmpbuf) == 0) {
	 *     // try body
	 * } else {
	 *     // exception occurred - dispatch to catch blocks
	 *     if (type_matches_catch1) { catch1_body; }
	 *     else if (type_matches_catch2) { catch2_body; }
	 *     else { _seh_reraise(); }
	 * }
	 * _seh_unregister(&reg);
	 */

	return NIL;
}

/*
 * Generate code for catch block
 *
 * catch (Type e) { ... }
 *
 * This function is called during parsing to build the catch handler tree.
 * In a full implementation, it would:
 * 1. Extract type information from exception_decl
 * 2. Create exception type descriptor for runtime type matching
 * 3. Generate code to extract exception object from _seh_get_cxx_exception()
 * 4. Bind exception to catch parameter variable
 * 5. Return a node representing the catch block for cxxtry() to process
 *
 * Current implementation: Placeholder that frees nodes.
 */
NODE *
cxxcatch(NODE *exception_decl, NODE *handler_body)
{
	/* TODO: Extract exception type and create type descriptor */
	/* Would use: type_spec from exception_decl to create runtime type info */

	if (exception_decl)
		tfree(exception_decl);

	if (handler_body)
		tfree(handler_body);

	/* TODO: Return node containing type descriptor and handler code */
	return NIL;
}

/*
 * Generate code for throw statement
 *
 * throw expr;  - throw new exception
 * throw;       - re-throw current exception
 *
 * A full implementation would generate code equivalent to:
 *
 * For "throw expr;":
 *   1. void *exception_obj = malloc(sizeof(expr_type));
 *   2. new (exception_obj) expr_type(expr);  // copy constructor
 *   3. _seh_translate_cxx_exception(exception_obj);
 *   4. _seh_raise_exception(EXCEPTION_CXX_EXCEPTION, 0, 1, &exception_obj);
 *   5. [unreachable code - raise_exception does not return]
 *
 * For "throw;" (re-throw):
 *   1. _seh_raise_exception(_seh_get_exception_code(), 0, 0, NULL);
 *
 * Current implementation: Placeholder warning.
 */
NODE *
cxxthrow(NODE *expr)
{
	static int warned = 0;

	if (!warned) {
		werror("throw statements not yet fully implemented");
		werror("Would require: exception object allocation, type descriptors, SEH calls");
		werror("See cxxcode.c:cxxthrow() for implementation details");
		warned = 1;
	}

	if (expr) {
		/* throw expr; - throw new exception */
		tfree(expr);

		/* TODO: Generate code:
		 * - Evaluate expr
		 * - Allocate exception object on heap
		 * - Call copy/move constructor
		 * - Call _seh_translate_cxx_exception()
		 * - Call _seh_raise_exception()
		 */
	} else {
		/* throw; - re-throw current exception */
		/* TODO: Generate call to _seh_raise_exception() with current exception */
	}

	return NIL;
}

/*
 * Generate code for exception declaration in catch block
 *
 * This handles:
 * - catch (Type var)
 * - catch (Type)
 * - catch (...)
 *
 * Full implementation will create a symbol table entry for the
 * exception variable and generate code to extract it from the
 * SEH exception structure.
 */
NODE *
cxxexception_decl(NODE *type_spec, NODE *declarator)
{
	/* TODO: Process exception declaration */
	/* For now, just return the declarator node */
	
	if (type_spec)
		tfree(type_spec);
	
	return declarator;
}
