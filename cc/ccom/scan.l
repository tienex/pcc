%{
/*	$Id$	*/

/*
 * Copyright (c) 2002 Anders Magnusson. All rights reserved.
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
%}


B			[0-1]
D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
P			[Pp][+-]?{D}+
FS			((f|F|l|L)?i|i?(f|F|l|L))
IS			(u|U|l|L|i)*
UL			({L}|[\x80-\xFF])
UC			(L|u|U)
US			(L|u|U|u8)

%{
#include <stdlib.h>
#include <errno.h>  
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

#include "pass1.h"
#include "cgram.h"
#include "unicode.h"

#define	bdebug flexbdebug

static P1ND *cvtdig(int radix);
static P1ND *charcon(void);
static P1ND *wcharcon(void);
static void control(int);
static void pragma(void);
static int dotfile;
int notype, parbal, inattr, parlvl, nodinit, inoso;
int kwdecode(struct symtab *s);
int objc_keyword(char *kw);

#define	CPP_IDENT 	2
#define	CPP_LINE 	3
#define	CPP_HASH	4

#ifdef STABS
#define	STABS_LINE(x) if (gflag && cftnsp) stabs_line(x)
#else
#define STABS_LINE(x)
#endif
#if defined(FLEX_SCANNER) && YY_FLEX_SUBMINOR_VERSION == 31
/* Hack to avoid unnecessary warnings */
FILE *yyget_in  (void);
FILE *yyget_out  (void);
int yyget_leng  (void);
char *yyget_text  (void);
void yyset_in (FILE *);
void yyset_out (FILE *);
int yyget_debug  (void);
void yyset_debug (int);
int yylex_destroy  (void);
extern int yyget_lineno (void);
extern void yyset_lineno (int);
#endif

%}

%%

@{UL}({UL}|{D})*	{ 	int i;
			char *kw = yytext + 1; /* skip @ */

			/* Check for Objective-C keywords */
			if ((i = objc_keyword(kw)) > 0)
				return i;

			/* Not a keyword, treat as @ followed by identifier */
			unput(yytext[yyleng-1]);
			for (i = yyleng-2; i > 0; i--)
				unput(yytext[i]);
			return OBJC_AT;
		}

{UL}({UL}|{D})*	{ 	struct symtab *s;
			int i = 0;

			yylval.strp = addname(yytext);
			if ((s = lookup(yylval.strp, SNOCREAT)) != NULL &&
			    s->sclass == KEYWORD)
				return kwdecode(s);

#ifdef GCC_COMPAT
			if (doing_init && nodinit == 0) {
				/* check for name: for old gcc compat */
				while ((i = input()) == ' ' || i == '\t')
					;
				if (i == ':')
					return(GCC_DESIG);
				unput(i);
			}
			if ((i = gcc_keyword(yylval.strp)) > 0) {
				if (i == PCC_OFFSETOF)
					inoso = 1;
				return i;
			}
#endif
			if (i == 0) {
				if (notype)
					return(C_NAME);
				return s && s->sclass == TYPEDEF ?
				    notype=1, C_TYPENAME : C_NAME;
			}
		}

0[xX]{H}+{IS}?		{ yylval.nodep = cvtdig(16); return(C_ICON); }
0{D}+{IS}?		{ yylval.nodep = cvtdig(8); return(C_ICON); }
0[bB]{B}+{IS}?		{ yylval.nodep = cvtdig(2); return(C_ICON); }
{D}+{IS}?		{ yylval.nodep = cvtdig(10); return(C_ICON); }
{UC}'(\\.|[^\\'])*'	{ yylval.nodep = wcharcon(); return(C_ICON); }
'(\\.|[^\\'])*'		{ yylval.nodep = charcon(); return(C_ICON); }

{D}+{E}{FS}?		{ yylval.flt = floatcon(yytext); return(C_FCON); }
{D}*"."{D}+({E})?{FS}?	{ yylval.flt = floatcon(yytext); return(C_FCON); }
{D}+"."{D}*({E})?{FS}?	{ yylval.flt = floatcon(yytext); return(C_FCON); }
0[xX]{H}*"."{H}+{P}{FS}? { yylval.flt = fhexcon(yytext); return(C_FCON); }
0[xX]{H}+"."{P}{FS}?	{ yylval.flt = fhexcon(yytext); return(C_FCON); }
0[xX]{H}+{P}{FS}?	{ yylval.flt = fhexcon(yytext); return(C_FCON); }

@\"(\\.|[^\\"])*\"	{ yylval.strp = yytext; return OBJC_STRING; }
@0[xX]{H}+{IS}?		{ yylval.nodep = cvtdig(16); return(C_ICON); }
@0{D}+{IS}?		{ yylval.nodep = cvtdig(8); return(C_ICON); }
@0[bB]{B}+{IS}?		{ yylval.nodep = cvtdig(2); return(C_ICON); }
@{D}+{IS}?		{ yylval.nodep = cvtdig(10); return(C_ICON); }
@{D}+{E}{FS}?		{ yylval.flt = floatcon(yytext + 1); return(C_FCON); }
@{D}*"."{D}+({E})?{FS}?	{ yylval.flt = floatcon(yytext + 1); return(C_FCON); }
@{D}+"."{D}*({E})?{FS}?	{ yylval.flt = floatcon(yytext + 1); return(C_FCON); }
@'(\\.|[^\\'])*'	{ yylval.nodep = charcon(); return(C_ICON); }
@"("			{ return OBJC_BOXED; }
@"["			{ return OBJC_ARRAY_START; }
@"{"			{ return OBJC_DICT_START; }
{US}?\"(\\.|[^\\"])*\"	{ yylval.strp = yytext; return C_STRING; }

"..."			{ return(C_ELLIPSIS); }
">>="			{ yylval.intval = RSEQ; return(C_ASOP); }
"<<="			{ yylval.intval = LSEQ; return(C_ASOP); }
"+="			{ yylval.intval = PLUSEQ; return(C_ASOP); }
"-="			{ yylval.intval = MINUSEQ; return(C_ASOP); }
"*="			{ yylval.intval = MULEQ; return(C_ASOP); }
"/="			{ yylval.intval = DIVEQ; return(C_ASOP); }
"%="			{ yylval.intval = MODEQ; return(C_ASOP); }
"&="			{ yylval.intval = ANDEQ; return(C_ASOP); }
"^="			{ yylval.intval = EREQ; return(C_ASOP); }
"|="			{ yylval.intval = OREQ; return(C_ASOP); }
">>"			{ yylval.intval = RS; return(C_SHIFTOP); }
"<<"			{ yylval.intval = LS; return(C_SHIFTOP); }
"++"			{ yylval.intval = INCR; return(C_INCOP); }
"--"			{ yylval.intval = DECR; return(C_INCOP); }
"->"			{ yylval.intval = STREF; return(C_STROP); }
"&&"			{ yylval.intval = ANDAND; return(C_ANDAND); }
"||"			{ yylval.intval = OROR; return(C_OROR); }
"<="			{ yylval.intval = LE; return(C_RELOP); }
">="			{ yylval.intval = GE; return(C_RELOP); }
"=="			{ yylval.intval = EQ; return(C_EQUOP); }
"!="			{ yylval.intval = NE; return(C_EQUOP); }
";"			{ notype = 0; return(';'); }
("{"|"<%")		{ notype = 0; return('{'); }
("}"|"%>")		{ if (rpole) notype = 1; return('}'); }
","			{ if (parbal && !inoso) notype = 0;
				if (parbal == 0) notype = 1; return(','); }
":"			{ if (doing_init) nodinit--; return(':'); }
"="			{ return('='); }
"("			{ parbal++; notype = 0; return('('); }
")"			{	parbal--;
				inoso = 0;
				if (parbal==0) { notype = 0; }
				if (inattr && parlvl == parbal)
					inattr = 0;
				return(')'); }
("["|"<:")		{ return('['); }
("]"|":>")		{ return(']'); }
"."			{ yylval.intval = DOT; return(C_STROP); }
"&"			{ return('&'); }
"!"			{ yylval.intval = NOT; return(C_UNOP); }
"~"			{ yylval.intval = COMPL; return(C_UNOP); }
"-"			{ return('-'); }
"+"			{ return('+'); }
"*"			{ if (parbal && notype == 0) notype = 1; return('*'); }
"/"			{ yylval.intval = DIV; return(C_DIVOP); }
"%"			{ yylval.intval = MOD; return(C_DIVOP); }
"<"			{ yylval.intval = LT; return(C_RELOP); }
">"			{ yylval.intval = GT; return(C_RELOP); }
"^"			{ return('^'); }
"|"			{ return('|'); }
"?"			{ if (doing_init) nodinit++; return('?'); }
"@"			{ return OBJC_AT; }
^#pragma[ \t].*		{ pragma(); }
^#ident[ \t].*		{ control(CPP_IDENT); }
^#line[ \t].*		{ control(CPP_LINE); }
^#.*			{ control(CPP_HASH); }

[ \t\v\f]		{ }
"\n"			{ ++lineno; STABS_LINE(lineno); }
.			{ /* ignore bad characters */ }

%%

int issyshdr;

int
yywrap(void)
{
	if (0) unput(0); /* quiet gcc */
	return(1);
}

#define	DEFKW	500
#define	KWFUNC	501
#define	KWNOT	502

struct keywords {
	char *name;
	int cword, wclass;
} keywords[] = {
	{ "__except", C_EXCEPT, DEFKW },
	{ "__finally", C_FINALLY, DEFKW },
	{ "__func__", 0, KWFUNC },
	{ "__leave", C_LEAVE, DEFKW },
	{ "__try", C_TRY, DEFKW },
	{ "_Alignas", C_ALIGNAS, DEFKW },
	{ "_Alignof", C_ALIGNOF, DEFKW },
	{ "_Atomic", C_ATOMIC, DEFKW },
	{ "asm", C_ASM, DEFKW },
	{ "auto", AUTO, C_CLASS },
	{ "_Bool", BOOL, C_TYPE },
	{ "break", C_BREAK, DEFKW },
	{ "case", C_CASE, DEFKW },
	{ "char", CHAR, C_TYPE },
	{ "continue", C_CONTINUE, DEFKW },
	{ "_Complex", COMPLEX, C_TYPE },
	{ "const", CON, C_QUALIFIER },
	{ "default", C_DEFAULT, DEFKW },
	{ "do", C_DO, DEFKW },
	{ "double", DOUBLE, C_TYPE },
	{ "else", C_ELSE, DEFKW },
	{ "__float128", LDOUBLE, C_TYPE },
	{ "enum", C_ENUM, KWNOT },
	{ "extern", EXTERN, C_CLASS },
	{ "float", FLOAT, C_TYPE },
	{ "for", C_FOR, DEFKW },
	{ "_Generic", C_GENERIC, DEFKW },
	{ "goto", C_GOTO, KWNOT },
	{ "if", C_IF, DEFKW },
	{ "_Imaginary", IMAG, C_TYPE },
	{ "inline", INLINE, C_FUNSPEC },
	{ "int", INT, C_TYPE },
	{ "long", LONG, C_TYPE },
	{ "_Noreturn", NORETURN, C_FUNSPEC },
	{ "register", REGISTER, C_CLASS },
	{ "restrict", 0, C_QUALIFIER },
	{ "return", C_RETURN, DEFKW },
	{ "short", SHORT, C_TYPE },
	{ "signed", SIGNED, C_TYPE },
	{ "sizeof", C_SIZEOF, DEFKW },
	{ "static", STATIC, C_CLASS },
	{ "_Static_assert", C_STATICASSERT, DEFKW },
	{ "struct", STNAME, C_STRUCT },
	{ "switch", C_SWITCH, DEFKW },
	{ "_Thread_local", THLOCAL, C_CLASS },
	{ "typedef", TYPEDEF, C_CLASS },
	{ "union", UNAME, C_STRUCT },
	{ "unsigned", UNSIGNED, C_TYPE },
	{ "void", VOID, C_TYPE },
	{ "volatile", VOL, C_QUALIFIER },
	{ "while", C_WHILE, DEFKW },
	{ "id", OBJC_ID, C_TYPE },
	{ "Class", OBJC_CLASS, C_TYPE },
	{ "SEL", OBJC_SEL, C_TYPE },
	{ "__strong", ARC_STRONG, C_QUALIFIER },
	{ "__weak", ARC_WEAK, C_QUALIFIER },
	{ "__unsafe_unretained", ARC_UNSAFE, C_QUALIFIER },
	{ "__autoreleasing", ARC_AUTORELEASING, C_QUALIFIER },
};
	

void
kwinit(void)
{
	struct symtab *s;
	int i, n = sizeof(keywords) / sizeof(keywords[0]);

	for (i = 0; i < n; i++) {
		s = lookup(addname(keywords[i].name), 0);
		s->sclass = KEYWORD;
		s->soffset = i;
	}
}

int
kwdecode(struct symtab *s)
{
	struct keywords *kw = &keywords[s->soffset];

	if (inattr && kw->cword != C_SIZEOF)
		return C_NAME;

	switch (kw->wclass) {
	case C_TYPE:
		yylval.type = kw->cword;
		notype=1;
		break;

	case C_CLASS:
		if (kw->cword == THLOCAL)
			uerror("_Thread_local not supported");
		yylval.type = kw->cword;
		break;

	case C_QUALIFIER:
		yylval.type = kw->cword;
		break;

	case C_FUNSPEC:
		yylval.type = kw->cword;
		break;

	case DEFKW:
		return kw->cword;

	case KWFUNC:
		if (cftnsp == NULL)
			uerror("__func__ outside function");
		yylval.strp = cftnsp->sname;
		return(C_STRING);

	case KWNOT:
		notype = 1;
		return kw->cword;

	case C_STRUCT:
		notype = 1;
		yylval.intval = kw->cword;
		break;

	default:
		cerror("keyword %s not found", kw->name);
	}
	return kw->wclass;
}

/* Objective-C keywords */
struct objc_keywords {
	char *name;
	int token;
} objc_keywords[] = {
	{ "interface", OBJC_INTERFACE },
	{ "implementation", OBJC_IMPLEMENTATION },
	{ "protocol", OBJC_PROTOCOL },
	{ "end", OBJC_END },
	{ "class", OBJC_CLASS_KEYWORD },
	{ "public", OBJC_PUBLIC },
	{ "private", OBJC_PRIVATE },
	{ "protected", OBJC_PROTECTED },
	{ "package", OBJC_PACKAGE },
	{ "selector", OBJC_SELECTOR },
	{ "encode", OBJC_ENCODE },
	{ "property", OBJC_PROPERTY },
	{ "synthesize", OBJC_SYNTHESIZE },
	{ "dynamic", OBJC_DYNAMIC },
	{ "optional", OBJC_OPTIONAL },
	{ "required", OBJC_REQUIRED },
	{ "try", OBJC_TRY },
	{ "catch", OBJC_CATCH },
	{ "finally", OBJC_FINALLY },
	{ "throw", OBJC_THROW },
	{ "synchronized", OBJC_SYNCHRONIZED },
	/* Boolean literals */
	{ "YES", OBJC_YES },
	{ "NO", OBJC_NO },
	/* ARC bridging casts */
	{ "__bridge", ARC_BRIDGE },
	{ "__bridge_retained", ARC_BRIDGE_RETAINED },
	{ "__bridge_transfer", ARC_BRIDGE_TRANSFER },
	{ NULL, 0 }
};

int
objc_keyword(char *kw)
{
	int i;

	for (i = 0; objc_keywords[i].name; i++) {
		if (strcmp(kw, objc_keywords[i].name) == 0)
			return objc_keywords[i].token;
	}
	return 0;
}

#ifdef notdef
static long double
typround(long double dc, char *e, TWORD *tw)
{
	int im = 0;

	*tw = DOUBLE;
	for (; *e; e++) {
		switch (*e) {
		case 'f':
		case 'F':
			*tw = FLOAT;
			dc = (float)dc;
			break;
		case 'l':
		case 'L':
			*tw = ctype(LDOUBLE);
			break;
		case 'i':
		case 'I':
			im = 1;
			break;
		}
	}
	if (*tw == DOUBLE)
		dc = (double)dc;
#ifndef NO_COMPLEX
	if (im)
		*tw += (FIMAG-FLOAT);
#endif
	return dc;
}

/*
 * Return which type current fp string has.
 */
static TWORD
endtyp(char *s)
{
	TWORD tw = DOUBLE;

	for (; *s; s++)
		;
	s--;
	if (*s == 'i' || *s == 'I')
		tw += (FIMAG-FLOAT), s--;
	if (*s == 'f' || *s == 'F')
		tw--, s--;
	else if (*s == 'l' || *s == 'L')
		tw++, s--;
	if ((*s == 'i' || *s == 'I') && ISFTY(tw))
		tw += (FIMAG-FLOAT), s--;
/* XXX complain */
	return tw;
}

FLT *
floatcon(char *s)
{
	FLT *flt;
	TWORD tw;

	flt = stmtalloc(sizeof(FLT));
#ifdef NATIVE_FLOATING_POINT
#ifdef HAVE_STRTOLD
	flt->fp = strtold(s, NULL);
#else
	flt->fp = strtod(s, NULL);
#endif
#else
	flt->sf = strtosf(s);		/* parse number */
#endif
	flt->t = tw = ctype(endtyp(s));
	if (ISITY(tw))
		tw -= (FIMAG-FLOAT);
	return flt;
}

static int
h2n(int ch)
{
	if (ch >= '0' && ch <= '9')
		return ch - '0';
	if (ch >= 'a' && ch <= 'f')
		return ch - 'a' + 10;
	return ch - 'A' + 10;
	
}

/*
 * Get exponent.  Max size is 16 bits.
 */
static int
fdecl(char *c, char **ep)
{
	int minus = 0;
	int val = 0;

	if (*c == '+')
		c++;
	else if (*c == '-')
		minus = 1, c++;

	while (isdigit((int)*c)) {
		val *= 10;
		val += *c++ - '0';
		if (val > 32769)
			val = 32769;
	}
	if (minus)
		val = -val;
	*ep = c;
	return val;
}

FLT *
fhexcon(char *c)
{
	FLT *flt;
	TWORD tw;
	char *ep;
	long double d;
	int i, ed;

	d = 0.0;
	ed = 0;
	c+= 2; /* skip 0x */
#define FSET(n) { d *= 2; if (i & n) d += 1.0; }
	for (; *c != '.' && *c != 'p' && *c != 'P'; c++) {
		i = h2n(*c);
		FSET(8); FSET(4); FSET(2); FSET(1);
	}
	if (*c != '.' && *c != 'p' && *c != 'P')
		cerror("fhexcon");
	if (*c == '.') {
		c++;
		for (; *c != 'p' && *c != 'P'; c++) {
			i = h2n(*c);
			FSET(8); FSET(4); FSET(2); FSET(1);
			ed -= 4;
		}
	}
	if (*c != 'P' && *c != 'p')
		cerror("fhexcon2");
	c++;
	ed += fdecl(c, &ep);

	/* avoid looping in vain. Idea from Fred J. Tydeman */
	if (ed > 32769) ed = 32769;
	if (ed < -32769) ed = -32769;

	while (ed > 0)
		d *= 2, ed--;
	while (ed < 0)
		d /= 2, ed++;
	d = typround(d, ep, &tw);
	flt = stmtalloc(sizeof(FLT));
	flt->fp = d;
	flt->t = tw;
	return flt;
}
#else /* NATIVE_FLOATING_POINT */
static TWORD
endtyp(char *s)
{
	TWORD tw = DOUBLE;

	for (; *s; s++)
		;
	s--;
	if (*s == 'i' || *s == 'I')
		tw += (FIMAG-FLOAT), s--;
	if (*s == 'f' || *s == 'F')
		tw--, s--;
	else if (*s == 'l' || *s == 'L')
		tw++, s--;
	if ((*s == 'i' || *s == 'I') && ISFTY(tw))
		tw += (FIMAG-FLOAT), s--;
/* XXX complain */
	return tw;
}

FLT *
fhexcon(char *s)
{
	FLT *flt = stmtalloc(sizeof(FLT));

	flt->t = ctype(endtyp(s));
	flt->sf = strtosf(s, flt->t);
	return flt;
}

FLT *
floatcon(char *s)
{
	return fhexcon(s);
}

#endif /* NATIVE_FLOATING_POINT */

P1ND *
cvtdig(int radix)
{
	P1ND *p;
	TWORD ntype;
	unsigned long long v;
	char *ch = yytext;
	int n, numl, numu;

	if (radix == 16 || radix == 2)
		ch += 2; /* Skip 0x or 0b */

	v = 0;
	while ((*ch >= '0' && *ch <= '9') || (*ch >= 'a' && *ch <= 'f') ||
	    (*ch >= 'A' && *ch <= 'F')) {
		v *= radix;
		n = *ch;
		n = (n <= '9' ? n - '0' : (n > 'F' ? n - 'a' : n - 'A') + 10);
		ch++;
		v += n;
	}
	/* Parse trailing chars */
	ntype = INT;
	numl = numu = 0;
	for (n = 0; n < 3; n++) {
		if (*ch == 0)
			break;
		if ((*ch == 'l' || *ch == 'L') && numl < 2)
			ntype+=2, numl++;
		else if ((*ch == 'u' || *ch == 'U') && numu < 1)
			ntype = ENUNSIGN(ntype), numu++;
		else if (*ch == 'i')
			ntype = DOUBLE;
		else
			break;
		ch++;
	}
	if (*ch)
		uerror("constant has too many '%c'", *ch);

	switch (ntype) {
	case DOUBLE: /* special case */
		p = block(FCON, NULL, NULL, IMAG, 0, 0);
		p->n_dcon = stmtalloc(sizeof(FLT));
		FLOAT_INT2FP(p->n_dcon, v, ULONGLONG);
		FLOAT_FP2FP(p->n_dcon, DOUBLE);
		return p;

	case INT:
	case LONG:
	case LONGLONG:
		if (radix == 10) {
			if (ntype == LONGLONG)
				break;
			if (v > MAX_LONG)
				ntype = LONGLONG;
			else if (v > MAX_INT)
				ntype = LONG;
		} else {
			if (v > MAX_LONGLONG) {
				ntype = ULONGLONG;
			} else if (v > MAX_ULONG) {
				if (ntype < LONGLONG)
					ntype = LONGLONG;
			} else if (v > MAX_LONG) {
				if (ntype < ULONG)
					ntype = ULONG;
			} else if (v > MAX_UNSIGNED) {
				if (ntype < LONG)
					ntype = LONG;
			} else if (v > MAX_INT) {
				if (ntype < UNSIGNED)
					ntype = UNSIGNED;
			}
		}
		break;
	case UNSIGNED:
	case ULONG:
		if (v > MAX_ULONG) {
			ntype = ULONGLONG;
		} else if (v > MAX_UNSIGNED)
			ntype = ULONG;
		break;	
	}	

	ntype = ctype(ntype);
	p = xbcon(v, NULL, ntype);
	ASGLVAL(p->n_slval, v);

	return p;
}

/*
 * return value of escaped character constant
 */
unsigned int
esccon(char **sptr)
{
	unsigned int val;
	char *wr = *sptr;
	char c;

	wr++;	/* skip \ */
	switch (c = *wr++) {
	case 'a': val = '\a'; break;
	case 'b': val = '\b'; break;
#ifdef GCC_COMPAT
	case 'e': val = '\033'; break;
#endif
	case 'f': val = '\f'; break;
	case 'n': val = '\n'; break;
	case 'r': val = '\r'; break;
	case 't': val = '\t'; break;
	case 'v': val = '\v'; break;

	case '\"': val = '\"'; break;
	case '\'': val = '\''; break;
	case '\?': val = '\?'; break;
	case '\\': val = '\\'; break;

	case 'x':
		val = 0;
		for (;;) {
			c = *wr;
			if (c >= '0' && c <= '9')
				c = c - '0';
			else if (c >= 'a' && c <= 'f')
				c = c - 'a' + 10;
			else if (c >= 'A' && c <= 'F')
				c = c - 'A' + 10;
			else
				break;

			val = (val << 4) + c;
			wr++;
		}
		break;

	case '0': case '1': case '2': case '3':
	case '4': case '5': case '6': case '7':
		val = (c - '0');
		c = *wr;
		if (c >= '0' && c <= '7') {
			wr++;
			val = (val << 3) + (c - '0');
			c = *wr;
			if (c >= '0' && c <= '7') {
				wr++;
				val = (val << 3) + (c - '0');
			}
		}
		break;

	default:
		werror("unknown escape sequence \\%c", c);
		val = c;
		break;
	}

	*sptr = wr;
	return val;
}

/*
 * Convert (one) character constant to an int.
 * Handle as unsigned except if only one character and char is signed.
 */
P1ND *
charcon(void)
{
	unsigned int val = 0, i = 0;
	char *pp = yytext;

	pp++;	/* skip ' */
	while (*pp != '\'') {
		val <<= SZCHAR; /* XXX big endian? */
		if (*pp == '\\')
			val |= esccon(&pp);
		else
			val |= (*pp++ & ((1 << SZCHAR)-1));
		i++;
	}

	if (i == 0)
		uerror("empty character constant");
	else if (i == 1 && xuchar == 0)
		val = (signed char)val; /* XXX other sizes of char? */
	else if (i > 1)
		werror("too many characters in character constant");

	return bcon(val);
}

/*
 * Convert a wide-character constant to an unsigned int
 */
P1ND *
wcharcon(void)
{
	unsigned int val = 0, i = 0;
	char *pp = yytext;

	pp++;	/* skip L */
	pp++;	/* skip ' */
	while (*pp != '\'') {
		/*
		 * although u82cp() does handle escaped values, we deal
		 * with them directly since otherwise you can't process
		 * values which might be valid utf8 prefix
		 */
		if (*pp == '\\')
			val = esccon(&pp);
		else
			val = (unsigned)u82cp(&pp);

		i++;
	}

	if (i == 0)
		uerror("empty wide-character constant");
	else if (i > 1)
		werror("too many characters in wide-character constant");

	return xbcon(val, NULL, ctype(UNSIGNED));
}

#ifndef MYDOTFILE
/*
 * Get basename and print it as '.file "basename.c"'
 */
static void
printdotfile(char *file)
{
	char *p;

	if ((p = strrchr(file, '/')) == NULL)
		p = file;
	else
		p++;
	printf(PRTPREF "\t.file \"%s\"\n", p);
}
#endif

void
control(int t)
{
	char *wr = yytext;
	char *eptr;
	int val;

	wr++;	/* Skip initial '#' */
	switch (t) {
	case CPP_IDENT:
		return;	/* Just skip these for now. */

	case CPP_LINE:
		wr += 4;
		/* FALLTHROUGH */
	case CPP_HASH:
		val = (unsigned)strtol(wr, &eptr, 10);
		if (wr == eptr)	/* Illegal string */
			goto bad;
		wr = eptr;
		lineno = val - 1;
		while (*wr && *wr != '\"')
			wr++;
		if (*wr == 0)
			return;
		if (*wr++ != '\"')
			goto bad;
		eptr = wr;
		while (*wr && *wr != '\"')
			wr++;
		if (*wr != '\"')
			goto bad;
		*wr++ = 0;
		free(ftitle);
		ftitle = xstrdup(eptr);
		while (*wr == ' ')
			wr++;
		issyshdr = 0;
		if (*wr == '3')
			issyshdr = 1;
#ifdef DWARF
		if (gflag)
			dwarf_file(ftitle);
#endif
#ifdef STABS
		if (gflag)
			stabs_file(ftitle);
#endif
		if (dotfile == 0) {
			dotfile++;
			printdotfile(ftitle);
		}
	}
	return;
bad:
	werror("%s: illegal control", yytext);
}

int pragma_allpacked;
int pragma_packed, pragma_aligned;

/* Watcom pragma aux support - struct defined in pass1.h */
struct pragma_aux_info pragma_aux_pending;

/* Calling convention definitions */
#define MAX_CALLING_CONVENTIONS 64

struct calling_convention {
	char *name;        /* Convention name */
	char *parm_regs;   /* Parameter register list */
	char *value_reg;   /* Return value register */
	char *modify_regs; /* Modified register list */
	int is_caller;     /* Caller cleans stack */
};

static struct calling_convention calling_conventions[MAX_CALLING_CONVENTIONS];
static int num_calling_conventions = 0;

/* Function to register a calling convention */
static void
register_calling_convention(const char *name, const char *parm_regs,
                            const char *value_reg, const char *modify_regs,
                            int is_caller)
{
	if (num_calling_conventions >= MAX_CALLING_CONVENTIONS)
		return;

	calling_conventions[num_calling_conventions].name = tmpstrdup(name);
	calling_conventions[num_calling_conventions].parm_regs =
	    parm_regs ? tmpstrdup(parm_regs) : NULL;
	calling_conventions[num_calling_conventions].value_reg =
	    value_reg ? tmpstrdup(value_reg) : NULL;
	calling_conventions[num_calling_conventions].modify_regs =
	    modify_regs ? tmpstrdup(modify_regs) : NULL;
	calling_conventions[num_calling_conventions].is_caller = is_caller;
	num_calling_conventions++;
}

/* Function to find a calling convention */
static struct calling_convention *
find_calling_convention(const char *name)
{
	int i;

	for (i = 0; i < num_calling_conventions; i++) {
		if (strcmp(calling_conventions[i].name, name) == 0)
			return &calling_conventions[i];
	}
	return NULL;
}

/* Initialize predefined calling conventions */
static void
init_calling_conventions(void)
{
	static int initialized = 0;

	if (initialized)
		return;
	initialized = 1;

	/* x86 calling conventions */
#if defined(mach_i386) || defined(mach_amd64) || defined(mach_i86)
	/* cdecl: caller cleanup, parameters on stack right-to-left */
	register_calling_convention("__cdecl", NULL, "eax", "eax ecx edx", 1);

	/* stdcall: callee cleanup, parameters on stack right-to-left */
	register_calling_convention("__stdcall", NULL, "eax", "eax ecx edx", 0);

	/* fastcall: first two params in ECX/EDX, rest on stack, callee cleanup */
	register_calling_convention("__fastcall", "ecx edx", "eax", "eax ecx edx", 0);

	/* thiscall: this in ECX, rest on stack, callee cleanup (C++) */
	register_calling_convention("__thiscall", "ecx", "eax", "eax ecx edx", 0);

	/* vectorcall: uses vector registers, x64 only */
	register_calling_convention("__vectorcall", "ecx edx", "eax", "eax ecx edx", 0);
#endif

	/* Generic conventions */
	register_calling_convention("_System", NULL, NULL, NULL, 0);
	register_calling_convention("_Optlink", NULL, NULL, NULL, 1);
	register_calling_convention("_Syscall", NULL, NULL, NULL, 0);
}

static int
pragmas_weak(char *str)
{
	struct symtab *sp;
	char *s1, *s2;

	if ((s1 = pragtok(NULL)) == NULL)
		return 1;
	if ((s2 = pragtok(NULL)) == NULL) {
		sp = lookup(addname(s1), SNORMAL);
#ifdef GCC_COMPAT
		sp->sap = attr_add(sp->sap, gcc_attr_parse(bdty(NAME, "weak")));
#else
		sp->sap = NULL;
#endif
	} else if (*s2 == '=') {
		if ((s2 = pragtok(NULL)) == NULL)
			return 1;
		sp = lookup(addname(s2), SNORMAL);
#ifdef GCC_COMPAT
		sp->sap = attr_add(sp->sap, gcc_attr_parse(bdty(CALL,
		    bdty(NAME, "aliasweak"), bdty(STRING, s1, 0))));
#else
		sp->sap = NULL;
#endif
	} else
		return 1;
	return 0;
}

char *pragstore;

/* trivial tokenizer for pragmas */
#define ps pragstore
char *
pragtok(char *sin)
{
	static char ss[2];
	char *rv;

	if (sin)
		ps = sin;

	for (; isspace((int)*ps); ps++)
		;
	if (*ps == 0)
		return NULL;
	for (rv = ps; isalpha((int)*ps) || isdigit((int)*ps) || *ps == '_'; ps++)
		;
	ss[0] = *ps;
	if (rv == ps) {
		rv = ss, ps++;
	} else {
		*ps = 0;
		rv = tmpstrdup(rv);
		*ps = ss[0];
	}
	return rv;
}

/* return 1 on error */
int
eat(int ch)
{
	char *s = pragtok(0);
	return (s == 0 || *s != ch);
}

static int
pragmas_alpack(char *t)
{
	char *s;
	int ap;

	ap = (s = pragtok(0)) ? atoi(s) : 1;
	if (strcmp(t, "packed") == 0)
		pragma_packed = ap;
	else
		pragma_aligned = ap;
	return 0;
}


/*
 * Packing control.
 */
static int
pragmas_pack(char *t)
{
#define	PACKSTKSZ 10
	static int packstk[PACKSTKSZ], packptr;
	char *s;

	if (eat('('))
		return 1;
	s = pragtok(0);
	if (*s == ')')
		return pragma_allpacked = 0;
	if (strcmp(s, "push") == 0) {
		if (packptr == PACKSTKSZ)
			uerror("too many push");
		packstk[packptr++] = pragma_allpacked;
		s = pragtok(0);
		if (*s == ')')
			return 0;
		if (*s != ',')
			return 1;
		s = pragtok(0);
	} else if (strcmp(s, "pop") == 0) {
		if (packptr == 0)
			uerror("stack empty");
		pragma_allpacked = packstk[--packptr];
		return eat(')');
	}

	if (*s < '0' || *s > '9') /* no number */
		return 1;
	pragma_allpacked = atoi(s);
	return eat(')');
}

static int      
pragmas_unsupp(char *t) 
{ 
	werror("#pragma %s unsupported", t);
	return 0; /* Just ignore */
}

static int
pragmas_stdc(char *t)
{
	return 0; /* Just ignore */
}

/*
 * Watcom pragma comment - embed comments/directives in object file
 * Syntax: #pragma comment(type, "string")
 */
static int
pragmas_comment(char *t)
{
	char *type, *str;

	if (eat('('))
		return 1;

	type = pragtok(NULL);
	if (type == NULL)
		return 1;

	/* Common types: lib, linker, compiler, user */
	if (eat(','))
		return 1;

	str = pragtok(NULL);
	/* String parsing would go here - for now just accept it */

	eat(')');
	return 0; /* Silently accept */
}

/*
 * Watcom pragma message - print message during compilation
 * Syntax: #pragma message("text")
 */
static int
pragmas_message(char *t)
{
	char *msg = NULL;
	char *tok;

	if (eat('('))
		return 1;

	/* Collect everything until closing paren as the message */
	tok = ps;
	while (*ps && *ps != ')')
		ps++;

	if (ps > tok) {
		size_t len = ps - tok;
		msg = malloc(len + 1);
		if (msg) {
			strncpy(msg, tok, len);
			msg[len] = '\0';
			/* Remove surrounding quotes if present */
			if (msg[0] == '"' && msg[len-1] == '"') {
				msg[len-1] = '\0';
				fprintf(stderr, "#pragma message: %s\n", msg + 1);
			} else {
				fprintf(stderr, "#pragma message: %s\n", msg);
			}
			free(msg);
		}
	}

	eat(')');
	return 0;
}

/*
 * Watcom pragma warning - control warning messages
 * Syntax: #pragma warning(push|pop|disable|enable : num...)
 */
static int
pragmas_warning(char *t)
{
	char *tok;

	if (eat('('))
		return 1;

	tok = pragtok(NULL);
	if (tok == NULL)
		return 1;

	/* Handle push, pop, disable, enable */
	if (strcmp(tok, "push") == 0 || strcmp(tok, "pop") == 0) {
		/* Accept but don't implement warning stack */
	} else if (strcmp(tok, "disable") == 0 || strcmp(tok, "enable") == 0) {
		/* Would process warning numbers after : */
		if (*ps == ':') {
			ps++;
			/* Skip warning numbers */
			while (pragtok(NULL) != NULL && *ps != ')')
				;
		}
	}

	while (*ps && *ps != ')')
		ps++;
	eat(')');
	return 0;
}

/*
 * Watcom pragma once - include file only once
 * Syntax: #pragma once
 */
static int
pragmas_once(char *t)
{
	/* This would need integration with the preprocessor */
	/* For now, just accept it silently */
	return 0;
}

/*
 * Watcom pragma intrinsic - use intrinsic function implementations
 * Syntax: #pragma intrinsic(func1, func2, ...)
 */
static int
pragmas_intrinsic(char *t)
{
	char *func;

	if (eat('('))
		return 1;

	/* Collect function names */
	while ((func = pragtok(NULL)) != NULL && *func != ')') {
		if (*func == ',')
			continue;
		/* Would mark function as intrinsic */
	}

	return 0;
}

/*
 * Watcom pragma function - disable intrinsic for specified functions
 * Syntax: #pragma function(func1, func2, ...)
 */
static int
pragmas_function(char *t)
{
	char *func;

	if (eat('('))
		return 1;

	/* Collect function names */
	while ((func = pragtok(NULL)) != NULL && *func != ')') {
		if (*func == ',')
			continue;
		/* Would disable intrinsic for function */
	}

	return 0;
}

/*
 * Watcom pragma code_seg/data_seg - control segment placement
 * Syntax: #pragma code_seg("name") or #pragma data_seg("name")
 */
static int
pragmas_segment(char *t)
{
	char *seg_name = NULL;

	if (eat('('))
		return 1;

	seg_name = pragtok(NULL);
	/* Would control segment for subsequent code/data */

	eat(')');
	return 0;
}

/*
 * Watcom pragma alloc_text - allocate function to specific text segment
 * Syntax: #pragma alloc_text(segment, function)
 */
static int
pragmas_alloc_text(char *t)
{
	char *segment, *function;

	if (eat('('))
		return 1;

	segment = pragtok(NULL);
	if (segment == NULL)
		return 1;

	if (eat(','))
		return 1;

	function = pragtok(NULL);
	/* Would place function in specified segment */

	eat(')');
	return 0;
}

/*
 * Watcom pragma inline_depth - set inline expansion depth
 * Syntax: #pragma inline_depth(n)
 */
static int
pragmas_inline_depth(char *t)
{
	char *depth;

	if (eat('('))
		return 1;

	depth = pragtok(NULL);
	/* Would control inline depth */

	eat(')');
	return 0;
}

/*
 * Watcom pragma inline_recursion - enable/disable recursive inlining
 * Syntax: #pragma inline_recursion(on|off)
 */
static int
pragmas_inline_recursion(char *t)
{
	char *setting;

	if (eat('('))
		return 1;

	setting = pragtok(NULL);
	/* Would control recursive inlining */

	eat(')');
	return 0;
}

/*
 * Watcom pragma auto_inline - enable/disable automatic inlining
 * Syntax: #pragma auto_inline(on|off)
 */
static int
pragmas_auto_inline(char *t)
{
	char *setting;

	if (eat('('))
		return 1;

	setting = pragtok(NULL);
	/* Would control auto inlining */

	eat(')');
	return 0;
}

/*
 * Watcom pragma enum - control enum size
 * Syntax: #pragma enum int|minimum|original|pop
 */
static int
pragmas_enum(char *t)
{
	char *setting = pragtok(NULL);

	if (setting == NULL)
		return 1;

	/* Accept int, minimum, original, pop */
	if (strcmp(setting, "int") == 0) {
		/* Force enum to int size */
	} else if (strcmp(setting, "minimum") == 0) {
		/* Use minimum required size */
	} else if (strcmp(setting, "original") == 0) {
		/* Restore original setting */
	} else if (strcmp(setting, "pop") == 0) {
		/* Pop enum setting from stack */
	}

	return 0;
}

/*
 * Watcom pragma read_only_file - mark file as read-only
 * Syntax: #pragma read_only_file
 */
static int
pragmas_read_only_file(char *t)
{
	/* Mark current file as read-only for dependencies */
	return 0;
}

/*
 * Watcom pragma library - specify library to link
 * Syntax: #pragma library("libname")
 */
static int
pragmas_library(char *t)
{
	char *libname;

	if (eat('('))
		return 1;

	libname = pragtok(NULL);
	/* Would add library to linker command */

	eat(')');
	return 0;
}

/*
 * Watcom pragma include_alias - create include file alias
 * Syntax: #pragma include_alias("alias", "realfile")
 */
static int
pragmas_include_alias(char *t)
{
	char *alias, *real;

	if (eat('('))
		return 1;

	alias = pragtok(NULL);
	if (alias == NULL)
		return 1;

	if (eat(','))
		return 1;

	real = pragtok(NULL);
	/* Would create preprocessor alias */

	eat(')');
	return 0;
}

/*
 * Watcom pragma error - emit error message
 * Syntax: #pragma error "message"
 */
static int
pragmas_error(char *t)
{
	char *msg = NULL;
	char *tok;

	/* Collect everything as the error message */
	tok = ps;
	while (*ps && *ps != '\n' && *ps != '\r')
		ps++;

	if (ps > tok) {
		size_t len = ps - tok;
		msg = malloc(len + 1);
		if (msg) {
			strncpy(msg, tok, len);
			msg[len] = '\0';
			/* Remove surrounding quotes if present */
			char *start = msg;
			char *end = msg + len - 1;
			while (*start == ' ' || *start == '\t') start++;
			while (end > start && (*end == ' ' || *end == '\t')) end--;
			if (*start == '"' && *end == '"') {
				end--;
				start++;
				*++end = '\0';
			}
			uerror("#pragma error: %s", start);
			free(msg);
		}
	}

	return 0;
}

/*
 * Watcom pragma disable_message - disable specific warning/error
 * Syntax: #pragma disable_message(num1, num2, ...)
 */
static int
pragmas_disable_message(char *t)
{
	char *num;

	if (eat('('))
		return 1;

	/* Collect message numbers */
	while ((num = pragtok(NULL)) != NULL && *num != ')') {
		if (*num == ',')
			continue;
		/* Would disable message number */
	}

	return 0;
}

/*
 * Watcom pragma enable_message - re-enable specific warning/error
 * Syntax: #pragma enable_message(num1, num2, ...)
 */
static int
pragmas_enable_message(char *t)
{
	char *num;

	if (eat('('))
		return 1;

	/* Collect message numbers */
	while ((num = pragtok(NULL)) != NULL && *num != ')') {
		if (*num == ',')
			continue;
		/* Would enable message number */
	}

	return 0;
}

/*
 * Watcom pragma extref - external reference
 * Syntax: #pragma extref symbol
 */
static int
pragmas_extref(char *t)
{
	char *symbol = pragtok(NULL);

	if (symbol == NULL)
		return 1;

	/* Would create external reference */
	return 0;
}

/*
 * Watcom pragma aux - defines calling conventions and register usage
 * Syntax: #pragma aux symbol [parm [reg...]] [value [reg]] [modify [reg...]] [caller]
 *         #pragma aux (convention) function_name
 *         #pragma aux convention_name parm [...] value [...] modify [...] caller
 */
static int
pragmas_aux(char *t)
{
	char *tok;
	struct calling_convention *cc;
	char *convention_name = NULL;
	int is_defining_convention = 0;

	/* Initialize calling conventions on first use */
	init_calling_conventions();

	/* Clear previous pragma aux info */
	pragma_aux_pending.symbol = NULL;
	pragma_aux_pending.parm_regs = NULL;
	pragma_aux_pending.value_reg = NULL;
	pragma_aux_pending.modify_regs = NULL;
	pragma_aux_pending.is_caller = 0;

	/* Get symbol name or convention reference */
	tok = pragtok(NULL);
	if (tok == NULL)
		return 1; /* Error: no symbol name */

	/* Check for calling convention application: #pragma aux (convention) function */
	if (*tok == '(') {
		char *conv_name = pragtok(NULL);
		if (conv_name == NULL)
			return 1;

		cc = find_calling_convention(conv_name);
		if (cc == NULL) {
			werror("unknown calling convention: %s", conv_name);
			return 1;
		}

		if (eat(')'))
			return 1;

		/* Get function name */
		tok = pragtok(NULL);
		if (tok == NULL)
			return 1;

		/* Apply calling convention to function */
		pragma_aux_pending.symbol = tmpstrdup(tok);
		pragma_aux_pending.parm_regs = cc->parm_regs ? tmpstrdup(cc->parm_regs) : NULL;
		pragma_aux_pending.value_reg = cc->value_reg ? tmpstrdup(cc->value_reg) : NULL;
		pragma_aux_pending.modify_regs = cc->modify_regs ? tmpstrdup(cc->modify_regs) : NULL;
		pragma_aux_pending.is_caller = cc->is_caller;

		return 0;
	}

	pragma_aux_pending.symbol = tmpstrdup(tok);

	/* Check if this is a calling convention definition (starts with __ or _) */
	if ((tok[0] == '_' && tok[1] == '_') || (tok[0] == '_' && isupper((int)tok[1]))) {
		is_defining_convention = 1;
		convention_name = tmpstrdup(tok);
	}

	/* Parse optional keywords */
	while ((tok = pragtok(NULL)) != NULL) {
		if (strcmp(tok, "parm") == 0) {
			/* Parse parameter registers: parm [reg1] [reg2] ... */
			char regbuf[256] = "";
			tok = pragtok(NULL);
			if (tok && *tok == '[') {
				/* Collect register list until we find something that's not a register spec */
				while ((tok = pragtok(NULL)) != NULL) {
					if (*tok == ']') {
						continue; /* Skip closing bracket */
					}
					if (*tok == '[') {
						continue; /* Skip opening bracket */
					}
					/* Check if this is a keyword for next section */
					if (strcmp(tok, "value") == 0 || strcmp(tok, "modify") == 0 ||
					    strcmp(tok, "caller") == 0) {
						/* Put it back by backing up ps */
						ps -= strlen(tok);
						break;
					}
					/* Accumulate register name */
					if (regbuf[0])
						strcat(regbuf, " ");
					strcat(regbuf, tok);
				}
				if (regbuf[0])
					pragma_aux_pending.parm_regs = tmpstrdup(regbuf);
			}
		} else if (strcmp(tok, "value") == 0) {
			/* Parse return value register: value [reg] */
			tok = pragtok(NULL);
			if (tok && *tok == '[') {
				tok = pragtok(NULL);
				if (tok && *tok != ']') {
					pragma_aux_pending.value_reg = tmpstrdup(tok);
				}
				/* Skip closing bracket if present */
				tok = pragtok(NULL);
			}
		} else if (strcmp(tok, "modify") == 0) {
			/* Parse modified registers: modify [reg1 reg2 ...] */
			char regbuf[256] = "";
			tok = pragtok(NULL);
			if (tok && *tok == '[') {
				while ((tok = pragtok(NULL)) != NULL) {
					if (*tok == ']')
						break;
					if (*tok == '[')
						continue;
					if (regbuf[0])
						strcat(regbuf, " ");
					strcat(regbuf, tok);
				}
				if (regbuf[0])
					pragma_aux_pending.modify_regs = tmpstrdup(regbuf);
			}
		} else if (strcmp(tok, "caller") == 0) {
			pragma_aux_pending.is_caller = 1;
		} else if (strcmp(tok, "routine") == 0) {
			pragma_aux_pending.is_caller = 0;
		} else if (strcmp(tok, "loadds") == 0) {
			/* loadds keyword - just accept it */
		} else if (strcmp(tok, "export") == 0) {
			/* export keyword - just accept it */
		} else if (*tok == '=') {
			/* Inline assembly - skip the rest */
			break;
		}
		/* Ignore other unknown keywords */
	}

	/* If this is a calling convention definition, register it */
	if (is_defining_convention && convention_name) {
		register_calling_convention(
			convention_name,
			pragma_aux_pending.parm_regs,
			pragma_aux_pending.value_reg,
			pragma_aux_pending.modify_regs,
			pragma_aux_pending.is_caller
		);
		/* Don't clear pragma_aux_pending so it can still apply to next symbol */
	}

	return 0; /* Success */
}

struct pragmas {
	char *name;
	int (*fun)(char *);
} pragmas[] = {
	{ "pack", pragmas_pack },
	{ "packed", pragmas_alpack },
	{ "aligned", pragmas_alpack },
	{ "rename", pragmas_unsupp },
	{ "aux", pragmas_aux },
	/* Watcom pragmas */
	{ "comment", pragmas_comment },
	{ "message", pragmas_message },
	{ "warning", pragmas_warning },
	{ "once", pragmas_once },
	{ "intrinsic", pragmas_intrinsic },
	{ "function", pragmas_function },
	{ "code_seg", pragmas_segment },
	{ "data_seg", pragmas_segment },
	{ "alloc_text", pragmas_alloc_text },
	{ "inline_depth", pragmas_inline_depth },
	{ "inline_recursion", pragmas_inline_recursion },
	{ "auto_inline", pragmas_auto_inline },
	{ "enum", pragmas_enum },
	{ "read_only_file", pragmas_read_only_file },
	{ "library", pragmas_library },
	{ "include_alias", pragmas_include_alias },
	{ "error", pragmas_error },
	{ "disable_message", pragmas_disable_message },
	{ "enable_message", pragmas_enable_message },
	{ "extref", pragmas_extref },
#ifdef GCC_COMPAT
	{ "GCC", pragmas_gcc },
#endif
	{ "STDC", pragmas_stdc },
	{ "weak", pragmas_weak },
	{ "ident", NULL },
	{ 0 },
};

/*
 * got a full pragma line.  Split it up here.
 */
static void
pragma(void)
{
	struct pragmas *p;
	char *t, *pt;

	if ((t = pragtok(&yytext[7])) != NULL) {
		pt = ps;
		for (p = pragmas; p->name; p++) {
			if (strcmp(t, p->name) == 0) {
				if (p->fun && (*p->fun)(t))
					uerror("bad argument to #pragma");
				return;
			}
		}
		ps = pt;
		if (mypragma(t))
			return;
	}
	warner(Wunknown_pragmas, t, ps);
}

void
cunput(char c)
{
	unput(c);
}
