/*	$Id$	*/
/*
 * Ruby front-end for PCC compiler
 *
 * This header extends the standard PCC pass1.h with Ruby-specific
 * definitions for implementing a Ruby compiler front-end.
 */

/* Include base compiler definitions */
#include "config.h"

#include <sys/types.h>
#include <stdarg.h>
#include <string.h>
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#include <stdlib.h>

#ifndef MKEXT
#include "external.h"
#else
typedef unsigned int bittype;
#endif
#include "manifest.h"
#include "softfloat.h"

/*
 * Storage classes (inherited from C front-end)
 */
#define SNULL		0
#define AUTO		1
#define EXTERN		2
#define STATIC		3
#define REGISTER	4
#define EXTDEF		5
#define THLOCAL		6
#define KEYWORD		7
#define MOS		8
#define PARAM		9
#define STNAME		10
#define MOU		11
#define UNAME		12
#define TYPEDEF		13
#define ENAME		15
#define MOE		16
#define USTATIC		18

/* Ruby-specific storage classes */
#define RUBY_IVAR	19	/* Instance variable */
#define RUBY_CVAR	20	/* Class variable */
#define RUBY_GVAR	21	/* Global variable */

#define FIELD		0200
#define FLDSIZ		0177
extern	char *scnames(int);

/*
 * Symbol table flags
 */
#define	SNORMAL		0
#define	STAGNAME	01
#define	SLBLNAME	02
#define	SMOSNAME	03
#define	SSTRING		04
#define	NSTYPES		05
#define	SMASK		07

#define	STLS		00010
#define SINSYS		00020
#define	SSTMT		SINSYS
#define SNOCREAT	00040
#define STEMP		00100
#define	SDYNARRAY	00200
#define	SINLINE		00400
#define	SBLK		SINLINE
#define	STNODE		01000
#define	SBUILTIN	02000
#define	SASG		04000
#define	SINREG		010000

/* Ruby-specific symbol flags */
#define RUBY_PRIVATE	020000	/* Private method/variable */
#define RUBY_PROTECTED	040000	/* Protected method */
#define RUBY_PUBLIC	0100000	/* Public method (default) */
#define RUBY_BLOCK	0200000	/* Symbol is a block/proc */
#define RUBY_METHOD	0400000	/* Symbol is a method */

#ifndef AL_INIT
#define	AL_INIT ALINT
#endif

struct rstack;
struct symtab;
union arglist;

/*
 * Dimension/prototype information
 */
union dimfun {
	int	ddim;
	union arglist *dfun;
};

/*
 * Argument list member info
 */
union arglist {
	TWORD type;
	union dimfun *df;
	struct attr *sap;
};
#define TNULL		INCREF(FARG)
#define TELLIPSIS 	INCREF(INCREF(FARG))

/*
 * Ruby-specific node extensions
 */
struct ruby_block_info {
	int nparams;		/* Number of block parameters */
	char **param_names;	/* Block parameter names */
	struct p1node *body;	/* Block body */
};

/*
 * Symbol table definition
 */
struct	symtab {
	struct	symtab *snext;
	int	soffset;
	char	sclass;
	char	slevel;
	short	sflags;
	char	*sname;
	TWORD	stype;
	TWORD	squal;
	union	dimfun *sdf;
	struct	attr *sap;
	/* Ruby-specific extensions */
	struct ruby_block_info *sblock;	/* For block/proc symbols */
};

#define	ISSOU(ty)   ((ty) == STRTY || (ty) == UNIONTY)

/*
 * Switch table
 */
struct swents {
	struct swents *next;
	CONSZ	sval;
	int	slab;
};
int mygenswitch(int, TWORD, struct swents **, int);

extern	int blevel;
extern	int oldstyle;

extern	int lineno, nerrors, issyshdr;

extern	char *ftitle;
extern	struct symtab *cftnsp;
extern	int autooff, maxautooff, argoff;

extern	OFFSZ inoff;

extern	int reached;
extern	int isinlining;
extern	int xinline, xgnu89, xgnu99;
extern	int bdebug, ddebug, edebug, idebug, ndebug;
extern	int odebug, pdebug, sdebug, tdebug, xdebug;

/* various labels */
extern	int brklab;
extern	int contlab;
extern	int flostat;
extern	int retlab;
extern	int doing_init, statinit;

struct p1node;

extern	short sztable[];
extern	char *astypnames[];

/* pragma globals */
extern int pragma_allpacked, pragma_packed, pragma_aligned;

/*
 * Flags used in flow analysis
 */
#define FBRK		02
#define FCONT		04
#define FDEF		010
#define FLOOP		020

/*
 * Location counters
 */
#define NOSEG		-1
#define PROG		0
#define DATA		1
#define RDATA		2
#define LDATA		3
#define UDATA		4
#define STRNG		5
#define PICDATA		6
#define PICRDATA	7
#define PICLDATA	8
#define TLSDATA		9
#define TLSUDATA	10
#define CTORS		11
#define DTORS		12
#define	NMSEG		13

extern int lastloc;
void locctr(int type, struct symtab *sp);

/*
 * Tree node structure (P1ND)
 */
#include "pass2.h"

/*
 * Ruby-specific tree manipulation functions
 */
struct p1node *ruby_block_node(int nparams, char **params, struct p1node *body);
struct p1node *ruby_method_call(struct p1node *receiver, char *method, struct p1node *args);
struct p1node *ruby_class_node(char *name, struct p1node *superclass, struct p1node *body);
struct p1node *ruby_string_interp(struct p1node *parts);

/* Function declarations */
void yyerror(const char *s);
int yyparse(void);
int yylex(void);
struct symtab *lookup(char *name, int flags);
struct p1node *buildtree(int op, struct p1node *l, struct p1node *r);
struct p1node *block(int op, struct p1node *l, struct p1node *r);
struct p1node *nametree(struct symtab *sp);
struct p1node *bcon(CONSZ val);
struct p1node *fcon(char *str);
struct p1node *string(char *str);
void ecomp(struct p1node *p);
void defid(struct symtab *sp, int class);

#endif /* pass1.h for Ruby */
