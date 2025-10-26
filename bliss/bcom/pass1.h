/*
 * Copyright (c) 2025 PCC BLISS Compiler
 *
 * Pass 1 definitions - Frontend data structures for BLISS
 * Based on PCC architecture
 */

#ifndef PASS1_H
#define PASS1_H

#include "../../mip/manifest.h"

/* Symbol table entry */
typedef struct symtab {
	char *sname;            /* Symbol name */
	int sclass;             /* Storage class */
	int slevel;             /* Scope level */
	struct node *stype;     /* Type information (MIP node) */
	int soffset;            /* Offset in stack frame */
	int sflags;             /* Miscellaneous flags */
	struct symtab *snext;   /* Next in hash chain */
} SYMTAB;

/* Storage classes (sclass) - BLISS specific */
#define SNULL      0    /* Undefined */
#define SLOCAL     1    /* Local variable */
#define SGLOBAL    2    /* Global variable */
#define SEXTERN    3    /* External reference */
#define SOWN       4    /* Own (static) variable */
#define SREGISTER  5    /* Register variable */
#define SLITERAL   6    /* Literal (compile-time constant) */
#define SBIND      7    /* Bind (compile-time binding) */
#define SROUTINE   8    /* Routine (function) */
#define SFORWARD   9    /* Forward declaration */
#define SLABEL     10   /* Label */
#define SFIELD     11   /* Structure field */
#define SSTACKLOCAL 12  /* Stack-local variable */

/* Symbol flags (sflags) */
#define SUSED      (1<<0)   /* Symbol was used */
#define SSET       (1<<1)   /* Symbol was assigned */
#define SVECTOR    (1<<2)   /* Vector (array) */
#define SBLOCKVEC  (1<<3)   /* Block vector */
#define SPRESET    (1<<4)   /* Has preset values */

/* Node structure - uses PCC MIP nodes from mip/node.h */
typedef struct node NODE;

/* Global variables */
extern int lineno;          /* Current line number */
extern int current_column;  /* Current column */
extern char *ftitle;        /* Current filename */
extern FILE *outfile;       /* Output file for IR */
extern int blevel;          /* Current block nesting level */
extern int errors;          /* Error count */

/* Symbol table functions */
void symtab_init(void);
SYMTAB *lookup(char *name, int level);
SYMTAB *install(char *name, int class, int level);
void hide(int level);
SYMTAB *find_symbol(char *name);

/* Error handling */
void error(const char *fmt, ...);
void warning(const char *fmt, ...);
void yyerror(const char *s);

/* Code generation - interface to MIP backend */
NODE *buildtree(int op, NODE *left, NODE *right);
NODE *block(int op, NODE *left, NODE *right, TWORD type);
NODE *mkty(TWORD type, int d, int s);
void ecomp(NODE *p);
void lcommprint(void);
void efcode(void);
void bfcode(void);
int getlab(void);

/* BLISS-specific code generation */
NODE *mkintconst(int value);
NODE *mkname(char *name);
NODE *mkcall(NODE *func, NODE *args);
NODE *mkassign(NODE *left, NODE *right);
NODE *mkif(NODE *cond, NODE *then_part, NODE *else_part);
NODE *mkloop(int type, NODE *cond, NODE *body);
NODE *mkblock(NODE *decls, NODE *stmts);

/* Type management (BLISS is largely untyped, but we need basics) */
#define BLISS_WORD     LONG    /* Default BLISS type (fullword) */
#define BLISS_BYTE     CHAR    /* Byte */
#define BLISS_VECTOR   PTR     /* Vector is a pointer */
#define BLISS_ROUTINE  FTN     /* Routine type */

/* Utility functions */
int is_constant(NODE *p);
int eval_constant(NODE *p);
void init_compiler(void);
void finish_compilation(void);

#endif /* PASS1_H */
