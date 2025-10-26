/*
 * Copyright (c) 2025 PCC ALGOL 60+ Compiler
 *
 * Pass 1 definitions - Frontend data structures
 * Based on PCC architecture, adapted for ALGOL 60+
 */

#ifndef PASS1_H
#define PASS1_H

#include "../../mip/manifest.h"
#include "error.h"

/* Symbol table entry */
typedef struct symtab {
	char *sname;            /* Symbol name */
	int sclass;             /* Storage class */
	int slevel;             /* Scope level (block depth) */
	struct tnode *stype;    /* Type information */
	int soffset;            /* Offset in stack frame */
	int sflags;             /* Miscellaneous flags */
	struct symtab *snext;   /* Next in hash chain */
	source_loc_t sloc;      /* Declaration location */
} SYMTAB;

/* Storage classes (sclass) */
#define SNULL      0    /* Undefined */
#define AUTO       1    /* Automatic variable (local var) */
#define STATIC     3    /* Static (own variables in ALGOL) */
#define PARAM      6    /* Procedure/function parameter */
#define LABEL      9    /* Label */
#define CONST      10   /* Constant */
#define PROC       13   /* Procedure (no return value) */
#define FUNC       12   /* Function (returns value) */

/* Symbol flags (sflags) */
#define SUSED      (1<<0)   /* Symbol was used */
#define SSET       (1<<1)   /* Symbol was assigned */
#define SFORWARD   (1<<2)   /* Forward declaration */
#define SBYNAME    (1<<3)   /* Call-by-name parameter */
#define SBYVALUE   (1<<4)   /* Call-by-value parameter */

/* Type node structure */
typedef struct tnode {
	int ttype;              /* Basic type */
	int tsize;              /* Size in bytes */
	int talign;             /* Alignment */
	union {
		struct {
			struct tnode *ret_type;  /* Return type (functions) */
			struct param_list *params;  /* Parameter list */
		} func;
		struct {
			struct tnode *elem_type;  /* Element type (arrays) */
			int dim_count;            /* Number of dimensions */
			struct {
				int lower;            /* Lower bound */
				int upper;            /* Upper bound */
			} *bounds;                /* Dimension bounds */
		} array;
	} tattr;
} TNODE;

/* Basic types (ttype) */
#define TNULL      0
#define TINTEGER   1    /* integer */
#define TREAL      2    /* real */
#define TBOOLEAN   3    /* Boolean */
#define TSTRING    4    /* string (extension) */
#define TARRAY     6    /* array */
#define TPROCEDURE 14   /* procedure */
#define TFUNCTION  15   /* procedure returning value */

/* Parameter list */
typedef struct param_list {
	char *pname;
	struct tnode *ptype;
	int pflags;              /* PBYVALUE, PBYNAME */
	struct param_list *pnext;
} PARAM_LIST;

/* Parameter flags */
#define PBYVALUE   (1<<0)   /* Call by value */
#define PBYNAME    (1<<1)   /* Call by name (Jensen's device) */

/* AST node types - these map to mip/manifest.h IR nodes */
/* We use the standard PCC IR nodes defined in manifest.h */

/* Global variables */
extern int lineno;          /* Current line number */
extern char *ftitle;        /* Current filename */
extern FILE *outfile;       /* Output file for IR */
extern int blevel;          /* Current block nesting level */

/* Built-in types */
extern TNODE *type_integer;
extern TNODE *type_real;
extern TNODE *type_boolean;
extern TNODE *type_string;

/* Symbol table functions */
void symtab_init(void);
SYMTAB *lookup(char *name);
SYMTAB *install(char *name, int sclass, TNODE *type);
void push_scope(void);
void pop_scope(void);

/* Type management */
void init_types(void);
TNODE *mktype(int basictype);
TNODE *mkarray(TNODE *elemtype, int ndims, int *lowers, int *uppers);
TNODE *mkfunction(TNODE *rettype, PARAM_LIST *params);
TNODE *mkprocedure(PARAM_LIST *params);
int type_compatible(TNODE *t1, TNODE *t2);
int type_size(TNODE *t);

/* Built-in functions and procedures */
void init_builtins(void);
int is_builtin(char *name);
SYMTAB *find_builtin(char *name);

/* Code generation interface */
void emit_code(int op, ...);
void emit_label(int label);
int new_label(void);
int new_temp(void);

#endif /* PASS1_H */
