/*
 * Copyright (c) 2025 PCC Pascal Compiler
 *
 * Pass 1 definitions - Frontend data structures
 * Based on PCC architecture, adapted for Pascal
 */

#ifndef PASS1_H
#define PASS1_H

#include "../../mip/manifest.h"
#include "error.h"
#include "dialect.h"

/* Symbol table entry */
typedef struct symtab {
	char *sname;            /* Symbol name */
	int sclass;             /* Storage class */
	int slevel;             /* Scope level */
	struct tnode *stype;    /* Type information */
	int soffset;            /* Offset in stack frame */
	int sflags;             /* Miscellaneous flags */
	struct symtab *snext;   /* Next in hash chain */
	source_loc_t sloc;      /* Declaration location */
} SYMTAB;

/* Storage classes (sclass) */
#define SNULL      0    /* Undefined */
#define AUTO       1    /* Automatic variable (local var) */
#define EXTERN     2    /* External reference */
#define STATIC     3    /* Static */
#define REGISTER   4    /* Register */
#define TYPEDEF    5    /* Type definition */
#define PARAM      6    /* Function parameter */
#define STNAME     7    /* Structure/record tag */
#define MOS        8    /* Member of structure */
#define LABEL      9    /* Label */
#define CONST      10   /* Constant */
#define TYPENAME   11   /* Type name */
#define FUNC       12   /* Function */
#define PROC       13   /* Procedure */
#define ENUMCONST  14   /* Enumeration constant */

/* Symbol flags (sflags) */
#define SUSED      (1<<0)   /* Symbol was used */
#define SSET       (1<<1)   /* Symbol was assigned */
#define SFORWARD   (1<<2)   /* Forward declaration */
#define SINLINE    (1<<3)   /* Inline function */
#define SEXTERNAL  (1<<4)   /* External linkage */

/* Type node structure (simplified) */
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
			int *dimensions;          /* Dimension sizes */
		} array;
		struct {
			char *tag;                /* Record/object tag */
			struct field_list *fields;  /* Field list */
		} record;
		struct {
			struct tnode *base;       /* Pointer base type */
		} ptr;
		struct {
			int low, high;            /* Subrange bounds */
		} subrange;
		struct {
			struct enum_list *values;  /* Enumeration values */
		} enumtype;
		struct {
			struct tnode *base;       /* Set base type */
		} set;
	} tattr;
} TNODE;

/* Basic types (ttype) */
#define TNULL      0
#define TINTEGER   1
#define TREAL      2
#define TBOOLEAN   3
#define TCHAR      4
#define TSTRING    5
#define TARRAY     6
#define TRECORD    7
#define TPOINTER   8
#define TSUBRANGE  9
#define TENUM      10
#define TSET       11
#define TFILE      12
#define TTEXT      13
#define TPROCEDURE 14
#define TFUNCTION  15
#define TOBJECT    16  /* Object type (OOP) */
#define TCLASS     17  /* Class type (Delphi) */
#define TINTERFACE 18  /* Interface type */
#define TVARIANT   19  /* Variant type */

/* Extended types for different dialects */
#define TBYTE      20  /* Unsigned 8-bit */
#define TWORD      21  /* Unsigned 16-bit */
#define TDWORD     22  /* Unsigned 32-bit */
#define TINT64     23  /* 64-bit integer */
#define TCURRENCY  24  /* Currency type (Delphi) */
#define TSHORTINT  25  /* Signed 8-bit */
#define TSMALLINT  26  /* Signed 16-bit */
#define TLONGINT   27  /* Signed 32-bit */
#define TSINGLE    28  /* 32-bit float */
#define TDOUBLE    29  /* 64-bit float */
#define TEXTENDED  30  /* 80-bit float */

/* Parameter list */
typedef struct param_list {
	char *pname;
	struct tnode *ptype;
	int pflags;              /* PVAR, PCONST, etc. */
	struct param_list *pnext;
} PARAM_LIST;

/* Parameter flags */
#define PVAR       (1<<0)   /* var parameter (pass by reference) */
#define PCONST     (1<<1)   /* const parameter (read-only) */
#define POUT       (1<<2)   /* out parameter */

/* Field list (for records) */
typedef struct field_list {
	char *fname;
	struct tnode *ftype;
	int foffset;
	struct field_list *fnext;
} FIELD_LIST;

/* Enumeration list */
typedef struct enum_list {
	char *ename;
	int evalue;
	struct enum_list *enext;
} ENUM_LIST;

/* AST node types (for expression trees) */
#define NAME       1    /* Identifier reference */
#define ICON       2    /* Integer constant */
#define FCON       3    /* Floating constant */
#define SCON       4    /* String constant */
#define CCON       5    /* Character constant */
#define PLUS       6    /* + */
#define MINUS      7    /* - */
#define MUL        8    /* * */
#define DIV        9    /* / (real division) */
#define INTDIV     10   /* div (integer division) */
#define MOD        11   /* mod */
#define AND        12   /* and */
#define OR         13   /* or */
#define NOT        14   /* not */
#define EQ         15   /* = */
#define NE         16   /* <> */
#define LT         17   /* < */
#define LE         18   /* <= */
#define GT         19   /* > */
#define GE         20   /* >= */
#define ASSIGN     21   /* := */
#define CALL       22   /* Function/procedure call */
#define SUBSCRIPT  23   /* Array subscript */
#define FIELD      24   /* Record field access */
#define DEREF      25   /* Pointer dereference (^) */
#define ADDR       26   /* Address-of (@) */

/* Global variables */
extern int lineno;          /* Current line number */
extern char *ftitle;        /* Current filename */
extern FILE *outfile;       /* Output file for IR */
extern int blevel;          /* Current block nesting level */

/* Symbol table functions */
void symtab_init(void);
SYMTAB *lookup(char *name, int level);
SYMTAB *install(char *name, int class, int level);
void hide(int level);
SYMTAB *find_symbol(char *name);

/* Type management */
TNODE *mktype(int basictype);
TNODE *mkarray(TNODE *elemtype, int *dims, int ndims);
TNODE *mkrecord(char *tag, FIELD_LIST *fields);
TNODE *mkpointer(TNODE *basetype);
TNODE *mksubrange(int low, int high);
TNODE *mkenum(ENUM_LIST *values);
TNODE *mkset(TNODE *basetype);
TNODE *mkfunction(TNODE *rettype, PARAM_LIST *params);
TNODE *mkprocedure(PARAM_LIST *params);
int type_compatible(TNODE *t1, TNODE *t2);
int type_size(TNODE *t);

/* Built-in functions and procedures */
void init_builtins(void);
int is_builtin(char *name);

/* Code generation interface */
void emit_code(int op, ...);
void emit_label(int label);
int new_label(void);
int new_temp(void);

#endif /* PASS1_H */
