/*
 * Copyright (c) 2025 PCC Xbase++ Compiler
 *
 * Pass 1 definitions - Frontend data structures for Xbase++
 * Based on PCC architecture, adapted for Xbase++
 */

#ifndef PASS1_H
#define PASS1_H

#include <stdio.h>
#include <stdlib.h>

/* Symbol table entry */
typedef struct symtab {
	char *sname;            /* Symbol name */
	int sclass;             /* Storage class */
	int slevel;             /* Scope level */
	struct tnode *stype;    /* Type information */
	int soffset;            /* Offset in stack frame */
	int sflags;             /* Miscellaneous flags */
	struct symtab *snext;   /* Next in hash chain */
	int sline;              /* Declaration line number */
} SYMTAB;

/* Storage classes (sclass) - Xbase++ specific */
#define S_NULL      0    /* Undefined */
#define S_AUTO      1    /* LOCAL variable (automatic) */
#define S_EXTERN    2    /* External reference */
#define S_STATIC    3    /* STATIC variable */
#define S_PARAM     6    /* Function parameter */
#define S_LABEL     9    /* Label */
#define S_FUNC      12   /* Function */
#define S_PROC      13   /* Procedure */
#define S_PUBLIC    20   /* PUBLIC variable */
#define S_PRIVATE   21   /* PRIVATE variable */
#define S_FIELD     22   /* Field/instance variable */
#define S_METHOD    23   /* Class method */
#define S_CLASS     24   /* Class definition */
#define S_MEMVAR    25   /* Memory variable (unscoped) */

/* Symbol flags (sflags) */
#define SUSED      (1<<0)   /* Symbol was used */
#define SSET       (1<<1)   /* Symbol was assigned */
#define SFORWARD   (1<<2)   /* Forward declaration */
#define SINLINE    (1<<3)   /* Inline function */
#define SEXTERNAL  (1<<4)   /* External linkage */
#define SEXPORTED  (1<<5)   /* Exported from module */
#define SINITIALIZED (1<<6) /* Variable has initializer */

/* Type node structure */
typedef struct tnode {
	int ttype;              /* Basic type */
	int tsize;              /* Size in bytes */
	int talign;             /* Alignment */
	union {
		struct {
			struct tnode *ret_type;  /* Return type (functions) */
			struct param_list *params;  /* Parameter list */
			int param_count;
		} func;
		struct {
			struct tnode *elem_type;  /* Element type (arrays) */
			int dim_count;            /* Number of dimensions */
			int *dimensions;          /* Dimension sizes */
		} array;
		struct {
			char *tag;                /* Class/object tag */
			struct field_list *fields;  /* Field list */
			struct method_list *methods; /* Method list */
			struct tnode *parent;     /* Parent class */
		} object;
		struct {
			struct tnode *param_types; /* CodeBlock parameter types */
			struct tnode *result_type; /* CodeBlock result type */
		} codeblock;
		struct {
			int precision;            /* Numeric precision */
			int scale;                /* Decimal places */
		} numeric;
		struct {
			int max_length;           /* String max length */
		} string;
	} tattr;
} TNODE;

/* Basic types (ttype) - Xbase++ data types */
#define TNULL      0    /* Null/undefined type */
#define TNUMERIC   1    /* Numeric (integer or float) */
#define TCHAR      2    /* Character string */
#define TDATE      3    /* Date value */
#define TLOGICAL   4    /* Logical (boolean) */
#define TARRAY     5    /* Array */
#define TOBJECT    6    /* Object instance */
#define TCODEBLOCK 7    /* Code block */
#define TMEMO      8    /* Memo field */
#define TNIL       9    /* NIL value */
#define TVARIANT   10   /* Variant/untyped */
#define TINTEGER   11   /* Integer subtype of numeric */
#define TFLOAT     12   /* Float subtype of numeric */
#define TTIMESTAMP 13   /* Timestamp (date+time) */
#define TSYMBOL    14   /* Symbol type */

/* Parameter list */
typedef struct param_list {
	char *pname;              /* Parameter name */
	struct tnode *ptype;      /* Parameter type */
	int pflags;               /* BY REF, optional, etc. */
	struct param_list *pnext; /* Next parameter */
} PARAM_LIST;

/* Field list (for classes/objects) */
typedef struct field_list {
	char *fname;              /* Field name */
	struct tnode *ftype;      /* Field type */
	int foffset;              /* Field offset */
	int fflags;               /* Access flags (PROTECTED, EXPORTED, etc.) */
	struct field_list *fnext; /* Next field */
} FIELD_LIST;

/* Method list (for classes) */
typedef struct method_list {
	char *mname;              /* Method name */
	struct tnode *mtype;      /* Method type (function type) */
	int mflags;               /* Access flags, VIRTUAL, etc. */
	struct method_list *mnext; /* Next method */
} METHOD_LIST;

/* AST node structure */
typedef struct node {
	int op;                   /* Operation/node type */
	struct tnode *n_type;     /* Result type */
	union {
		struct {              /* Binary/unary operators */
			struct node *left;
			struct node *right;
		} bn;
		struct {              /* Integer constant */
			long long val;
		} ival;
		struct {              /* Float constant */
			double val;
		} fval;
		struct {              /* String constant */
			char *str;
			int len;
		} sval;
		struct {              /* Symbol reference */
			struct symtab *sym;
		} sym;
		struct {              /* Function call */
			struct node *func;
			struct node *args;
		} call;
		struct {              /* Array subscript */
			struct node *arr;
			struct node *idx;
		} subscr;
	} n;
} NODE;

/* AST node operations */
#define N_PLUS     1    /* + */
#define N_MINUS    2    /* - */
#define N_MUL      3    /* * */
#define N_DIV      4    /* / */
#define N_MOD      5    /* % */
#define N_ASSIGN   6    /* = or := */
#define N_EQ       7    /* == */
#define N_NE       8    /* != or <> */
#define N_LT       9    /* < */
#define N_LE      10    /* <= */
#define N_GT      11    /* > */
#define N_GE      12    /* >= */
#define N_AND     13    /* .AND. */
#define N_OR      14    /* .OR. */
#define N_NOT     15    /* .NOT. or ! */
#define N_UMINUS  16    /* Unary minus */
#define N_ICON    17    /* Integer constant */
#define N_FCON    18    /* Float constant */
#define N_SCON    19    /* String constant */
#define N_NAME    20    /* Variable reference */
#define N_CALL    21    /* Function call */
#define N_SUBSCR  22    /* Array subscript */
#define N_FIELD   23    /* Field/member access */
#define N_IF      24    /* IF statement */
#define N_WHILE   25    /* DO WHILE */
#define N_FOR     26    /* FOR loop */
#define N_RETURN  27    /* RETURN */
#define N_BLOCK   28    /* Statement block */

/* Global variables */
extern int lineno;          /* Current line number */
extern char *filename;      /* Current file name */
extern int errors;          /* Error count */
extern int warnings;        /* Warning count */

/* Function prototypes */

/* symtab.c */
SYMTAB *lookup(char *name, int level);
SYMTAB *install(char *name, int sclass, struct tnode *type, int level);
void symtab_init(void);
void enter_scope(void);
void exit_scope(void);

/* types.c */
TNODE *make_type(int ttype);
TNODE *make_array_type(TNODE *elem, int *dims, int ndims);
TNODE *make_func_type(TNODE *ret, PARAM_LIST *params);
TNODE *make_object_type(char *tag);
int type_equal(TNODE *t1, TNODE *t2);
int type_compatible(TNODE *t1, TNODE *t2);
int type_size(TNODE *t);

/* tree.c */
NODE *make_node(int op, NODE *left, NODE *right);
NODE *make_icon(long long val);
NODE *make_fcon(double val);
NODE *make_scon(char *str);
NODE *make_name(SYMTAB *sym);
void walk_tree(NODE *n);

/* builtins.c */
void init_builtins(void);
SYMTAB *lookup_builtin(char *name);

/* main.c */
void yyerror(const char *s);
void error(const char *fmt, ...);
void warning(const char *fmt, ...);

#endif /* PASS1_H */
