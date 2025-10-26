/*
 * Copyright (c) 2025 PCC Paradox PAL Compiler
 *
 * Pass 1 definitions - Frontend data structures for PAL/ObjectPAL
 * Based on PCC architecture
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
	struct symtab *sowner;  /* Owning object/type (for methods) */
} SYMTAB;

/* Storage classes (sclass) */
#define SNULL      0    /* Undefined */
#define AUTO       1    /* Automatic variable (local var) */
#define EXTERN     2    /* External reference */
#define STATIC     3    /* Static */
#define PARAM      6    /* Function/procedure parameter */
#define LABEL      9    /* Label */
#define CONST      10   /* Constant */
#define TYPENAME   11   /* Type name */
#define METHOD     12   /* Method/procedure */
#define FIELD      13   /* Object field/property */
#define ENUMCONST  14   /* Enumeration constant */
#define BUILTIN    15   /* Built-in function/procedure */

/* Symbol flags (sflags) */
#define SUSED      (1<<0)   /* Symbol was used */
#define SSET       (1<<1)   /* Symbol was assigned */
#define SFORWARD   (1<<2)   /* Forward declaration */
#define SEXTERNAL  (1<<4)   /* External linkage */
#define SPRIVATE   (1<<5)   /* Private member */
#define SPROTECTED (1<<6)   /* Protected member (ObjectPAL) */
#define SPUBLIC    (1<<7)   /* Public member */

/* Type node structure */
typedef struct tnode {
	int ttype;              /* Basic type */
	int tsize;              /* Size in bytes */
	int talign;             /* Alignment */
	union {
		struct {
			struct tnode *ret_type;  /* Return type (procedures) */
			struct param_list *params;  /* Parameter list */
		} proc;
		struct {
			struct tnode *elem_type;  /* Element type (arrays) */
			int dim_count;            /* Number of dimensions */
			int *dimensions;          /* Dimension sizes */
		} array;
		struct {
			char *tag;                /* Record/object tag */
			struct field_list *fields;  /* Field list */
			struct method_list *methods; /* Method list (ObjectPAL) */
			struct tnode *parent;     /* Parent object type */
		} object;
		struct {
			struct tnode *base;       /* Pointer base type */
		} ptr;
		struct {
			int low, high;            /* Subrange bounds */
		} subrange;
		struct {
			struct enum_list *values;  /* Enumeration values */
		} enumtype;
	} tattr;
} TNODE;

/* Basic types (ttype) */
#define TNULL      0
#define TSMALLINT  1    /* SmallInt (16-bit signed) */
#define TSHORTINT  2    /* ShortInt (8-bit signed) */
#define TLONGINT   3    /* LongInt (32-bit signed) */
#define TNUMBER    4    /* Number (BCD decimal) */
#define TCURRENCY  5    /* Currency type */
#define TLOGICAL   6    /* Logical (boolean) */
#define TALPHANUMERIC 7 /* String type */
#define TDATE      8    /* Date type */
#define TTIME      9    /* Time type */
#define TDATETIME  10   /* DateTime type */
#define TTIMESTAMP 11   /* Timestamp type */
#define TMEMO      12   /* Memo (long text) */
#define TBLOB      13   /* Binary large object */
#define TGRAPHIC   14   /* Graphic/image */
#define TFORMATTED_MEMO 15 /* Formatted memo (RTF) */
#define TAUTOINCREMENT 16  /* AutoIncrement field */
#define TBYTES     17   /* Bytes (binary data) */
#define TARRAY     18   /* Array type */
#define TRECORD    19   /* Record type */
#define TOBJECT    20   /* Object type (ObjectPAL) */
#define TPOINTER   21   /* Pointer/reference */
#define TSUBRANGE  22   /* Subrange */
#define TENUM      23   /* Enumeration */
#define TPROCEDURE 24   /* Procedure/method */
#define TVARIANT   25   /* Variant (any type) */

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

/* Field list (for records/objects) */
typedef struct field_list {
	char *fname;
	struct tnode *ftype;
	int foffset;
	int fflags;              /* Field visibility flags */
	struct field_list *fnext;
} FIELD_LIST;

/* Method list (for ObjectPAL) */
typedef struct method_list {
	char *mname;
	struct tnode *mtype;     /* Method signature */
	int mflags;              /* SPRIVATE, SPROTECTED, SPUBLIC */
	struct method_list *mnext;
} METHOD_LIST;

/* Enumeration list */
typedef struct enum_list {
	char *ename;
	int evalue;
	struct enum_list *enext;
} ENUM_LIST;

/* AST node types (for expression trees) */
#define NAME       1    /* Identifier reference */
#define ICON       2    /* Integer constant */
#define FCON       3    /* Number constant (BCD) */
#define SCON       4    /* String constant */
#define LCON       5    /* Logical constant (True/False) */
#define DCON       6    /* Date constant */
#define PLUS       10   /* + */
#define MINUS      11   /* - */
#define MUL        12   /* * */
#define DIV        13   /* / */
#define MOD        14   /* mod */
#define AND        15   /* and */
#define OR         16   /* or */
#define NOT        17   /* not */
#define EQ         18   /* = */
#define NE         19   /* <> */
#define LT         20   /* < */
#define LE         21   /* <= */
#define GT         22   /* > */
#define GE         23   /* >= */
#define ASSIGN     24   /* = (assignment) */
#define CALL       25   /* Procedure/method call */
#define SUBSCRIPT  26   /* Array subscript */
#define FIELD      27   /* Object field/method access */
#define DEREF      28   /* Pointer dereference */

/* Statement node types */
#define STMT_IF        100  /* if statement */
#define STMT_WHILE     101  /* while loop */
#define STMT_FOR       102  /* for loop */
#define STMT_FOREACH   103  /* foreach loop (ObjectPAL) */
#define STMT_SWITCH    104  /* switch statement */
#define STMT_CASE      105  /* case statement */
#define STMT_RETURN    106  /* return statement */
#define STMT_BREAK     107  /* break statement */
#define STMT_CONTINUE  108  /* continue statement */
#define STMT_TRY       109  /* try-except block */
#define STMT_BLOCK     110  /* compound statement */
#define STMT_EXPR      111  /* expression statement */

/* Global variables */
extern int lineno;          /* Current line number */
extern int current_column;  /* Current column */
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
TNODE *mkobject(char *tag, FIELD_LIST *fields, METHOD_LIST *methods, TNODE *parent);
TNODE *mkpointer(TNODE *basetype);
TNODE *mksubrange(int low, int high);
TNODE *mkenum(ENUM_LIST *values);
TNODE *mkprocedure(TNODE *rettype, PARAM_LIST *params);
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
