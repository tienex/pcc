/*
 * Copyright (c) 2025 PCC BASIC Compiler
 *
 * Pass 1 definitions - Frontend data structures for PBC
 * Based on PCC architecture, adapted for BASIC
 */

#ifndef PASS1_H
#define PASS1_H

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
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
#define SC_SNULL      0    /* Undefined */
#define SC_AUTO       1    /* Automatic variable (local var) */
#define SC_EXTERN     2    /* External reference */
#define SC_STATIC     3    /* Static */
#define SC_PARAM      4    /* Subroutine parameter */
#define SC_LABEL      5    /* Line number label */
#define SC_CONST      6    /* Constant */
#define SC_FUNC       7    /* Function (DEF FN) */
#define SC_SUB        8    /* Subroutine (SUB/GOSUB) */
#define SC_ARRAY      9    /* Array variable */
#define SC_COMMON     10   /* COMMON variable */

/* Symbol flags (sflags) */
#define SUSED      (1<<0)   /* Symbol was used */
#define SSET       (1<<1)   /* Symbol was assigned */
#define SDIM       (1<<2)   /* Array was DIMensioned */
#define SSHARED    (1<<3)   /* SHARED variable */
#define SEXTERNAL  (1<<4)   /* External linkage */

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
			int *dimensions;          /* Dimension sizes */
			int *lower_bounds;        /* Lower bounds (for OPTION BASE) */
		} array;
		struct {
			int length;               /* String length (fixed strings) */
		} string;
		struct {
			struct field_list *fields;  /* Field list (TYPE/UDT) */
		} record;
	} tattr;
} TNODE;

/* Basic types (ttype) */
#define TNULL      0
#define TINTEGER   1    /* INTEGER or % */
#define TLONG      2    /* LONG or & */
#define TSINGLE    3    /* SINGLE or ! */
#define TDOUBLE    4    /* DOUBLE or # */
#define TSTRING    5    /* STRING or $ */
#define TARRAY     6    /* Array type */
#define TRECORD    7    /* TYPE/User-defined type */

/* Extended types for different dialects */
#define TBYTE      8    /* BYTE (unsigned 8-bit) */
#define TBOOLEAN   9    /* BOOLEAN */
#define TCURRENCY  10   /* CURRENCY (VB) */
#define TVARIANT   11   /* VARIANT (VB) */
#define TDATE      12   /* DATE (VB) */
#define TDECIMAL   13   /* DECIMAL (VB) */
#define TOBJECT    14   /* OBJECT (VB) */

/* Parameter list */
typedef struct param_list {
	char *pname;
	struct tnode *ptype;
	int pflags;              /* PBYREF, PBYVAL, etc. */
	struct param_list *pnext;
} PARAM_LIST;

/* Parameter flags */
#define PBYREF     (1<<0)   /* BYREF (pass by reference) */
#define PBYVAL     (1<<1)   /* BYVAL (pass by value) */
#define POPTIONAL  (1<<2)   /* OPTIONAL parameter */

/* Field list (for TYPEs/UDTs) */
typedef struct field_list {
	char *fname;
	struct tnode *ftype;
	int foffset;
	struct field_list *fnext;
} FIELD_LIST;

/* Line number table entry */
typedef struct line_entry {
	int line_num;           /* Line number */
	int label_id;           /* Generated label ID */
	struct line_entry *next;
} LINE_ENTRY;

/* AST node types (for expression trees) */
#define NODE_NAME       1    /* Identifier reference */
#define NODE_ICON       2    /* Integer constant */
#define NODE_FCON       3    /* Floating constant */
#define NODE_SCON       4    /* String constant */
#define NODE_PLUS       5    /* + */
#define NODE_MINUS      6    /* - */
#define NODE_MUL        7    /* * */
#define NODE_DIV        8    /* / */
#define NODE_INTDIV     9    /* \ (integer division) */
#define NODE_MOD        10   /* MOD */
#define NODE_AND        11   /* AND */
#define NODE_OR         12   /* OR */
#define NODE_NOT        13   /* NOT */
#define NODE_XOR        14   /* XOR */
#define NODE_EQV        15   /* EQV */
#define NODE_IMP        16   /* IMP */
#define NODE_EQ         17   /* = */
#define NODE_NE         18   /* <> */
#define NODE_LT         19   /* < */
#define NODE_LE         20   /* <= */
#define NODE_GT         21   /* > */
#define NODE_GE         22   /* >= */
#define NODE_ASSIGN     23   /* = (assignment) */
#define NODE_CALL       24   /* Function/sub call */
#define NODE_SUBSCRIPT  25   /* Array subscript */
#define NODE_FIELD      26   /* Record field access (.) */
#define NODE_CONCAT     27   /* String concatenation (+) */
#define NODE_POWER      28   /* ^ (exponentiation) */

/* Statement types */
#define STMT_PRINT     1
#define STMT_INPUT     2
#define STMT_LET       3
#define STMT_IF        4
#define STMT_GOTO      5
#define STMT_GOSUB     6
#define STMT_RETURN    7
#define STMT_FOR       8
#define STMT_NEXT      9
#define STMT_WHILE     10
#define STMT_WEND      11
#define STMT_DO        12
#define STMT_LOOP      13
#define STMT_SELECT    14
#define STMT_CASE      15
#define STMT_END       16
#define STMT_STOP      17
#define STMT_DIM       18
#define STMT_REDIM     19
#define STMT_ERASE     20
#define STMT_SUB       21
#define STMT_FUNCTION  22
#define STMT_CALL      23
#define STMT_EXIT      24
#define STMT_OPEN      25
#define STMT_CLOSE     26
#define STMT_READ      27
#define STMT_WRITE     28
#define STMT_DATA      29
#define STMT_RESTORE   30

/* Global variables */
extern int lineno;          /* Current line number */
extern char *ftitle;        /* Current filename */
extern FILE *outfile;       /* Output file for IR */
extern int blevel;          /* Current block nesting level */
extern int option_base;     /* OPTION BASE (0 or 1) */
extern int current_line_num; /* Current BASIC line number */

/* Symbol table functions */
void symtab_init(void);
SYMTAB *lookup(char *name, int level);
SYMTAB *install(char *name, int class, int level);
void hide(int level);
SYMTAB *find_symbol(char *name);

/* Type management */
TNODE *mktype(int basictype);
TNODE *mkarray(TNODE *elemtype, int *dims, int *lower, int ndims);
TNODE *mkstring(int length);
TNODE *mkrecord(FIELD_LIST *fields);
TNODE *mkfunction(TNODE *rettype, PARAM_LIST *params);
int type_compatible(TNODE *t1, TNODE *t2);
int type_size(TNODE *t);
TNODE *get_var_type(char *name);  /* Get type from variable name suffix */

/* Line number management */
void init_line_table(void);
int add_line_number(int line_num);
int find_line_label(int line_num);

/* Built-in functions and statements */
void init_builtins(void);
int is_builtin_func(char *name);
int is_builtin_stmt(char *name);

/* Code generation interface */
void emit_code(int op, ...);
void emit_label(int label);
int new_label(void);
int new_temp(void);

/* Utility functions */
int is_numeric_type(int ttype);
int is_string_type(int ttype);
char *type_suffix(char *name);  /* Get type suffix ($, %, &, !, #) */

#endif /* PASS1_H */
