/*
 * Copyright (c) 2025 PCC PL/I Compiler
 *
 * Pass 1 definitions - Frontend data structures for PL/I
 * Based on PCC architecture
 */

#ifndef PASS1_H
#define PASS1_H

/* Include PCC infrastructure */
#include "../../mip/manifest.h"
#include "../../mip/pass2.h"

/* PL/I-specific includes */
#include "error.h"
#include "dialect.h"

/* Use PCC's NODE as P1ND for consistency with C compiler */
#define P1ND NODE

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
	int sattr;              /* Attributes (ALIGNED, UNALIGNED, etc.) */
} SYMTAB;

/* Storage classes (sclass) */
#define SNULL      0    /* Undefined */
#define AUTO       1    /* Automatic (local variable) */
#define STATIC     2    /* Static */
#define EXTERNAL   3    /* External */
#define BASED      4    /* Based variable */
#define CONTROLLED 5    /* Controlled storage */
#define DEFINED    6    /* Defined variable */
#define PARAM      7    /* Parameter */
#define BUILTIN    8    /* Built-in function */
#define LABEL      9    /* Label */
#define ENTRY      10   /* Entry point */
#define CONST      11   /* Named constant */
#define TYPEDEF    12   /* Type definition */
#define FILE_VAR   13   /* File variable */
#define CONDITION  14   /* Condition variable */

/* Symbol flags (sflags) */
#define SUSED       (1<<0)   /* Symbol was used */
#define SSET        (1<<1)   /* Symbol was assigned */
#define SRECURSIVE  (1<<2)   /* Recursive procedure */
#define SREENTRANT  (1<<3)   /* Reentrant */
#define SEXTERNAL   (1<<4)   /* External linkage */
#define SINTERNAL   (1<<5)   /* Internal procedure */
#define SGENERIC    (1<<6)   /* Generic function */

/* Symbol attributes (sattr) */
#define ATTR_ALIGNED    (1<<0)   /* ALIGNED attribute */
#define ATTR_UNALIGNED  (1<<1)   /* UNALIGNED attribute */
#define ATTR_PACKED     (1<<2)   /* Packed */
#define ATTR_BINARY     (1<<3)   /* BINARY attribute */
#define ATTR_DECIMAL    (1<<4)   /* DECIMAL attribute */
#define ATTR_FLOAT      (1<<5)   /* FLOAT attribute */
#define ATTR_SIGNED     (1<<6)   /* SIGNED */
#define ATTR_UNSIGNED   (1<<7)   /* UNSIGNED */

/* Type node structure */
typedef struct tnode {
	int ttype;              /* Basic type */
	int tsize;              /* Size in bytes */
	int talign;             /* Alignment */
	int tprec;              /* Precision (for FIXED/FLOAT) */
	int tscale;             /* Scale (for FIXED DECIMAL) */
	union {
		struct {
			struct tnode *ret_type;  /* Return type */
			struct param_list *params;  /* Parameter list */
			int options;             /* Options (RECURSIVE, etc.) */
		} func;
		struct {
			struct tnode *elem_type;  /* Element type */
			int ndims;                /* Number of dimensions */
			int *lower;               /* Lower bounds */
			int *upper;               /* Upper bounds */
		} array;
		struct {
			char *tag;                /* Structure tag */
			struct member_list *members;  /* Structure members */
			int level;                /* Structure level */
		} structure;
		struct {
			struct tnode *base;       /* Pointer/offset base */
			int area;                 /* Area number */
		} ptr;
		struct {
			int len;                  /* String length */
			int varying;              /* VARYING flag */
		} string;
		struct {
			struct tnode *base_type;  /* Base type for picture */
			char *picture;            /* Picture specification */
		} picture;
		struct {
			struct tnode **types;     /* Union member types */
			int nmembers;             /* Number of members */
		} pli_union;
	} tattr;
} TNODE;

/* Basic types (ttype) */
#define TNULL        0
#define TFIXED       1   /* FIXED (integer) */
#define TFLOAT       2   /* FLOAT (real) */
#define TBIT         3   /* BIT */
#define TCHAR        4   /* CHARACTER */
#define TPOINTER     5   /* POINTER */
#define TOFFSET      6   /* OFFSET */
#define TAREA        7   /* AREA */
#define TFILE        8   /* FILE */
#define TLABEL       9   /* LABEL */
#define TENTRY       10  /* ENTRY */
#define TTASK        11  /* TASK */
#define TEVENT       12  /* EVENT */
#define TFORMAT      13  /* FORMAT */
#define TGENERIC     14  /* GENERIC */
#define TBUILTIN     15  /* BUILTIN */
#define TSTRUCT      16  /* STRUCTURE */
#define TUNION       17  /* UNION */
#define TPICTURE     18  /* Picture type */

/* Extended types for PL/M and variants */
#define TBYTE        20  /* BYTE (PL/M) */
#define TWORD        21  /* WORD (PL/M) */
#define TDWORD       22  /* DWORD (PL/M) */
#define TADDRESS     23  /* ADDRESS (PL/M) */
#define TINTEGER     24  /* INTEGER (PL/M) */
#define TREAL        25  /* REAL (PL/M) */

/* Type attributes */
#define TATTR_BINARY    (1<<0)   /* BINARY */
#define TATTR_DECIMAL   (1<<1)   /* DECIMAL */
#define TATTR_SIGNED    (1<<2)   /* SIGNED */
#define TATTR_UNSIGNED  (1<<3)   /* UNSIGNED */
#define TATTR_VARYING   (1<<4)   /* VARYING */
#define TATTR_ALIGNED   (1<<5)   /* ALIGNED */
#define TATTR_UNALIGNED (1<<6)   /* UNALIGNED */

/* Parameter list */
typedef struct param_list {
	char *pname;
	struct tnode *ptype;
	int pflags;              /* Parameter attributes */
	struct param_list *pnext;
} PARAM_LIST;

/* Parameter flags */
#define PNULL      0
#define PBYREF     (1<<0)   /* Pass by reference */
#define PBYVAL     (1<<1)   /* Pass by value */
#define POPTIONAL  (1<<2)   /* Optional parameter */

/* Structure member list */
typedef struct member_list {
	char *mname;
	struct tnode *mtype;
	int moffset;
	int mlevel;              /* Nesting level in structure */
	int mdim;                /* Dimension (for arrays) */
	struct member_list *mnext;
} MEMBER_LIST;

/* AST node types */
#define NAME       1    /* Identifier */
#define ICON       2    /* Integer constant */
#define FCON       3    /* Float constant */
#define SCON       4    /* String constant */
#define BCON       5    /* Bit constant */
#define PLUS       6    /* + */
#define MINUS      7    /* - */
#define MUL        8    /* * */
#define DIV        9    /* / */
#define POWER      10   /* ** */
#define CONCAT     11   /* || */
#define AND        12   /* & */
#define OR         13   /* | */
#define NOT        14   /* ^ or NOT */
#define EQ         15   /* = */
#define NE         16   /* ^= or ~= */
#define LT         17   /* < */
#define LE         18   /* <= */
#define GT         19   /* > */
#define GE         20   /* >= */
#define ASSIGN     21   /* = (assignment) */
#define CALL       22   /* Function call */
#define SUBSCRIPT  23   /* Array subscript */
#define MEMBER     24   /* Structure member */
#define DEREF      25   /* Pointer dereference */
#define ADDR       26   /* Address-of */

/* Statement types */
#define STMT_ASSIGN    100
#define STMT_IF        101
#define STMT_DO        102
#define STMT_CALL      103
#define STMT_RETURN    104
#define STMT_GOTO      105
#define STMT_EXIT      106
#define STMT_STOP      107
#define STMT_BEGIN     108
#define STMT_END       109
#define STMT_PROC      110
#define STMT_ENTRY     111
#define STMT_DECLARE   112
#define STMT_ALLOCATE  113
#define STMT_FREE      114
#define STMT_GET       115
#define STMT_PUT       116
#define STMT_READ      117
#define STMT_WRITE     118
#define STMT_SIGNAL    119
#define STMT_REVERT    120
#define STMT_WAIT      121

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
void enter_scope(void);
void exit_scope(void);

/* Type management */
TNODE *mktype(int basictype);
TNODE *mkfixed(int prec, int scale, int attrs);
TNODE *mkfloat(int prec, int attrs);
TNODE *mkbit(int len);
TNODE *mkchar(int len, int varying);
TNODE *mkarray(TNODE *elemtype, int ndims, int *lower, int *upper);
TNODE *mkstruct(char *tag, MEMBER_LIST *members, int level);
TNODE *mkpointer(TNODE *basetype);
TNODE *mkpicture(char *pic);
int type_compatible(TNODE *t1, TNODE *t2);
int type_size(TNODE *t);

/* Built-in functions */
void init_builtins(void);
int is_builtin(char *name);

/* Code generation interface */
void emit_code(int op, ...);
void emit_label(int label);
int new_label(void);
int new_temp(void);

#endif /* PASS1_H */
