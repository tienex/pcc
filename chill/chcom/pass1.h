/*
 * Copyright (c) 2025 PCC CHILL Compiler
 *
 * Pass 1 definitions - Frontend data structures for CHILL
 * CCITT High Level Language (Z.200)
 */

#ifndef PASS1_H
#define PASS1_H

#include <stdio.h>
#include <stdint.h>

/* Symbol table entry */
typedef struct symtab {
	char *sname;            /* Symbol name */
	int sclass;             /* Storage class */
	int slevel;             /* Scope level */
	struct mode_def *smode; /* Mode (type) information */
	int soffset;            /* Offset in stack frame */
	int sflags;             /* Miscellaneous flags */
	struct symtab *snext;   /* Next in hash chain */
	int sline;              /* Declaration line */
} SYMTAB;

/* Storage classes (sclass) - Use S_ prefix to avoid token conflicts */
#define SNULL      0    /* Undefined */
#define SAUTO      1    /* Automatic (LOC) */
#define SSTATIC    2    /* Static (DCL) */
#define SEXTERN    3    /* External (SYN) */
#define SCONST     4    /* Constant */
#define SLABEL     5    /* Label */
#define SPROC      6    /* Procedure */
#define SPROCESS   7    /* Process */
#define SSIGNAL    8    /* Signal */
#define SMODE      9    /* Mode name */
#define SPARAM     10   /* Formal parameter */
#define SSYNONYM   11   /* Synonym (SYN) */

/* Symbol flags (sflags) */
#define SUSED      (1<<0)   /* Symbol was used */
#define SSET       (1<<1)   /* Symbol was assigned */
#define SFORWARD   (1<<2)   /* Forward declaration */
#define SEXTERNAL  (1<<3)   /* External linkage */
#define SRECURSIVE (1<<4)   /* Recursive procedure/process */

/* Mode (type) definition */
typedef struct mode_def {
	int mtype;              /* Basic mode type */
	int msize;              /* Size in bytes */
	int malign;             /* Alignment */
	union {
		struct {
			struct mode_def *ret_mode;  /* Return mode (procedures) */
			struct param_list *params;  /* Parameter list */
		} proc;
		struct {
			struct mode_def *elem_mode; /* Element mode (arrays) */
			int *index_modes;           /* Index mode specifications */
			int ndims;                  /* Number of dimensions */
		} array;
		struct {
			char *tag;                  /* Structure tag */
			struct field_list *fields;  /* Field list */
		} structure;
		struct {
			struct mode_def *base;      /* Reference base mode */
		} ref;
		struct {
			int64_t low, high;          /* Range bounds */
			struct mode_def *parent;    /* Parent discrete mode */
		} range;
		struct {
			struct name_list *names;    /* Set element names */
		} powerset;
		struct {
			struct variant_list *variants; /* Variant alternatives */
		} variant;
	} mattr;
} MODE_DEF;

/* Basic mode types (mtype) */
#define MNULL      0
#define MINT       1    /* Integer modes */
#define MREAL      2    /* Real modes */
#define MBOOL      3    /* Boolean */
#define MCHAR      4    /* Character */
#define MARRAY     5    /* Array */
#define MSTRUCT    6    /* Structure */
#define MREF       7    /* Reference (pointer) */
#define MRANGE     8    /* Discrete range */
#define MPOWERSET  9    /* Powerset (set) */
#define MPROC      10   /* Procedure mode */
#define MINSTANCE  11   /* Instance mode */
#define MVARIANT   12   /* Variant structure */
#define MBITS      13   /* Bits (bit string) */
#define MCHARS     14   /* Chars (character string) */
#define MTEXT      15   /* Text mode */
#define MDURATION  16   /* Duration (time) */
#define MTIME      17   /* Absolute time */

/* Predefined integer modes */
#define MBYTE      18   /* BYTE (8-bit) */
#define MINT8      19   /* INT (8-bit signed) */
#define MUINT8     20   /* UINT (8-bit unsigned) */
#define MINT16     21   /* INT (16-bit signed) */
#define MUINT16    22   /* UINT (16-bit unsigned) */
#define MINT32     23   /* INT (32-bit signed) */
#define MUINT32    24   /* UINT (32-bit unsigned) */
#define MINT64     25   /* INT (64-bit signed) */
#define MUINT64    26   /* UINT (64-bit unsigned) */

/* Parameter list */
typedef struct param_list {
	char *pname;
	struct mode_def *pmode;
	int pflags;              /* LOC, IN, OUT, INOUT */
	struct param_list *pnext;
} PARAM_LIST;

/* Parameter flags */
#define PLOC       (1<<0)   /* LOC (pass by location) */
#define PIN        (1<<1)   /* IN (input only) */
#define POUT       (1<<2)   /* OUT (output only) */
#define PINOUT     (PIN|POUT)  /* INOUT (input/output) */

/* Field list (for structures) */
typedef struct field_list {
	char *fname;
	struct mode_def *fmode;
	int foffset;
	struct field_list *fnext;
} FIELD_LIST;

/* Name list (for sets/enumerations) */
typedef struct name_list {
	char *nname;
	int nvalue;
	struct name_list *nnext;
} NAME_LIST;

/* Variant list */
typedef struct variant_list {
	struct tag_list *tags;      /* Tag values for this variant */
	struct field_list *fields;  /* Fields in this variant */
	struct variant_list *vnext;
} VARIANT_LIST;

/* Tag list */
typedef struct tag_list {
	int64_t tvalue;
	struct tag_list *tnext;
} TAG_LIST;

/* AST node types - tokens are defined by yacc in y.tab.h */
/* These are only used internally for AST representation */
#define AST_SUBSCRIPT  1001   /* Array subscript */
#define AST_FIELD      1002   /* Structure field access */
#define AST_DEREF      1003   /* -> (reference dereference) */
#define AST_ADDR       1004   /* ->(mode) (reference creation) */
#define AST_SLICE      1005   /* (i:j) (string/array slice) */

/* Global variables */
extern int lineno;          /* Current line number */
extern int current_column;  /* Current column */
extern char *current_file;  /* Current filename */
extern FILE *outfile;       /* Output file for IR */
extern int blevel;          /* Current block nesting level */
extern int nerrors;         /* Error count */
extern int nwarnings;       /* Warning count */
extern int verbose;         /* Verbose output flag */
extern char *module_name;   /* Current module name */

/* Symbol table functions */
void symtab_init(void);
SYMTAB *lookup(const char *name, int level);
SYMTAB *install(const char *name, int class, int level);
void hide(int level);
SYMTAB *find_symbol(const char *name);

/* Mode management */
MODE_DEF *mkmode_int(int size, int is_signed);
MODE_DEF *mkmode_real(int size);
MODE_DEF *mkmode_bool(void);
MODE_DEF *mkmode_char(void);
MODE_DEF *mkmode_array(MODE_DEF *elem, int *dims, int ndims);
MODE_DEF *mkmode_struct(char *tag, FIELD_LIST *fields);
MODE_DEF *mkmode_ref(MODE_DEF *base);
MODE_DEF *mkmode_range(int64_t low, int64_t high, MODE_DEF *parent);
MODE_DEF *mkmode_powerset(NAME_LIST *names);
MODE_DEF *mkmode_proc(MODE_DEF *retmode, PARAM_LIST *params);
MODE_DEF *mkmode_bits(int length);
MODE_DEF *mkmode_chars(int length);
int mode_compatible(MODE_DEF *m1, MODE_DEF *m2);
int mode_size(MODE_DEF *m);

/* Predefined modes */
extern MODE_DEF *mode_int;      /* INT */
extern MODE_DEF *mode_bool;     /* BOOL */
extern MODE_DEF *mode_char;     /* CHAR */
extern MODE_DEF *mode_real;     /* REAL */
extern MODE_DEF *mode_duration; /* DURATION */
extern MODE_DEF *mode_time;     /* TIME */

/* Built-in procedures and functions */
void init_builtins(void);
int is_builtin(const char *name);

/* Error reporting */
void error(const char *fmt, ...);
void warning(const char *fmt, ...);
void yyerror(const char *s);

/* Code generation interface */
void emit_code(int op, ...);
void emit_label(int label);
int new_label(void);
int new_temp(void);

/* Assembly code generation */
void codegen_init(const char *output_filename, const char *modname);
void codegen_prologue(void);
void codegen_data_section(void);
void codegen_procedure(const char *name);
void codegen_module_init(void);
void codegen_main(void);
void codegen_epilogue(void);
void codegen_finish(void);
void codegen_module(const char *output_filename, const char *modname);

#endif /* PASS1_H */
