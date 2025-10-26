/*
 * Copyright (c) 2025 PCC Go Compiler
 *
 * Pass 1 definitions - Frontend data structures for Go
 * Based on PCC architecture
 */

#ifndef PASS1_H
#define PASS1_H

#include "../../mip/manifest.h"
#include "error.h"

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
	char *pkg_name;         /* Package name (for imports) */
} SYMTAB;

/* Storage classes (sclass) */
#define SNULL      0    /* Undefined */
#define AUTO       1    /* Local variable */
#define EXTERN     2    /* External reference */
#define STATIC     3    /* Static (package-level) */
#define REGISTER   4    /* Register hint */
#define TYPEDEF    5    /* Type definition */
#define PARAM      6    /* Function parameter */
#define STNAME     7    /* Struct tag */
#define MOS        8    /* Member of structure */
#define LABEL      9    /* Label (for goto) */
#define CONST      10   /* Constant */
#define TYPENAME   11   /* Type name */
#define FUNC       12   /* Function */
#define PKGNAME    13   /* Package name */
#define INTERFACE  14   /* Interface type */

/* Symbol flags (sflags) */
#define SUSED      (1<<0)   /* Symbol was used */
#define SSET       (1<<1)   /* Symbol was assigned */
#define SEXPORTED  (1<<2)   /* Exported (capitalized) */
#define SBLANK     (1<<3)   /* Blank identifier _ */
#define SMETHOD    (1<<4)   /* Method (receiver function) */
#define SVARIADIC  (1<<5)   /* Variadic function */

/* Type node structure for Go */
typedef struct tnode {
	int ttype;              /* Basic type */
	int tsize;              /* Size in bytes */
	int talign;             /* Alignment */
	union {
		struct {
			struct tnode *ret_type;      /* Return type (functions) */
			struct param_list *params;    /* Parameter list */
			struct param_list *results;   /* Result list (multiple returns) */
			int is_variadic;              /* Variadic function */
		} func;
		struct {
			struct tnode *elem_type;      /* Element type (arrays/slices) */
			int length;                   /* Array length (-1 for slice) */
		} array;
		struct {
			char *tag;                    /* Struct tag */
			struct field_list *fields;    /* Field list */
		} structure;
		struct {
			struct tnode *base;           /* Pointer base type */
		} ptr;
		struct {
			struct tnode *key_type;       /* Map key type */
			struct tnode *val_type;       /* Map value type */
		} map;
		struct {
			struct tnode *elem_type;      /* Channel element type */
			int direction;                /* 0=bidirectional, 1=send, 2=recv */
		} chan;
		struct {
			char *iface_name;             /* Interface name */
			struct method_list *methods;  /* Method list */
		} iface;
	} tattr;
} TNODE;

/* Basic Go types (ttype) */
#define TNULL       0
#define TBOOL       1   /* bool */
#define TINT8       2   /* int8 */
#define TUINT8      3   /* uint8, byte */
#define TINT16      4   /* int16 */
#define TUINT16     5   /* uint16 */
#define TINT32      6   /* int32, rune */
#define TUINT32     7   /* uint32 */
#define TINT64      8   /* int64 */
#define TUINT64     9   /* uint64 */
#define TINT        10  /* int (platform-dependent) */
#define TUINT       11  /* uint (platform-dependent) */
#define TUINTPTR    12  /* uintptr */
#define TFLOAT32    13  /* float32 */
#define TFLOAT64    14  /* float64 */
#define TCOMPLEX64  15  /* complex64 */
#define TCOMPLEX128 16  /* complex128 */
#define TSTRING     17  /* string */
#define TARRAY      18  /* array */
#define TSLICE      19  /* slice */
#define TSTRUCT     20  /* struct */
#define TPOINTER    21  /* pointer */
#define TFUNCTION   22  /* function */
#define TINTERFACE  23  /* interface */
#define TMAP        24  /* map */
#define TCHAN       25  /* channel */
#define TPACKAGE    26  /* package (pseudo-type) */

/* Parameter list */
typedef struct param_list {
	char *pname;
	struct tnode *ptype;
	int pflags;
	struct param_list *pnext;
} PARAM_LIST;

/* Field list (for structs) */
typedef struct field_list {
	char *fname;                  /* Field name (NULL for embedded) */
	struct tnode *ftype;          /* Field type */
	int foffset;                  /* Offset in struct */
	char *tag;                    /* Field tag (string literal) */
	struct field_list *fnext;
} FIELD_LIST;

/* Method list (for interfaces) */
typedef struct method_list {
	char *mname;
	struct tnode *mtype;          /* Function type */
	struct method_list *mnext;
} METHOD_LIST;

/* AST node types */
#define NAME       100   /* Identifier reference */
#define ICON       101   /* Integer constant */
#define FCON       102   /* Floating constant */
#define SCON       103   /* String constant */
#define CCON       104   /* Rune constant */
#define NILIT      105   /* nil literal */

/* Operators (map to IR operations) */
#define OPLUS      106   /* + */
#define OMINUS     107   /* - */
#define OMUL       108   /* * */
#define ODIV       109   /* / */
#define OMOD       110   /* % */
#define OAND       111   /* & */
#define OOR        112   /* | */
#define OXOR       113   /* ^ */
#define OANDNOT    114   /* &^ */
#define OSHL       115   /* << */
#define OSHR       116   /* >> */
#define OLAND      117   /* && */
#define OLOR       118   /* || */
#define ONOT       119   /* ! */
#define OEQ        120   /* == */
#define ONE        121   /* != */
#define OLT        122   /* < */
#define OLE        123   /* <= */
#define OGT        124   /* > */
#define OGE        125   /* >= */
#define OASSIGN    126   /* = */
#define OCALL      127   /* Function call */
#define OINDEX     128   /* Array/slice/map index */
#define OFIELD     129   /* Struct field access */
#define ODEREF     130   /* Pointer dereference (*) */
#define OADDR      131   /* Address-of (&) */
#define OMAKE      132   /* make() */
#define ONEW       133   /* new() */
#define OLEN       134   /* len() */
#define OCAP       135   /* cap() */
#define OAPPEND    136   /* append() */
#define OCOPY      137   /* copy() */
#define ODELETE    138   /* delete() */
#define OPANIC     139   /* panic() */
#define ORECOVER   140   /* recover() */
#define OPRINT     141   /* print() */
#define OPRINTLN   142   /* println() */

/* Statement types */
#define SBLOCK     200   /* { ... } */
#define SIF        201   /* if */
#define SFOR       202   /* for */
#define SSWITCH    203   /* switch */
#define SSELECT    204   /* select */
#define SRETURN    205   /* return */
#define SBREAK     206   /* break */
#define SCONTINUE  207   /* continue */
#define SGOTO      208   /* goto */
#define SFALL      209   /* fallthrough */
#define SGO        210   /* go (goroutine) */
#define SDEFER     211   /* defer */
#define SLABEL     212   /* label: */

/* Global variables */
extern int lineno;          /* Current line number */
extern int current_column;  /* Current column */
extern char *ftitle;        /* Current filename */
extern char *current_file;  /* Current source file */
extern FILE *outfile;       /* Output file for IR */
extern int blevel;          /* Current block nesting level */
extern char *package_name;  /* Current package name */

/* Symbol table functions */
void symtab_init(void);
SYMTAB *lookup(char *name, int level);
SYMTAB *install(char *name, int class, int level);
void hide(int level);
SYMTAB *find_symbol(char *name);
int is_exported(char *name);

/* Type management */
TNODE *mktype(int basictype);
TNODE *mkarray(TNODE *elemtype, int length);
TNODE *mkslice(TNODE *elemtype);
TNODE *mkstruct(char *tag, FIELD_LIST *fields);
TNODE *mkpointer(TNODE *basetype);
TNODE *mkfunction(TNODE *rettype, PARAM_LIST *params, PARAM_LIST *results);
TNODE *mkinterface(char *name, METHOD_LIST *methods);
TNODE *mkmap(TNODE *keytype, TNODE *valtype);
TNODE *mkchan(TNODE *elemtype, int direction);
int type_compatible(TNODE *t1, TNODE *t2);
int type_size(TNODE *t);
void init_types(void);

/* Built-in functions */
void init_builtins(void);
int is_builtin(char *name);

/* Code generation interface to MIP backend */
void emit_code(int op, ...);
void emit_label(int label);
int new_label(void);
int new_temp(void);

/* Utility functions */
char *str_copy(const char *s);
void *xmalloc(size_t size);
void *xcalloc(size_t nmemb, size_t size);

#endif /* PASS1_H */
