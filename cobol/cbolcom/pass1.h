/*
 * COBOL compiler pass1 definitions
 * Supports OO COBOL with multi-vendor dialect compatibility
 */

#ifndef _PASS1_H
#define _PASS1_H

#include <stdio.h>
#include "manifest.h"
#include "node.h"

/* COBOL dialects */
#define DIALECT_DEFAULT 0
#define DIALECT_IBM     1
#define DIALECT_DEC     2
#define DIALECT_HP      3
#define DIALECT_MS      4

/* Division types */
#define DIV_IDENTIFICATION  1
#define DIV_ENVIRONMENT     2
#define DIV_DATA            3
#define DIV_PROCEDURE       4

/* Data types - based on PICTURE clauses */
#define COB_TYPE_NUMERIC        1   /* 9 */
#define COB_TYPE_ALPHABETIC     2   /* A */
#define COB_TYPE_ALPHANUMERIC   3   /* X */
#define COB_TYPE_EDITED         4   /* Z, *, +, -, etc. */
#define COB_TYPE_POINTER        5   /* POINTER */
#define COB_TYPE_OBJECT         6   /* OBJECT REFERENCE */

/* Storage classes */
#define COB_CLASS_WORKING       1
#define COB_CLASS_LOCAL         2
#define COB_CLASS_LINKAGE       3
#define COB_CLASS_FILE          4
#define COB_CLASS_CONSTANT      5

/* PICTURE clause info */
struct picture {
	int pic_type;           /* COB_TYPE_* */
	int pic_digits;         /* Total digits (9's) */
	int pic_scale;          /* Decimal places (V position) */
	int pic_size;           /* Total byte size */
	int pic_sign;           /* S present */
	char *pic_string;       /* Original PICTURE string */
};

/* COBOL symbol table entry */
struct cobsym {
	struct cobsym *next;    /* Next in hash chain */
	char *name;             /* Symbol name */
	int level;              /* 01-49, 66, 77, 88 */
	int sclass;             /* Storage class (COB_CLASS_*) */
	int offset;             /* Offset in structure */
	struct picture *pic;    /* PICTURE clause */
	struct cobsym *parent;  /* Parent for structured data */
	struct cobsym *children; /* First child */
	struct cobsym *sibling; /* Next sibling */

	/* OO COBOL extensions */
	int is_class;           /* Class definition */
	int is_method;          /* Method definition */
	int is_property;        /* Property definition */
	char *inherits;         /* Inheritance (class name) */

	/* Dialect-specific attributes */
	void *dialect_data;     /* Vendor-specific extensions */
};

/* File descriptor */
struct cobol_file {
	char *name;             /* File name */
	char *organization;     /* SEQUENTIAL, INDEXED, RELATIVE */
	char *access_mode;      /* SEQUENTIAL, RANDOM, DYNAMIC */
	char *record_key;       /* Key field name */
	struct cobsym *fd;      /* File descriptor symbol */
};

/* Method/function descriptor */
struct cobol_method {
	char *name;             /* Method name */
	struct cobsym *params;  /* Parameter list */
	struct cobsym *locals;  /* Local variables */
	int is_static;          /* STATIC method */
	int is_final;           /* FINAL method */
	char *returns;          /* RETURNING type */
};

/* Class descriptor */
struct cobol_class {
	char *name;             /* Class name */
	char *inherits;         /* Parent class */
	struct cobsym *properties;  /* Property list */
	struct cobol_method *methods; /* Method list */
	int is_abstract;        /* ABSTRACT class */
	int is_final;           /* FINAL class */
};

/* Global state */
extern int lineno;
extern int nerrors;
extern int nwarnings;
extern int dialect;
extern int current_division;
extern FILE *infile;
extern FILE *outfile;

/* Symbol table functions */
struct cobsym *lookup(const char *name);
struct cobsym *install(const char *name, int level);
void enter_scope(void);
void exit_scope(void);

/* Type system */
struct picture *parse_picture(const char *pic);
int get_cobol_size(struct picture *pic);
TWORD picture_to_type(struct picture *pic);

/* Error handling */
void yyerror(const char *s);
void error_at(int line, const char *fmt, ...);
void warning_at(int line, const char *fmt, ...);

/* Code generation */
void gen_prologue(void);
void gen_epilogue(void);
NODE *gen_move(NODE *src, NODE *dst);
NODE *gen_call(const char *name, NODE *args);
NODE *gen_if(NODE *cond, NODE *then_part, NODE *else_part);
NODE *gen_perform(const char *label, NODE *until);

/* Dialect support */
void set_dialect(int d);
int is_reserved_word(const char *word);
void apply_dialect_semantics(void);

/* Built-in functions */
NODE *gen_builtin_accept(NODE *var);
NODE *gen_builtin_display(NODE *args);
NODE *gen_builtin_compute(NODE *result, NODE *expr);

/* OO COBOL support */
struct cobol_class *define_class(const char *name, const char *inherits);
struct cobol_method *define_method(const char *name, int is_static);
NODE *gen_invoke(const char *obj, const char *method, NODE *args);
NODE *gen_new(const char *classname);

/* Expression handling */
NODE *buildtree(int op, NODE *left, NODE *right);
NODE *make_icon(CONSZ val);
NODE *make_name(const char *name);

/* Utility */
char *xstrdup(const char *s);
void *xmalloc(size_t size);

#endif /* _PASS1_H */
