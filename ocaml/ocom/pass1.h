/*
 * Copyright (c) 2025 PCC OCaml Compiler
 *
 * Pass 1 header - Common definitions for OCaml frontend
 */

#ifndef PASS1_H
#define PASS1_H

#include <stdio.h>
#include <stdarg.h>

/* Error tracking */
extern int nerrors;
extern int nwarnings;
extern int max_errors;
extern int use_color;
extern int show_caret;
extern char *current_file;
extern int lineno;
extern int current_column;

/* Symbol table entry */
struct symtab {
	char *name;
	int type;
	struct tnode *typeinfo;
	struct symtab *next;
};

/* Type node */
struct tnode {
	int ttype;
	int tsize;
	int talign;
	struct tnode *tsubtype;
	struct tnode *tnext;
};

typedef struct tnode TNODE;

/* Basic types */
#define TINT      1
#define TFLOAT    2
#define TCHAR     3
#define TSTRING   4
#define TBOOL     5
#define TUNIT     6
#define TLIST     7
#define TARRAY    8
#define TFUNCTION 9
#define TRECORD   10
#define TVARIANT  11
#define TPOINTER  12

/* Predefined types */
extern TNODE *int_type;
extern TNODE *float_type;
extern TNODE *bool_type;
extern TNODE *char_type;
extern TNODE *string_type;
extern TNODE *unit_type;

/* Functions from error.c */
void error_init(void);
void error(const char *fmt, ...);
void warning(const char *fmt, ...);
void fatal(const char *fmt, ...);

/* Functions from symtab.c */
void symtab_init(void);
struct symtab *lookup(const char *name);
struct symtab *install(const char *name, int type);
void enter_scope(void);
void exit_scope(void);

/* Functions from types.c */
void init_types(void);
TNODE *mktype(int basictype);
TNODE *mkarray(TNODE *elemtype);
TNODE *mkfunction(TNODE *argtype, TNODE *rettype);
int compatible_types(TNODE *t1, TNODE *t2);

/* Functions from builtins.c */
void init_builtins(void);

#endif /* PASS1_H */
