/*
 * Copyright (c) 2025 PCC Prolog Compiler
 *
 * Pass 1 definitions for Prolog compiler
 * Data structures and function declarations
 */

#ifndef PASS1_H
#define PASS1_H

#include <stdio.h>
#include <stdint.h>

/* Prolog term types */
typedef enum {
	TERM_ATOM,          /* Atom (constant symbol) */
	TERM_VARIABLE,      /* Variable */
	TERM_INTEGER,       /* Integer constant */
	TERM_FLOAT,         /* Float constant */
	TERM_STRING,        /* String literal */
	TERM_COMPOUND,      /* Compound term functor(args) */
	TERM_LIST,          /* List [a, b, c] */
	TERM_NIL,           /* Empty list [] */
	TERM_CONS,          /* List constructor [H|T] */
	TERM_ANON,          /* Anonymous variable _ */
	TERM_CUT,           /* Cut ! */
	TERM_FUNCTOR,       /* Functor reference */
} TermType;

/* Term structure */
struct term {
	TermType type;
	union {
		char *atom;          /* Atom name */
		char *var;           /* Variable name */
		long ival;           /* Integer value */
		double fval;         /* Float value */
		char *str;           /* String value */
		struct {
			char *functor;       /* Functor name */
			int arity;           /* Number of arguments */
			struct termlist *args; /* Argument list */
		} compound;
		struct {
			struct term *head;   /* List head */
			struct term *tail;   /* List tail */
		} cons;
	} data;
	int lineno;          /* Source line number */
	int column;          /* Source column */
	struct term *next;   /* For linked lists */
};

/* Term list structure */
struct termlist {
	struct term *term;
	struct termlist *next;
	int count;           /* Number of terms in list */
};

/* Clause structure */
struct clause {
	struct term *head;   /* Head of clause (fact or rule) */
	struct term *body;   /* Body of clause (NULL for facts) */
	int lineno;
	struct clause *next; /* Link to next clause */
};

/* Predicate declaration (Turbo Prolog style) */
struct pred_decl {
	char *name;          /* Predicate name */
	int arity;           /* Number of arguments */
	char **arg_types;    /* Argument type names (domains) */
	int is_dynamic;      /* Dynamic predicate flag */
	int is_multifile;    /* Multifile flag */
	struct pred_decl *next;
};

/* Domain declaration (Turbo Prolog style) */
struct domain_decl {
	char *name;          /* Domain name */
	char *definition;    /* Domain definition */
	struct domain_decl *next;
};

/* Symbol table entry for predicates */
struct predicate {
	char *name;          /* Predicate name */
	int arity;           /* Arity */
	struct clause *clauses; /* Linked list of clauses */
	int is_builtin;      /* Built-in predicate flag */
	int is_dynamic;      /* Can be modified at runtime */
	int is_multifile;    /* Can be defined in multiple files */
	int clause_count;    /* Number of clauses */
	struct predicate *next; /* Hash table link */
};

/* Module structure (GNU Prolog) */
struct module {
	char *name;          /* Module name */
	struct predicate *exports; /* Exported predicates */
	struct module *imports;    /* Imported modules */
	struct module *next;
};

/* Compiler options */
struct compiler_options {
	int turbo_mode;      /* Turbo Prolog compatibility mode */
	int gnu_mode;        /* GNU Prolog compatibility mode */
	int debug;           /* Debug mode */
	int optimize;        /* Optimization level */
	int warnings;        /* Warning level */
	char *output_file;   /* Output file name */
	int output_format;   /* 0=C, 1=bytecode, 2=WAM */
};

/* Global variables */
extern int lineno;
extern int current_column;
extern struct compiler_options options;
extern struct predicate *predicate_table[256]; /* Hash table */
extern struct clause *clause_list;
extern struct module *current_module;

/* Function declarations */

/* Lexer */
int yylex(void);
void reset_lexer(void);

/* Parser */
int yyparse(void);
void yyerror(const char *s);

/* Error handling */
void error(const char *fmt, ...);
void warning(const char *fmt, ...);
void fatal(const char *fmt, ...);

/* Term creation */
struct term *make_atom(char *name);
struct term *make_variable(char *name);
struct term *make_integer(long value);
struct term *make_float(double value);
struct term *make_string(char *value);
struct term *make_compound(char *functor, struct termlist *args);
struct term *make_list(struct termlist *elements);
struct term *make_cons(struct term *head, struct term *tail);
struct term *make_nil(void);
struct term *make_anonymous(void);
struct term *make_cut(void);

/* Term list operations */
struct termlist *make_termlist(struct term *term);
struct termlist *append_term(struct termlist *list, struct term *term);
int termlist_length(struct termlist *list);

/* Clause operations */
struct clause *make_fact(struct term *head);
struct clause *make_rule(struct term *head, struct term *body);
void add_clause(struct clause *clause);

/* Predicate operations */
struct predicate *lookup_predicate(char *name, int arity);
struct predicate *define_predicate(char *name, int arity);
void add_predicate_clause(struct predicate *pred, struct clause *clause);

/* Symbol table */
void init_symtab(void);
void dump_symtab(void);

/* Code generation */
void generate_code(void);
void generate_c_code(FILE *fp);
void generate_bytecode(FILE *fp);
void generate_wam(FILE *fp);

/* Built-in predicates */
void init_builtins(void);
int is_builtin(char *name, int arity);

/* Unification (runtime support) */
int unify(struct term *t1, struct term *t2);
struct term *copy_term(struct term *t);
void free_term(struct term *t);

/* Pretty printing */
void print_term(FILE *fp, struct term *t);
void print_clause(FILE *fp, struct clause *c);
void print_program(FILE *fp);

/* Utilities */
char *strdup_check(const char *s);
void *malloc_check(size_t size);
unsigned int hash_string(const char *str);

#endif /* PASS1_H */
