/*
 * Copyright (c) 2025 PCC Prolog Runtime Library
 *
 * Runtime data structures and types
 * Based on Warren Abstract Machine (WAM) architecture
 */

#ifndef PROLOG_RUNTIME_H
#define PROLOG_RUNTIME_H

#include <stdio.h>
#include <stdint.h>
#include <setjmp.h>

/* Version */
#define PROLOG_VERSION "1.0.0"
#define WAM_VERSION 1

/* Memory sizes (in words) */
#define HEAP_SIZE       (1024 * 1024)  /* 1M words */
#define STACK_SIZE      (256 * 1024)   /* 256K words */
#define TRAIL_SIZE      (256 * 1024)   /* 256K words */
#define PDL_SIZE        (64 * 1024)    /* 64K words (push-down list) */

/* Term tag types (low 3 bits) */
typedef enum {
	TAG_REF = 0,        /* Reference (unbound variable) */
	TAG_STRUCT = 1,     /* Structure (compound term) */
	TAG_LIST = 2,       /* List cons cell */
	TAG_INT = 3,        /* Integer (tagged) */
	TAG_ATOM = 4,       /* Atom */
	TAG_FLOAT = 5,      /* Float (pointer to heap) */
	TAG_VAR = 6,        /* Variable (named) */
	TAG_STR = 7         /* String */
} tag_t;

/* Word type - tagged pointer/value */
typedef uintptr_t word_t;

/* Extract tag from word */
#define TAG(w)          ((w) & 0x7)
#define UNTAG(w)        ((w) & ~0x7UL)
#define MAKE_TAG(ptr, tag) (((uintptr_t)(ptr)) | (tag))

/* Tag predicates */
#define IS_REF(w)       (TAG(w) == TAG_REF)
#define IS_STRUCT(w)    (TAG(w) == TAG_STRUCT)
#define IS_LIST(w)      (TAG(w) == TAG_LIST)
#define IS_INT(w)       (TAG(w) == TAG_INT)
#define IS_ATOM(w)      (TAG(w) == TAG_ATOM)
#define IS_FLOAT(w)     (TAG(w) == TAG_FLOAT)
#define IS_VAR(w)       (TAG(w) == TAG_VAR)
#define IS_STR(w)       (TAG(w) == TAG_STR)

/* Integer tagging (for small integers) */
#define INT_TAG_BITS    3
#define MAX_TAGGED_INT  ((1L << (sizeof(word_t)*8 - INT_TAG_BITS - 1)) - 1)
#define MIN_TAGGED_INT  (-(1L << (sizeof(word_t)*8 - INT_TAG_BITS - 1)))
#define MAKE_INT(n)     (((word_t)((n) << INT_TAG_BITS)) | TAG_INT)
#define GET_INT(w)      ((intptr_t)(w) >> INT_TAG_BITS)

/* Atom table entry */
typedef struct atom_entry {
	char *name;
	int length;
	uint32_t hash;
	struct atom_entry *next;
} atom_entry_t;

/* Functor (name/arity pair) */
typedef struct functor {
	atom_entry_t *name;
	int arity;
} functor_t;

/* String representation */
typedef struct string {
	size_t length;
	char data[];  /* Flexible array member */
} string_t;

/* Floating point (boxed) */
typedef struct float_box {
	double value;
} float_box_t;

/* Choice point (for backtracking) */
typedef struct choice_point {
	word_t *continuation;       /* Continuation pointer */
	word_t *env;                /* Environment pointer */
	word_t *trail_top;          /* Trail top at time of choice */
	word_t *heap_top;           /* Heap top at time of choice */
	struct choice_point *prev;  /* Previous choice point */
	word_t *clause;             /* Next clause to try */
	int arity;                  /* Number of arguments */
	word_t args[];              /* Saved arguments */
} choice_point_t;

/* Environment frame (for deterministic predicates) */
typedef struct environment {
	word_t *continuation;       /* Return address */
	struct environment *prev;   /* Previous environment */
	int size;                   /* Size of this frame */
	word_t slots[];             /* Local variables */
} environment_t;

/* Predicate definition */
typedef struct predicate {
	functor_t *functor;
	int (*builtin)(word_t *args);  /* NULL if not builtin */
	word_t *clauses;               /* Pointer to clause chain */
	int clause_count;
	int is_dynamic;
	int is_multifile;
	struct predicate *next;        /* Hash table link */
} predicate_t;

/* Clause representation */
typedef struct clause {
	predicate_t *pred;
	word_t *head;                  /* Head pattern */
	word_t *body;                  /* Body goals (NULL for facts) */
	struct clause *next;           /* Next clause */
} clause_t;

/* Stream (for I/O) */
typedef struct stream {
	FILE *fp;
	char *name;
	int flags;                     /* Read/write/binary/etc */
	int eof_action;
	struct stream *next;
} stream_t;

/* Stream flags */
#define STREAM_READ     0x01
#define STREAM_WRITE    0x02
#define STREAM_BINARY   0x04
#define STREAM_EOF      0x08
#define STREAM_ERROR    0x10

/* Prolog engine state */
typedef struct prolog_engine {
	/* Memory areas */
	word_t *heap;                  /* Heap (terms) */
	word_t *heap_top;              /* Top of heap */
	word_t *heap_limit;

	word_t *stack;                 /* Local stack (environments) */
	word_t *stack_top;
	word_t *stack_limit;

	word_t *trail;                 /* Trail (for backtracking) */
	word_t *trail_top;
	word_t *trail_limit;

	word_t *pdl;                   /* Push-down list (for unification) */
	word_t *pdl_top;
	word_t *pdl_limit;

	/* Current state */
	environment_t *env;            /* Current environment */
	choice_point_t *choice;        /* Current choice point */
	word_t *continuation;          /* Continuation pointer */

	/* Atom and functor tables */
	atom_entry_t **atom_table;
	int atom_table_size;

	predicate_t **pred_table;
	int pred_table_size;

	/* Standard streams */
	stream_t *current_input;
	stream_t *current_output;
	stream_t *current_error;
	stream_t *stream_list;

	/* Flags and settings */
	int debug;
	int trace;
	int occurs_check;

	/* Exception handling */
	jmp_buf *exception_handler;
	word_t exception_term;

	/* Statistics */
	uint64_t inferences;
	uint64_t choice_points;
	uint64_t unifications;

} prolog_engine_t;

/* Global engine instance */
extern prolog_engine_t *prolog_engine;

/* Initialization and cleanup */
prolog_engine_t *prolog_init(void);
void prolog_cleanup(prolog_engine_t *eng);
void prolog_reset(prolog_engine_t *eng);

/* Atom table */
atom_entry_t *atom_intern(prolog_engine_t *eng, const char *name, int len);
atom_entry_t *atom_lookup(prolog_engine_t *eng, const char *name, int len);
#define atom_intern_cstr(eng, s) atom_intern(eng, s, strlen(s))
#define atom_lookup_cstr(eng, s) atom_lookup(eng, s, strlen(s))

/* Functor creation */
functor_t *functor_create(prolog_engine_t *eng, const char *name, int arity);
functor_t *functor_lookup(prolog_engine_t *eng, const char *name, int arity);

/* Term construction */
word_t make_ref(prolog_engine_t *eng, word_t *addr);
word_t make_atom(prolog_engine_t *eng, const char *name);
word_t make_integer(prolog_engine_t *eng, intptr_t value);
word_t make_float(prolog_engine_t *eng, double value);
word_t make_string(prolog_engine_t *eng, const char *str, size_t len);
word_t make_struct(prolog_engine_t *eng, functor_t *f, word_t *args);
word_t make_list(prolog_engine_t *eng, word_t head, word_t tail);
word_t make_nil(prolog_engine_t *eng);
word_t make_var(prolog_engine_t *eng);

/* Unification */
int unify(prolog_engine_t *eng, word_t t1, word_t t2);
word_t deref(prolog_engine_t *eng, word_t term);
void trail(prolog_engine_t *eng, word_t *addr);
void unwind_trail(prolog_engine_t *eng, word_t *trail_mark);

/* Backtracking */
choice_point_t *create_choice_point(prolog_engine_t *eng, int arity);
void restore_choice_point(prolog_engine_t *eng, choice_point_t *cp);
void discard_choice_point(prolog_engine_t *eng);

/* Predicate management */
predicate_t *predicate_define(prolog_engine_t *eng, functor_t *f);
predicate_t *predicate_lookup(prolog_engine_t *eng, functor_t *f);
void predicate_add_clause(prolog_engine_t *eng, predicate_t *pred,
                          word_t *head, word_t *body);

/* Execution */
int prolog_call(prolog_engine_t *eng, word_t goal);
int prolog_query(prolog_engine_t *eng, const char *query);

/* Built-in predicates */
void register_builtins(prolog_engine_t *eng);
int builtin_true(prolog_engine_t *eng, word_t *args);
int builtin_fail(prolog_engine_t *eng, word_t *args);
int builtin_cut(prolog_engine_t *eng, word_t *args);
int builtin_unify(prolog_engine_t *eng, word_t *args);
int builtin_write(prolog_engine_t *eng, word_t *args);
int builtin_nl(prolog_engine_t *eng, word_t *args);

/* Arithmetic */
int eval_arith(prolog_engine_t *eng, word_t expr, intptr_t *result);
int eval_arith_float(prolog_engine_t *eng, word_t expr, double *result);

/* I/O */
stream_t *stream_open(prolog_engine_t *eng, const char *path, const char *mode);
void stream_close(prolog_engine_t *eng, stream_t *s);
int stream_getc(prolog_engine_t *eng, stream_t *s);
int stream_putc(prolog_engine_t *eng, stream_t *s, int c);

/* Printing */
void print_term(prolog_engine_t *eng, FILE *fp, word_t term);
void print_term_canonical(prolog_engine_t *eng, FILE *fp, word_t term);

/* Error handling */
void prolog_error(prolog_engine_t *eng, const char *type, const char *msg);
void prolog_throw(prolog_engine_t *eng, word_t exception);
int prolog_catch(prolog_engine_t *eng, word_t goal, word_t catcher,
                word_t recovery);

/* Utilities */
int term_compare(prolog_engine_t *eng, word_t t1, word_t t2);
int term_ground(prolog_engine_t *eng, word_t term);
word_t term_copy(prolog_engine_t *eng, word_t term);

/* Memory allocation helpers */
word_t *heap_alloc(prolog_engine_t *eng, size_t words);
word_t *stack_alloc(prolog_engine_t *eng, size_t words);
int is_heap_addr(prolog_engine_t *eng, word_t *addr);
int is_stack_addr(prolog_engine_t *eng, word_t *addr);

/* Statistics */
void prolog_statistics(prolog_engine_t *eng, FILE *fp);

/* Arithmetic built-ins */
int builtin_is(prolog_engine_t *eng, word_t *args);
int builtin_arith_equal(prolog_engine_t *eng, word_t *args);
int builtin_arith_noteq(prolog_engine_t *eng, word_t *args);
int builtin_arith_less(prolog_engine_t *eng, word_t *args);
int builtin_arith_lesseq(prolog_engine_t *eng, word_t *args);
int builtin_arith_greater(prolog_engine_t *eng, word_t *args);
int builtin_arith_greatereq(prolog_engine_t *eng, word_t *args);

/* Additional built-ins */
int builtin_var(prolog_engine_t *eng, word_t *args);
int builtin_integer(prolog_engine_t *eng, word_t *args);
int builtin_functor(prolog_engine_t *eng, word_t *args);

/* Term operations */
int term_functor(prolog_engine_t *eng, word_t term, functor_t **f_out);
int term_arg(prolog_engine_t *eng, int n, word_t term, word_t *arg_out);
int term_univ(prolog_engine_t *eng, word_t term, word_t *list_out);
int term_from_univ(prolog_engine_t *eng, word_t list, word_t *term_out);
int term_arity(prolog_engine_t *eng, word_t term);

#endif /* PROLOG_RUNTIME_H */
