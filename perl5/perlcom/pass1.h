/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 * Perl 5 Compiler - Pass 1 definitions
 */

#include "config.h"

#include <sys/types.h>
#include <stdarg.h>
#include <string.h>
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#include <stdlib.h>

#ifndef MKEXT
#include "external.h"
#else
typedef unsigned int bittype;
#endif
#include "manifest.h"
#include "softfloat.h"

/*
 * Perl 5 storage classes
 */
#define SNULL		0
#define P_AUTO		1	/* my variable */
#define P_EXTERN	2	/* our variable (package global) */
#define P_STATIC	3	/* state variable */
#define P_PARAM		4	/* subroutine parameter */
#define P_GLOBAL	5	/* global variable */
#define P_LOCAL		6	/* local variable (dynamic scope) */

/*
 * Perl 5 variable types
 */
#define P_SCALAR	1	/* $var */
#define P_ARRAY		2	/* @var */
#define P_HASH		3	/* %var */
#define P_SUB		4	/* &sub */
#define P_GLOB		5	/* *var */
#define P_REF		6	/* reference */

/*
 * Perl 5 specific type definitions
 */
#define PERLSV		23	/* Perl scalar value */
#define PERLAV		24	/* Perl array value */
#define PERLHV		25	/* Perl hash value */
#define PERLCV		26	/* Perl code value */
#define PERLGV		27	/* Perl glob value */

/*
 * Symbol table entry for Perl
 */
struct perl_symtab {
	char *name;		/* Variable name */
	int vartype;		/* P_SCALAR, P_ARRAY, P_HASH, etc */
	int storage;		/* Storage class */
	int scope;		/* Scope level */
	void *value;		/* Compile-time value if constant */
	struct perl_symtab *next;
};

/*
 * Perl operator precedence and associativity
 */
#define PERL_LEFT	1
#define PERL_RIGHT	2
#define PERL_NONASSOC	3

/*
 * Function prototypes
 */
void perl_init(void);
void perl_finish(void);
struct perl_symtab *perl_lookup(char *name, int vartype);
struct perl_symtab *perl_install(char *name, int vartype, int storage);
void perl_enter_scope(void);
void perl_leave_scope(void);

/* Tree building functions */
void *perl_scalar_node(char *name);
void *perl_array_node(char *name);
void *perl_hash_node(char *name);
void *perl_binop_node(int op, void *left, void *right);
void *perl_unop_node(int op, void *operand);
void *perl_const_node(int type, void *value);

/* Code generation */
void perl_gen_assign(void *lval, void *rval);
void perl_gen_sub_call(char *name, void *args);
void perl_gen_if(void *cond, void *then_block, void *else_block);
void perl_gen_while(void *cond, void *block);
void perl_gen_for(void *init, void *cond, void *incr, void *block);
void perl_gen_foreach(void *var, void *list, void *block);

/* Built-in function handlers */
void perl_builtin_print(void *args);
void perl_builtin_push(void *array, void *values);
void perl_builtin_pop(void *array);
void perl_builtin_shift(void *array);
void perl_builtin_unshift(void *array, void *values);

/* Regular expression support */
void *perl_regex_compile(char *pattern, char *flags);
void *perl_regex_match(void *string, void *regex);
void *perl_regex_subst(void *string, void *pattern, void *replacement, char *flags);

/* Global variables */
extern int perl_lineno;
extern char *perl_filename;
extern int perl_nerrors;
extern int perl_scope_level;
extern struct perl_symtab *perl_symtab_head;

/* Error handling */
void perl_error(char *fmt, ...);
void perl_warning(char *fmt, ...);
void perl_fatal(char *fmt, ...);

#endif /* PASS1_H */
