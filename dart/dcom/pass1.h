/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * Dart compiler pass 1 definitions
 */

#ifndef _PASS1_H_
#define _PASS1_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/* Dart type kinds */
typedef enum {
	D_VOID,
	D_BOOL,
	D_INT,
	D_DOUBLE,
	D_STRING,
	D_LIST,
	D_MAP,
	D_DYNAMIC,
	D_OBJECT,
	D_NULL,
	D_FUNCTION,
	D_CLASS,
	D_TYPEDEF
} DartTypeKind;

/* Dart node types */
typedef enum {
	N_PROGRAM,
	N_IMPORT,
	N_LIBRARY,
	N_CLASS,
	N_FUNCTION,
	N_METHOD,
	N_VARIABLE,
	N_PARAMETER,
	N_FIELD,
	N_CONSTRUCTOR,
	N_BLOCK,
	N_IF,
	N_FOR,
	N_WHILE,
	N_RETURN,
	N_EXPRESSION,
	N_CALL,
	N_BINARY_OP,
	N_UNARY_OP,
	N_LITERAL,
	N_IDENTIFIER
} NodeType;

/* Symbol table entry */
typedef struct symbol {
	char *name;
	DartTypeKind type;
	int scope_level;
	struct symbol *next;
	void *type_info;  /* Additional type information */
} Symbol;

/* AST node */
typedef struct node {
	NodeType type;
	int lineno;
	int column;
	union {
		int intval;
		double doubleval;
		char *strval;
		struct {
			struct node *left;
			struct node *right;
		} binary;
		struct node *child;
	} data;
	struct node *next;
	Symbol *symbol;
} Node;

/* Error reporting */
void yyerror(const char *s);
void dart_error(int lineno, int column, const char *fmt, ...);
void dart_warning(int lineno, int column, const char *fmt, ...);

/* Symbol table management */
Symbol *symtab_lookup(const char *name);
Symbol *symtab_insert(const char *name, DartTypeKind type);
void symtab_enter_scope(void);
void symtab_exit_scope(void);

/* AST construction */
Node *make_node(NodeType type);
Node *make_binary_op(int op, Node *left, Node *right);
Node *make_literal_int(int value);
Node *make_literal_double(double value);
Node *make_literal_string(char *value);
Node *make_identifier(char *name);

/* Type checking */
DartTypeKind check_type(Node *node);
int types_compatible(DartTypeKind t1, DartTypeKind t2);

/* Code generation */
void generate_code(Node *root);

/* Global variables */
extern FILE *yyin;
extern int yylineno;
extern int yycolumn;
extern char *yytext;
extern Node *ast_root;

#endif /* _PASS1_H_ */
