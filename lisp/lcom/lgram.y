/*
 * Copyright (c) 2025 PCC Common LISP Compiler
 *
 * Grammar for Common LISP
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

extern int yylex(void);
extern void yyerror(const char *);
extern int lineno;
%}

%union {
	long integer;
	double floating;
	char *string;
	lisp_value_t *value;
}

/* Tokens */
%token LPAREN RPAREN
%token QUOTE BACKQUOTE COMMA COMMA_AT FUNCTION_QUOTE
%token NIL T
%token DEFUN DEFVAR DEFPARAMETER DEFCONSTANT
%token SETQ LET LETSTAR LAMBDA
%token IF COND CASE WHEN UNLESS
%token PROGN PROG1 PROG2
%token AND OR NOT
%token DO DOLIST DOTIMES LOOP RETURN

%token <integer> INTEGER
%token <floating> FLOAT
%token <string> STRING
%token <string> SYMBOL

%type <value> expr
%type <value> atom
%type <value> list
%type <value> expr_list
%type <value> defun_form
%type <value> defvar_form
%type <value> setq_form
%type <value> let_form
%type <value> lambda_form
%type <value> if_form
%type <value> progn_form
%type <value> function_call
%type <value> quoted_expr
%type <value> param_list

%%

program:
	/* empty */
	| program top_level_form
	;

top_level_form:
	expr                { emit_expr($1); }
	;

expr:
	atom                { $$ = $1; }
	| list              { $$ = $1; }
	| quoted_expr       { $$ = $1; }
	;

atom:
	NIL                 { $$ = make_nil(); }
	| T                 { $$ = make_t(); }
	| INTEGER           { $$ = make_integer($1); }
	| FLOAT             { $$ = make_float($1); }
	| STRING            { $$ = make_string($1); free($1); }
	| SYMBOL            { $$ = make_symbol($1); free($1); }
	;

quoted_expr:
	QUOTE expr          { $$ = make_cons(make_symbol("quote"),
	                                      make_cons($2, make_nil())); }
	| BACKQUOTE expr    { $$ = make_cons(make_symbol("backquote"),
	                                      make_cons($2, make_nil())); }
	;

list:
	LPAREN RPAREN       { $$ = make_nil(); }
	| LPAREN expr_list RPAREN
	                    { $$ = $2; }
	| defun_form
	| defvar_form
	| setq_form
	| let_form
	| lambda_form
	| if_form
	| progn_form
	| function_call
	;

expr_list:
	expr                { $$ = make_cons($1, make_nil()); }
	| expr expr_list    { $$ = make_cons($1, $2); }
	;

defun_form:
	LPAREN DEFUN SYMBOL param_list expr_list RPAREN
	                    {
	                        symbol_t *sym = define_function($3, $4, $5);
	                        emit_function($3, $4, $5);
	                        $$ = make_symbol($3);
	                        free($3);
	                    }
	;

defvar_form:
	LPAREN DEFVAR SYMBOL expr RPAREN
	                    {
	                        symbol_t *sym = define_symbol($3, LISP_SYMBOL);
	                        sym->value = $4;
	                        $$ = make_symbol($3);
	                        free($3);
	                    }
	| LPAREN DEFVAR SYMBOL RPAREN
	                    {
	                        symbol_t *sym = define_symbol($3, LISP_SYMBOL);
	                        $$ = make_symbol($3);
	                        free($3);
	                    }
	;

setq_form:
	LPAREN SETQ SYMBOL expr RPAREN
	                    {
	                        $$ = make_cons(make_symbol("setq"),
	                            make_cons(make_symbol($3),
	                                make_cons($4, make_nil())));
	                        free($3);
	                    }
	;

let_form:
	LPAREN LET LPAREN expr_list RPAREN expr_list RPAREN
	                    {
	                        $$ = make_cons(make_symbol("let"),
	                            make_cons($4, $6));
	                    }
	;

lambda_form:
	LPAREN LAMBDA param_list expr_list RPAREN
	                    {
	                        $$ = make_cons(make_symbol("lambda"),
	                            make_cons($3, $4));
	                    }
	;

if_form:
	LPAREN IF expr expr RPAREN
	                    {
	                        $$ = make_cons(make_symbol("if"),
	                            make_cons($3,
	                                make_cons($4, make_nil())));
	                    }
	| LPAREN IF expr expr expr RPAREN
	                    {
	                        $$ = make_cons(make_symbol("if"),
	                            make_cons($3,
	                                make_cons($4,
	                                    make_cons($5, make_nil()))));
	                    }
	;

progn_form:
	LPAREN PROGN expr_list RPAREN
	                    {
	                        $$ = make_cons(make_symbol("progn"), $3);
	                    }
	;

function_call:
	LPAREN SYMBOL expr_list RPAREN
	                    {
	                        $$ = make_cons(make_symbol($2), $3);
	                        free($2);
	                    }
	| LPAREN SYMBOL RPAREN
	                    {
	                        $$ = make_cons(make_symbol($2), make_nil());
	                        free($2);
	                    }
	;

param_list:
	LPAREN RPAREN       { $$ = make_nil(); }
	| LPAREN expr_list RPAREN
	                    { $$ = $2; }
	;

%%

void
yyerror(const char *s)
{
	fprintf(stderr, "Error at line %d: %s\n", lineno, s);
	nerrors++;
	if (nerrors >= max_errors) {
		fprintf(stderr, "Too many errors, aborting.\n");
		exit(1);
	}
}
