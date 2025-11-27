/*
 * Copyright (c) 2025 PCC OCaml Compiler
 *
 * Parser grammar for OCaml
 * Supports OCaml 4.x and 5.x core language features
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Forward declarations */
int yylex(void);
void yyerror(const char *s);

%}

%union {
	int integer;
	double floating;
	char character;
	char *string;
	struct symtab *symptr;
	struct tnode *node;
}

/* Keywords */
%token AND AS ASSERT OBEGIN CLASS CONSTRAINT DO DONE DOWNTO
%token ELSE OEND EXCEPTION EXTERNAL FALSE FOR FUN FUNCTION FUNCTOR
%token IF IN INCLUDE INHERIT INITIALIZER LAZY LET MATCH METHOD
%token MODULE MUTABLE NEW NONREC OBJECT OF OPEN OR PRIVATE REC
%token SIG STRUCT THEN TO TRUE TRY TYPE VAL VIRTUAL WHEN WHILE WITH

/* Operators and special symbols */
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token SEMI SEMISEMI COMMA DOT DOTDOT COLON COLONCOLON COLONEQ COLONGT
%token EQ EQEQ LT LE GT GE NE BANGEQ
%token PLUS MINUS STAR SLASH PERCENT STARSTAR
%token AT CARET BANG TILDE PIPE PIPEPIPE AMP AMPAMP
%token ARROW LARROW FATARROW QUESTION UNDERSCORE BACKQUOTE HASH

/* Literals */
%token <integer> INT_LITERAL
%token <floating> FLOAT_LITERAL
%token <character> CHAR_LITERAL
%token <string> STRING_LITERAL
%token <string> IDENT

/* Non-terminals */
%type <node> implementation structure_item structure_items
%type <node> expression simple_expr expr_comma_list
%type <node> pattern simple_pattern pattern_comma_list
%type <node> type_expr simple_type_expr
%type <node> let_binding value_binding
%type <node> match_case match_cases
%type <node> constant

/* Precedence and associativity (from lowest to highest) */
%right SEMI
%right LET
%right IF
%right ARROW FATARROW
%right OR PIPEPIPE
%right AMP AMPAMP
%left EQ NE LT GT LE GE EQEQ BANGEQ
%right COLONCOLON AT CARET
%left PLUS MINUS
%left STAR SLASH PERCENT
%right STARSTAR
%left DOT

%%

/* Top-level structure */
implementation:
	  /* empty */
		{ $$ = NULL; }
	| structure_items
		{ $$ = $1; }
	;

structure_items:
	  structure_item
		{ $$ = $1; }
	| structure_items SEMISEMI structure_item
		{ $$ = $3; }
	| structure_items structure_item
		{ $$ = $2; }
	;

structure_item:
	  LET let_binding
		{ $$ = $2; }
	| LET REC let_binding
		{ $$ = $3; }
	| TYPE type_definition
		{ $$ = NULL; }
	| EXCEPTION exception_definition
		{ $$ = NULL; }
	| MODULE module_binding
		{ $$ = NULL; }
	| OPEN module_path
		{ $$ = NULL; }
	| expression
		{ $$ = $1; }
	;

/* Let bindings */
let_binding:
	  value_binding
		{ $$ = $1; }
	| value_binding AND let_binding
		{ $$ = $1; }
	;

value_binding:
	  pattern EQ expression
		{ $$ = NULL; }
	| IDENT parameter_list EQ expression
		{ $$ = NULL; }
	;

parameter_list:
	  simple_pattern
	| parameter_list simple_pattern
	;

/* Type expressions */
type_expr:
	  simple_type_expr
		{ $$ = $1; }
	| type_expr ARROW type_expr
		{ $$ = NULL; }
	| type_expr STAR type_expr
		{ $$ = NULL; }
	;

simple_type_expr:
	  IDENT
		{ $$ = NULL; }
	| UNDERSCORE
		{ $$ = NULL; }
	| LPAREN type_expr RPAREN
		{ $$ = $2; }
	| type_expr IDENT
		{ $$ = NULL; }
	;

/* Type definitions */
type_definition:
	  IDENT EQ type_expr
	| IDENT EQ type_expr AND type_definition
	;

/* Exception definitions */
exception_definition:
	  IDENT
	| IDENT OF type_expr
	;

/* Module expressions */
module_binding:
	  IDENT EQ module_expr
	;

module_expr:
	  module_path
	| STRUCT structure_items OEND
	| FUNCTOR LPAREN IDENT COLON module_type RPAREN ARROW module_expr
	;

module_type:
	  SIG structure_items OEND
	| module_path
	;

module_path:
	  IDENT
	| module_path DOT IDENT
	;

/* Patterns */
pattern:
	  simple_pattern
		{ $$ = $1; }
	| pattern COLONCOLON pattern
		{ $$ = NULL; }
	| pattern COMMA pattern_comma_list
		{ $$ = NULL; }
	| pattern AS IDENT
		{ $$ = NULL; }
	| pattern PIPE pattern
		{ $$ = NULL; }
	;

pattern_comma_list:
	  pattern
		{ $$ = $1; }
	| pattern COMMA pattern_comma_list
		{ $$ = NULL; }
	;

simple_pattern:
	  IDENT
		{ $$ = NULL; }
	| UNDERSCORE
		{ $$ = NULL; }
	| constant
		{ $$ = $1; }
	| LPAREN pattern RPAREN
		{ $$ = $2; }
	| LBRACK pattern_list_opt RBRACK
		{ $$ = NULL; }
	| pattern_constructor
		{ $$ = NULL; }
	;

pattern_list_opt:
	  /* empty */
	| pattern_list
	;

pattern_list:
	  pattern
	| pattern SEMI pattern_list
	;

pattern_constructor:
	  IDENT simple_pattern
	;

/* Expressions */
expression:
	  simple_expr
		{ $$ = $1; }
	| LET let_binding IN expression
		{ $$ = NULL; }
	| LET REC let_binding IN expression
		{ $$ = NULL; }
	| FUN parameter_list ARROW expression
		{ $$ = NULL; }
	| FUNCTION match_cases
		{ $$ = NULL; }
	| IF expression THEN expression
		{ $$ = NULL; }
	| IF expression THEN expression ELSE expression
		{ $$ = NULL; }
	| MATCH expression WITH match_cases
		{ $$ = NULL; }
	| TRY expression WITH match_cases
		{ $$ = NULL; }
	| expression SEMI expression
		{ $$ = NULL; }
	| expression COMMA expr_comma_list
		{ $$ = NULL; }
	| expression COLONCOLON expression
		{ $$ = NULL; }
	| expression infix_op expression
		{ $$ = NULL; }
	| expression simple_expr
		{ $$ = NULL; }
	;

expr_comma_list:
	  expression
		{ $$ = $1; }
	| expression COMMA expr_comma_list
		{ $$ = NULL; }
	;

simple_expr:
	  constant
		{ $$ = $1; }
	| IDENT
		{ $$ = NULL; }
	| LPAREN expression RPAREN
		{ $$ = $2; }
	| LPAREN expression COLON type_expr RPAREN
		{ $$ = $2; }
	| OBEGIN expression OEND
		{ $$ = $2; }
	| LBRACK expr_list_opt RBRACK
		{ $$ = NULL; }
	| LBRACE record_expr RBRACE
		{ $$ = NULL; }
	| simple_expr DOT IDENT
		{ $$ = NULL; }
	| simple_expr LBRACK expression RBRACK
		{ $$ = NULL; }
	| prefix_op simple_expr
		{ $$ = NULL; }
	;

expr_list_opt:
	  /* empty */
	| expr_list
	;

expr_list:
	  expression
	| expression SEMI expr_list
	;

record_expr:
	  field_expr_list
	;

field_expr_list:
	  field_expr
	| field_expr SEMI field_expr_list
	;

field_expr:
	  IDENT EQ expression
	;

/* Match cases */
match_cases:
	  match_case
		{ $$ = $1; }
	| match_case PIPE match_cases
		{ $$ = $1; }
	;

match_case:
	  pattern ARROW expression
		{ $$ = NULL; }
	| pattern WHEN expression ARROW expression
		{ $$ = NULL; }
	;

/* Constants */
constant:
	  INT_LITERAL
		{ $$ = NULL; }
	| FLOAT_LITERAL
		{ $$ = NULL; }
	| CHAR_LITERAL
		{ $$ = NULL; }
	| STRING_LITERAL
		{ $$ = NULL; }
	| TRUE
		{ $$ = NULL; }
	| FALSE
		{ $$ = NULL; }
	| LPAREN RPAREN
		{ $$ = NULL; }
	| LBRACK RBRACK
		{ $$ = NULL; }
	;

/* Operators */
infix_op:
	  PLUS | MINUS | STAR | SLASH | PERCENT | STARSTAR
	| EQ | NE | LT | LE | GT | GE | EQEQ | BANGEQ
	| AMP | AMPAMP | PIPE | PIPEPIPE
	| AT | CARET | COLONCOLON
	;

prefix_op:
	  MINUS | BANG | TILDE
	;

%%

void
yyerror(const char *s)
{
	extern int lineno;
	extern char *ftitle;

	fprintf(stderr, "%s:%d: error: %s\n", ftitle, lineno, s);
	nerrors++;

	if (nerrors >= max_errors) {
		fprintf(stderr, "Too many errors, compilation aborted.\n");
		exit(1);
	}
}
