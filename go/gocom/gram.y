/*
 * Copyright (c) 2025 PCC Go Compiler
 *
 * Parser grammar for Go
 * Based on Go Language Specification
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
	int ival;
	double dval;
	char *sval;
	struct symtab *symptr;
	struct tnode *typeptr;
	struct param_list *paramptr;
	struct field_list *fieldptr;
}

/* Keywords */
%token BREAK CASE CHAN CONST CONTINUE DEFAULT DEFER ELSE FALLTHROUGH
%token FOR FUNC GO GOTO IF IMPORT INTERFACE MAP PACKAGE RANGE RETURN
%token SELECT STRUCT SWITCH TYPE VAR

/* Operators */
%token PLUS MINUS STAR SLASH PERCENT
%token AMP PIPE CARET SHL SHR ANDNOT
%token ADDASSIGN SUBASSIGN MULASSIGN DIVASSIGN MODASSIGN
%token ANDASSIGN ORASSIGN XORASSIGN SHLASSIGN SHRASSIGN ANDNOTASSIGN
%token LAND LOR ARROW INC DEC
%token EQ LT GT ASSIGN NOT
%token NE LE GE DEFINE ELLIPSIS

/* Delimiters */
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token COMMA DOT SEMICOLON COLON

/* Literals */
%token <ival> INTLIT
%token <dval> FLOATLIT
%token <ival> RUNELIT
%token <sval> STRINGLIT
%token <sval> IDENT

/* Types */
%type <typeptr> type type_lit array_type struct_type pointer_type
%type <typeptr> function_type interface_type slice_type map_type channel_type
%type <fieldptr> field_decl field_decl_list
%type <paramptr> parameter_decl parameter_list

/* Precedence (lowest to highest) */
%left LOR
%left LAND
%left EQ NE LT LE GT GE
%left PIPE CARET
%left AMP ANDNOT
%left SHL SHR
%left PLUS MINUS
%left STAR SLASH PERCENT
%right NOT
%left DOT LBRACK LPAREN

%%

/* Source file */
source_file:
	  package_clause SEMICOLON import_decl_list top_level_decl_list
		{ /* Complete source file */ }
	;

/* Package clause */
package_clause:
	  PACKAGE IDENT
		{
			package_name = $2;
		}
	;

/* Import declarations */
import_decl_list:
	  /* empty */
	| import_decl_list import_decl SEMICOLON
	;

import_decl:
	  IMPORT import_spec
	| IMPORT LPAREN import_spec_list RPAREN
	;

import_spec_list:
	  /* empty */
	| import_spec_list import_spec SEMICOLON
	;

import_spec:
	  STRINGLIT
		{ /* import "package" */ }
	| IDENT STRINGLIT
		{ /* import name "package" */ }
	| DOT STRINGLIT
		{ /* import . "package" */ }
	;

/* Top-level declarations */
top_level_decl_list:
	  /* empty */
	| top_level_decl_list top_level_decl SEMICOLON
	;

top_level_decl:
	  declaration
	| function_decl
	| method_decl
	;

declaration:
	  const_decl
	| type_decl
	| var_decl
	;

/* Const declarations */
const_decl:
	  CONST const_spec
	| CONST LPAREN const_spec_list RPAREN
	;

const_spec_list:
	  /* empty */
	| const_spec_list const_spec SEMICOLON
	;

const_spec:
	  identifier_list
		{ /* Iota-based constants */ }
	| identifier_list ASSIGN expression_list
		{ /* Explicit constants */ }
	| identifier_list type ASSIGN expression_list
		{ /* Typed constants */ }
	;

/* Type declarations */
type_decl:
	  TYPE type_spec
	| TYPE LPAREN type_spec_list RPAREN
	;

type_spec_list:
	  /* empty */
	| type_spec_list type_spec SEMICOLON
	;

type_spec:
	  IDENT type
		{
			SYMTAB *sp = install($1, TYPENAME, blevel);
			if (sp != NULL)
				sp->stype = $2;
		}
	;

/* Var declarations */
var_decl:
	  VAR var_spec
	| VAR LPAREN var_spec_list RPAREN
	;

var_spec_list:
	  /* empty */
	| var_spec_list var_spec SEMICOLON
	;

var_spec:
	  identifier_list type
		{
			/* var x, y int */
		}
	| identifier_list type ASSIGN expression_list
		{
			/* var x, y int = 1, 2 */
		}
	| identifier_list ASSIGN expression_list
		{
			/* var x, y = 1, 2 */
		}
	;

/* Short variable declaration */
short_var_decl:
	  identifier_list DEFINE expression_list
		{
			/* x := 1 */
		}
	;

/* Function declaration */
function_decl:
	  FUNC IDENT signature
		{
			SYMTAB *sp = install($2, FUNC, blevel);
			if (sp != NULL)
				sp->stype = $3;
		}
	| FUNC IDENT signature function_body
		{
			SYMTAB *sp = install($2, FUNC, blevel);
			if (sp != NULL)
				sp->stype = $3;
		}
	;

/* Method declaration */
method_decl:
	  FUNC receiver IDENT signature
		{
			/* Method with receiver */
		}
	| FUNC receiver IDENT signature function_body
		{
			/* Method implementation */
		}
	;

receiver:
	  LPAREN IDENT type RPAREN
		{
			/* (r Type) */
		}
	| LPAREN STAR IDENT type RPAREN
		{
			/* (*r Type) - pointer receiver */
		}
	;

/* Function signature */
signature:
	  parameters
		{
			$$ = mkfunction(NULL, $1, NULL);
		}
	| parameters result
		{
			$$ = mkfunction($2, $1, NULL);
		}
	;

result:
	  parameters
		{ $$ = NULL; /* TODO: Handle multiple returns */ }
	| type
		{ $$ = $1; }
	;

parameters:
	  LPAREN RPAREN
		{ $$ = NULL; }
	| LPAREN parameter_list RPAREN
		{ $$ = $2; }
	| LPAREN parameter_list COMMA RPAREN
		{ $$ = $2; }
	;

parameter_list:
	  parameter_decl
		{ $$ = $1; }
	| parameter_list COMMA parameter_decl
		{
			/* Link parameters */
			PARAM_LIST *p = $1;
			while (p->pnext != NULL)
				p = p->pnext;
			p->pnext = $3;
			$$ = $1;
		}
	;

parameter_decl:
	  IDENT type
		{
			$$ = (PARAM_LIST *)xcalloc(1, sizeof(PARAM_LIST));
			$$->pname = $1;
			$$->ptype = $2;
			$$->pnext = NULL;
		}
	| type
		{
			$$ = (PARAM_LIST *)xcalloc(1, sizeof(PARAM_LIST));
			$$->pname = NULL;
			$$->ptype = $1;
			$$->pnext = NULL;
		}
	| IDENT ELLIPSIS type
		{
			/* Variadic parameter */
			$$ = (PARAM_LIST *)xcalloc(1, sizeof(PARAM_LIST));
			$$->pname = $1;
			$$->ptype = mkslice($3);
			$$->pflags = 1;  /* Variadic */
			$$->pnext = NULL;
		}
	;

function_body:
	  block
	;

/* Types */
type:
	  type_lit
		{ $$ = $1; }
	| IDENT
		{
			/* Named type - look up in symbol table */
			SYMTAB *sp = find_symbol($1);
			if (sp != NULL && sp->sclass == TYPENAME)
				$$ = sp->stype;
			else {
				error("undefined type '%s'", $1);
				$$ = mktype(TINT);
			}
		}
	| LPAREN type RPAREN
		{ $$ = $2; }
	;

type_lit:
	  array_type      { $$ = $1; }
	| struct_type    { $$ = $1; }
	| pointer_type   { $$ = $1; }
	| function_type  { $$ = $1; }
	| interface_type { $$ = $1; }
	| slice_type     { $$ = $1; }
	| map_type       { $$ = $1; }
	| channel_type   { $$ = $1; }
	;

array_type:
	  LBRACK INTLIT RBRACK type
		{
			$$ = mkarray($4, $2);
		}
	;

slice_type:
	  LBRACK RBRACK type
		{
			$$ = mkslice($3);
		}
	;

struct_type:
	  STRUCT LBRACE RBRACE
		{
			$$ = mkstruct(NULL, NULL);
		}
	| STRUCT LBRACE field_decl_list RBRACE
		{
			$$ = mkstruct(NULL, $3);
		}
	| STRUCT LBRACE field_decl_list SEMICOLON RBRACE
		{
			$$ = mkstruct(NULL, $3);
		}
	;

field_decl_list:
	  field_decl
		{ $$ = $1; }
	| field_decl_list SEMICOLON field_decl
		{
			/* Link fields */
			FIELD_LIST *f = $1;
			while (f->fnext != NULL)
				f = f->fnext;
			f->fnext = $3;
			$$ = $1;
		}
	;

field_decl:
	  identifier_list type
		{
			/* TODO: Handle multiple fields */
			$$ = (FIELD_LIST *)xcalloc(1, sizeof(FIELD_LIST));
			$$->fname = NULL;  /* Will be set from identifier_list */
			$$->ftype = $2;
			$$->fnext = NULL;
		}
	| identifier_list type STRINGLIT
		{
			/* Field with tag */
			$$ = (FIELD_LIST *)xcalloc(1, sizeof(FIELD_LIST));
			$$->fname = NULL;
			$$->ftype = $2;
			$$->tag = $3;
			$$->fnext = NULL;
		}
	;

pointer_type:
	  STAR type
		{
			$$ = mkpointer($2);
		}
	;

function_type:
	  FUNC signature
		{
			$$ = $2;
		}
	;

interface_type:
	  INTERFACE LBRACE RBRACE
		{
			$$ = mkinterface(NULL, NULL);
		}
	| INTERFACE LBRACE method_spec_list RBRACE
		{
			$$ = mkinterface(NULL, NULL);  /* TODO: method list */
		}
	;

method_spec_list:
	  method_spec
	| method_spec_list SEMICOLON method_spec
	;

method_spec:
	  IDENT signature
		{ /* Method specification */ }
	;

map_type:
	  MAP LBRACK type RBRACK type
		{
			$$ = mkmap($3, $5);
		}
	;

channel_type:
	  CHAN type
		{
			$$ = mkchan($2, 0);  /* Bidirectional */
		}
	| CHAN ARROW type
		{
			$$ = mkchan($3, 1);  /* Send-only */
		}
	| ARROW CHAN type
		{
			$$ = mkchan($3, 2);  /* Receive-only */
		}
	;

/* Statements */
block:
	  LBRACE statement_list RBRACE
	;

statement_list:
	  /* empty */
	| statement_list statement SEMICOLON
	;

statement:
	  declaration
	| simple_stmt
	| return_stmt
	| break_stmt
	| continue_stmt
	| goto_stmt
	| fallthrough_stmt
	| block
	| if_stmt
	| switch_stmt
	| for_stmt
	| defer_stmt
	| go_stmt
	;

simple_stmt:
	  expression
	| assignment_stmt
	| short_var_decl
	| inc_dec_stmt
	;

assignment_stmt:
	  expression_list ASSIGN expression_list
		{ /* x = y */ }
	| expression_list ADDASSIGN expression_list
		{ /* x += y */ }
	/* TODO: Other assignment operators */
	;

inc_dec_stmt:
	  expression INC
		{ /* x++ */ }
	| expression DEC
		{ /* x-- */ }
	;

return_stmt:
	  RETURN
		{ /* return */ }
	| RETURN expression_list
		{ /* return x, y */ }
	;

break_stmt:
	  BREAK
		{ /* break */ }
	| BREAK IDENT
		{ /* break label */ }
	;

continue_stmt:
	  CONTINUE
		{ /* continue */ }
	| CONTINUE IDENT
		{ /* continue label */ }
	;

goto_stmt:
	  GOTO IDENT
		{ /* goto label */ }
	;

fallthrough_stmt:
	  FALLTHROUGH
		{ /* fallthrough */ }
	;

if_stmt:
	  IF expression block
		{ /* if x { } */ }
	| IF expression block ELSE if_stmt
		{ /* if x { } else if y { } */ }
	| IF expression block ELSE block
		{ /* if x { } else { } */ }
	| IF simple_stmt SEMICOLON expression block
		{ /* if init; cond { } */ }
	| IF simple_stmt SEMICOLON expression block ELSE if_stmt
		{ /* if init; cond { } else if */ }
	| IF simple_stmt SEMICOLON expression block ELSE block
		{ /* if init; cond { } else { } */ }
	;

switch_stmt:
	  SWITCH block
		{ /* switch { case: ... } */ }
	| SWITCH expression block
		{ /* switch x { case: ... } */ }
	| SWITCH simple_stmt SEMICOLON block
		{ /* switch init; { case: ... } */ }
	| SWITCH simple_stmt SEMICOLON expression block
		{ /* switch init; x { case: ... } */ }
	;

for_stmt:
	  FOR block
		{ /* for { } - infinite loop */ }
	| FOR expression block
		{ /* for x < 10 { } */ }
	| FOR simple_stmt SEMICOLON expression SEMICOLON simple_stmt block
		{ /* for i := 0; i < 10; i++ { } */ }
	| FOR RANGE expression block
		{ /* for range x { } */ }
	| FOR expression_list ASSIGN RANGE expression block
		{ /* for i, v = range x { } */ }
	| FOR identifier_list DEFINE RANGE expression block
		{ /* for i, v := range x { } */ }
	;

defer_stmt:
	  DEFER expression
		{ /* defer f() */ }
	;

go_stmt:
	  GO expression
		{ /* go f() */ }
	;

/* Expressions */
expression:
	  primary_expr
	| unary_expr
	| binary_expr
	;

primary_expr:
	  operand
	| primary_expr selector
	| primary_expr index
	| primary_expr slice
	| primary_expr call
	;

operand:
	  literal
	| IDENT
		{ /* Identifier reference */ }
	| LPAREN expression RPAREN
		{ $$ = $2; }
	;

literal:
	  INTLIT
		{ /* Integer literal */ }
	| FLOATLIT
		{ /* Float literal */ }
	| RUNELIT
		{ /* Rune literal */ }
	| STRINGLIT
		{ /* String literal */ }
	;

selector:
	  DOT IDENT
		{ /* x.field */ }
	;

index:
	  LBRACK expression RBRACK
		{ /* x[i] */ }
	;

slice:
	  LBRACK expression COLON expression RBRACK
		{ /* x[low:high] */ }
	| LBRACK expression COLON RBRACK
		{ /* x[low:] */ }
	| LBRACK COLON expression RBRACK
		{ /* x[:high] */ }
	| LBRACK COLON RBRACK
		{ /* x[:] */ }
	;

call:
	  LPAREN RPAREN
		{ /* f() */ }
	| LPAREN expression_list RPAREN
		{ /* f(x, y) */ }
	| LPAREN expression_list COMMA RPAREN
		{ /* f(x, y,) */ }
	;

unary_expr:
	  PLUS expression
		{ /* +x */ }
	| MINUS expression
		{ /* -x */ }
	| NOT expression
		{ /* !x */ }
	| CARET expression
		{ /* ^x */ }
	| STAR expression
		{ /* *x (pointer dereference) */ }
	| AMP expression
		{ /* &x (address-of) */ }
	| ARROW expression
		{ /* <-x (channel receive) */ }
	;

binary_expr:
	  expression LOR expression
		{ /* x || y */ }
	| expression LAND expression
		{ /* x && y */ }
	| expression EQ expression
		{ /* x == y */ }
	| expression NE expression
		{ /* x != y */ }
	| expression LT expression
		{ /* x < y */ }
	| expression LE expression
		{ /* x <= y */ }
	| expression GT expression
		{ /* x > y */ }
	| expression GE expression
		{ /* x >= y */ }
	| expression PLUS expression
		{ /* x + y */ }
	| expression MINUS expression
		{ /* x - y */ }
	| expression PIPE expression
		{ /* x | y */ }
	| expression CARET expression
		{ /* x ^ y */ }
	| expression STAR expression
		{ /* x * y */ }
	| expression SLASH expression
		{ /* x / y */ }
	| expression PERCENT expression
		{ /* x % y */ }
	| expression SHL expression
		{ /* x << y */ }
	| expression SHR expression
		{ /* x >> y */ }
	| expression AMP expression
		{ /* x & y */ }
	| expression ANDNOT expression
		{ /* x &^ y */ }
	;

/* Lists */
identifier_list:
	  IDENT
		{ /* Single identifier */ }
	| identifier_list COMMA IDENT
		{ /* Multiple identifiers */ }
	;

expression_list:
	  expression
		{ /* Single expression */ }
	| expression_list COMMA expression
		{ /* Multiple expressions */ }
	;

%%

void
yyerror(const char *s)
{
	error("%s", s);
}
