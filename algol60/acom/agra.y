/*
 * Copyright (c) 2025 PCC ALGOL 60+ Compiler
 *
 * Parser grammar for ALGOL 60+
 * This implements a subset of ALGOL 60 with some extensions
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

extern int yylex(void);
extern char *yytext;

%}

%union {
	int ival;
	double fval;
	char *sval;
	struct symtab *sym;
	struct tnode *type;
}

/* Keywords */
%token BEGIN_KW END_KW IF THEN ELSE FOR DO WHILE STEP UNTIL GOTO
%token INTEGER REAL BOOLEAN STRING ARRAY
%token VALUE OWN PROCEDURE LABEL SWITCH

/* Operators */
%token ASSIGN
%token AND OR NOT
%token LT LE EQ GE GT NE
%token PLUS MINUS MUL DIV POWER

/* Delimiters */
%token LPAREN RPAREN LBRACKET RBRACKET
%token COMMA SEMI COLON DOT

/* Constants and identifiers */
%token <ival> INT_CONST BOOL_CONST
%token <fval> REAL_CONST
%token <sval> IDENT STRING_CONST

/* Operator precedence (lowest to highest) */
%right ASSIGN
%left OR
%left AND
%left NOT
%left EQ NE LT LE GT GE
%left PLUS MINUS
%left MUL DIV
%right POWER
%right UMINUS

%type <type> type_specifier
%type <sym> declaration

%%

/* Top-level program */
program
	: block
		{
			printf("Program parsed successfully\n");
		}
	;

/* Block structure */
block
	: BEGIN_KW opt_declarations opt_statements END_KW
		{ }
	| statement
		{ }
	;

/* Optional declarations */
opt_declarations
	: /* empty */
	| declarations
	;

declarations
	: declaration
	| declarations declaration
	;

/* Declarations */
declaration
	: type_decl
	| proc_decl
	| label_decl
	;

/* Type declaration */
type_decl
	: type_specifier ident_list SEMI
		{
			/* Install all identifiers with this type */
		}
	| ARRAY IDENT LBRACKET bound_pair_list RBRACKET type_specifier SEMI
		{
			/* Array declaration */
		}
	| OWN type_specifier ident_list SEMI
		{
			/* Own (static) variable declaration */
		}
	;

type_specifier
	: INTEGER
		{ $$ = type_integer; }
	| REAL
		{ $$ = type_real; }
	| BOOLEAN
		{ $$ = type_boolean; }
	| STRING
		{ $$ = type_string; }
	;

ident_list
	: IDENT
	| ident_list COMMA IDENT
	;

bound_pair_list
	: bound_pair
	| bound_pair_list COMMA bound_pair
	;

bound_pair
	: expression COLON expression
	;

/* Procedure declaration */
proc_decl
	: PROCEDURE IDENT SEMI block SEMI
		{
			/* Simple procedure without parameters */
		}
	| PROCEDURE IDENT LPAREN formal_param_list RPAREN SEMI value_part block SEMI
		{
			/* Procedure with parameters */
		}
	;

formal_param_list
	: IDENT
	| formal_param_list COMMA IDENT
	;

value_part
	: /* empty */
	| VALUE ident_list SEMI
	;

/* Label declaration */
label_decl
	: LABEL ident_list SEMI
	;

/* Optional statements */
opt_statements
	: /* empty */
	| statements
	;

statements
	: statement
	| statements SEMI statement
	;

/* Statement */
statement
	: /* empty */
	| assignment_stmt
	| if_stmt
	| for_stmt
	| while_stmt
	| goto_stmt
	| proc_call
	| block
	| IDENT COLON statement
		{ /* Labeled statement */ }
	;

assignment_stmt
	: IDENT ASSIGN expression
		{
			/* Generate assignment code */
		}
	| IDENT LBRACKET subscript_list RBRACKET ASSIGN expression
		{
			/* Array assignment */
		}
	;

subscript_list
	: expression
	| subscript_list COMMA expression
	;

if_stmt
	: IF expression THEN statement
		{ }
	| IF expression THEN statement ELSE statement
		{ }
	;

for_stmt
	: FOR IDENT ASSIGN expression STEP expression UNTIL expression DO statement
		{ /* for-step-until loop */ }
	| FOR IDENT ASSIGN expression WHILE expression DO statement
		{ /* for-while loop */ }
	;

while_stmt
	: WHILE expression DO statement
		{ }
	;

goto_stmt
	: GOTO IDENT
		{ }
	;

proc_call
	: IDENT
		{
			/* Procedure call without parameters */
		}
	| IDENT LPAREN arg_list RPAREN
		{
			/* Procedure/function call with arguments */
		}
	;

arg_list
	: expression
	| arg_list COMMA expression
	;

/* Expressions */
expression
	: INT_CONST
		{ }
	| REAL_CONST
		{ }
	| BOOL_CONST
		{ }
	| STRING_CONST
		{ }
	| IDENT
		{ /* Variable reference */ }
	| IDENT LBRACKET subscript_list RBRACKET
		{ /* Array subscript */ }
	| IDENT LPAREN arg_list RPAREN
		{ /* Function call */ }
	| expression PLUS expression
		{ /* Addition */ }
	| expression MINUS expression
		{ /* Subtraction */ }
	| expression MUL expression
		{ /* Multiplication */ }
	| expression DIV expression
		{ /* Division */ }
	| expression POWER expression
		{ /* Exponentiation */ }
	| MINUS expression %prec UMINUS
		{ /* Unary minus */ }
	| expression EQ expression
		{ /* Equality */ }
	| expression NE expression
		{ /* Inequality */ }
	| expression LT expression
		{ /* Less than */ }
	| expression LE expression
		{ /* Less than or equal */ }
	| expression GT expression
		{ /* Greater than */ }
	| expression GE expression
		{ /* Greater than or equal */ }
	| expression AND expression
		{ /* Logical and */ }
	| expression OR expression
		{ /* Logical or */ }
	| NOT expression
		{ /* Logical not */ }
	| LPAREN expression RPAREN
		{ $$ = $2; }
	| IF expression THEN expression ELSE expression
		{ /* Conditional expression */ }
	;

%%

/* Error handler */
void
yyerror(const char *s)
{
	error("%s at or near '%s'", s, yytext);
}
