/*
 * Copyright (c) 2025 PCC PL/I Compiler
 *
 * Parser grammar for PL/I and dialects (PL/M, PL/C, etc.)
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Forward declarations */
int yylex(void);

%}

%union {
	int ival;
	double fval;
	char *sval;
	struct symtab *symptr;
	struct tnode *typeptr;
	struct param_list *paramptr;
	struct member_list *memberptr;
}

/* Keywords - Core PL/I */
%token ALLOCATE BBEGIN BY CALL CLOSE DECLARE DO ELSE END ENTRY EXIT
%token FFILE FORMAT FREE GET GO GOTO IF LEAVE ON OPEN PROCEDURE PUT
%token READ RETURN REVERT SELECT SIGNAL STOP THEN TO WAIT WHEN WHILE WRITE

/* Data types and attributes */
%token FIXED FLOAT BINARY DECIMAL BIT CHARACTER VARYING POINTER OFFSET
%token AREA LABEL FORMAT

/* Storage classes */
%token AUTOMATIC STATIC BASED CONTROLLED DEFINED

/* Attributes */
%token ALIGNED UNALIGNED SIGNED UNSIGNED INITIAL EXTERNAL INTERNAL
%token BUILTIN GENERIC RECURSIVE REENTRANT OPTIONS MAIN RETURNS

/* I/O keywords */
%token STREAM RECORD INPUT OUTPUT UPDATE SEQUENTIAL DIRECT KEYED
%token BACKWARDS ENVIRONMENT

/* PL/M specific */
%token ADDRESS AT BYTE WORD DWORD DATA INTERRUPT LITERALLY PUBLIC
%token REAL INTEGER

/* Control flow */
%token CASE OTHERWISE ITERATE

/* Preprocessor */
%token PP_ACTIVATE PP_DECLARE PP_DEACTIVATE PP_INCLUDE PP_REPLACE

/* Operators */
%token ASSIGN EQ NE LT LE GT GE ARROW CONCAT POWER
%token PLUS MINUS STAR DIV AND OR NOT
%token LPAREN RPAREN LBRACK RBRACK COMMA SEMI COLON DOT

/* Literals and identifiers */
%token <ival> INTEGER_CONST
%token <fval> FLOAT_CONST
%token <sval> STRING_LITERAL BIT_LITERAL
%token <sval> IDENT

/* Type declarations */
%type <typeptr> type_spec data_type array_spec structure_spec
%type <symptr> identifier
%type <paramptr> parameter_list parameter
%type <memberptr> member_list member_decl

/* Precedence and associativity (lowest to highest) */
%right ASSIGN
%left OR
%left AND
%left NOT
%left EQ NE
%left LT LE GT GE
%left CONCAT
%left PLUS MINUS
%left STAR DIV
%right POWER
%left DOT ARROW LBRACK

%%

/* Program structure */
program:
	  procedure_list
		{ /* Complete PL/I program */ }
	| plm_module
		{ /* PL/M module */ }
	;

plm_module:
	  IDENT COLON DO SEMI statement_list END IDENT SEMI
		{ /* PL/M module format */
		  if (strcmp($1, $7) != 0) {
			  error("module name mismatch: '%s' vs '%s'", $1, $7);
		  }
		}
	;

procedure_list:
	  procedure
	| procedure_list procedure
	;

/* Procedure declaration */
procedure:
	  procedure_header SEMI procedure_body END identifier_opt SEMI
		{ /* Standard procedure */
		  exit_scope();
		}
	| entry_statement
	;

procedure_header:
	  identifier COLON PROCEDURE options_clause_opt
		{ enter_scope();
		  SYMTAB *sp = install($1->sname, PROC, blevel);
		  sp->sflags |= SEXTERNAL;
		}
	| identifier COLON PROCEDURE LPAREN parameter_list RPAREN options_clause_opt
		{ enter_scope();
		  SYMTAB *sp = install($1->sname, PROC, blevel);
		  /* Handle parameters */
		}
	| identifier COLON PROCEDURE LPAREN parameter_list RPAREN RETURNS LPAREN type_spec RPAREN options_clause_opt
		{ enter_scope();
		  SYMTAB *sp = install($1->sname, FUNC, blevel);
		  sp->stype = $9;
		}
	;

options_clause_opt:
	  /* empty */
	| OPTIONS LPAREN option_list RPAREN
	;

option_list:
	  option
	| option_list COMMA option
	;

option:
	  MAIN
		{ /* MAIN option */ }
	| REENTRANT
		{ /* REENTRANT option */ }
	| RECURSIVE
		{ /* RECURSIVE option */ }
	;

parameter_list:
	  parameter
		{ $$ = $1; }
	| parameter_list COMMA parameter
		{ $3->pnext = $1; $$ = $3; }
	;

parameter:
	  identifier
		{ $$ = malloc(sizeof(PARAM_LIST));
		  $$->pname = $1->sname;
		  $$->ptype = NULL;
		  $$->pflags = 0;
		  $$->pnext = NULL;
		}
	| STAR identifier
		{ /* Pointer parameter (PL/M) */
		  $$ = malloc(sizeof(PARAM_LIST));
		  $$->pname = $2->sname;
		  $$->ptype = NULL;
		  $$->pflags = PBYREF;
		  $$->pnext = NULL;
		}
	;

procedure_body:
	  declarations_opt statement_list
	;

declarations_opt:
	  /* empty */
	| declarations
	;

declarations:
	  declaration
	| declarations declaration
	;

/* Declaration statement */
declaration:
	  DECLARE declare_list SEMI
	| dcl_statement
	| plm_declare
	;

dcl_statement:
	  DECLARE LPAREN declare_list RPAREN SEMI
		{ /* Factored DECLARE */ }
	;

plm_declare:
	  DECLARE identifier type_spec plm_init_opt attribute_list_opt SEMI
		{ /* PL/M style declare */
		  SYMTAB *sp = install($2->sname, AUTO, blevel);
		  sp->stype = $3;
		}
	| DECLARE identifier LITERALLY STRING_LITERAL SEMI
		{ /* PL/M LITERALLY (macro) */
		  if (!ALLOW_LITERALLY()) {
			  error("LITERALLY not allowed in this dialect");
		  }
		  /* Handle as macro */
		}
	;

plm_init_opt:
	  /* empty */
	| DATA LPAREN init_list RPAREN
		{ /* PL/M DATA clause */ }
	| AT LPAREN INTEGER_CONST RPAREN
		{ /* PL/M AT clause */ }
	;

declare_list:
	  declare_item
	| declare_list COMMA declare_item
	;

declare_item:
	  identifier type_spec attribute_list_opt
		{ SYMTAB *sp = install($1->sname, AUTO, blevel);
		  sp->stype = $2;
		}
	| identifier type_spec attribute_list_opt INITIAL LPAREN init_list RPAREN
		{ SYMTAB *sp = install($1->sname, AUTO, blevel);
		  sp->stype = $2;
		  /* Handle initializer */
		}
	| LPAREN identifier_list RPAREN type_spec attribute_list_opt
		{ /* Multiple identifiers with same type */ }
	;

type_spec:
	  data_type
	| array_spec
	| structure_spec
	| POINTER
		{ $$ = mktype(TPOINTER); }
	| LABEL
		{ $$ = mktype(TLABEL); }
	| ENTRY
		{ $$ = mktype(TENTRY); }
	| FILE
		{ $$ = mktype(TFILE); }
	;

data_type:
	  FIXED precision_opt scale_opt base_opt
		{ $$ = mktype(TFIXED); }
	| FLOAT precision_opt base_opt
		{ $$ = mktype(TFLOAT); }
	| BIT LPAREN INTEGER_CONST RPAREN varying_opt
		{ $$ = mkbit($3); }
	| CHARACTER LPAREN INTEGER_CONST RPAREN varying_opt
		{ $$ = mkchar($3, 0); }
	| BYTE
		{ $$ = mktype(TBYTE); }
	| WORD
		{ $$ = mktype(TWORD); }
	| DWORD
		{ $$ = mktype(TDWORD); }
	| ADDRESS
		{ $$ = mktype(TADDRESS); }
	| INTEGER
		{ $$ = mktype(TINTEGER); }
	| REAL
		{ $$ = mktype(TREAL); }
	;

precision_opt:
	  /* empty */
	| LPAREN INTEGER_CONST RPAREN
	;

scale_opt:
	  /* empty */
	| COMMA INTEGER_CONST
	;

base_opt:
	  /* empty */
	| BINARY
	| DECIMAL
	;

varying_opt:
	  /* empty */
	| VARYING
	;

array_spec:
	  LPAREN bound_list RPAREN type_spec
		{ /* Array specification */ }
	;

bound_list:
	  bound
	| bound_list COMMA bound
	;

bound:
	  INTEGER_CONST
		{ /* Single bound (1:n) */ }
	| INTEGER_CONST COLON INTEGER_CONST
		{ /* Lower:upper bound */ }
	| STAR
		{ /* Adjustable extent */ }
	;

structure_spec:
	  INTEGER member_list
		{ /* Structure with level number */ }
	;

member_list:
	  member_decl
		{ $$ = $1; }
	| member_list COMMA member_decl
		{ $3->mnext = $1; $$ = $3; }
	;

member_decl:
	  INTEGER identifier type_spec
		{ $$ = malloc(sizeof(MEMBER_LIST));
		  $$->mname = $2->sname;
		  $$->mtype = $3;
		  $$->mlevel = $1;
		  $$->mnext = NULL;
		}
	;

attribute_list_opt:
	  /* empty */
	| attribute_list
	;

attribute_list:
	  attribute
	| attribute_list attribute
	;

attribute:
	  STATIC
	| AUTOMATIC
	| BASED
	| CONTROLLED
	| DEFINED
	| ALIGNED
	| UNALIGNED
	| SIGNED
	| UNSIGNED
	| EXTERNAL
	| INTERNAL
	| BUILTIN
	| GENERIC
	| RECURSIVE
	| REENTRANT
	| PUBLIC
		{ /* PL/M PUBLIC */ }
	;

init_list:
	  initializer
	| init_list COMMA initializer
	;

initializer:
	  constant
	| LPAREN init_list RPAREN
	;

/* Entry statement */
entry_statement:
	  ENTRY identifier LPAREN parameter_list RPAREN SEMI
		{ /* Entry point */ }
	;

/* Statement list */
statement_list:
	  statement
	| statement_list statement
	;

statement:
	  assignment_stmt
	| call_stmt
	| do_stmt
	| if_stmt
	| select_stmt
	| goto_stmt
	| return_stmt
	| exit_stmt
	| stop_stmt
	| io_stmt
	| allocation_stmt
	| on_stmt
	| null_stmt
	| labeled_stmt
	| SEMI
		{ /* Empty statement */ }
	;

labeled_stmt:
	  identifier COLON statement
		{ /* Labeled statement */ }
	;

null_stmt:
	  SEMI
	;

assignment_stmt:
	  expression ASSIGN expression SEMI
		{ /* Assignment */ }
	;

call_stmt:
	  CALL identifier LPAREN argument_list_opt RPAREN SEMI
		{ /* Procedure call */ }
	| CALL identifier SEMI
		{ /* Call with no arguments */ }
	;

argument_list_opt:
	  /* empty */
	| argument_list
	;

argument_list:
	  expression
	| argument_list COMMA expression
	;

do_stmt:
	  DO SEMI statement_list END SEMI
		{ /* Simple DO block */ }
	| DO identifier ASSIGN expression TO expression by_clause_opt SEMI statement_list END SEMI
		{ /* DO loop */ }
	| DO WHILE LPAREN expression RPAREN SEMI statement_list END SEMI
		{ /* DO WHILE */ }
	| DO CASE expression SEMI case_list END SEMI
		{ /* DO CASE (PL/M) */ }
	;

by_clause_opt:
	  /* empty */
	| BY expression
	;

case_list:
	  statement
	| case_list SEMI statement
	;

if_stmt:
	  IF expression THEN statement
		{ /* IF without ELSE */ }
	| IF expression THEN statement ELSE statement
		{ /* IF with ELSE */ }
	;

select_stmt:
	  SELECT SEMI when_list otherwise_opt END SEMI
		{ /* SELECT statement */ }
	;

when_list:
	  when_clause
	| when_list when_clause
	;

when_clause:
	  WHEN LPAREN expression RPAREN statement
	;

otherwise_opt:
	  /* empty */
	| OTHERWISE statement
	;

goto_stmt:
	  GO TO identifier SEMI
	| GOTO identifier SEMI
	;

return_stmt:
	  RETURN SEMI
		{ /* RETURN without value */ }
	| RETURN LPAREN expression RPAREN SEMI
		{ /* RETURN with value */ }
	;

exit_stmt:
	  EXIT SEMI
	;

stop_stmt:
	  STOP SEMI
	;

io_stmt:
	  GET file_opt data_list SEMI
	| PUT file_opt data_list SEMI
	| READ file_spec INTO LPAREN identifier RPAREN SEMI
	| WRITE file_spec FROM LPAREN identifier RPAREN SEMI
	| OPEN FILE LPAREN identifier RPAREN file_attributes_opt SEMI
	| CLOSE FILE LPAREN identifier RPAREN SEMI
	;

file_opt:
	  /* empty */
	| FILE LPAREN identifier RPAREN
	;

file_spec:
	  FILE LPAREN identifier RPAREN
	;

file_attributes_opt:
	  /* empty */
	| file_attributes
	;

file_attributes:
	  file_attribute
	| file_attributes file_attribute
	;

file_attribute:
	  STREAM
	| RECORD
	| INPUT
	| OUTPUT
	| UPDATE
	| SEQUENTIAL
	| DIRECT
	| KEYED
	;

data_list:
	  data_item
	| data_list COMMA data_item
	;

data_item:
	  expression
	| STRING_LITERAL
	;

allocation_stmt:
	  ALLOCATE identifier SEMI
	| FREE identifier SEMI
	;

on_stmt:
	  ON condition on_action SEMI
		{ /* ON condition handler */ }
	;

condition:
	  identifier
		{ /* Named condition */ }
	;

on_action:
	  statement
	| BBEGIN statement_list END
	;

/* Expressions */
expression:
	  primary_expr
	| expression PLUS expression
		{ /* Addition */ }
	| expression MINUS expression
		{ /* Subtraction */ }
	| expression STAR expression
		{ /* Multiplication */ }
	| expression DIV expression
		{ /* Division */ }
	| expression POWER expression
		{ /* Exponentiation */ }
	| expression CONCAT expression
		{ /* Concatenation */ }
	| expression AND expression
		{ /* Logical AND */ }
	| expression OR expression
		{ /* Logical OR */ }
	| NOT expression
		{ /* Logical NOT */ }
	| expression EQ expression
		{ /* Equal */ }
	| expression NE expression
		{ /* Not equal */ }
	| expression LT expression
		{ /* Less than */ }
	| expression LE expression
		{ /* Less or equal */ }
	| expression GT expression
		{ /* Greater than */ }
	| expression GE expression
		{ /* Greater or equal */ }
	| MINUS expression %prec NOT
		{ /* Unary minus */ }
	| PLUS expression %prec NOT
		{ /* Unary plus */ }
	| LPAREN expression RPAREN
		{ /* Parenthesized expression */ }
	;

primary_expr:
	  identifier
	| constant
	| function_call
	| array_ref
	| member_ref
	;

function_call:
	  identifier LPAREN argument_list_opt RPAREN
		{ /* Function call */ }
	;

array_ref:
	  identifier LPAREN expression_list RPAREN
		{ /* Array subscript */ }
	;

member_ref:
	  expression DOT identifier
		{ /* Structure member access */ }
	| expression ARROW identifier
		{ /* Pointer dereference and member */ }
	;

expression_list:
	  expression
	| expression_list COMMA expression
	;

constant:
	  INTEGER_CONST
		{ /* Integer constant */ }
	| FLOAT_CONST
		{ /* Float constant */ }
	| STRING_LITERAL
		{ /* String constant */ }
	| BIT_LITERAL
		{ /* Bit string constant */ }
	;

/* Utility rules */
identifier_list:
	  identifier
	| identifier_list COMMA identifier
	;

identifier:
	  IDENT
		{ $$ = find_symbol($1);
		  if ($$ == NULL) {
			  $$ = install($1, SNULL, blevel);
		  }
		}
	;

identifier_opt:
	  /* empty */
	| identifier
	;

%%

/* Additional C code */
