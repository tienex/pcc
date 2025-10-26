/*
 * Copyright (c) 2025 PCC CHILL Compiler
 *
 * Parser grammar for CHILL
 * CCITT High Level Language (Z.200)
 * Minimal implementation
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
	int64_t ival;
	double dval;
	char cval;
	char *sval;
	struct symtab *sym;
	struct mode_def *mode;
}

/* Tokens */
%token <sval> IDENT STRINGLIT
%token <ival> INTLIT
%token <dval> REALLIT
%token <cval> CHARLIT

/* Keywords */
%token ACCESS AND ARRAY ASSERT AT BASED BBEGIN BIN BOOL BY
%token CALL CASE CAUSE CHAR CHARS CONTEXT CONTINUE CYCLE
%token DCL DELAY DO DOWN DURATION DYNAMIC
%token ELSE ELSIF END ESAC EVENT EXCEPTIONS EXIT
%token FI FOR FORBID GENERAL GOTO GRANT
%token IF IN INIT INLINE INOUT INT
%token LOC MOD MODULE NEWMODE NONREF NOT
%token OD OF ON OR OUT
%token POWERSET PRIORITY PROC PROCESS
%token READ REAL RECEIVE RECURSIVE REF REGION REM REMOTE
%token RESULT RETURN RETURNS
%token SEIZE SEND SET SIGNAL SIMPLE SPEC START STATIC STEP STOP STRUCT
%token SYN SYNMODE
%token TEXT THEN THIS TIMEOUT TIME TO TYPE
%token UP
%token WHILE WITH
%token XOR

/* Predefined types and constants */
%token BYTE UBYTE SHORT USHORT LONG ULONG
%token CTRUE CFALSE CNULL

/* Operators */
%token ASSIGN EQ NE LT LE GT GE
%token PLUS MINUS MULT DIV CONCAT
%token ARROW

/* Type specifications */
%type <mode> mode_spec
%type <sym> name_decl

/* Operator precedence and associativity */
%left OR XOR
%left AND
%nonassoc EQ NE LT LE GT GE IN
%left PLUS MINUS CONCAT
%left MULT DIV MOD REM
%right NOT
%left '(' ')' '[' ']' '.' ARROW

%%

/* Top-level structure */
program:
	  module
	| /* empty */
	;

/* Module definition */
module:
	  MODULE IDENT ';' opt_spec_part opt_body END ';'
		{
			if (module_name) free(module_name);
			module_name = strdup($2);
			if (verbose) printf("Module: %s\n", $2);
		}
	;

opt_spec_part:
	  SPEC module_spec_list
	| /* empty */
	;

module_spec_list:
	  module_spec
	| module_spec_list module_spec
	;

module_spec:
	  declaration
	| grant_statement
	;

opt_body:
	  body
	| /* empty */
	;

body:
	  statement_list
	;

/* Declarations */
declaration:
	  DCL name_decl_list mode_spec opt_init ';'
		{ if (verbose) printf("Declaration\n"); }
	| NEWMODE IDENT '=' mode_spec ';'
		{ if (verbose) printf("Newmode: %s\n", $2); }
	| SYN IDENT '=' IDENT ';'
		{ if (verbose) printf("Synonym: %s = %s\n", $2, $4); }
	| proc_decl
	| process_decl
	;

name_decl_list:
	  name_decl
	| name_decl_list ',' name_decl
	;

name_decl:
	  IDENT
		{
			$$ = install($1, SAUTO, blevel);
			if (verbose) printf("Name: %s\n", $1);
		}
	;

/* Mode (type) specifications */
mode_spec:
	  BOOL
		{ $$ = mode_bool; }
	| CHAR
		{ $$ = mode_char; }
	| INT
		{ $$ = mode_int; }
	| REAL
		{ $$ = mode_real; }
	| IDENT
		{ $$ = NULL; /* Look up mode */ }
	| REF mode_spec
		{ $$ = mkmode_ref($2); }
	| ARRAY '[' index_mode_list ']' mode_spec
		{ $$ = NULL; /* Array mode */ }
	| STRUCT '(' field_list ')'
		{ $$ = NULL; /* Struct mode */ }
	| POWERSET mode_spec
		{ $$ = NULL; /* Powerset mode */ }
	;

index_mode_list:
	  mode_spec
	| index_mode_list ',' mode_spec
	;

field_list:
	  field_decl
	| field_list ',' field_decl
	;

field_decl:
	  IDENT mode_spec
	;

opt_init:
	  '=' expression
	| /* empty */
	;

/* Procedure declaration */
proc_decl:
	  PROC IDENT '(' opt_formal_params ')' opt_returns ';' opt_body END ';'
		{ if (verbose) printf("Procedure: %s\n", $2); }
	;

/* Process declaration */
process_decl:
	  PROCESS IDENT '(' opt_formal_params ')' ';' opt_body END ';'
		{ if (verbose) printf("Process: %s\n", $2); }
	;

opt_formal_params:
	  formal_param_list
	| /* empty */
	;

formal_param_list:
	  formal_param
	| formal_param_list ',' formal_param
	;

formal_param:
	  IDENT mode_spec
	| LOC IDENT mode_spec
	| IN IDENT mode_spec
	| OUT IDENT mode_spec
	| INOUT IDENT mode_spec
	;

opt_returns:
	  RETURNS '(' mode_spec ')'
	| /* empty */
	;

/* Grant statement */
grant_statement:
	  GRANT IDENT ';'
		{ if (verbose) printf("Grant: %s\n", $2); }
	;

/* Statements */
statement_list:
	  statement
	| statement_list statement
	;

statement:
	  assignment_statement
	| if_statement
	| case_statement
	| do_statement
	| while_statement
	| for_statement
	| call_statement
	| return_statement
	| exit_statement
	| goto_statement
	| null_statement
	| begin_end_statement
	;

assignment_statement:
	  expression ASSIGN expression ';'
		{ if (verbose) printf("Assignment\n"); }
	;

if_statement:
	  IF expression THEN statement_list elsif_parts opt_else_part FI ';'
		{ if (verbose) printf("If statement\n"); }
	;

elsif_parts:
	  /* empty */
	| elsif_parts ELSIF expression THEN statement_list
	;

opt_else_part:
	  ELSE statement_list
	| /* empty */
	;

case_statement:
	  CASE expression OF case_label_list ESAC ';'
		{ if (verbose) printf("Case statement\n"); }
	;

case_label_list:
	  case_label
	| case_label_list case_label
	;

case_label:
	  '(' expression_list ')' ':' statement_list
	| ELSE ':' statement_list
	;

do_statement:
	  DO opt_with_part ';' statement_list OD ';'
		{ if (verbose) printf("Do statement\n"); }
	;

opt_with_part:
	  WITH IDENT
	| /* empty */
	;

while_statement:
	  DO WHILE expression ';' statement_list OD ';'
		{ if (verbose) printf("While statement\n"); }
	;

for_statement:
	  DO FOR IDENT ASSIGN expression TO expression opt_by_part ';'
	    statement_list OD ';'
		{ if (verbose) printf("For statement\n"); }
	;

opt_by_part:
	  BY expression
	| /* empty */
	;

call_statement:
	  CALL IDENT '(' opt_arg_list ')' ';'
		{ if (verbose) printf("Call: %s\n", $2); }
	;

opt_arg_list:
	  expression_list
	| /* empty */
	;

return_statement:
	  RETURN opt_result ';'
		{ if (verbose) printf("Return\n"); }
	;

opt_result:
	  '(' expression ')'
	| /* empty */
	;

exit_statement:
	  EXIT IDENT ';'
		{ if (verbose) printf("Exit\n"); }
	;

goto_statement:
	  GOTO IDENT ';'
		{ if (verbose) printf("Goto: %s\n", $2); }
	;

null_statement:
	  ';'
	;

begin_end_statement:
	  BBEGIN statement_list END ';'
		{ if (verbose) printf("Begin-end block\n"); }
	;

/* Expressions */
expression_list:
	  expression
	| expression_list ',' expression
	;

expression:
	  IDENT
	| INTLIT
	| REALLIT
	| CHARLIT
	| STRINGLIT
	| CTRUE
	| CFALSE
	| CNULL
	| expression PLUS expression
	| expression MINUS expression
	| expression MULT expression
	| expression DIV expression
	| expression MOD expression
	| expression REM expression
	| expression AND expression
	| expression OR expression
	| expression XOR expression
	| expression EQ expression
	| expression NE expression
	| expression LT expression
	| expression LE expression
	| expression GT expression
	| expression GE expression
	| expression IN expression
	| expression CONCAT expression
	| NOT expression
	| MINUS expression	%prec NOT
	| '(' expression ')'
	| expression '[' expression ']'
	| expression '.' IDENT
	| expression ARROW
	| IDENT '(' opt_arg_list ')'
	;

%%

/* Additional required functions */
extern int verbose;
