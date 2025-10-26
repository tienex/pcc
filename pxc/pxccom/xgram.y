/*
 * Copyright (c) 2025 PCC Xbase++ Compiler
 *
 * Parser grammar for Xbase++
 * Supports Xbase++ / Clipper / Harbour / xHarbour syntax
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Forward declarations */
int yylex(void);
void yyerror(const char *s);

/* Global state */
int current_scope = 0;

%}

%union {
	long long ival;
	double fval;
	char *sval;
	struct symtab *symptr;
	struct tnode *typeptr;
	struct node *nodeptr;
}

/* Keywords - organized by category */
/* Program structure */
%token FUNCTION PROCEDURE RETURN PARAMETERS

/* Variable declarations */
%token LOCAL STATIC PUBLIC PRIVATE MEMVAR FIELD

/* Control structures */
%token IF ELSE ELSEIF ENDIF IIF
%token DO WHILE ENDDO EXIT LOOP
%token FOR TO STEP NEXT
%token CASE OTHERWISE ENDCASE
%token BBEGIN SEQUENCE END RECOVER BREAK

/* OOP */
%token CLASS ENDCLASS METHOD ENDMETHOD DATA VAR INLINE
%token VIRTUAL CONSTRUCTOR DESTRUCTOR INHERIT FROM EXPORT
%token PROTECTED HIDDEN READONLY SHARED SYNC

/* Database commands */
%token USE SELECT GO GOTO SKIP SEEK LOCATE CONTINUE
%token REPLACE DELETE RECALL PACK ZAP APPEND BLANK
%token INDEX REINDEX SET CLOSE COMMIT UNLOCK ALIAS
%token IN EXCLUSIVE NEW ADDITIVE

/* Other keywords */
%token WITH REQUEST EXTERNAL INIT ANNOUNCE AS IS

/* Operators */
%token ASSIGN PLUSASSIGN MINUSASSIGN MULASSIGN DIVASSIGN MODASSIGN EXPASSIGN
%token EQ NE LT LE GT GE SUBSTR
%token INCR DECR POWER
%token AND OR NOT
%token ARROW DCOLON

/* Literals */
%token <ival> INTEGER
%token <fval> FLOAT
%token <sval> STRING
%token <sval> DATE
%token <sval> IDENT
%token <sval> MACROVAR
%token LTRUE LFALSE NIL

/* Special */
%token EOL

/* Type declarations for grammar rules */
%type <nodeptr> expression expr_list opt_expr_list opt_expression
%type <nodeptr> primary_expr postfix_expr unary_expr binary_expr
%type <nodeptr> statement statement_list opt_statement_list
%type <nodeptr> if_stmt while_stmt for_stmt case_stmt
%type <nodeptr> return_stmt opt_return_stmt expr_stmt
%type <symptr> identifier

/* Precedence and associativity (lowest to highest) */
%right ASSIGN PLUSASSIGN MINUSASSIGN MULASSIGN DIVASSIGN MODASSIGN EXPASSIGN
%left OR
%left AND
%left EQ NE
%left LT LE GT GE SUBSTR
%left '+' '-'
%left '*' '/' '%'
%right NOT UMINUS
%right POWER
%left ARROW DCOLON
%left '.' '[' '('
%right INCR DECR

%%

/* Top level - a program is a sequence of definitions */
program:
	  /* empty */
	| definition_list
	;

definition_list:
	  definition
	| definition_list definition
	;

definition:
	  function_def
	| procedure_def
	| class_def
	| variable_decl EOL
	| EOL  /* Empty lines are allowed */
	;

/* Function definition */
function_def:
	  FUNCTION identifier opt_param_list opt_eol
		{
			$2->sclass = S_FUNC;
			enter_scope();
			$<symptr>$ = $2;
		}
	  opt_statement_list
	  opt_return_stmt
		{
			SYMTAB *func = $<symptr>5;
			struct node *body = $6;

			/* Add return statement to body if present */
			if ($7 && body) {
				/* Append return to body */
				body = make_node(N_BLOCK, body, $7);
			} else if ($7) {
				body = $7;
			}

			/* Run semantic analysis */
			semantic_analyze_function(func, body);

			/* Generate code */
			codegen_function(func, body);

			exit_scope();
		}
	;

/* Procedure definition */
procedure_def:
	  PROCEDURE identifier opt_param_list opt_eol
		{
			$2->sclass = S_PROC;
			enter_scope();
			$<symptr>$ = $2;
		}
	  opt_statement_list
	  opt_return_stmt
		{
			SYMTAB *proc = $<symptr>5;
			struct node *body = $6;

			/* Add return statement to body if present */
			if ($7 && body) {
				body = make_node(N_BLOCK, body, $7);
			} else if ($7) {
				body = $7;
			}

			/* Run semantic analysis */
			semantic_analyze_function(proc, body);

			/* Generate code */
			codegen_function(proc, body);

			exit_scope();
		}
	;

/* Class definition */
class_def:
	  CLASS identifier opt_from_clause opt_eol
		{
			enter_scope();
		}
	  opt_class_members
	  ENDCLASS opt_eol
		{
			exit_scope();
		}
	;

opt_from_clause:
	  /* empty */
	| FROM identifier
	| INHERIT identifier
	;

opt_class_members:
	  /* empty */
	| class_member_list
	;

class_member_list:
	  class_member
	| class_member_list class_member
	;

class_member:
	  data_member
	| method_def
	| EOL
	;

data_member:
	  DATA identifier opt_eol
	| VAR identifier opt_eol
	| EXPORT DATA identifier opt_eol
	| PROTECTED DATA identifier opt_eol
	| HIDDEN DATA identifier opt_eol
	;

method_def:
	  METHOD identifier opt_param_list opt_eol
		{
			enter_scope();
		}
	  opt_statement_list
	  opt_return_stmt
		{
			exit_scope();
		}
	| INLINE METHOD identifier opt_param_list opt_eol expression opt_eol
	;

/* Parameters */
opt_param_list:
	  /* empty */
	| '(' ')'
	| '(' param_list ')'
	;

param_list:
	  identifier
	| param_list ',' identifier
	;

/* Variable declarations */
variable_decl:
	  LOCAL var_list
	| STATIC var_list
	| PUBLIC var_list
	| PRIVATE var_list
	| MEMVAR var_list
	| FIELD field_list
	;

var_list:
	  var_item
	| var_list ',' var_item
	;

var_item:
	  identifier
	| identifier ASSIGN expression
	;

field_list:
	  identifier
	| field_list ',' identifier
	;

/* Statements */
opt_statement_list:
	  /* empty */
		{ $$ = NULL; }
	| statement_list
	;

statement_list:
	  statement
	| statement_list opt_eol statement
		{ $$ = make_node(N_BLOCK, $1, $3); }
	;

statement:
	  expr_stmt
		{ $$ = $1; }
	| if_stmt
		{ $$ = $1; }
	| while_stmt
		{ $$ = $1; }
	| for_stmt
		{ $$ = $1; }
	| case_stmt
		{ $$ = $1; }
	| return_stmt
		{ $$ = $1; }
	| break_stmt
		{ $$ = NULL; }
	| exit_stmt
		{ $$ = NULL; }
	| loop_stmt
		{ $$ = NULL; }
	| variable_decl
		{ $$ = NULL; }
	| db_command
		{ $$ = NULL; }
	| EOL
		{ $$ = NULL; }
	;

expr_stmt:
	  expression opt_eol
	| expression ';'
	;

/* IF statement */
if_stmt:
	  IF expression opt_eol
	  opt_statement_list
	  opt_elseif_list
	  opt_else_clause
	  ENDIF opt_eol
		{ $$ = make_node(N_IF, $2, $4); }
	;

opt_elseif_list:
	  /* empty */
	| elseif_list
	;

elseif_list:
	  elseif_clause
	| elseif_list elseif_clause
	;

elseif_clause:
	  ELSEIF expression opt_eol
	  opt_statement_list
	;

opt_else_clause:
	  /* empty */
	| ELSE opt_eol
	  opt_statement_list
	;

/* WHILE loop */
while_stmt:
	  DO WHILE expression opt_eol
	  opt_statement_list
	  ENDDO opt_eol
		{ $$ = make_node(N_WHILE, $3, $5); }
	;

/* FOR loop */
for_stmt:
	  FOR identifier ASSIGN expression TO expression opt_step opt_eol
	  opt_statement_list
	  NEXT opt_eol
		{ $$ = make_node(N_FOR, NULL, NULL); }
	;

opt_step:
	  /* empty */
	| STEP expression
	;

/* CASE statement */
case_stmt:
	  DO CASE opt_eol
	  case_list
	  opt_otherwise
	  ENDCASE opt_eol
		{ $$ = NULL; }
	;

case_list:
	  case_clause
	| case_list case_clause
	;

case_clause:
	  CASE expression opt_eol
	  opt_statement_list
	;

opt_otherwise:
	  /* empty */
	| OTHERWISE opt_eol
	  opt_statement_list
	;

/* Other statements */
return_stmt:
	  RETURN opt_expression opt_eol
		{ $$ = make_node(N_RETURN, $2, NULL); }
	;

opt_return_stmt:
	  /* empty */
		{ $$ = NULL; }
	| return_stmt
		{ $$ = $1; }
	;

break_stmt:
	  BREAK opt_eol
	;

exit_stmt:
	  EXIT opt_eol
	;

loop_stmt:
	  LOOP opt_eol
	;

/* Database commands */
db_command:
	  USE opt_expression opt_eol
	| SELECT expression opt_eol
	| GO expression opt_eol
	| SKIP opt_expression opt_eol
	| SEEK expression opt_eol
	| LOCATE FOR expression opt_eol
	| REPLACE field_assign_list opt_eol
	| DELETE opt_eol
	| RECALL opt_eol
	| APPEND BLANK opt_eol
	| CLOSE opt_eol
	;

field_assign_list:
	  field_assign
	| field_assign_list ',' field_assign
	;

field_assign:
	  identifier WITH expression
	;

/* Expressions */
opt_expression:
	  /* empty */
		{ $$ = NULL; }
	| expression
	;

expression:
	  binary_expr
	| unary_expr
	| postfix_expr
	| primary_expr
	;

binary_expr:
	  expression '+' expression
		{ $$ = make_node(N_PLUS, $1, $3); }
	| expression '-' expression
		{ $$ = make_node(N_MINUS, $1, $3); }
	| expression '*' expression
		{ $$ = make_node(N_MUL, $1, $3); }
	| expression '/' expression
		{ $$ = make_node(N_DIV, $1, $3); }
	| expression '%' expression
		{ $$ = make_node(N_MOD, $1, $3); }
	| expression POWER expression
		{ $$ = make_node(N_PLUS, $1, $3); /* TODO: POWER op */ }
	| expression EQ expression
		{ $$ = make_node(N_EQ, $1, $3); }
	| expression NE expression
		{ $$ = make_node(N_NE, $1, $3); }
	| expression LT expression
		{ $$ = make_node(N_LT, $1, $3); }
	| expression LE expression
		{ $$ = make_node(N_LE, $1, $3); }
	| expression GT expression
		{ $$ = make_node(N_GT, $1, $3); }
	| expression GE expression
		{ $$ = make_node(N_GE, $1, $3); }
	| expression AND expression
		{ $$ = make_node(N_AND, $1, $3); }
	| expression OR expression
		{ $$ = make_node(N_OR, $1, $3); }
	| expression SUBSTR expression
		{ $$ = make_node(N_PLUS, $1, $3); /* TODO: SUBSTR op */ }
	| expression ASSIGN expression
		{ $$ = make_node(N_ASSIGN, $1, $3); }
	| expression PLUSASSIGN expression
		{ $$ = make_node(N_ASSIGN, $1, make_node(N_PLUS, $1, $3)); }
	| expression MINUSASSIGN expression
		{ $$ = make_node(N_ASSIGN, $1, make_node(N_MINUS, $1, $3)); }
	;

unary_expr:
	  NOT expression
		{ $$ = make_node(N_NOT, $2, NULL); }
	| '-' expression %prec UMINUS
		{ $$ = make_node(N_UMINUS, $2, NULL); }
	| '+' expression %prec UMINUS
		{ $$ = $2; }
	| INCR expression
		{ $$ = make_node(N_ASSIGN, $2, make_node(N_PLUS, $2, make_icon(1))); }
	| DECR expression
		{ $$ = make_node(N_ASSIGN, $2, make_node(N_MINUS, $2, make_icon(1))); }
	;

postfix_expr:
	  expression '[' expression ']'
		{ $$ = make_node(N_SUBSCR, $1, $3); }
	| expression '.' identifier
		{ $$ = make_node(N_FIELD, $1, make_name($3)); }
	| expression ARROW identifier
		{ $$ = make_node(N_FIELD, $1, make_name($3)); }
	| expression DCOLON identifier
		{ $$ = make_node(N_FIELD, $1, make_name($3)); }
	| expression '(' opt_expr_list ')'
		{ $$ = make_node(N_CALL, $1, $3); }
	| expression INCR
		{ $$ = make_node(N_ASSIGN, $1, make_node(N_PLUS, $1, make_icon(1))); }
	| expression DECR
		{ $$ = make_node(N_ASSIGN, $1, make_node(N_MINUS, $1, make_icon(1))); }
	;

primary_expr:
	  INTEGER
		{ $$ = make_icon($1); }
	| FLOAT
		{ $$ = make_fcon($1); }
	| STRING
		{ $$ = make_scon($1); }
	| DATE
		{ $$ = make_scon($1); /* TODO: date type */ }
	| LTRUE
		{ $$ = make_icon(1); }
	| LFALSE
		{ $$ = make_icon(0); }
	| NIL
		{ $$ = make_icon(0); /* TODO: NIL */ }
	| identifier
		{ $$ = make_name($1); }
	| '(' expression ')'
		{ $$ = $2; }
	| '{' opt_expr_list '}'
		{ $$ = NULL; /* TODO: array literal */ }
	| '{' '|' opt_param_list '|' expression '}'
		{ $$ = NULL; /* TODO: code block */ }
	| IIF '(' expression ',' expression ',' expression ')'
		{ $$ = make_node(N_IF, $3, make_node(N_BLOCK, $5, $7)); }
	| '@' expression
		{ $$ = $2; /* TODO: macro expansion */ }
	| '&' IDENT
		{ $$ = NULL; /* TODO: macro variable */ }
	;

opt_expr_list:
	  /* empty */
		{ $$ = NULL; }
	| expr_list
	;

expr_list:
	  expression
	| expr_list ',' expression
		{ $$ = make_node(N_BLOCK, $1, $3); /* TODO: expr list */ }
	;

/* Identifier */
identifier:
	  IDENT
		{
			SYMTAB *sp = lookup($1, current_scope);
			if (sp == NULL) {
				sp = install($1, S_NULL, NULL, current_scope);
			}
			$$ = sp;
		}
	;

/* Optional EOL */
opt_eol:
	  /* empty */
	| EOL
	| opt_eol EOL
	;

%%

/* Error handling */
void yyerror(const char *s) {
	error("%s at line %d", s, lineno);
}
