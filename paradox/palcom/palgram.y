/*
 * Copyright (c) 2025 PCC Paradox PAL Compiler
 *
 * Parser grammar for PAL and ObjectPAL
 * Supports multiple Paradox/ObjectPAL versions
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
%token AND ARRAY AS BBEGIN BREAK CASE CONST CONTINUE DEFAULT
%token DO DOWNTO ELSE ELSIF END ENDFOR ENDFOREACH ENDIF ENDMETHOD
%token ENDPROC ENDSCAN ENDSWITCH ENDWHILE EXCEPT FALSE FOR FOREACH
%token FROM FUNCTION IF IN INTDIV LIKE METHOD MOD NOT OF OR
%token PRIVATE PROC PROTECTED PUBLIC QUIT RECORD RETURN
%token SCAN SELF STEP SWITCH THEN TO TRUE TRY TYPE UNTIL
%token USES VAR WHILE WITH

/* Type keywords */
%token TSMALLINT TSHORTINT TLONGINT TNUMBER TCURRENCY
%token TLOGICAL TSTRING TDATE TTIME TDATETIME TTIMESTAMP
%token TMEMO TBLOB TGRAPHIC TVARIANT

/* Operators */
%token ASSIGN DOTDOT NE LE GE EQ LT GT
%token PLUS MINUS STAR SLASH UPARROW AT
%token DOT COMMA COLON SEMICOLON
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE

/* Literals */
%token <ival> INTCONST LOGICALCONST
%token <dval> NUMBERCONST
%token <sval> STRINGCONST
%token <sval> IDENT

/* Type declarations */
%type <typeptr> type_spec simple_type array_type record_type
%type <symptr> identifier
%type <paramptr> param_list param_decl opt_param_list
%type <fieldptr> field_list field_decl

/* Precedence and associativity */
%left OR
%left AND
%left EQ NE LT LE GT GE IN LIKE
%left PLUS MINUS
%left STAR SLASH INTDIV MOD
%right NOT UPARROW
%left DOT LBRACK LPAREN

%%

/* Program structure */
program:
	  /* Empty program */
	  { /* Empty */ }
	| toplevel_list
	  { /* Program with declarations/procedures */ }
	;

toplevel_list:
	  toplevel_decl
	| toplevel_list toplevel_decl
	;

toplevel_decl:
	  var_declaration
	| const_declaration
	| type_declaration
	| proc_declaration
	| method_declaration
	| uses_declaration
	;

/* Uses declaration (library imports) */
uses_declaration:
	  USES identifier_list SEMICOLON
	  { /* Import libraries */ }
	;

identifier_list:
	  identifier
	| identifier_list COMMA identifier
	;

/* Variable declarations */
var_declaration:
	  VAR var_decl_list
	;

var_decl_list:
	  var_decl
	| var_decl_list var_decl
	;

var_decl:
	  identifier_list COLON type_spec SEMICOLON
	  { /* Declare variables */ }
	;

/* Constant declarations */
const_declaration:
	  CONST const_decl_list
	;

const_decl_list:
	  const_decl
	| const_decl_list const_decl
	;

const_decl:
	  identifier EQ expression SEMICOLON
	  { /* Declare constant */ }
	;

/* Type declarations */
type_declaration:
	  TYPE type_decl_list
	;

type_decl_list:
	  type_decl
	| type_decl_list type_decl
	;

type_decl:
	  identifier EQ type_spec SEMICOLON
	  { /* Define type alias */ }
	;

/* Type specifications */
type_spec:
	  simple_type
	  { $$ = $1; }
	| array_type
	  { $$ = $1; }
	| record_type
	  { $$ = $1; }
	;

simple_type:
	  TSMALLINT     { $$ = mktype(TSMALLINT); }
	| TSHORTINT     { $$ = mktype(TSHORTINT); }
	| TLONGINT      { $$ = mktype(TLONGINT); }
	| TNUMBER       { $$ = mktype(TNUMBER); }
	| TCURRENCY     { $$ = mktype(TCURRENCY); }
	| TLOGICAL      { $$ = mktype(TLOGICAL); }
	| TSTRING       { $$ = mktype(TALPHANUMERIC); }
	| TDATE         { $$ = mktype(TDATE); }
	| TTIME         { $$ = mktype(TTIME); }
	| TDATETIME     { $$ = mktype(TDATETIME); }
	| TTIMESTAMP    { $$ = mktype(TTIMESTAMP); }
	| TMEMO         { $$ = mktype(TMEMO); }
	| TBLOB         { $$ = mktype(TBLOB); }
	| TGRAPHIC      { $$ = mktype(TGRAPHIC); }
	| TVARIANT      { $$ = mktype(TVARIANT); }
	| identifier
	  { /* Type reference */ $$ = NULL; }
	;

array_type:
	  ARRAY LBRACK expression RBRACK OF type_spec
	  { /* Create array type */ $$ = NULL; }
	| ARRAY LBRACK expression DOTDOT expression RBRACK OF type_spec
	  { /* Array with bounds */ $$ = NULL; }
	;

record_type:
	  RECORD field_list END
	  { /* Create record type */ $$ = NULL; }
	;

field_list:
	  field_decl
	  { $$ = $1; }
	| field_list field_decl
	  { /* Append field to list */ $$ = $1; }
	;

field_decl:
	  identifier_list COLON type_spec SEMICOLON
	  { /* Field declaration */ $$ = NULL; }
	;

/* Procedure declaration */
proc_declaration:
	  PROC identifier opt_param_list proc_body ENDPROC
	  { /* Define procedure */ }
	;

proc_body:
	  local_declarations statement_list
	;

local_declarations:
	  /* empty */
	| local_declarations var_declaration
	| local_declarations const_declaration
	| local_declarations type_declaration
	;

/* Method declaration (ObjectPAL) */
method_declaration:
	  METHOD identifier opt_param_list method_body ENDMETHOD
	  { if (!ALLOW_METHODS()) {
	      error("methods not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
	| visibility METHOD identifier opt_param_list method_body ENDMETHOD
	  { if (!ALLOW_METHODS()) {
	      error("methods not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
	;

visibility:
	  PRIVATE
	| PROTECTED
	| PUBLIC
	;

method_body:
	  local_declarations statement_list
	;

/* Parameter list */
opt_param_list:
	  /* empty */
	  { $$ = NULL; }
	| LPAREN RPAREN
	  { $$ = NULL; }
	| LPAREN param_list RPAREN
	  { $$ = $2; }
	;

param_list:
	  param_decl
	  { $$ = $1; }
	| param_list COMMA param_decl
	  { /* Append parameter */ $$ = $1; }
	;

param_decl:
	  identifier COLON type_spec
	  { /* Parameter declaration */ $$ = NULL; }
	| VAR identifier COLON type_spec
	  { /* var parameter (by reference) */ $$ = NULL; }
	| CONST identifier COLON type_spec
	  { /* const parameter */ $$ = NULL; }
	;

/* Statement list */
statement_list:
	  /* empty */
	| statement_list statement
	;

statement:
	  simple_statement
	| structured_statement
	;

simple_statement:
	  assignment_statement
	| procedure_call
	| return_statement
	| break_statement
	| continue_statement
	| quit_statement
	;

assignment_statement:
	  lvalue ASSIGN expression
	  { /* Assignment */ }
	| lvalue EQ expression
	  { /* Assignment (PAL style uses =) */ }
	;

lvalue:
	  identifier
	| lvalue DOT identifier
	  { /* Field/method access */ }
	| lvalue LBRACK expression RBRACK
	  { /* Array subscript */ }
	;

procedure_call:
	  identifier opt_arguments
	  { /* Call procedure */ }
	| lvalue DOT identifier opt_arguments
	  { /* Call method */ }
	;

opt_arguments:
	  /* empty */
	| LPAREN RPAREN
	| LPAREN argument_list RPAREN
	;

argument_list:
	  expression
	| argument_list COMMA expression
	;

return_statement:
	  RETURN
	  { /* Return from procedure */ }
	| RETURN expression
	  { /* Return value from function */ }
	;

break_statement:
	  BREAK
	  { if (!dialect_features->allow_break_continue) {
	      error("break not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
	;

continue_statement:
	  CONTINUE
	  { if (!dialect_features->allow_break_continue) {
	      error("continue not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
	;

quit_statement:
	  QUIT
	  { /* Quit program */ }
	;

/* Structured statements */
structured_statement:
	  if_statement
	| while_statement
	| for_statement
	| foreach_statement
	| switch_statement
	| try_statement
	| scan_statement
	| begin_end_block
	;

if_statement:
	  IF expression THEN statement_list endif_part
	;

endif_part:
	  ENDIF
	| elsif_list ENDIF
	| elsif_list ELSE statement_list ENDIF
	| ELSE statement_list ENDIF
	;

elsif_list:
	  ELSIF expression THEN statement_list
	| elsif_list ELSIF expression THEN statement_list
	;

while_statement:
	  WHILE expression DO statement_list ENDWHILE
	  { if (!dialect_features->allow_while_loop) {
	      error("while loop not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
	;

for_statement:
	  FOR identifier EQ expression TO expression for_body ENDFOR
	  { /* for loop ascending */ }
	| FOR identifier EQ expression DOWNTO expression for_body ENDFOR
	  { /* for loop descending */ }
	| FOR identifier EQ expression TO expression STEP expression for_body ENDFOR
	  { /* for loop with step */ }
	;

for_body:
	  statement_list
	;

foreach_statement:
	  FOREACH identifier IN expression foreach_body ENDFOREACH
	  { if (!ALLOW_FOREACH()) {
	      error("foreach loop not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
	;

foreach_body:
	  statement_list
	;

switch_statement:
	  SWITCH expression case_list endswitch_part
	  { if (!dialect_features->allow_switch_statement) {
	      error("switch statement not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
	;

case_list:
	  case_clause
	| case_list case_clause
	;

case_clause:
	  CASE expression COLON statement_list
	;

endswitch_part:
	  ENDSWITCH
	| DEFAULT COLON statement_list ENDSWITCH
	;

try_statement:
	  TRY statement_list EXCEPT statement_list END
	  { if (!ALLOW_TRY_EXCEPT()) {
	      error("try-except not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
	;

scan_statement:
	  SCAN identifier COLON statement_list ENDSCAN
	  { /* Scan through table */ }
	;

begin_end_block:
	  BBEGIN statement_list END
	;

/* Expressions */
expression:
	  simple_expression
	| expression EQ simple_expression
	  { /* Equality */ }
	| expression NE simple_expression
	  { /* Inequality */ }
	| expression LT simple_expression
	  { /* Less than */ }
	| expression LE simple_expression
	  { /* Less or equal */ }
	| expression GT simple_expression
	  { /* Greater than */ }
	| expression GE simple_expression
	  { /* Greater or equal */ }
	| expression IN simple_expression
	  { /* Set membership */ }
	| expression LIKE simple_expression
	  { /* String pattern match */ }
	;

simple_expression:
	  term
	| PLUS term
	  { /* Unary plus */ }
	| MINUS term
	  { /* Unary minus */ }
	| simple_expression PLUS term
	  { /* Addition */ }
	| simple_expression MINUS term
	  { /* Subtraction */ }
	| simple_expression OR term
	  { /* Logical OR */ }
	;

term:
	  factor
	| term STAR factor
	  { /* Multiplication */ }
	| term SLASH factor
	  { /* Division */ }
	| term INTDIV factor
	  { /* Integer division */ }
	| term MOD factor
	  { /* Modulo */ }
	| term AND factor
	  { /* Logical AND */ }
	;

factor:
	  primary
	| NOT factor
	  { /* Logical NOT */ }
	| UPARROW factor
	  { /* Pointer dereference */ }
	| AT factor
	  { /* Address-of */ }
	;

primary:
	  constant
	| lvalue
	| function_call
	| LPAREN expression RPAREN
	  { /* Parenthesized expression */ }
	| SELF
	  { /* Self reference (ObjectPAL) */ }
	;

function_call:
	  identifier LPAREN argument_list RPAREN
	  { /* Function call */ }
	| identifier LPAREN RPAREN
	  { /* Function call with no args */ }
	;

constant:
	  INTCONST
	  { /* Integer constant */ }
	| NUMBERCONST
	  { /* Floating point constant */ }
	| STRINGCONST
	  { /* String constant */ }
	| LOGICALCONST
	  { /* Boolean constant */ }
	;

identifier:
	  IDENT
	  { /* Identifier */ $$ = NULL; }
	;

%%

void yyerror(const char *s)
{
	error("%s", s);
}
