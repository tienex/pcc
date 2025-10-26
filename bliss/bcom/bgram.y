/*
 * Copyright (c) 2025 PCC BLISS Compiler
 *
 * Parser grammar for BLISS
 * BLISS - Basic Language for Implementation of System Software
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
	char *sval;
	struct symtab *symptr;
	struct node *nodeptr;
}

/* Keywords - Module structure */
%token MODULE BBEGIN END ROUTINE GLOBAL BIND LITERAL LOCAL FORWARD EXTERNAL
%token OWN REGISTER STACKLOCAL

/* Keywords - Control structures */
%token IF THEN ELSE CASE OF OUTRANGE INRANGE SELECTONE SELECTA SELECTU OTHERWISE TES

/* Keywords - Loop structures */
%token DO WHILE UNTIL INCR DECR FROM TO BY

/* Keywords - Control flow */
%token RETURN LEAVE EXITLOOP LABEL

/* Keywords - Data structures */
%token VECTOR BLOCKVECTOR STRUCTURE FIELD SET PRESET REF CODECOMMENT

/* Keywords - Special forms */
%token LINKAGE PLIT UPLIT ENABLE SIGNAL

/* Comparison operators */
%token EQL EQLU NEQ NEQU LSS LSSU LEQ LEQU GTR GTRU GEQ GEQU

/* Logical operators */
%token NOT AND OR XOR EQV

/* Operators */
%token EQ COLON SEMICOLON COMMA DOT PLUS MINUS STAR SLASH UPARROW
%token LANGLE RANGLE LBRACK RBRACK LPAREN RPAREN AT
%token PERCENTLP PERCENTLANGLE RANGLEPCENT

/* Literals */
%token <ival> INTCONST
%token <ival> ANGLECONST
%token <sval> STRINGCONST
%token <sval> IDENT

/* Type declarations */
%type <nodeptr> expression simple_expression term factor
%type <nodeptr> primary block_expression
%type <nodeptr> statement compound_statement routine_body
%type <symptr> name

/* Precedence - from lowest to highest */
%left COMMA
%right EQ
%left OR
%left AND XOR
%left EQV
%left EQL EQLU NEQ NEQU LSS LSSU LEQ LEQU GTR GTRU GEQ GEQU
%left PLUS MINUS
%left STAR SLASH
%right NOT UPARROW DOT
%left LBRACK LPAREN

%%

/* Module structure */
module:
	  MODULE name module_parameters_opt EQ
	    module_declarations
	    BBEGIN
	    routine_body
	    END
		{ /* Complete module */ }
	;

module_parameters_opt:
	  /* empty */
	| LPAREN module_parameter_list RPAREN
	;

module_parameter_list:
	  module_parameter
	| module_parameter_list COMMA module_parameter
	;

module_parameter:
	  name
	| name EQ expression
	;

module_declarations:
	  /* empty */
	| module_declarations module_declaration
	;

module_declaration:
	  LITERAL literal_list SEMICOLON
	| BIND bind_list SEMICOLON
	| GLOBAL global_list SEMICOLON
	| OWN own_list SEMICOLON
	| EXTERNAL external_list SEMICOLON
	| ROUTINE routine_declaration SEMICOLON
	| FORWARD forward_list SEMICOLON
	| LINKAGE linkage_declaration SEMICOLON
	| STRUCTURE structure_declaration SEMICOLON
	;

/* Literal declarations */
literal_list:
	  literal_item
	| literal_list COMMA literal_item
	;

literal_item:
	  name EQ expression
	;

/* Bind declarations */
bind_list:
	  bind_item
	| bind_list COMMA bind_item
	;

bind_item:
	  name EQ expression
	;

/* Global declarations */
global_list:
	  global_item
	| global_list COMMA global_item
	;

global_item:
	  name
	| name COLON attributes
	| REGISTER name
	| REGISTER name EQ expression
	;

attributes:
	  VECTOR
	| VECTOR LBRACK expression RBRACK
	| BLOCKVECTOR LBRACK expression RBRACK
	;

/* Own (static) declarations */
own_list:
	  own_item
	| own_list COMMA own_item
	;

own_item:
	  name
	| name COLON attributes
	| name PRESET LPAREN preset_list RPAREN
	;

preset_list:
	  expression
	| preset_list COMMA expression
	;

/* External declarations */
external_list:
	  external_item
	| external_list COMMA external_item
	;

external_item:
	  name
	| name COLON attributes
	;

/* Forward declarations */
forward_list:
	  forward_item
	| forward_list COMMA forward_item
	;

forward_item:
	  name
	;

/* Routine declarations */
routine_declaration:
	  name routine_parameters_opt EQ routine_body
		{ /* Install routine in symbol table */ }
	;

routine_parameters_opt:
	  /* empty */
	| LPAREN formal_parameter_list RPAREN
	;

formal_parameter_list:
	  formal_parameter
	| formal_parameter_list COMMA formal_parameter
	;

formal_parameter:
	  name
	;

routine_body:
	  block_expression
	;

/* Linkage declarations */
linkage_declaration:
	  name EQ LPAREN linkage_spec_list RPAREN
	;

linkage_spec_list:
	  linkage_spec
	| linkage_spec_list COMMA linkage_spec
	;

linkage_spec:
	  name
	| name COLON name
	;

/* Structure declarations */
structure_declaration:
	  name LBRACK struct_parameter_list RBRACK EQ
	    LBRACK field_list RBRACK
	;

struct_parameter_list:
	  name
	| struct_parameter_list COMMA name
	;

field_list:
	  field_item
	| field_list COMMA field_item
	;

field_item:
	  name EQ set_expression
	;

set_expression:
	  LBRACK expression COMMA expression COMMA expression RBRACK
	;

/* Statements */
statement:
	  expression
		{ $$ = $1; }
	;

compound_statement:
	  BBEGIN statement_sequence END
		{ /* Generate compound statement */ }
	;

statement_sequence:
	  statement
	| statement_sequence SEMICOLON statement
	;

/* Block expression */
block_expression:
	  BBEGIN
	    local_declarations
	    statement_sequence
	    END
		{ /* Block with local declarations */ }
	;

local_declarations:
	  /* empty */
	| local_declarations local_declaration
	;

local_declaration:
	  LOCAL local_list SEMICOLON
	| BIND bind_list SEMICOLON
	| LITERAL literal_list SEMICOLON
	| LABEL label_list SEMICOLON
	| STACKLOCAL stacklocal_list SEMICOLON
	| REGISTER register_list SEMICOLON
	;

local_list:
	  local_item
	| local_list COMMA local_item
	;

local_item:
	  name
	| name COLON attributes
	| name PRESET LPAREN preset_list RPAREN
	;

label_list:
	  name
	| label_list COMMA name
	;

stacklocal_list:
	  stacklocal_item
	| stacklocal_list COMMA stacklocal_item
	;

stacklocal_item:
	  name
	;

register_list:
	  register_item
	| register_list COMMA register_item
	;

register_item:
	  name
	| name EQ expression
	;

/* Expressions */
expression:
	  simple_expression
		{ $$ = $1; }
	| assignment_expression
	| if_expression
	| case_expression
	| select_expression
	| loop_expression
	| block_expression
	| leave_expression
	| return_expression
	;

assignment_expression:
	  simple_expression EQ expression
		{ /* Generate assignment */ }
	;

if_expression:
	  IF simple_expression THEN expression
		{ /* If without else */ }
	| IF simple_expression THEN expression ELSE expression
		{ /* If with else */ }
	;

case_expression:
	  CASE expression FROM expression TO expression OF
	    SET case_item_list TES
	;

case_item_list:
	  case_item
	| case_item_list SEMICOLON case_item
	;

case_item:
	  LBRACK expression_list RBRACK COLON expression
	| INRANGE COLON expression
	| OUTRANGE COLON expression
	;

select_expression:
	  SELECTONE expression OF
	    SET select_item_list TES
	| SELECTA expression OF
	    SET select_item_list TES
	| SELECTU expression OF
	    SET select_item_list TES
	;

select_item_list:
	  select_item
	| select_item_list SEMICOLON select_item
	;

select_item:
	  LBRACK expression_list RBRACK COLON expression
	| OTHERWISE COLON expression
	;

loop_expression:
	  DO expression WHILE expression
	| DO expression UNTIL expression
	| WHILE expression DO expression
	| UNTIL expression DO expression
	| INCR name FROM expression TO expression BY expression DO expression
	| INCR name FROM expression TO expression DO expression
	| DECR name FROM expression TO expression BY expression DO expression
	| DECR name FROM expression TO expression DO expression
	| DO expression
	;

leave_expression:
	  LEAVE name
	| LEAVE name WITH expression
	| EXITLOOP
	| EXITLOOP expression
	;

return_expression:
	  RETURN
	| RETURN expression
	;

simple_expression:
	  term
		{ $$ = $1; }
	| simple_expression PLUS term
		{ /* Generate addition */ }
	| simple_expression MINUS term
		{ /* Generate subtraction */ }
	| simple_expression OR term
		{ /* Generate OR */ }
	;

term:
	  factor
		{ $$ = $1; }
	| term STAR factor
		{ /* Generate multiplication */ }
	| term SLASH factor
		{ /* Generate division */ }
	| term AND factor
		{ /* Generate AND */ }
	| term XOR factor
		{ /* Generate XOR */ }
	;

factor:
	  primary
		{ $$ = $1; }
	| NOT factor
		{ /* Generate NOT */ }
	| MINUS factor
		{ /* Generate negation */ }
	| PLUS factor
		{ $$ = $2; }
	| factor EQL factor
		{ /* Generate EQL comparison */ }
	| factor EQLU factor
		{ /* Generate EQLU comparison */ }
	| factor NEQ factor
		{ /* Generate NEQ comparison */ }
	| factor NEQU factor
		{ /* Generate NEQU comparison */ }
	| factor LSS factor
		{ /* Generate LSS comparison */ }
	| factor LSSU factor
		{ /* Generate LSSU comparison */ }
	| factor LEQ factor
		{ /* Generate LEQ comparison */ }
	| factor LEQU factor
		{ /* Generate LEQU comparison */ }
	| factor GTR factor
		{ /* Generate GTR comparison */ }
	| factor GTRU factor
		{ /* Generate GTRU comparison */ }
	| factor GEQ factor
		{ /* Generate GEQ comparison */ }
	| factor GEQU factor
		{ /* Generate GEQU comparison */ }
	;

primary:
	  INTCONST
		{ /* Generate integer constant */ }
	| ANGLECONST
		{ /* Generate angle bracket constant */ }
	| STRINGCONST
		{ /* Generate string constant */ }
	| name
		{ /* Generate name reference */ }
	| LPAREN expression RPAREN
		{ $$ = $2; }
	| function_call
	| DOT name
		{ /* Field reference */ }
	| UPARROW name
		{ /* Pointer dereference */ }
	| primary LBRACK expression RBRACK
		{ /* Array/vector subscript */ }
	| primary DOT name
		{ /* Field access */ }
	| primary UPARROW
		{ /* Fetch through pointer */ }
	| REF name
		{ /* Get address */ }
	| PLIT plit_expression
	| UPLIT plit_expression
	| CODECOMMENT LPAREN expression COMMA STRINGCONST RPAREN
		{ /* Code comment */ }
	;

function_call:
	  name LPAREN expression_list RPAREN
		{ /* Generate function call */ }
	| name LPAREN RPAREN
		{ /* Function call with no args */ }
	;

plit_expression:
	  LPAREN expression_list RPAREN
	| LBRACK expression_list RBRACK
	;

expression_list:
	  expression
	| expression_list COMMA expression
	;

name:
	  IDENT
		{ /* Look up or install name */ }
	;

%%

/* Error handling */
void yyerror(const char *s) {
	fprintf(stderr, "Error: %s at line %d\n", s, yylineno);
}
