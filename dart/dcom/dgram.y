%{
/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart parser grammar
 */

#include "pass1.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int yylex(void);
extern int yylineno;
extern int yycolumn;

Node *ast_root = NULL;

void yyerror(const char *s) {
	dart_error(yylineno, yycolumn, "%s", s);
}

%}

%union {
	int intval;
	double doubleval;
	char *strval;
	struct node *node;
}

/* Tokens */
%token <intval> INTEGER_LITERAL
%token <doubleval> DOUBLE_LITERAL
%token <strval> STRING_LITERAL IDENTIFIER

/* Keywords */
%token ABSTRACT AS ASSERT ASYNC AWAIT BREAK CASE CATCH CLASS CONST CONTINUE
%token DEFAULT DEFERRED DO ELSE ENUM EXPORT EXTENDS EXTENSION EXTERNAL
%token FACTORY FALSE FINAL FINALLY FOR GET IF IMPLEMENTS IMPORT IN INTERFACE
%token IS LATE LIBRARY MIXIN NEW NULL_LIT ON OPERATOR PART REQUIRED RETHROW
%token RETURN SET SHOW STATIC SUPER SWITCH SYNC THIS THROW TRUE TRY TYPEDEF
%token VAR VOID WHILE WITH YIELD

/* Built-in types */
%token INT DOUBLE BOOL STRING LIST MAP SET_TYPE DYNAMIC OBJECT FUNCTION NUM

/* Operators */
%token PLUS MINUS TIMES DIVIDE MODULO INT_DIVIDE
%token EQ NE LT GT LE GE
%token AND OR NOT
%token BIT_AND BIT_OR BIT_XOR BIT_NOT LSHIFT RSHIFT URSHIFT
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIVIDE_ASSIGN
%token MODULO_ASSIGN INT_DIVIDE_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN
%token LSHIFT_ASSIGN RSHIFT_ASSIGN URSHIFT_ASSIGN
%token INCREMENT DECREMENT
%token QUESTION COLON NULL_COALESCE NULL_COALESCE_ASSIGN NULL_AWARE
%token SPREAD NULL_SPREAD ARROW

/* Delimiters */
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token SEMICOLON COMMA DOT CASCADE AT HASH

/* Nonterminals */
%type <node> program compilation_unit library_name
%type <node> import_or_export import_directive export_directive
%type <node> part_directive part_of_directive
%type <node> metadata annotation
%type <node> top_level_definition class_definition function_definition
%type <node> variable_declaration type_alias
%type <node> class_body class_member field_declaration method_declaration
%type <node> constructor_declaration getter_declaration setter_declaration
%type <node> type type_name type_arguments type_argument
%type <node> type_parameter type_parameters
%type <node> formal_parameter_list formal_parameter
%type <node> named_parameter optional_parameter
%type <node> function_body block_statement
%type <node> statement simple_statement expression_statement
%type <node> local_variable_declaration if_statement for_statement
%type <node> while_statement do_statement switch_statement
%type <node> return_statement break_statement continue_statement
%type <node> try_statement throw_statement assert_statement
%type <node> expression assignment_expression conditional_expression
%type <node> logical_or_expression logical_and_expression equality_expression
%type <node> relational_expression bitwise_or_expression bitwise_xor_expression
%type <node> bitwise_and_expression shift_expression additive_expression
%type <node> multiplicative_expression unary_expression postfix_expression
%type <node> primary_expression literal list_literal map_literal
%type <node> identifier argument_list

/* Operator precedence */
%left COMMA
%right ASSIGN PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIVIDE_ASSIGN
%right QUESTION COLON
%left OR
%left AND
%left BIT_OR
%left BIT_XOR
%left BIT_AND
%left EQ NE
%left LT GT LE GE IS AS
%left LSHIFT RSHIFT URSHIFT
%left PLUS MINUS
%left TIMES DIVIDE MODULO INT_DIVIDE
%right NOT BIT_NOT INCREMENT DECREMENT
%left DOT LBRACKET LPAREN

%%

program:
	compilation_unit
		{ ast_root = $1; }
	;

compilation_unit:
	/* empty */
		{ $$ = make_node(N_PROGRAM); }
	| library_name import_or_export top_level_definition
		{ $$ = make_node(N_PROGRAM); }
	;

library_name:
	LIBRARY identifier SEMICOLON
		{ $$ = make_node(N_LIBRARY); }
	| /* empty */
		{ $$ = NULL; }
	;

import_or_export:
	/* empty */
		{ $$ = NULL; }
	| import_or_export import_directive
		{ $$ = $1; }
	| import_or_export export_directive
		{ $$ = $1; }
	;

import_directive:
	IMPORT STRING_LITERAL SEMICOLON
		{ $$ = make_node(N_IMPORT); }
	| IMPORT STRING_LITERAL AS identifier SEMICOLON
		{ $$ = make_node(N_IMPORT); }
	;

export_directive:
	EXPORT STRING_LITERAL SEMICOLON
		{ $$ = make_node(N_IMPORT); }
	;

part_directive:
	PART STRING_LITERAL SEMICOLON
		{ $$ = NULL; }
	;

part_of_directive:
	PART OF identifier SEMICOLON
		{ $$ = NULL; }
	;

metadata:
	/* empty */
		{ $$ = NULL; }
	| metadata annotation
		{ $$ = $1; }
	;

annotation:
	AT identifier
		{ $$ = NULL; }
	| AT identifier DOT identifier
		{ $$ = NULL; }
	| AT identifier LPAREN argument_list RPAREN
		{ $$ = NULL; }
	;

top_level_definition:
	/* empty */
		{ $$ = NULL; }
	| top_level_definition class_definition
		{ $$ = $1; }
	| top_level_definition function_definition
		{ $$ = $1; }
	| top_level_definition variable_declaration
		{ $$ = $1; }
	| top_level_definition type_alias
		{ $$ = $1; }
	;

class_definition:
	CLASS identifier class_body
		{ $$ = make_node(N_CLASS); }
	| CLASS identifier type_parameters class_body
		{ $$ = make_node(N_CLASS); }
	| CLASS identifier EXTENDS type class_body
		{ $$ = make_node(N_CLASS); }
	| CLASS identifier EXTENDS type IMPLEMENTS type class_body
		{ $$ = make_node(N_CLASS); }
	| ABSTRACT CLASS identifier class_body
		{ $$ = make_node(N_CLASS); }
	;

class_body:
	LBRACE RBRACE
		{ $$ = NULL; }
	| LBRACE class_member RBRACE
		{ $$ = $2; }
	;

class_member:
	/* empty */
		{ $$ = NULL; }
	| class_member field_declaration
		{ $$ = $1; }
	| class_member method_declaration
		{ $$ = $1; }
	| class_member constructor_declaration
		{ $$ = $1; }
	| class_member getter_declaration
		{ $$ = $1; }
	| class_member setter_declaration
		{ $$ = $1; }
	;

field_declaration:
	type identifier SEMICOLON
		{ $$ = make_node(N_FIELD); }
	| FINAL type identifier ASSIGN expression SEMICOLON
		{ $$ = make_node(N_FIELD); }
	| VAR identifier ASSIGN expression SEMICOLON
		{ $$ = make_node(N_FIELD); }
	| STATIC type identifier SEMICOLON
		{ $$ = make_node(N_FIELD); }
	;

method_declaration:
	type identifier LPAREN formal_parameter_list RPAREN function_body
		{ $$ = make_node(N_METHOD); }
	| VOID identifier LPAREN formal_parameter_list RPAREN function_body
		{ $$ = make_node(N_METHOD); }
	| STATIC type identifier LPAREN formal_parameter_list RPAREN function_body
		{ $$ = make_node(N_METHOD); }
	;

constructor_declaration:
	identifier LPAREN formal_parameter_list RPAREN function_body
		{ $$ = make_node(N_CONSTRUCTOR); }
	| identifier DOT identifier LPAREN formal_parameter_list RPAREN function_body
		{ $$ = make_node(N_CONSTRUCTOR); }
	;

getter_declaration:
	type GET identifier function_body
		{ $$ = make_node(N_METHOD); }
	;

setter_declaration:
	VOID SET identifier LPAREN formal_parameter RPAREN function_body
		{ $$ = make_node(N_METHOD); }
	;

function_definition:
	type identifier LPAREN formal_parameter_list RPAREN function_body
		{ $$ = make_node(N_FUNCTION); }
	| VOID identifier LPAREN formal_parameter_list RPAREN function_body
		{ $$ = make_node(N_FUNCTION); }
	;

variable_declaration:
	type identifier SEMICOLON
		{ $$ = make_node(N_VARIABLE); }
	| type identifier ASSIGN expression SEMICOLON
		{ $$ = make_node(N_VARIABLE); }
	| VAR identifier ASSIGN expression SEMICOLON
		{ $$ = make_node(N_VARIABLE); }
	| FINAL type identifier ASSIGN expression SEMICOLON
		{ $$ = make_node(N_VARIABLE); }
	| CONST type identifier ASSIGN expression SEMICOLON
		{ $$ = make_node(N_VARIABLE); }
	;

type_alias:
	TYPEDEF type identifier SEMICOLON
		{ $$ = NULL; }
	| TYPEDEF type identifier type_parameters SEMICOLON
		{ $$ = NULL; }
	;

type:
	type_name
		{ $$ = $1; }
	| type_name type_arguments
		{ $$ = $1; }
	| type_name QUESTION
		{ $$ = $1; }
	| VOID
		{ $$ = NULL; }
	| DYNAMIC
		{ $$ = NULL; }
	;

type_name:
	identifier
		{ $$ = $1; }
	| INT
		{ $$ = NULL; }
	| DOUBLE
		{ $$ = NULL; }
	| BOOL
		{ $$ = NULL; }
	| STRING
		{ $$ = NULL; }
	| NUM
		{ $$ = NULL; }
	| LIST
		{ $$ = NULL; }
	| MAP
		{ $$ = NULL; }
	| OBJECT
		{ $$ = NULL; }
	| FUNCTION
		{ $$ = NULL; }
	;

type_arguments:
	LT type GT
		{ $$ = NULL; }
	| LT type COMMA type GT
		{ $$ = NULL; }
	;

type_argument:
	type
		{ $$ = $1; }
	;

type_parameter:
	identifier
		{ $$ = $1; }
	| identifier EXTENDS type
		{ $$ = $1; }
	;

type_parameters:
	LT type_parameter GT
		{ $$ = NULL; }
	| LT type_parameter COMMA type_parameter GT
		{ $$ = NULL; }
	;

formal_parameter_list:
	/* empty */
		{ $$ = NULL; }
	| formal_parameter
		{ $$ = $1; }
	| formal_parameter_list COMMA formal_parameter
		{ $$ = $1; }
	;

formal_parameter:
	type identifier
		{ $$ = make_node(N_PARAMETER); }
	| FINAL type identifier
		{ $$ = make_node(N_PARAMETER); }
	| THIS DOT identifier
		{ $$ = make_node(N_PARAMETER); }
	;

named_parameter:
	type identifier
		{ $$ = make_node(N_PARAMETER); }
	| type identifier ASSIGN expression
		{ $$ = make_node(N_PARAMETER); }
	| REQUIRED type identifier
		{ $$ = make_node(N_PARAMETER); }
	;

optional_parameter:
	LBRACKET formal_parameter RBRACKET
		{ $$ = $2; }
	| LBRACKET formal_parameter ASSIGN expression RBRACKET
		{ $$ = $2; }
	;

function_body:
	block_statement
		{ $$ = $1; }
	| ARROW expression SEMICOLON
		{ $$ = make_node(N_RETURN); }
	| SEMICOLON
		{ $$ = NULL; }
	;

block_statement:
	LBRACE RBRACE
		{ $$ = make_node(N_BLOCK); }
	| LBRACE statement RBRACE
		{ $$ = $2; }
	;

statement:
	/* empty */
		{ $$ = NULL; }
	| statement simple_statement
		{ $$ = $1; }
	| statement if_statement
		{ $$ = $1; }
	| statement for_statement
		{ $$ = $1; }
	| statement while_statement
		{ $$ = $1; }
	| statement do_statement
		{ $$ = $1; }
	| statement switch_statement
		{ $$ = $1; }
	| statement try_statement
		{ $$ = $1; }
	| statement block_statement
		{ $$ = $1; }
	;

simple_statement:
	expression_statement
		{ $$ = $1; }
	| local_variable_declaration
		{ $$ = $1; }
	| return_statement
		{ $$ = $1; }
	| break_statement
		{ $$ = $1; }
	| continue_statement
		{ $$ = $1; }
	| throw_statement
		{ $$ = $1; }
	| assert_statement
		{ $$ = $1; }
	;

expression_statement:
	expression SEMICOLON
		{ $$ = $1; }
	;

local_variable_declaration:
	type identifier SEMICOLON
		{ $$ = make_node(N_VARIABLE); }
	| type identifier ASSIGN expression SEMICOLON
		{ $$ = make_node(N_VARIABLE); }
	| VAR identifier ASSIGN expression SEMICOLON
		{ $$ = make_node(N_VARIABLE); }
	| FINAL type identifier ASSIGN expression SEMICOLON
		{ $$ = make_node(N_VARIABLE); }
	;

if_statement:
	IF LPAREN expression RPAREN statement
		{ $$ = make_node(N_IF); }
	| IF LPAREN expression RPAREN statement ELSE statement
		{ $$ = make_node(N_IF); }
	;

for_statement:
	FOR LPAREN expression SEMICOLON expression SEMICOLON expression RPAREN statement
		{ $$ = make_node(N_FOR); }
	| FOR LPAREN VAR identifier IN expression RPAREN statement
		{ $$ = make_node(N_FOR); }
	| FOR LPAREN identifier IN expression RPAREN statement
		{ $$ = make_node(N_FOR); }
	;

while_statement:
	WHILE LPAREN expression RPAREN statement
		{ $$ = make_node(N_WHILE); }
	;

do_statement:
	DO statement WHILE LPAREN expression RPAREN SEMICOLON
		{ $$ = make_node(N_WHILE); }
	;

switch_statement:
	SWITCH LPAREN expression RPAREN LBRACE RBRACE
		{ $$ = NULL; }
	;

return_statement:
	RETURN SEMICOLON
		{ $$ = make_node(N_RETURN); }
	| RETURN expression SEMICOLON
		{ $$ = make_node(N_RETURN); }
	;

break_statement:
	BREAK SEMICOLON
		{ $$ = NULL; }
	| BREAK identifier SEMICOLON
		{ $$ = NULL; }
	;

continue_statement:
	CONTINUE SEMICOLON
		{ $$ = NULL; }
	| CONTINUE identifier SEMICOLON
		{ $$ = NULL; }
	;

throw_statement:
	THROW expression SEMICOLON
		{ $$ = NULL; }
	;

try_statement:
	TRY block_statement CATCH LPAREN identifier RPAREN block_statement
		{ $$ = NULL; }
	| TRY block_statement FINALLY block_statement
		{ $$ = NULL; }
	;

assert_statement:
	ASSERT LPAREN expression RPAREN SEMICOLON
		{ $$ = NULL; }
	;

expression:
	assignment_expression
		{ $$ = $1; }
	;

assignment_expression:
	conditional_expression
		{ $$ = $1; }
	| postfix_expression ASSIGN expression
		{ $$ = make_binary_op(ASSIGN, $1, $3); }
	| postfix_expression PLUS_ASSIGN expression
		{ $$ = make_binary_op(PLUS_ASSIGN, $1, $3); }
	| postfix_expression MINUS_ASSIGN expression
		{ $$ = make_binary_op(MINUS_ASSIGN, $1, $3); }
	;

conditional_expression:
	logical_or_expression
		{ $$ = $1; }
	| logical_or_expression QUESTION expression COLON expression
		{ $$ = $1; }
	;

logical_or_expression:
	logical_and_expression
		{ $$ = $1; }
	| logical_or_expression OR logical_and_expression
		{ $$ = make_binary_op(OR, $1, $3); }
	;

logical_and_expression:
	equality_expression
		{ $$ = $1; }
	| logical_and_expression AND equality_expression
		{ $$ = make_binary_op(AND, $1, $3); }
	;

equality_expression:
	relational_expression
		{ $$ = $1; }
	| equality_expression EQ relational_expression
		{ $$ = make_binary_op(EQ, $1, $3); }
	| equality_expression NE relational_expression
		{ $$ = make_binary_op(NE, $1, $3); }
	;

relational_expression:
	shift_expression
		{ $$ = $1; }
	| relational_expression LT shift_expression
		{ $$ = make_binary_op(LT, $1, $3); }
	| relational_expression GT shift_expression
		{ $$ = make_binary_op(GT, $1, $3); }
	| relational_expression LE shift_expression
		{ $$ = make_binary_op(LE, $1, $3); }
	| relational_expression GE shift_expression
		{ $$ = make_binary_op(GE, $1, $3); }
	;

bitwise_or_expression:
	bitwise_xor_expression
		{ $$ = $1; }
	| bitwise_or_expression BIT_OR bitwise_xor_expression
		{ $$ = make_binary_op(BIT_OR, $1, $3); }
	;

bitwise_xor_expression:
	bitwise_and_expression
		{ $$ = $1; }
	| bitwise_xor_expression BIT_XOR bitwise_and_expression
		{ $$ = make_binary_op(BIT_XOR, $1, $3); }
	;

bitwise_and_expression:
	shift_expression
		{ $$ = $1; }
	| bitwise_and_expression BIT_AND shift_expression
		{ $$ = make_binary_op(BIT_AND, $1, $3); }
	;

shift_expression:
	additive_expression
		{ $$ = $1; }
	| shift_expression LSHIFT additive_expression
		{ $$ = make_binary_op(LSHIFT, $1, $3); }
	| shift_expression RSHIFT additive_expression
		{ $$ = make_binary_op(RSHIFT, $1, $3); }
	;

additive_expression:
	multiplicative_expression
		{ $$ = $1; }
	| additive_expression PLUS multiplicative_expression
		{ $$ = make_binary_op(PLUS, $1, $3); }
	| additive_expression MINUS multiplicative_expression
		{ $$ = make_binary_op(MINUS, $1, $3); }
	;

multiplicative_expression:
	unary_expression
		{ $$ = $1; }
	| multiplicative_expression TIMES unary_expression
		{ $$ = make_binary_op(TIMES, $1, $3); }
	| multiplicative_expression DIVIDE unary_expression
		{ $$ = make_binary_op(DIVIDE, $1, $3); }
	| multiplicative_expression MODULO unary_expression
		{ $$ = make_binary_op(MODULO, $1, $3); }
	;

unary_expression:
	postfix_expression
		{ $$ = $1; }
	| MINUS unary_expression
		{ $$ = $2; }
	| NOT unary_expression
		{ $$ = $2; }
	| BIT_NOT unary_expression
		{ $$ = $2; }
	| INCREMENT postfix_expression
		{ $$ = $2; }
	| DECREMENT postfix_expression
		{ $$ = $2; }
	;

postfix_expression:
	primary_expression
		{ $$ = $1; }
	| postfix_expression INCREMENT
		{ $$ = $1; }
	| postfix_expression DECREMENT
		{ $$ = $1; }
	| postfix_expression DOT identifier
		{ $$ = $1; }
	| postfix_expression LBRACKET expression RBRACKET
		{ $$ = $1; }
	| postfix_expression LPAREN argument_list RPAREN
		{ $$ = make_node(N_CALL); }
	;

primary_expression:
	literal
		{ $$ = $1; }
	| identifier
		{ $$ = $1; }
	| LPAREN expression RPAREN
		{ $$ = $2; }
	| THIS
		{ $$ = NULL; }
	| SUPER
		{ $$ = NULL; }
	| list_literal
		{ $$ = $1; }
	| map_literal
		{ $$ = $1; }
	;

literal:
	INTEGER_LITERAL
		{ $$ = make_literal_int($1); }
	| DOUBLE_LITERAL
		{ $$ = make_literal_double($1); }
	| STRING_LITERAL
		{ $$ = make_literal_string($1); }
	| TRUE
		{ $$ = make_literal_int(1); }
	| FALSE
		{ $$ = make_literal_int(0); }
	| NULL_LIT
		{ $$ = NULL; }
	;

list_literal:
	LBRACKET RBRACKET
		{ $$ = NULL; }
	| LBRACKET expression RBRACKET
		{ $$ = $2; }
	;

map_literal:
	LBRACE RBRACE
		{ $$ = NULL; }
	;

identifier:
	IDENTIFIER
		{ $$ = make_identifier($1); }
	;

argument_list:
	/* empty */
		{ $$ = NULL; }
	| expression
		{ $$ = $1; }
	| argument_list COMMA expression
		{ $$ = $1; }
	;

%%
