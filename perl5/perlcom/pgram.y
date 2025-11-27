%{
/*	$Id$	*/
/*
 * Perl 5 Compiler - Parser Grammar
 */

#include "pass1.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int yylex(void);
void yyerror(const char *s);

/* Global state */
int perl_nerrors = 0;
int perl_scope_level = 0;
struct perl_symtab *perl_symtab_head = NULL;

%}

%union {
	int intval;
	double floatval;
	char *string;
	void *node;
	void *list;
}

/* Keywords */
%token SUB MY OUR LOCAL STATE
%token IF ELSIF ELSE UNLESS
%token WHILE UNTIL FOR FOREACH DO
%token NEXT LAST REDO RETURN
%token USE REQUIRE PACKAGE
%token BEGIN_BLOCK END_BLOCK
%token EVAL DEFINED UNDEF

/* Built-in functions */
%token PRINT PRINTF SAY
%token PUSH POP SHIFT UNSHIFT
%token KEYS VALUES EACH DELETE EXISTS
%token JOIN SPLIT GREP MAP SORT REVERSE
%token LENGTH SUBSTR INDEX RINDEX
%token CHOMP CHOP
%token OPEN CLOSE READ WRITE
%token DIE WARN

/* Literals and identifiers */
%token <intval> INTEGER
%token <floatval> FLOAT
%token <string> STRING REGEX_PATTERN
%token <string> IDENT
%token <string> SCALAR ARRAY HASH SUBNAME GLOB

/* Operators */
%token MATCH_OP NOT_MATCH_OP
%token EQ_OP NE_OP LE_OP GE_OP CMP_OP
%token STR_EQ STR_NE STR_LT STR_GT STR_LE STR_GE STR_CMP
%token AND_OP OR_OP NOT_OP XOR_OP
%token INC_OP DEC_OP
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token AND_ASSIGN OR_ASSIGN XOR_ASSIGN
%token SHL_ASSIGN SHR_ASSIGN CONCAT_ASSIGN
%token POW_OP SHL_OP SHR_OP
%token ARROW DEREF RANGE ELLIPSIS

%type <node> program statement statement_list expression
%type <node> scalar_expr array_expr hash_expr
%type <node> sub_definition sub_call
%type <node> if_statement while_statement for_statement foreach_statement
%type <node> assignment conditional_expr logical_expr
%type <node> relational_expr additive_expr multiplicative_expr
%type <node> unary_expr postfix_expr primary_expr
%type <node> variable literal
%type <list> argument_list expression_list parameter_list

/* Operator precedence (lowest to highest) */
%right '=' ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN CONCAT_ASSIGN
%right '?' ':'
%left OR_OP
%left AND_OP
%left '|'
%left '^'
%left '&'
%nonassoc EQ_OP NE_OP STR_EQ STR_NE
%nonassoc '<' '>' LE_OP GE_OP STR_LT STR_GT STR_LE STR_GE
%nonassoc CMP_OP STR_CMP
%left SHL_OP SHR_OP
%left '+' '-' '.'
%left '*' '/' '%'
%left MATCH_OP NOT_MATCH_OP
%right '!' '~' NOT_OP
%right POW_OP
%left INC_OP DEC_OP
%left DEREF
%nonassoc UMINUS UPLUS

%%

program:
	  /* empty */
	| statement_list
	;

statement_list:
	  statement
	| statement_list statement
	;

statement:
	  expression ';'
	| sub_definition
	| if_statement
	| while_statement
	| for_statement
	| foreach_statement
	| NEXT ';'			{ /* Generate next statement */ }
	| LAST ';'			{ /* Generate last statement */ }
	| REDO ';'			{ /* Generate redo statement */ }
	| RETURN ';'			{ /* Generate return with no value */ }
	| RETURN expression ';'		{ /* Generate return with value */ }
	| USE IDENT ';'			{ /* Handle use directive */ }
	| PACKAGE IDENT ';'		{ /* Handle package declaration */ }
	| BEGIN_BLOCK '{' statement_list '}'	{ /* BEGIN block */ }
	| END_BLOCK '{' statement_list '}'	{ /* END block */ }
	| ';'				{ /* Empty statement */ }
	;

/* Subroutine definition */
sub_definition:
	  SUB IDENT '{' statement_list '}'
		{ perl_enter_scope(); /* process sub body */ perl_leave_scope(); }
	| SUB IDENT '(' parameter_list ')' '{' statement_list '}'
		{ perl_enter_scope(); /* process sub with params */ perl_leave_scope(); }
	;

parameter_list:
	  MY SCALAR			{ /* Add parameter */ }
	| parameter_list ',' MY SCALAR	{ /* Add to parameter list */ }
	;

/* Control structures */
if_statement:
	  IF '(' expression ')' '{' statement_list '}'
	| IF '(' expression ')' '{' statement_list '}' ELSE '{' statement_list '}'
	| IF '(' expression ')' '{' statement_list '}' elsif_chain
	| IF '(' expression ')' '{' statement_list '}' elsif_chain ELSE '{' statement_list '}'
	| UNLESS '(' expression ')' '{' statement_list '}'
	;

elsif_chain:
	  ELSIF '(' expression ')' '{' statement_list '}'
	| elsif_chain ELSIF '(' expression ')' '{' statement_list '}'
	;

while_statement:
	  WHILE '(' expression ')' '{' statement_list '}'
	| UNTIL '(' expression ')' '{' statement_list '}'
	| DO '{' statement_list '}' WHILE '(' expression ')' ';'
	| DO '{' statement_list '}' UNTIL '(' expression ')' ';'
	;

for_statement:
	  FOR '(' expression ';' expression ';' expression ')' '{' statement_list '}'
	| FOR '(' ';' expression ';' expression ')' '{' statement_list '}'
	| FOR '(' expression ';' ';' expression ')' '{' statement_list '}'
	| FOR '(' ';' ';' expression ')' '{' statement_list '}'
	;

foreach_statement:
	  FOREACH MY SCALAR '(' expression ')' '{' statement_list '}'
	| FOREACH SCALAR '(' expression ')' '{' statement_list '}'
	| FOR MY SCALAR '(' expression ')' '{' statement_list '}'
	| FOR SCALAR '(' expression ')' '{' statement_list '}'
	;

/* Expressions */
expression:
	  assignment
	| conditional_expr
	;

assignment:
	  variable '=' expression		{ $$ = perl_binop_node('=', $1, $3); }
	| variable ADD_ASSIGN expression	{ $$ = perl_binop_node(ADD_ASSIGN, $1, $3); }
	| variable SUB_ASSIGN expression	{ $$ = perl_binop_node(SUB_ASSIGN, $1, $3); }
	| variable MUL_ASSIGN expression	{ $$ = perl_binop_node(MUL_ASSIGN, $1, $3); }
	| variable DIV_ASSIGN expression	{ $$ = perl_binop_node(DIV_ASSIGN, $1, $3); }
	| variable MOD_ASSIGN expression	{ $$ = perl_binop_node(MOD_ASSIGN, $1, $3); }
	| variable CONCAT_ASSIGN expression	{ $$ = perl_binop_node(CONCAT_ASSIGN, $1, $3); }
	| MY SCALAR '=' expression		{ /* Declare and assign scalar */ }
	| MY ARRAY '=' expression		{ /* Declare and assign array */ }
	| MY HASH '=' expression		{ /* Declare and assign hash */ }
	;

conditional_expr:
	  logical_expr
	| logical_expr '?' expression ':' conditional_expr
	;

logical_expr:
	  relational_expr
	| logical_expr AND_OP relational_expr	{ $$ = perl_binop_node(AND_OP, $1, $3); }
	| logical_expr OR_OP relational_expr	{ $$ = perl_binop_node(OR_OP, $1, $3); }
	| logical_expr XOR_OP relational_expr	{ $$ = perl_binop_node(XOR_OP, $1, $3); }
	;

relational_expr:
	  additive_expr
	| relational_expr '<' additive_expr	{ $$ = perl_binop_node('<', $1, $3); }
	| relational_expr '>' additive_expr	{ $$ = perl_binop_node('>', $1, $3); }
	| relational_expr LE_OP additive_expr	{ $$ = perl_binop_node(LE_OP, $1, $3); }
	| relational_expr GE_OP additive_expr	{ $$ = perl_binop_node(GE_OP, $1, $3); }
	| relational_expr EQ_OP additive_expr	{ $$ = perl_binop_node(EQ_OP, $1, $3); }
	| relational_expr NE_OP additive_expr	{ $$ = perl_binop_node(NE_OP, $1, $3); }
	| relational_expr STR_EQ additive_expr	{ $$ = perl_binop_node(STR_EQ, $1, $3); }
	| relational_expr STR_NE additive_expr	{ $$ = perl_binop_node(STR_NE, $1, $3); }
	| relational_expr STR_LT additive_expr	{ $$ = perl_binop_node(STR_LT, $1, $3); }
	| relational_expr STR_GT additive_expr	{ $$ = perl_binop_node(STR_GT, $1, $3); }
	| relational_expr STR_LE additive_expr	{ $$ = perl_binop_node(STR_LE, $1, $3); }
	| relational_expr STR_GE additive_expr	{ $$ = perl_binop_node(STR_GE, $1, $3); }
	| relational_expr CMP_OP additive_expr	{ $$ = perl_binop_node(CMP_OP, $1, $3); }
	| relational_expr STR_CMP additive_expr	{ $$ = perl_binop_node(STR_CMP, $1, $3); }
	| relational_expr MATCH_OP additive_expr	{ $$ = perl_binop_node(MATCH_OP, $1, $3); }
	| relational_expr NOT_MATCH_OP additive_expr	{ $$ = perl_binop_node(NOT_MATCH_OP, $1, $3); }
	;

additive_expr:
	  multiplicative_expr
	| additive_expr '+' multiplicative_expr	{ $$ = perl_binop_node('+', $1, $3); }
	| additive_expr '-' multiplicative_expr	{ $$ = perl_binop_node('-', $1, $3); }
	| additive_expr '.' multiplicative_expr	{ $$ = perl_binop_node('.', $1, $3); }
	;

multiplicative_expr:
	  unary_expr
	| multiplicative_expr '*' unary_expr	{ $$ = perl_binop_node('*', $1, $3); }
	| multiplicative_expr '/' unary_expr	{ $$ = perl_binop_node('/', $1, $3); }
	| multiplicative_expr '%' unary_expr	{ $$ = perl_binop_node('%', $1, $3); }
	| multiplicative_expr POW_OP unary_expr	{ $$ = perl_binop_node(POW_OP, $1, $3); }
	;

unary_expr:
	  postfix_expr
	| '+' unary_expr %prec UPLUS	{ $$ = perl_unop_node('+', $2); }
	| '-' unary_expr %prec UMINUS	{ $$ = perl_unop_node('-', $2); }
	| '!' unary_expr		{ $$ = perl_unop_node('!', $2); }
	| '~' unary_expr		{ $$ = perl_unop_node('~', $2); }
	| NOT_OP unary_expr		{ $$ = perl_unop_node(NOT_OP, $2); }
	| INC_OP variable		{ $$ = perl_unop_node(INC_OP, $2); }
	| DEC_OP variable		{ $$ = perl_unop_node(DEC_OP, $2); }
	| DEFINED '(' expression ')'	{ $$ = perl_unop_node(DEFINED, $3); }
	;

postfix_expr:
	  primary_expr
	| variable INC_OP		{ $$ = perl_unop_node(INC_OP, $1); }
	| variable DEC_OP		{ $$ = perl_unop_node(DEC_OP, $1); }
	| SCALAR '[' expression ']'	{ /* Array element access */ }
	| ARRAY '[' expression ']'	{ /* Array element access */ }
	| HASH '{' expression '}'	{ /* Hash element access */ }
	| sub_call
	;

primary_expr:
	  variable
	| literal
	| '(' expression ')'		{ $$ = $2; }
	| '[' expression_list ']'	{ /* Array constructor */ }
	| '{' expression_list '}'	{ /* Hash constructor */ }
	| sub_call
	;

sub_call:
	  IDENT '(' argument_list ')'	{ /* Subroutine call with args */ }
	| IDENT '(' ')'			{ /* Subroutine call without args */ }
	| IDENT				{ /* Bareword - could be sub call */ }
	| PRINT argument_list		{ perl_builtin_print($2); }
	| PRINT				{ /* Print $_ */ }
	| PUSH '(' ARRAY ',' expression_list ')'	{ perl_builtin_push(NULL, $5); }
	| POP '(' ARRAY ')'		{ perl_builtin_pop(NULL); }
	| SHIFT '(' ARRAY ')'		{ perl_builtin_shift(NULL); }
	| KEYS '(' HASH ')'		{ /* keys function */ }
	| VALUES '(' HASH ')'		{ /* values function */ }
	| LENGTH '(' expression ')'	{ /* length function */ }
	;

argument_list:
	  expression
	| argument_list ',' expression
	;

expression_list:
	  expression
	| expression_list ',' expression
	;

variable:
	  SCALAR			{ $$ = perl_scalar_node($1); }
	| ARRAY				{ $$ = perl_array_node($1); }
	| HASH				{ $$ = perl_hash_node($1); }
	;

scalar_expr:
	  SCALAR			{ $$ = perl_scalar_node($1); }
	| SCALAR '[' expression ']'	{ /* Array subscript */ }
	| HASH '{' expression '}'	{ /* Hash subscript */ }
	;

array_expr:
	  ARRAY				{ $$ = perl_array_node($1); }
	| '@' '{' expression '}'	{ /* Array slice */ }
	;

hash_expr:
	  HASH				{ $$ = perl_hash_node($1); }
	| '%' '{' expression '}'	{ /* Hash slice */ }
	;

literal:
	  INTEGER			{ $$ = perl_const_node(INT, &$1); }
	| FLOAT				{ $$ = perl_const_node(FLOAT, &$1); }
	| STRING			{ $$ = perl_const_node(PERLSV, $1); }
	| REGEX_PATTERN			{ $$ = perl_regex_compile($1, ""); }
	;

%%

void yyerror(const char *s) {
	perl_error("%s at line %d", s, perl_lineno);
}
