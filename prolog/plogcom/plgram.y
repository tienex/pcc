/*
 * Copyright (c) 2025 PCC Prolog Compiler
 *
 * Parser grammar for Prolog
 * Compatible with Turbo Prolog and GNU Prolog
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Forward declarations */
int yylex(void);
void yyerror(const char *s);

/* External variables */
extern int lineno;
extern int current_column;

%}

%union {
	long ival;
	double dval;
	char *str;
	struct term *term;
	struct clause *clause;
	struct termlist *termlist;
	struct pred_decl *pred_decl;
	struct domain_decl *domain_decl;
}

/* Keywords - Turbo Prolog specific */
%token DOMAINS PREDICATES CLAUSES GOAL DATABASE CONSTANTS

/* Keywords - GNU Prolog / ISO specific */
%token DYNAMIC MULTIFILE DISCONTIGUOUS MODULE USE_MODULE PUBLIC META_PREDICATE OP

/* Control and logic */
%token IF QUERY CUT

/* Operators */
%token UNIFY NOTUNIFY EQUAL NOTEQUAL
%token TERMLESS TERMLESSEQ TERMGREATER TERMGREATEREQ
%token LESS GREATER LESSEQ GREATEREQ
%token ARITHEQUAL ARITHNOTEQ
%token IS MOD REM DIV INTDIV
%token PLUS MINUS MULTIPLY DIVIDE POWER
%token SHIFTLEFT SHIFTRIGHT
%token BITOR BITAND BITNOT BITXOR
%token ARROW

/* Delimiters */
%token COMMA SEMICOLON DOT PIPE
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE

/* Built-in predicates */
%token TRUE FAIL CALL
%token ASSERTA ASSERTZ RETRACT RETRACTALL
%token FINDALL BAGOF SETOF
%token FUNCTOR ARG COPY_TERM
%token ATOM_BUILTIN NUMBER_BUILTIN VAR NONVAR INTEGER FLOAT COMPOUND ATOMIC
%token WRITE WRITELN READ GET PUT NL HALT ABORT

/* Terminals */
%token <str> ATOM VARIABLE ANONYMOUS STRING_LIT
%token <ival> INTEGER_CONST
%token <dval> FLOAT_CONST

/* Non-terminals */
%type <clause> program clause fact rule query
%type <clause> clause_list turbo_clause gnu_clause
%type <term> term simple_term compound_term list list_elements
%type <term> goal goal_sequence body
%type <termlist> term_list term_list_ne argument_list
%type <pred_decl> predicate_declaration
%type <domain_decl> domain_declaration

/* Operator precedence (Prolog standard) */
%right IF                    /* 1200 - :- */
%left SEMICOLON              /* 1100 - ; (disjunction) */
%left ARROW                  /* 1050 - -> (if-then) */
%left COMMA                  /* 1000 - , (conjunction) */
%nonassoc UNIFY NOTUNIFY EQUAL NOTEQUAL TERMLESS TERMLESSEQ TERMGREATER TERMGREATEREQ /* 700 */
%nonassoc IS ARITHEQUAL ARITHNOTEQ LESS LESSEQ GREATER GREATEREQ  /* 700 */
%left PLUS MINUS BITOR       /* 500 */
%left MULTIPLY DIVIDE DIV MOD REM BITAND SHIFTLEFT SHIFTRIGHT  /* 400 */
%right POWER                 /* 200 */
%nonassoc BITNOT             /* 200 */

%%

/* Top-level program structure */
program:
	  /* Empty program */
		{ $$ = NULL; }
	| turbo_prolog_program
		{ /* Turbo Prolog style program */ }
	| gnu_prolog_program
		{ /* GNU Prolog / ISO style program */ }
	;

/* Turbo Prolog program structure */
turbo_prolog_program:
	  turbo_sections
	;

turbo_sections:
	  turbo_section
	| turbo_sections turbo_section
	;

turbo_section:
	  domains_section
	| predicates_section
	| clauses_section
	| goal_section
	| database_section
	| constants_section
	;

domains_section:
	  DOMAINS domain_declarations
		{ /* Define domains (types) */ }
	;

domain_declarations:
	  /* empty */
	| domain_declarations domain_declaration
	;

domain_declaration:
	  ATOM
		{ /* Simple domain */ }
	| ATOM UNIFY domain_definition
		{ /* Domain with definition */ }
	;

domain_definition:
	  ATOM
		{ /* Basic type */ }
	| compound_domain
		{ /* Compound domain (e.g., list, structure) */ }
	;

compound_domain:
	  ATOM LPAREN domain_list RPAREN
	;

domain_list:
	  ATOM
	| domain_list COMMA ATOM
	;

predicates_section:
	  PREDICATES predicate_declarations
		{ /* Declare predicates with their signatures */ }
	;

predicate_declarations:
	  /* empty */
	| predicate_declarations predicate_declaration
	;

predicate_declaration:
	  ATOM
		{ /* Predicate with no arguments */ }
	| ATOM LPAREN domain_list RPAREN
		{ /* Predicate with typed arguments */ }
	;

clauses_section:
	  CLAUSES clause_list
		{ /* Define clauses */ }
	;

goal_section:
	  GOAL goal_sequence DOT
		{ /* Initial goal (query) */ }
	;

database_section:
	  DATABASE predicate_declarations
		{ /* Dynamic predicates */ }
	;

constants_section:
	  CONSTANTS constant_declarations
		{ /* Define constants */ }
	;

constant_declarations:
	  /* empty */
	| constant_declarations constant_declaration
	;

constant_declaration:
	  ATOM UNIFY term
	;

/* GNU Prolog / ISO Prolog program structure */
gnu_prolog_program:
	  clause_list
		{ /* Standard Prolog: just a list of clauses */ }
	;

clause_list:
	  clause
		{ $$ = $1; }
	| clause_list clause
		{ /* Link clauses together */ }
	;

clause:
	  fact
		{ $$ = $1; }
	| rule
		{ $$ = $1; }
	| query
		{ $$ = $1; }
	| directive
		{ /* Directive */ }
	;

/* Directives - GNU Prolog / ISO */
directive:
	  IF term DOT
		{ /* Directive: :- directive. */ }
	| IF DYNAMIC term DOT
		{ /* Dynamic predicate declaration */ }
	| IF MULTIFILE term DOT
		{ /* Multifile predicate */ }
	| IF DISCONTIGUOUS term DOT
		{ /* Discontiguous predicate */ }
	| IF MODULE LPAREN ATOM COMMA list RPAREN DOT
		{ /* Module declaration */ }
	| IF USE_MODULE LPAREN term RPAREN DOT
		{ /* Use module */ }
	| IF OP LPAREN term COMMA term COMMA term RPAREN DOT
		{ /* Operator definition */ }
	| IF META_PREDICATE term DOT
		{ /* Meta-predicate declaration */ }
	;

/* Facts */
fact:
	  term DOT
		{ /* Create fact clause */ }
	;

/* Rules */
rule:
	  term IF body DOT
		{ /* Create rule: head :- body. */ }
	;

/* Queries */
query:
	  QUERY body DOT
		{ /* Query: ?- goal. */ }
	;

/* Body of a rule (goal sequence) */
body:
	  goal_sequence
		{ $$ = $1; }
	;

goal_sequence:
	  goal
		{ $$ = $1; }
	| goal COMMA goal_sequence
		{ /* Conjunction */ }
	| goal SEMICOLON goal_sequence
		{ /* Disjunction */ }
	| goal ARROW goal_sequence
		{ /* If-then: goal -> then_part */ }
	| goal ARROW goal_sequence SEMICOLON goal_sequence
		{ /* If-then-else: goal -> then_part ; else_part */ }
	| LPAREN goal_sequence RPAREN
		{ $$ = $2; }
	;

goal:
	  term
		{ $$ = $1; }
	| CUT
		{ /* Cut (!) */ }
	| TRUE
		{ /* Always succeeds */ }
	| FAIL
		{ /* Always fails */ }
	| CALL LPAREN term RPAREN
		{ /* Meta-call */ }
	;

/* Terms */
term:
	  simple_term
		{ $$ = $1; }
	| compound_term
		{ $$ = $1; }
	| list
		{ $$ = $1; }
	| term UNIFY term
		{ /* Unification */ }
	| term NOTUNIFY term
		{ /* Not unifiable */ }
	| term EQUAL term
		{ /* Equality */ }
	| term NOTEQUAL term
		{ /* Not equal */ }
	| term IS term
		{ /* Arithmetic evaluation */ }
	| term ARITHEQUAL term
		{ /* Arithmetic equality */ }
	| term ARITHNOTEQ term
		{ /* Arithmetic inequality */ }
	| term LESS term
		{ /* Arithmetic less than */ }
	| term LESSEQ term
		{ /* Arithmetic less or equal */ }
	| term GREATER term
		{ /* Arithmetic greater than */ }
	| term GREATEREQ term
		{ /* Arithmetic greater or equal */ }
	| term TERMLESS term
		{ /* Term less than */ }
	| term TERMLESSEQ term
		{ /* Term less or equal */ }
	| term TERMGREATER term
		{ /* Term greater than */ }
	| term TERMGREATEREQ term
		{ /* Term greater or equal */ }
	| term PLUS term
		{ /* Arithmetic addition */ }
	| term MINUS term
		{ /* Arithmetic subtraction */ }
	| term MULTIPLY term
		{ /* Arithmetic multiplication */ }
	| term DIVIDE term
		{ /* Arithmetic division */ }
	| term DIV term
		{ /* Integer division */ }
	| term INTDIV term
		{ /* Integer division (//) */ }
	| term MOD term
		{ /* Modulo */ }
	| term REM term
		{ /* Remainder */ }
	| term POWER term
		{ /* Power */ }
	| term SHIFTLEFT term
		{ /* Bit shift left */ }
	| term SHIFTRIGHT term
		{ /* Bit shift right */ }
	| term BITAND term
		{ /* Bitwise and */ }
	| term BITOR term
		{ /* Bitwise or */ }
	| term BITXOR term
		{ /* Bitwise xor */ }
	| BITNOT term
		{ /* Bitwise not */ }
	| MINUS term %prec BITNOT
		{ /* Unary minus */ }
	| PLUS term %prec BITNOT
		{ /* Unary plus */ }
	| LPAREN term RPAREN
		{ $$ = $2; }
	;

simple_term:
	  ATOM
		{ /* Atom */ }
	| VARIABLE
		{ /* Variable */ }
	| ANONYMOUS
		{ /* Anonymous variable (_) */ }
	| INTEGER_CONST
		{ /* Integer constant */ }
	| FLOAT_CONST
		{ /* Float constant */ }
	| STRING_LIT
		{ /* String literal */ }
	;

compound_term:
	  ATOM LPAREN argument_list RPAREN
		{ /* Compound term: functor(args) */ }
	| LBRACE term RBRACE
		{ /* Curly brackets term: {term} */ }
	;

argument_list:
	  term_list_ne
		{ $$ = $1; }
	;

term_list_ne:
	  term
		{ /* Non-empty term list */ }
	| term COMMA term_list_ne
		{ /* Multiple terms */ }
	;

term_list:
	  /* empty */
		{ $$ = NULL; }
	| term_list_ne
		{ $$ = $1; }
	;

/* Lists */
list:
	  LBRACK RBRACK
		{ /* Empty list */ }
	| LBRACK list_elements RBRACK
		{ /* List with elements */ }
	;

list_elements:
	  term
		{ /* Single element list */ }
	| term COMMA list_elements
		{ /* Multiple elements */ }
	| term PIPE term
		{ /* List with tail: [Head|Tail] */ }
	| term COMMA list_elements PIPE term
		{ /* [H1, H2, ...|Tail] */ }
	;

%%

void yyerror(const char *s) {
	fprintf(stderr, "Error at line %d, column %d: %s\n",
	        lineno, current_column, s);
}
