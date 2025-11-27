/*
 * Copyright (c) 2025 PCC Pascal Compiler
 *
 * Parser grammar for Pascal
 * Supports multiple dialects: ISO, Borland, Delphi, FreePascal, etc.
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
	double dval;
	char *sval;
	struct symtab *symptr;
	struct tnode *typeptr;
}

/* Keywords */
%token AND ARRAY BBEGIN CASE CONST DIV DO DOWNTO ELSE END FFILE FOR
%token FUNCTION GOTO IF IN LABEL MOD NIL NOT OF OR PACKED PROCEDURE
%token PROGRAM RECORD REPEAT SET THEN TO TYPE UNTIL VAR WHILE WITH

/* Extended keywords (dialect-specific) */
%token UNIT USES INTERFACE IMPLEMENTATION MODULE IMPORT EXPORT
%token OBJECT CLASS PROPERTY INHERITED CONSTRUCTOR DESTRUCTOR
%token VIRTUAL OVERRIDE BREAK CONTINUE ASM INLINE FORWARD EXTERNAL
%token ABSOLUTE STRING

/* Operators */
%token ASSIGN DOTDOT NE LE GE EQ LT GT
%token PLUS MINUS STAR SLASH UPARROW AT
%token DOT COMMA COLON SEMICOLON
%token LPAREN RPAREN LBRACK RBRACK

/* Literals */
%token <ival> INTCONST
%token <dval> REALCONST
%token <sval> STRINGCONST
%token <sval> IDENT

/* Type declarations */
%type <typeptr> type_denoter simple_type structured_type
%type <typeptr> array_type record_type pointer_type set_type
%type <symptr> identifier

/* Precedence and associativity */
%left OR
%left AND
%left EQ NE LT LE GT GE IN
%left PLUS MINUS
%left STAR SLASH DIV MOD
%right NOT UPARROW
%left DOT LBRACK

%%

/* Program structure */
program:
	  program_heading semicolon block DOT
		{ /* Complete program */ }
	| unit_heading semicolon interface_part implementation_part optional_init DOT
		{ /* Unit structure (Borland/Delphi) */ }
	| block DOT
		{ /* Simplified program without header */ }
	;

program_heading:
	  PROGRAM identifier
		{ if (dialect_features->require_program_header) {
			/* Program header required */
		  }
		}
	| PROGRAM identifier LPAREN identifier_list RPAREN
		{ /* Program with parameters */ }
	;

unit_heading:
	  UNIT identifier
		{ if (!ALLOW_UNITS()) {
			error("unit not allowed in %s", get_dialect_name(current_dialect));
		  }
		}
	;

interface_part:
	  INTERFACE uses_clause_opt declarations
	;

implementation_part:
	  IMPLEMENTATION uses_clause_opt declarations
	;

uses_clause_opt:
	  /* empty */
	| uses_clause
	;

uses_clause:
	  USES identifier_list semicolon
		{ if (!ALLOW_USES_CLAUSE()) {
			error("uses clause not allowed in %s", get_dialect_name(current_dialect));
		  }
		}
	;

optional_init:
	  /* empty */
	| initialization_part
	;

initialization_part:
	  BBEGIN statement_sequence END
	;

/* Block structure */
block:
	  declarations compound_statement
	;

declarations:
	  /* empty */
	| declarations declaration
	;

declaration:
	  label_declaration_part
	| const_definition_part
	| type_definition_part
	| var_declaration_part
	| procedure_declaration
	| function_declaration
	;

/* Label declarations */
label_declaration_part:
	  LABEL label_list semicolon
	;

label_list:
	  label
	| label_list COMMA label
	;

label:
	  INTCONST
	;

/* Constant definitions */
const_definition_part:
	  CONST const_definition_list
	;

const_definition_list:
	  const_definition
	| const_definition_list const_definition
	;

const_definition:
	  identifier EQ constant semicolon
		{ SYMTAB *sp = install($1->sname, CONST, blevel);
		  /* Store constant value */
		}
	;

constant:
	  INTCONST
	| REALCONST
	| STRINGCONST
	| identifier
	| PLUS INTCONST
	| MINUS INTCONST
	| PLUS REALCONST
	| MINUS REALCONST
	;

/* Type definitions */
type_definition_part:
	  TYPE type_definition_list
	;

type_definition_list:
	  type_definition
	| type_definition_list type_definition
	;

type_definition:
	  identifier EQ type_denoter semicolon
		{ SYMTAB *sp = install($1->sname, TYPEDEF, blevel);
		  sp->stype = $3;
		}
	;

type_denoter:
	  simple_type
		{ $$ = $1; }
	| structured_type
		{ $$ = $1; }
	| pointer_type
		{ $$ = $1; }
	;

simple_type:
	  identifier
		{ /* Look up type */
		  SYMTAB *sp = find_symbol($1->sname);
		  if (sp && sp->sclass == TYPEDEF)
			$$ = sp->stype;
		  else {
			error("undefined type '%s'", $1->sname);
			$$ = integer_type;
		  }
		}
	| LPAREN identifier_list RPAREN
		{ /* Enumeration type */
		  $$ = mkenum(NULL);  /* Simplified */
		}
	| constant DOTDOT constant
		{ /* Subrange type */
		  $$ = mksubrange(0, 0);  /* Simplified */
		}
	;

structured_type:
	  PACKED unpacked_structured_type
		{ $$ = $2; /* Mark as packed */ }
	| unpacked_structured_type
		{ $$ = $1; }
	;

unpacked_structured_type:
	  array_type
	| record_type
	| set_type
	| FFILE OF type_denoter
		{ $$ = mktype(TFILE); }
	;

array_type:
	  ARRAY LBRACK index_list RBRACK OF type_denoter
		{ /* Create array type */
		  int dims[10] = {0};  /* Simplified */
		  $$ = mkarray($6, dims, 1);
		}
	;

index_list:
	  simple_type
	| index_list COMMA simple_type
	;

record_type:
	  RECORD field_list END
		{ $$ = mkrecord(NULL, NULL); /* Simplified */ }
	;

field_list:
	  fixed_part
	| fixed_part semicolon variant_part
	| variant_part
	;

fixed_part:
	  record_section
	| fixed_part semicolon record_section
	;

record_section:
	  identifier_list COLON type_denoter
		{ /* Add fields to record */ }
	;

variant_part:
	  CASE tag_field OF variant_list
	;

tag_field:
	  identifier COLON identifier
	| identifier
	;

variant_list:
	  variant
	| variant_list semicolon variant
	;

variant:
	  case_label_list COLON LPAREN field_list RPAREN
	;

set_type:
	  SET OF simple_type
		{ if (!ALLOW_SET_OPERATORS()) {
			error("set types not allowed in %s", get_dialect_name(current_dialect));
		  }
		  $$ = mkset($3);
		}
	;

pointer_type:
	  UPARROW identifier
		{ $$ = mkpointer(NULL); /* Will be resolved later */ }
	;

/* Variable declarations */
var_declaration_part:
	  VAR var_declaration_list
	;

var_declaration_list:
	  var_declaration
	| var_declaration_list var_declaration
	;

var_declaration:
	  identifier_list COLON type_denoter semicolon
		{ /* Install variables in symbol table */ }
	| identifier_list COLON type_denoter ABSOLUTE INTCONST semicolon
		{ if (!ALLOW_ABSOLUTE()) {
			error("absolute directive not allowed in %s", get_dialect_name(current_dialect));
		  }
		}
	;

/* Procedure declarations */
procedure_declaration:
	  procedure_heading semicolon directive_list semicolon
		{ /* Forward declaration */ }
	| procedure_heading semicolon block semicolon
		{ /* Complete procedure */
		  hide(blevel);
		  blevel--;
		}
	;

procedure_heading:
	  PROCEDURE identifier
		{ blevel++;
		  install($2->sname, PROC, blevel-1);
		}
	| PROCEDURE identifier LPAREN formal_parameter_list RPAREN
		{ blevel++;
		  install($2->sname, PROC, blevel-1);
		}
	;

/* Function declarations */
function_declaration:
	  function_heading semicolon directive_list semicolon
		{ /* Forward declaration */ }
	| function_heading semicolon block semicolon
		{ /* Complete function */
		  hide(blevel);
		  blevel--;
		}
	;

function_heading:
	  FUNCTION identifier COLON identifier
		{ blevel++;
		  SYMTAB *sp = install($2->sname, FUNC, blevel-1);
		  /* Set return type */
		}
	| FUNCTION identifier LPAREN formal_parameter_list RPAREN COLON identifier
		{ blevel++;
		  SYMTAB *sp = install($2->sname, FUNC, blevel-1);
		}
	;

directive_list:
	  directive
	| directive_list semicolon directive
	;

directive:
	  FORWARD
		{ /* Forward directive */ }
	| EXTERNAL
		{ if (!ALLOW_EXTERNAL_DIRECTIVE()) {
			error("external directive not allowed in %s", get_dialect_name(current_dialect));
		  }
		}
	| INLINE
		{ if (!ALLOW_INLINE_DIRECTIVE()) {
			error("inline directive not allowed in %s", get_dialect_name(current_dialect));
		  }
		}
	;

formal_parameter_list:
	  formal_parameter_section
	| formal_parameter_list semicolon formal_parameter_section
	;

formal_parameter_section:
	  identifier_list COLON identifier
		{ /* Value parameters */ }
	| VAR identifier_list COLON identifier
		{ /* VAR parameters (by reference) */ }
	| CONST identifier_list COLON identifier
		{ /* CONST parameters (read-only) */ }
	;

/* Statements */
compound_statement:
	  BBEGIN statement_sequence END
	;

statement_sequence:
	  statement
	| statement_sequence semicolon statement
	;

statement:
	  /* empty */
	| label COLON unlabelled_statement
	| unlabelled_statement
	;

unlabelled_statement:
	  simple_statement
	| structured_statement
	;

simple_statement:
	  assignment_statement
	| procedure_statement
	| goto_statement
	| break_statement
	| continue_statement
	;

assignment_statement:
	  variable ASSIGN expression
		{ /* Generate assignment code */ }
	;

procedure_statement:
	  identifier
		{ /* Procedure call with no parameters */ }
	| identifier LPAREN actual_parameter_list RPAREN
		{ /* Procedure call */ }
	;

goto_statement:
	  GOTO label
		{ /* Generate goto */ }
	;

break_statement:
	  BREAK
		{ if (!ALLOW_BREAK_CONTINUE()) {
			error("break statement not allowed in %s", get_dialect_name(current_dialect));
		  }
		}
	;

continue_statement:
	  CONTINUE
		{ if (!ALLOW_BREAK_CONTINUE()) {
			error("continue statement not allowed in %s", get_dialect_name(current_dialect));
		  }
		}
	;

structured_statement:
	  compound_statement
	| if_statement
	| case_statement
	| repeat_statement
	| while_statement
	| for_statement
	| with_statement
	;

if_statement:
	  IF expression THEN statement
	| IF expression THEN statement ELSE statement
	;

case_statement:
	  CASE expression OF case_list END
	| CASE expression OF case_list semicolon ELSE statement_sequence END
		{ if (!ALLOW_CASE_ELSE()) {
			error("else clause in case not allowed in %s", get_dialect_name(current_dialect));
		  }
		}
	;

case_list:
	  case_element
	| case_list semicolon case_element
	;

case_element:
	  case_label_list COLON statement
	;

case_label_list:
	  case_label
	| case_label_list COMMA case_label
	;

case_label:
	  constant
	| constant DOTDOT constant
	;

repeat_statement:
	  REPEAT statement_sequence UNTIL expression
	;

while_statement:
	  WHILE expression DO statement
	;

for_statement:
	  FOR identifier ASSIGN expression TO expression DO statement
	| FOR identifier ASSIGN expression DOWNTO expression DO statement
	;

with_statement:
	  WITH record_variable_list DO statement
	;

record_variable_list:
	  variable
	| record_variable_list COMMA variable
	;

/* Expressions */
expression:
	  simple_expression
	| simple_expression relational_operator simple_expression
	;

simple_expression:
	  term
	| PLUS term
	| MINUS term
	| simple_expression adding_operator term
	;

term:
	  factor
	| term multiplying_operator factor
	;

factor:
	  variable
	| INTCONST
	| REALCONST
	| STRINGCONST
	| NIL
	| LPAREN expression RPAREN
	| NOT factor
	| function_call
	| set_constructor
	;

function_call:
	  identifier LPAREN actual_parameter_list RPAREN
	;

set_constructor:
	  LBRACK member_designator_list RBRACK
	| LBRACK RBRACK
	;

member_designator_list:
	  member_designator
	| member_designator_list COMMA member_designator
	;

member_designator:
	  expression
	| expression DOTDOT expression
	;

variable:
	  identifier
	| indexed_variable
	| field_designator
	| pointer_variable
	;

indexed_variable:
	  variable LBRACK expression_list RBRACK
	;

field_designator:
	  variable DOT identifier
	;

pointer_variable:
	  variable UPARROW
	;

actual_parameter_list:
	  actual_parameter
	| actual_parameter_list COMMA actual_parameter
	;

actual_parameter:
	  expression
	;

expression_list:
	  expression
	| expression_list COMMA expression
	;

relational_operator:
	  EQ | NE | LT | LE | GT | GE | IN
	;

adding_operator:
	  PLUS | MINUS | OR
	;

multiplying_operator:
	  STAR | SLASH | DIV | MOD | AND
	;

/* Utilities */
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

semicolon:
	  SEMICOLON
	| /* empty - optional in some contexts */
	;

%%

/* Empty for now */
