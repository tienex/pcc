%{
/*
 * COBOL grammar with OO extensions
 * Supports DEC, IBM, HP, and Microsoft dialects
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

extern int yylex(void);
extern void yyerror(const char *s);

static struct cobol_class *current_class = NULL;
static struct cobol_method *current_method = NULL;

%}

%union {
	int intval;
	double floatval;
	char *string;
	struct cobsym *symbol;
	struct picture *pic;
	NODE *node;
}

/* Tokens */
%token <string> IDENTIFIER STRING_LITERAL
%token <intval> INTEGER LEVEL_NUMBER
%token <floatval> FLOAT

/* Division headers */
%token IDENTIFICATION_DIVISION ENVIRONMENT_DIVISION DATA_DIVISION PROCEDURE_DIVISION

/* Section headers */
%token CONFIGURATION_SECTION INPUT_OUTPUT_SECTION FILE_SECTION
%token WORKING_STORAGE_SECTION LOCAL_STORAGE_SECTION LINKAGE_SECTION

/* OO COBOL keywords */
%token CLASS CLASS_ID METHOD METHOD_ID OBJECT INHERITS INTERFACE INTERFACE_ID
%token FACTORY INVOKE NEW SELF SUPER PROPERTY STATIC INSTANCE FINAL ABSTRACT OVERRIDE

/* Data description */
%token PIC VALUE REDEFINES OCCURS TIMES DEPENDING_ON INDEXED_BY USAGE
%token COMP COMP_1 COMP_2 COMP_3 COMP_4 COMP_5 BINARY PACKED_DECIMAL DISPLAY_USAGE
%token POINTER OBJECT_REFERENCE

/* Procedure keywords */
%token ACCEPT ADD CALL COMPUTE DELETE DISPLAY DIVIDE_OP EVALUATE EXIT GOTO
%token IF ELSE END_IF MOVE MULTIPLY PERFORM END_PERFORM READ RETURN RETURNING
%token REWRITE SEARCH SET STOP STRING_STMT SUBTRACT UNSTRING WRITE

/* Control flow */
%token UNTIL VARYING WHEN OTHER THROUGH THRU BY GIVING REMAINDER INTO FROM

/* File operations */
%token OPEN CLOSE INPUT OUTPUT IO EXTEND FILE FD SD SELECT ASSIGN
%token ORGANIZATION ACCESS_MODE SEQUENTIAL INDEXED RELATIVE RANDOM DYNAMIC RECORD_KEY

/* Operators */
%token AND OR NOT EQUAL GREATER LESS GREATER_EQUAL LESS_EQUAL NOT_EQUAL
%token PLUS MINUS POWER

/* Misc */
%token PROGRAM_ID AUTHOR DATE_WRITTEN IS ARE TO OF IN WITH ALL USING
%token TRUE FALSE ZERO SPACE NULL_TOK
%token INITIAL CONTROL FILLER END PROGRAM RUN

/* Punctuation */
%token DOT COMMA SEMICOLON COLON LPAREN RPAREN

/* Precedence */
%left OR
%left AND
%left NOT
%left EQUAL NOT_EQUAL LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS
%left TIMES DIVIDE
%right POWER
%left UMINUS

%type <node> expression arithmetic_expr condition statement_list
%type <node> statement move_stmt add_stmt compute_stmt if_stmt perform_stmt
%type <node> literal_expr display_stmt accept_stmt call_stmt invoke_stmt exit_stmt stop_stmt
%type <node> statement_list_opt statements_opt
%type <symbol> data_item data_items
%type <pic> picture_clause picture_clause_opt
%type <string> identifier picture_string

%%

program:
	  identification_division
	  environment_division_opt
	  data_division_opt
	  procedure_division_opt
	  end_program_opt
	;

/* IDENTIFICATION DIVISION */
identification_division:
	  IDENTIFICATION_DIVISION DOT
	  program_id_para
	  identification_paras_opt
	| CLASS_ID DOT identifier DOT
	  {
		current_class = define_class($3, NULL);
	  }
	  class_options_opt
	;

program_id_para:
	  PROGRAM_ID DOT identifier DOT
	| PROGRAM_ID DOT identifier IS INITIAL DOT
	;

identification_paras_opt:
	  /* empty */
	| identification_paras
	;

identification_paras:
	  identification_para
	| identification_paras identification_para
	;

identification_para:
	  AUTHOR DOT anything_until_dot
	| DATE_WRITTEN DOT anything_until_dot
	;

anything_until_dot:
	  /* Simplified - just skip tokens */
	;

class_options_opt:
	  /* empty */
	| class_options
	;

class_options:
	  class_option
	| class_options class_option
	;

class_option:
	  INHERITS FROM identifier DOT
	  {
		if (current_class)
			current_class->inherits = $3;
	  }
	| FINAL DOT
	  {
		if (current_class)
			current_class->is_final = 1;
	  }
	| ABSTRACT DOT
	  {
		if (current_class)
			current_class->is_abstract = 1;
	  }
	;

/* ENVIRONMENT DIVISION */
environment_division_opt:
	  /* empty */
	| ENVIRONMENT_DIVISION DOT environment_sections_opt
	;

environment_sections_opt:
	  /* empty */
	| environment_sections
	;

environment_sections:
	  environment_section
	| environment_sections environment_section
	;

environment_section:
	  CONFIGURATION_SECTION DOT configuration_paras_opt
	| INPUT_OUTPUT_SECTION DOT io_paras_opt
	;

configuration_paras_opt:
	  /* empty */
	;

io_paras_opt:
	  /* empty */
	| file_control_para
	;

file_control_para:
	  FILE MINUS CONTROL DOT select_clauses_opt
	;

select_clauses_opt:
	  /* empty */
	| select_clauses
	;

select_clauses:
	  select_clause
	| select_clauses select_clause
	;

select_clause:
	  SELECT identifier ASSIGN TO identifier DOT
	;

/* DATA DIVISION */
data_division_opt:
	  /* empty */
	| DATA_DIVISION DOT data_sections_opt
	;

data_sections_opt:
	  /* empty */
	| data_sections
	;

data_sections:
	  data_section
	| data_sections data_section
	;

data_section:
	  FILE_SECTION DOT file_descriptions_opt
	| WORKING_STORAGE_SECTION DOT data_items_opt
	| LOCAL_STORAGE_SECTION DOT data_items_opt
	| LINKAGE_SECTION DOT data_items_opt
	;

file_descriptions_opt:
	  /* empty */
	| file_descriptions
	;

file_descriptions:
	  file_description
	| file_descriptions file_description
	;

file_description:
	  FD identifier DOT data_items_opt
	| SD identifier DOT data_items_opt
	;

data_items_opt:
	  /* empty */
	| data_items
	;

data_items:
	  data_item
	  { $$ = $1; }
	| data_items data_item
	  { $$ = $2; }
	;

data_item:
	  LEVEL_NUMBER identifier picture_clause_opt DOT
	  {
		struct cobsym *sym = install($2, $1);
		sym->pic = $3;
		$$ = sym;
	  }
	| LEVEL_NUMBER identifier REDEFINES identifier picture_clause_opt DOT
	  {
		struct cobsym *sym = install($2, $1);
		sym->pic = $5;
		$$ = sym;
	  }
	| LEVEL_NUMBER identifier OCCURS INTEGER TIMES picture_clause_opt DOT
	  {
		struct cobsym *sym = install($2, $1);
		sym->pic = $6;
		$$ = sym;
	  }
	| LEVEL_NUMBER FILLER picture_clause_opt DOT
	  {
		struct cobsym *sym = install("FILLER", $1);
		sym->pic = $3;
		$$ = sym;
	  }
	;

picture_clause_opt:
	  /* empty */
	  { $$ = NULL; }
	| picture_clause
	  { $$ = $1; }
	;

picture_clause:
	  PIC IS picture_string
	  { $$ = parse_picture($3); }
	| PIC picture_string
	  { $$ = parse_picture($2); }
	| VALUE IS literal
	  { $$ = NULL; /* Handle VALUE separately */ }
	| USAGE IS usage_type
	  { $$ = NULL; /* Handle USAGE separately */ }
	;

picture_string:
	  IDENTIFIER
	  { $$ = $1; }
	| STRING_LITERAL
	  { $$ = $1; }
	;

usage_type:
	  BINARY | COMP | COMP_1 | COMP_2 | COMP_3 | COMP_4 | COMP_5
	| PACKED_DECIMAL | DISPLAY_USAGE | POINTER | OBJECT_REFERENCE
	;

literal:
	  INTEGER
	| FLOAT
	| STRING_LITERAL
	| ZERO
	| SPACE
	| NULL_TOK
	;

/* PROCEDURE DIVISION */
procedure_division_opt:
	  /* empty */
	| PROCEDURE_DIVISION DOT statements_opt
	| PROCEDURE_DIVISION USING parameter_list DOT statements_opt
	| METHOD_ID DOT identifier DOT method_body
	  {
		current_method = define_method($3, 0);
	  }
	;

parameter_list:
	  identifier
	| parameter_list identifier
	;

method_body:
	  method_options_opt
	  PROCEDURE_DIVISION method_using_opt DOT
	  statements_opt
	  END METHOD DOT
	;

method_options_opt:
	  /* empty */
	| method_options
	;

method_options:
	  method_option
	| method_options method_option
	;

method_option:
	  STATIC DOT
	  {
		if (current_method)
			current_method->is_static = 1;
	  }
	| FINAL DOT
	  {
		if (current_method)
			current_method->is_final = 1;
	  }
	;

method_using_opt:
	  /* empty */
	| USING parameter_list
	;

statements_opt:
	  /* empty */
	  { $$ = NULL; }
	| statement_list
	  { $$ = $1; }
	;

statement_list:
	  statement
	  { $$ = $1; }
	| statement_list statement
	  { $$ = buildtree(COMOP, $1, $2); }
	;

statement:
	  move_stmt
	| add_stmt
	| compute_stmt
	| if_stmt
	| perform_stmt
	| display_stmt
	| accept_stmt
	| call_stmt
	| invoke_stmt
	| exit_stmt
	| stop_stmt
	;

move_stmt:
	  MOVE expression TO identifier DOT
	  {
		NODE *dst = make_name($4);
		$$ = gen_move($2, dst);
	  }
	;

add_stmt:
	  ADD expression TO identifier DOT
	  {
		NODE *var = make_name($4);
		NODE *sum = buildtree(PLUS, var, $2);
		$$ = gen_move(sum, var);
	  }
	| ADD expression TO identifier GIVING identifier DOT
	  {
		NODE *sum = buildtree(PLUS, make_name($4), $2);
		NODE *dst = make_name($6);
		$$ = gen_move(sum, dst);
	  }
	;

compute_stmt:
	  COMPUTE identifier EQUAL expression DOT
	  {
		$$ = gen_builtin_compute(make_name($2), $4);
	  }
	;

if_stmt:
	  IF condition statement_list_opt end_if_opt
	  {
		$$ = gen_if($2, $3, NULL);
	  }
	| IF condition statement_list_opt ELSE statement_list_opt END_IF DOT
	  {
		$$ = gen_if($2, $3, $5);
	  }
	;

statement_list_opt:
	  /* empty */
	  { $$ = NULL; }
	| statement_list
	  { $$ = $1; }
	;

end_if_opt:
	  DOT
	| END_IF DOT
	;

perform_stmt:
	  PERFORM identifier DOT
	  {
		$$ = gen_perform($2, NULL);
	  }
	| PERFORM identifier UNTIL condition DOT
	  {
		$$ = gen_perform($2, $4);
	  }
	| PERFORM statement_list END_PERFORM DOT
	  {
		$$ = $2;
	  }
	;

display_stmt:
	  DISPLAY expression DOT
	  {
		$$ = gen_builtin_display($2);
	  }
	;

accept_stmt:
	  ACCEPT identifier DOT
	  {
		$$ = gen_builtin_accept(make_name($2));
	  }
	;

call_stmt:
	  CALL identifier USING argument_list_opt DOT
	  {
		$$ = gen_call($2, NULL);
	  }
	;

invoke_stmt:
	  INVOKE identifier STRING_LITERAL USING argument_list_opt RETURNING identifier DOT
	  {
		$$ = gen_invoke($2, $3, NULL);
	  }
	| INVOKE identifier STRING_LITERAL USING argument_list_opt DOT
	  {
		$$ = gen_invoke($2, $3, NULL);
	  }
	;

argument_list_opt:
	  /* empty */
	| argument_list
	;

argument_list:
	  expression
	| argument_list expression
	;

exit_stmt:
	  EXIT DOT
	  {
		$$ = NULL; /* Generate appropriate exit code */
	  }
	| EXIT METHOD DOT
	  {
		$$ = NULL; /* Generate method return */
	  }
	;

stop_stmt:
	  STOP RUN DOT
	  {
		$$ = NULL; /* Generate program termination */
	  }
	;

condition:
	  expression EQUAL expression
	  { $$ = buildtree(EQ, $1, $3); }
	| expression NOT_EQUAL expression
	  { $$ = buildtree(NE, $1, $3); }
	| expression GREATER expression
	  { $$ = buildtree(GT, $1, $3); }
	| expression LESS expression
	  { $$ = buildtree(LT, $1, $3); }
	| expression GREATER_EQUAL expression
	  { $$ = buildtree(GE, $1, $3); }
	| expression LESS_EQUAL expression
	  { $$ = buildtree(LE, $1, $3); }
	| condition AND condition
	  { $$ = buildtree(ANDAND, $1, $3); }
	| condition OR condition
	  { $$ = buildtree(OROR, $1, $3); }
	| NOT condition
	  { $$ = buildtree(NOT, $2, NULL); }
	| LPAREN condition RPAREN
	  { $$ = $2; }
	;

expression:
	  arithmetic_expr
	  { $$ = $1; }
	| literal_expr
	;

literal_expr:
	  INTEGER
	  { $$ = make_icon($1); }
	| FLOAT
	  { $$ = make_icon((CONSZ)$1); } /* Simplified */
	| STRING_LITERAL
	  { $$ = NULL; /* Handle strings */ }
	| ZERO
	  { $$ = make_icon(0); }
	| SPACE
	  { $$ = NULL; /* Handle spaces */ }
	;

arithmetic_expr:
	  identifier
	  { $$ = make_name($1); }
	| expression PLUS expression
	  { $$ = buildtree(PLUS, $1, $3); }
	| expression MINUS expression
	  { $$ = buildtree(MINUS, $1, $3); }
	| expression TIMES expression
	  { $$ = buildtree(MUL, $1, $3); }
	| expression DIVIDE_OP expression
	  { $$ = buildtree(DIV, $1, $3); }
	| expression POWER expression
	  { $$ = NULL; /* Handle power */ }
	| MINUS expression %prec UMINUS
	  { $$ = buildtree(UMINUS, $2, NULL); }
	| LPAREN expression RPAREN
	  { $$ = $2; }
	;

identifier:
	  IDENTIFIER
	  { $$ = $1; }
	;

end_program_opt:
	  /* empty */
	| END PROGRAM identifier DOT
	| END CLASS identifier DOT
	;

%%
