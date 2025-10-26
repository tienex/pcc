%{
/*
 * Copyright (c) 2025 PCC BASIC Compiler
 *
 * Parser grammar for BASIC
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

extern int yylex(void);
extern void yyerror(const char *s);
extern FILE *yyin;
extern int lineno;

%}

%union {
	int ival;
	double dval;
	char *sval;
}

%token <ival> NUMBER
%token <dval> REAL_NUM
%token <sval> STRING_LIT IDENT
%token PRINT INPUT LET IF THEN ELSE END FOR TO STEP NEXT
%token GOTO GOSUB RETURN WHILE WEND DO LOOP DIM AS
%token TINTEGER_KW TLONG_KW TSINGLE_KW TDOUBLE_KW TSTRING_KW
%token SUB FUNCTION EXIT CALL SHARED STATIC COMMON
%token OPTION BASE EXPLICIT
%token AND OR NOT XOR MOD
%token EQ NE LT LE GT GE
%token COLON SEMICOLON COMMA LPAREN RPAREN
%token PLUS MINUS MUL DIV INTDIV POWER
%token EOL

%left OR XOR
%left AND
%left NOT
%left EQ NE LT LE GT GE
%left PLUS MINUS
%left MUL DIV INTDIV
%left MOD
%right POWER
%right UMINUS

%%

program:
	/* empty */
	| program line
	;

line:
	EOL
	| NUMBER statement_list EOL
	{
		current_line_num = $1;
	}
	| statement_list EOL
	;

statement_list:
	statement
	| statement_list COLON statement
	;

statement:
	/* empty */
	| PRINT print_list
	{
		fprintf(outfile, "; PRINT statement\n");
	}
	| INPUT input_list
	{
		fprintf(outfile, "; INPUT statement\n");
	}
	| LET IDENT EQ expr
	{
		fprintf(outfile, "; LET %s = expr\n", $2);
		free($2);
	}
	| IDENT EQ expr
	{
		fprintf(outfile, "; %s = expr\n", $1);
		free($1);
	}
	| IF expr THEN statement
	{
		fprintf(outfile, "; IF...THEN\n");
	}
	| IF expr THEN statement ELSE statement
	{
		fprintf(outfile, "; IF...THEN...ELSE\n");
	}
	| GOTO NUMBER
	{
		fprintf(outfile, "; GOTO %d\n", $2);
	}
	| GOSUB NUMBER
	{
		fprintf(outfile, "; GOSUB %d\n", $2);
	}
	| RETURN
	{
		fprintf(outfile, "; RETURN\n");
	}
	| FOR IDENT EQ expr TO expr
	{
		fprintf(outfile, "; FOR %s\n", $2);
		free($2);
	}
	| FOR IDENT EQ expr TO expr STEP expr
	{
		fprintf(outfile, "; FOR %s STEP\n", $2);
		free($2);
	}
	| NEXT IDENT
	{
		fprintf(outfile, "; NEXT %s\n", $2);
		free($2);
	}
	| NEXT
	{
		fprintf(outfile, "; NEXT\n");
	}
	| WHILE expr
	{
		fprintf(outfile, "; WHILE\n");
	}
	| WEND
	{
		fprintf(outfile, "; WEND\n");
	}
	| DO
	{
		fprintf(outfile, "; DO\n");
	}
	| LOOP
	{
		fprintf(outfile, "; LOOP\n");
	}
	| DIM dim_list
	{
		fprintf(outfile, "; DIM\n");
	}
	| END
	{
		fprintf(outfile, "; END\n");
	}
	| SUB IDENT
	{
		fprintf(outfile, "; SUB %s\n", $2);
		free($2);
	}
	| FUNCTION IDENT
	{
		fprintf(outfile, "; FUNCTION %s\n", $2);
		free($2);
	}
	;

print_list:
	/* empty */
	| print_item
	| print_list SEMICOLON print_item
	| print_list COMMA print_item
	;

print_item:
	expr
	| STRING_LIT
	{
		fprintf(outfile, "; print string: %s\n", $1);
		free($1);
	}
	;

input_list:
	IDENT
	{
		fprintf(outfile, "; input %s\n", $1);
		free($1);
	}
	| input_list COMMA IDENT
	{
		fprintf(outfile, "; input %s\n", $3);
		free($3);
	}
	;

dim_list:
	dim_item
	| dim_list COMMA dim_item
	;

dim_item:
	IDENT LPAREN NUMBER RPAREN
	{
		fprintf(outfile, "; dim %s(%d)\n", $1, $3);
		free($1);
	}
	| IDENT LPAREN NUMBER COMMA NUMBER RPAREN
	{
		fprintf(outfile, "; dim %s(%d,%d)\n", $1, $3, $5);
		free($1);
	}
	;

expr:
	NUMBER
	{
		fprintf(outfile, "; number: %d\n", $1);
	}
	| REAL_NUM
	{
		fprintf(outfile, "; real: %f\n", $1);
	}
	| STRING_LIT
	{
		fprintf(outfile, "; string: %s\n", $1);
		free($1);
	}
	| IDENT
	{
		fprintf(outfile, "; variable: %s\n", $1);
		free($1);
	}
	| IDENT LPAREN expr RPAREN
	{
		fprintf(outfile, "; array or function: %s()\n", $1);
		free($1);
	}
	| expr PLUS expr
	{
		fprintf(outfile, "; +\n");
	}
	| expr MINUS expr
	{
		fprintf(outfile, "; -\n");
	}
	| expr MUL expr
	{
		fprintf(outfile, "; *\n");
	}
	| expr DIV expr
	{
		fprintf(outfile, "; /\n");
	}
	| expr INTDIV expr
	{
		fprintf(outfile, "; \\\n");
	}
	| expr MOD expr
	{
		fprintf(outfile, "; MOD\n");
	}
	| expr POWER expr
	{
		fprintf(outfile, "; ^\n");
	}
	| expr EQ expr
	{
		fprintf(outfile, "; =\n");
	}
	| expr NE expr
	{
		fprintf(outfile, "; <>\n");
	}
	| expr LT expr
	{
		fprintf(outfile, "; <\n");
	}
	| expr LE expr
	{
		fprintf(outfile, "; <=\n");
	}
	| expr GT expr
	{
		fprintf(outfile, "; >\n");
	}
	| expr GE expr
	{
		fprintf(outfile, "; >=\n");
	}
	| expr AND expr
	{
		fprintf(outfile, "; AND\n");
	}
	| expr OR expr
	{
		fprintf(outfile, "; OR\n");
	}
	| expr XOR expr
	{
		fprintf(outfile, "; XOR\n");
	}
	| NOT expr
	{
		fprintf(outfile, "; NOT\n");
	}
	| MINUS expr %prec UMINUS
	{
		fprintf(outfile, "; unary -\n");
	}
	| LPAREN expr RPAREN
	{
		fprintf(outfile, "; (expr)\n");
	}
	;

%%

/* Nothing else needed */
