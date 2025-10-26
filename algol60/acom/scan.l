/*
 * Copyright (c) 2025 PCC ALGOL 60+ Compiler
 *
 * Lexical analyzer for ALGOL 60+
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"
#include "y.tab.h"

/* Track column position */
static int column = 1;

/* Update location tracking */
#define YY_USER_ACTION \
	current_column = column; \
	column += yyleng;

/* Comment nesting level */
static int comment_level = 0;

%}

%option noyywrap
%option yylineno

%x COMMENT

DIGIT           [0-9]
LETTER          [a-zA-Z]
IDENT_START     {LETTER}
IDENT_CHAR      ({LETTER}|{DIGIT}|_)
WHITESPACE      [ \t\r]

%%

    /* Comments - ALGOL style: comment ... ; */
"comment" {
	BEGIN(COMMENT);
}

<COMMENT>";" {
	BEGIN(INITIAL);
}

<COMMENT>\n {
	current_line++;
	column = 1;
}

<COMMENT>. {
	/* Ignore comment content */
}

    /* Keywords - ALGOL 60 reserved words */
"begin"         { return BEGIN_KW; }
"end"           { return END_KW; }
"if"            { return IF; }
"then"          { return THEN; }
"else"          { return ELSE; }
"for"           { return FOR; }
"do"            { return DO; }
"while"         { return WHILE; }
"step"          { return STEP; }
"until"         { return UNTIL; }
"goto"          { return GOTO; }

    /* Type keywords */
"integer"       { return INTEGER; }
"real"          { return REAL; }
"boolean"       { return BOOLEAN; }
"string"        { return STRING; }
"array"         { return ARRAY; }

    /* Declaration keywords */
"value"         { return VALUE; }
"own"           { return OWN; }
"procedure"     { return PROCEDURE; }
"label"         { return LABEL; }
"switch"        { return SWITCH; }

    /* Boolean constants */
"true"          { yylval.ival = 1; return BOOL_CONST; }
"false"         { yylval.ival = 0; return BOOL_CONST; }

    /* Logical operators */
"and"           { return AND; }
"or"            { return OR; }
"not"           { return NOT; }

    /* Relational operators */
"<"             { return LT; }
"<="            { return LE; }
"="             { return EQ; }
">="            { return GE; }
">"             { return GT; }
"<>"            { return NE; }
"\u2260"        { return NE; }   /* ≠ */
"\u2264"        { return LE; }   /* ≤ */
"\u2265"        { return GE; }   /* ≥ */

    /* Arithmetic operators */
"+"             { return PLUS; }
"-"             { return MINUS; }
"*"             { return MUL; }
"/"             { return DIV; }
"÷"             { return DIV; }
"^"             { return POWER; }
"\u2191"        { return POWER; } /* ↑ (up arrow) */

    /* Assignment */
":="            { return ASSIGN; }

    /* Delimiters */
"("             { return LPAREN; }
")"             { return RPAREN; }
"["             { return LBRACKET; }
"]"             { return RBRACKET; }
","             { return COMMA; }
";"             { return SEMI; }
":"             { return COLON; }

    /* Special symbols */
"."             { return DOT; }

    /* Identifiers */
{IDENT_START}{IDENT_CHAR}* {
	yylval.sval = strdup(yytext);
	return IDENT;
}

    /* Integer constants */
{DIGIT}+ {
	yylval.ival = atoi(yytext);
	return INT_CONST;
}

    /* Real constants */
{DIGIT}+"."{DIGIT}* |
{DIGIT}*"."{DIGIT}+ {
	yylval.fval = atof(yytext);
	return REAL_CONST;
}

{DIGIT}+[eE][+-]?{DIGIT}+ |
{DIGIT}+"."{DIGIT}*[eE][+-]?{DIGIT}+ |
{DIGIT}*"."{DIGIT}+[eE][+-]?{DIGIT}+ {
	yylval.fval = atof(yytext);
	return REAL_CONST;
}

    /* String literals (extension) */
\"([^\"\n]|\"\")*\" {
	/* Handle "" as escaped quote */
	char *s = strdup(yytext + 1);  /* Skip opening quote */
	s[strlen(s) - 1] = '\0';       /* Remove closing quote */
	yylval.sval = s;
	return STRING_CONST;
}

    /* Newline */
\n {
	current_line++;
	column = 1;
}

    /* Whitespace */
{WHITESPACE}+ {
	/* Ignore */
}

    /* Unknown character */
. {
	error("unexpected character '%c' (ASCII %d)", yytext[0], (int)yytext[0]);
}

%%

/* Called when lexer is done */
int lineno = 1;
char *ftitle = "<stdin>";
