/*
 * Copyright (c) 2025 PCC Go Compiler
 *
 * Lexical analyzer for Go
 * Based on Go Language Specification
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"
#include "y.tab.h"

/* Track column position */
int current_column = 1;

/* Update location tracking */
#define YY_USER_ACTION \
	current_column += yyleng;

/* Comment nesting for /* */ style */
static int comment_level = 0;

%}

%option noyywrap
%option yylineno

%x COMMENT
%x LINECOMMENT
%x RAWSTRING

LETTER          [a-zA-Z_]
DIGIT           [0-9]
HEX_DIGIT       [0-9a-fA-F]
OCT_DIGIT       [0-7]
BIN_DIGIT       [01]
IDENT           {LETTER}({LETTER}|{DIGIT})*
WHITESPACE      [ \t\r]

%%

    /* Whitespace */
{WHITESPACE}+   { /* ignore */ }

    /* Newlines (significant in Go for semicolon insertion) */
\n {
	lineno++;
	current_column = 1;
	return SEMICOLON;  /* Automatic semicolon insertion */
}

    /* Line comments */
"//" {
	BEGIN(LINECOMMENT);
}

<LINECOMMENT>\n {
	lineno++;
	current_column = 1;
	BEGIN(INITIAL);
	return SEMICOLON;
}

<LINECOMMENT>. {
	/* Ignore comment content */
}

    /* Block comments */
"/*" {
	comment_level = 1;
	BEGIN(COMMENT);
}

<COMMENT>"/*" {
	comment_level++;
}

<COMMENT>"*/" {
	comment_level--;
	if (comment_level == 0)
		BEGIN(INITIAL);
}

<COMMENT>\n {
	lineno++;
	current_column = 1;
}

<COMMENT>. {
	/* Ignore comment content */
}

    /* Keywords */
"break"         { return BREAK; }
"case"          { return CASE; }
"chan"          { return CHAN; }
"const"         { return CONST; }
"continue"      { return CONTINUE; }
"default"       { return DEFAULT; }
"defer"         { return DEFER; }
"else"          { return ELSE; }
"fallthrough"   { return FALLTHROUGH; }
"for"           { return FOR; }
"func"          { return FUNC; }
"go"            { return GO; }
"goto"          { return GOTO; }
"if"            { return IF; }
"import"        { return IMPORT; }
"interface"     { return INTERFACE; }
"map"           { return MAP; }
"package"       { return PACKAGE; }
"range"         { return RANGE; }
"return"        { return RETURN; }
"select"        { return SELECT; }
"struct"        { return STRUCT; }
"switch"        { return SWITCH; }
"type"          { return TYPE; }
"var"           { return VAR; }

    /* Operators and delimiters */
"+"             { return PLUS; }
"-"             { return MINUS; }
"*"             { return STAR; }
"/"             { return SLASH; }
"%"             { return PERCENT; }

"&"             { return AMP; }
"|"             { return PIPE; }
"^"             { return CARET; }
"<<"            { return SHL; }
">>"            { return SHR; }
"&^"            { return ANDNOT; }

"+="            { return ADDASSIGN; }
"-="            { return SUBASSIGN; }
"*="            { return MULASSIGN; }
"/="            { return DIVASSIGN; }
"%="            { return MODASSIGN; }

"&="            { return ANDASSIGN; }
"|="            { return ORASSIGN; }
"^="            { return XORASSIGN; }
"<<="           { return SHLASSIGN; }
">>="           { return SHRASSIGN; }
"&^="           { return ANDNOTASSIGN; }

"&&"            { return LAND; }
"||"            { return LOR; }
"<-"            { return ARROW; }
"++"            { return INC; }
"--"            { return DEC; }

"=="            { return EQ; }
"<"             { return LT; }
">"             { return GT; }
"="             { return ASSIGN; }
"!"             { return NOT; }

"!="            { return NE; }
"<="            { return LE; }
">="            { return GE; }
":="            { return DEFINE; }
"..."           { return ELLIPSIS; }

"("             { return LPAREN; }
")"             { return RPAREN; }
"["             { return LBRACK; }
"]"             { return RBRACK; }
"{"             { return LBRACE; }
"}"             { return RBRACE; }

","             { return COMMA; }
"."             { return DOT; }
";"             { return SEMICOLON; }
":"             { return COLON; }

    /* Integer literals */
0[xX]{HEX_DIGIT}+ {
	yylval.ival = (int)strtol(yytext, NULL, 16);
	return INTLIT;
}

0[oO]{OCT_DIGIT}+ {
	yylval.ival = (int)strtol(yytext + 2, NULL, 8);
	return INTLIT;
}

0[bB]{BIN_DIGIT}+ {
	yylval.ival = (int)strtol(yytext + 2, NULL, 2);
	return INTLIT;
}

{DIGIT}+ {
	yylval.ival = atoi(yytext);
	return INTLIT;
}

    /* Float literals */
{DIGIT}+"."{DIGIT}* {
	yylval.dval = atof(yytext);
	return FLOATLIT;
}

{DIGIT}*"."{DIGIT}+ {
	yylval.dval = atof(yytext);
	return FLOATLIT;
}

{DIGIT}+[eE][+-]?{DIGIT}+ {
	yylval.dval = atof(yytext);
	return FLOATLIT;
}

{DIGIT}+"."{DIGIT}*[eE][+-]?{DIGIT}+ {
	yylval.dval = atof(yytext);
	return FLOATLIT;
}

    /* Rune literals */
'(\\.|[^\\'])' {
	/* TODO: Handle escape sequences properly */
	if (yytext[1] == '\\') {
		/* Escape sequence */
		switch (yytext[2]) {
		case 'n': yylval.ival = '\n'; break;
		case 't': yylval.ival = '\t'; break;
		case 'r': yylval.ival = '\r'; break;
		case '\\': yylval.ival = '\\'; break;
		case '\'': yylval.ival = '\''; break;
		default: yylval.ival = yytext[2];
		}
	} else {
		yylval.ival = yytext[1];
	}
	return RUNELIT;
}

    /* String literals (interpreted) */
\"([^\\\"]|\\.)*\" {
	/* Copy string without quotes */
	int len = yyleng - 2;
	yylval.sval = (char *)xmalloc(len + 1);
	strncpy(yylval.sval, yytext + 1, len);
	yylval.sval[len] = '\0';
	return STRINGLIT;
}

    /* Raw string literals */
"`" {
	BEGIN(RAWSTRING);
	yylval.sval = (char *)xmalloc(1);
	yylval.sval[0] = '\0';
}

<RAWSTRING>"`" {
	BEGIN(INITIAL);
	return STRINGLIT;
}

<RAWSTRING>\n {
	/* Raw strings can contain newlines */
	lineno++;
	current_column = 1;
	/* Append newline to string */
	int len = yylval.sval ? strlen(yylval.sval) : 0;
	yylval.sval = (char *)realloc(yylval.sval, len + 2);
	yylval.sval[len] = '\n';
	yylval.sval[len + 1] = '\0';
}

<RAWSTRING>. {
	/* Append character to string */
	int len = yylval.sval ? strlen(yylval.sval) : 0;
	yylval.sval = (char *)realloc(yylval.sval, len + 2);
	yylval.sval[len] = yytext[0];
	yylval.sval[len + 1] = '\0';
}

    /* Identifiers */
{IDENT} {
	yylval.sval = str_copy(yytext);
	return IDENT;
}

    /* Anything else is an error */
. {
	error("unexpected character '%c' (0x%02x)", yytext[0], (unsigned char)yytext[0]);
}

%%

/* External declarations */
int lineno = 1;
char *current_file = "<stdin>";
