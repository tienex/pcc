/*
 * Copyright (c) 2025 PCC Prolog Compiler
 *
 * Lexical analyzer for Prolog
 * Compatible with Turbo Prolog and GNU Prolog
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "pass1.h"
#include "y.tab.h"

/* Track column position */
static int column = 1;
static int comment_depth = 0;

/* Update location tracking */
#define YY_USER_ACTION \
	current_column = column; \
	column += yyleng;

/* String buffer for collecting string/atom literals */
static char string_buf[4096];
static char *string_buf_ptr;

extern int lineno;
extern int current_column;

%}

%option noyywrap
%option yylineno

%x COMMENT
%x LINECOMMENT
%x STRING
%x QATOM

DIGIT           [0-9]
UPPER           [A-Z]
LOWER           [a-z]
ALPHA           [a-zA-Z]
ALNUM           [a-zA-Z0-9_]
WHITESPACE      [ \t\r]

%%

    /* Line comments - % style (both Turbo Prolog and GNU Prolog) */
"%" {
	BEGIN(LINECOMMENT);
}

<LINECOMMENT>\n {
	lineno++;
	column = 1;
	BEGIN(INITIAL);
}

<LINECOMMENT>. {
	/* Ignore comment content */
}

    /* Block comments - /* ... */ style (both dialects) */
"/*" {
	comment_depth = 1;
	BEGIN(COMMENT);
}

<COMMENT>"/*" {
	comment_depth++;
}

<COMMENT>"*/" {
	comment_depth--;
	if (comment_depth == 0)
		BEGIN(INITIAL);
}

<COMMENT>\n {
	lineno++;
	column = 1;
}

<COMMENT>. {
	/* Ignore comment content */
}

    /* Keywords and operators */
":-"            { return IF; }
"-->"           { return ARROW; }
"?-"            { return QUERY; }
"!"             { return CUT; }
"is"            { return IS; }
"mod"           { return MOD; }
"rem"           { return REM; }
"div"           { return DIV; }

    /* Comparison operators */
"="             { return UNIFY; }
"\\="           { return NOTUNIFY; }
"=="            { return EQUAL; }
"\\=="          { return NOTEQUAL; }
"@<"            { return TERMLESS; }
"@=<"           { return TERMLESSEQ; }
"@>"            { return TERMGREATER; }
"@>="           { return TERMGREATEREQ; }
"=<"            { return LESSEQ; }
">="            { return GREATEREQ; }
"<"             { return LESS; }
">"             { return GREATER; }
"=:="           { return ARITHEQUAL; }
"=\\="          { return ARITHNOTEQ; }

    /* Arithmetic operators */
"+"             { return PLUS; }
"-"             { return MINUS; }
"*"             { return MULTIPLY; }
"/"             { return DIVIDE; }
"//"            { return INTDIV; }
"<<"            { return SHIFTLEFT; }
">>"            { return SHIFTRIGHT; }
"**"            { return POWER; }
"^"             { return POWER; }

    /* Bitwise operators */
"\\/"           { return BITOR; }
"/\\"           { return BITAND; }
"\\"            { return BITNOT; }
"xor"           { return BITXOR; }

    /* List and other operators */
"|"             { return PIPE; }
","             { return COMMA; }
";"             { return SEMICOLON; }
"."             { return DOT; }
"("             { return LPAREN; }
")"             { return RPAREN; }
"["             { return LBRACK; }
"]"             { return RBRACK; }
"{"             { return LBRACE; }
"}"             { return RBRACE; }

    /* Turbo Prolog specific keywords */
"domains"       { return DOMAINS; }
"predicates"    { return PREDICATES; }
"clauses"       { return CLAUSES; }
"goal"          { return GOAL; }
"database"      { return DATABASE; }
"constants"     { return CONSTANTS; }

    /* GNU Prolog / ISO Prolog keywords */
"dynamic"       { return DYNAMIC; }
"multifile"     { return MULTIFILE; }
"discontiguous" { return DISCONTIGUOUS; }
"module"        { return MODULE; }
"use_module"    { return USE_MODULE; }
"public"        { return PUBLIC; }
"meta_predicate" { return META_PREDICATE; }
"op"            { return OP; }

    /* Built-in predicates (common to both) */
"true"          { return TRUE; }
"fail"          { return FAIL; }
"call"          { return CALL; }
"asserta"       { return ASSERTA; }
"assertz"       { return ASSERTZ; }
"retract"       { return RETRACT; }
"retractall"    { return RETRACTALL; }
"findall"       { return FINDALL; }
"bagof"         { return BAGOF; }
"setof"         { return SETOF; }
"functor"       { return FUNCTOR; }
"arg"           { return ARG; }
"copy_term"     { return COPY_TERM; }
"atom"          { return ATOM_BUILTIN; }
"number"        { return NUMBER_BUILTIN; }
"var"           { return VAR; }
"nonvar"        { return NONVAR; }
"integer"       { return INTEGER; }
"float"         { return FLOAT; }
"compound"      { return COMPOUND; }
"atomic"        { return ATOMIC; }
"write"         { return WRITE; }
"writeln"       { return WRITELN; }
"read"          { return READ; }
"get"           { return GET; }
"put"           { return PUT; }
"nl"            { return NL; }
"halt"          { return HALT; }
"abort"         { return ABORT; }

    /* Variables - start with uppercase or underscore */
"_"             { yylval.str = strdup("_"); return ANONYMOUS; }
{UPPER}{ALNUM}* { yylval.str = strdup(yytext); return VARIABLE; }
"_"{ALNUM}+     { yylval.str = strdup(yytext); return VARIABLE; }

    /* Atoms - start with lowercase */
{LOWER}{ALNUM}* { yylval.str = strdup(yytext); return ATOM; }

    /* Quoted atoms - 'atom with spaces' */
"'" {
	string_buf_ptr = string_buf;
	BEGIN(QATOM);
}

<QATOM>"''" {
	/* Escaped single quote */
	*string_buf_ptr++ = '\'';
}

<QATOM>"'" {
	/* End of quoted atom */
	*string_buf_ptr = '\0';
	yylval.str = strdup(string_buf);
	BEGIN(INITIAL);
	return ATOM;
}

<QATOM>\n {
	/* Allow newlines in quoted atoms */
	*string_buf_ptr++ = '\n';
	lineno++;
	column = 1;
}

<QATOM>. {
	*string_buf_ptr++ = yytext[0];
}

    /* Strings - "string" */
"\"" {
	string_buf_ptr = string_buf;
	BEGIN(STRING);
}

<STRING>"\"\"" {
	/* Escaped double quote */
	*string_buf_ptr++ = '"';
}

<STRING>"\"" {
	/* End of string */
	*string_buf_ptr = '\0';
	yylval.str = strdup(string_buf);
	BEGIN(INITIAL);
	return STRING_LIT;
}

<STRING>\\n {
	*string_buf_ptr++ = '\n';
}

<STRING>\\t {
	*string_buf_ptr++ = '\t';
}

<STRING>\\r {
	*string_buf_ptr++ = '\r';
}

<STRING>\\\\ {
	*string_buf_ptr++ = '\\';
}

<STRING>\\. {
	/* Other escape sequences - just copy the character */
	*string_buf_ptr++ = yytext[1];
}

<STRING>\n {
	/* Allow newlines in strings */
	*string_buf_ptr++ = '\n';
	lineno++;
	column = 1;
}

<STRING>. {
	*string_buf_ptr++ = yytext[0];
}

    /* Numbers - integer */
{DIGIT}+ {
	yylval.ival = atol(yytext);
	return INTEGER_CONST;
}

    /* Numbers - hexadecimal (0x...) */
"0"[xX][0-9a-fA-F]+ {
	yylval.ival = strtol(yytext + 2, NULL, 16);
	return INTEGER_CONST;
}

    /* Numbers - octal (0o...) */
"0"[oO][0-7]+ {
	yylval.ival = strtol(yytext + 2, NULL, 8);
	return INTEGER_CONST;
}

    /* Numbers - binary (0b...) */
"0"[bB][01]+ {
	yylval.ival = strtol(yytext + 2, NULL, 2);
	return INTEGER_CONST;
}

    /* Numbers - floating point */
{DIGIT}+"."{DIGIT}+ {
	yylval.dval = atof(yytext);
	return FLOAT_CONST;
}

{DIGIT}+"."{DIGIT}+[eE][+-]?{DIGIT}+ {
	yylval.dval = atof(yytext);
	return FLOAT_CONST;
}

{DIGIT}+[eE][+-]?{DIGIT}+ {
	yylval.dval = atof(yytext);
	return FLOAT_CONST;
}

    /* Whitespace */
{WHITESPACE}+ {
	/* Skip whitespace */
}

    /* Newline */
"\n" {
	lineno++;
	column = 1;
}

    /* Anything else is an error */
. {
	error("Illegal character: '%c' (0x%02x)", yytext[0], (unsigned char)yytext[0]);
}

%%

/* Reset lexer state */
void reset_lexer(void) {
	column = 1;
	comment_depth = 0;
	BEGIN(INITIAL);
}
