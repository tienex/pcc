/*
 * Copyright (c) 2025 PCC OCaml Compiler
 *
 * Lexical analyzer for OCaml
 * Supports OCaml 4.x and 5.x syntax
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"
#include "y.tab.h"

/* Track column position */
static int column = 1;
extern int lineno;

/* Update location tracking */
#define YY_USER_ACTION \
	current_column = column; \
	column += yyleng;

/* Comment nesting level */
static int comment_level = 0;

/* String buffer for string literals */
static char string_buf[4096];
static char *string_ptr;

%}

%option noyywrap
%option yylineno

%x COMMENT
%x STRING

DIGIT           [0-9]
LETTER          [a-zA-Z]
IDENT_START     ({LETTER}|_)
IDENT_CHAR      ({LETTER}|{DIGIT}|_|')
WHITESPACE      [ \t\r]

%%

    /* OCaml nested comments (* ... *) */
"(*" {
	comment_level = 1;
	BEGIN(COMMENT);
}

<COMMENT>"(*" {
	comment_level++;
}

<COMMENT>"*)" {
	comment_level--;
	if (comment_level == 0)
		BEGIN(INITIAL);
}

<COMMENT>\n {
	lineno++;
	column = 1;
}

<COMMENT>. {
	/* Ignore comment content */
}

    /* Keywords */
"and"           { return AND; }
"as"            { return AS; }
"assert"        { return ASSERT; }
"begin"         { return OBEGIN; }
"class"         { return CLASS; }
"constraint"    { return CONSTRAINT; }
"do"            { return DO; }
"done"          { return DONE; }
"downto"        { return DOWNTO; }
"else"          { return ELSE; }
"end"           { return OEND; }
"exception"     { return EXCEPTION; }
"external"      { return EXTERNAL; }
"false"         { return FALSE; }
"for"           { return FOR; }
"fun"           { return FUN; }
"function"      { return FUNCTION; }
"functor"       { return FUNCTOR; }
"if"            { return IF; }
"in"            { return IN; }
"include"       { return INCLUDE; }
"inherit"       { return INHERIT; }
"initializer"   { return INITIALIZER; }
"lazy"          { return LAZY; }
"let"           { return LET; }
"match"         { return MATCH; }
"method"        { return METHOD; }
"module"        { return MODULE; }
"mutable"       { return MUTABLE; }
"new"           { return NEW; }
"nonrec"        { return NONREC; }
"object"        { return OBJECT; }
"of"            { return OF; }
"open"          { return OPEN; }
"or"            { return OR; }
"private"       { return PRIVATE; }
"rec"           { return REC; }
"sig"           { return SIG; }
"struct"        { return STRUCT; }
"then"          { return THEN; }
"to"            { return TO; }
"true"          { return TRUE; }
"try"           { return TRY; }
"type"          { return TYPE; }
"val"           { return VAL; }
"virtual"       { return VIRTUAL; }
"when"          { return WHEN; }
"while"         { return WHILE; }
"with"          { return WITH; }

    /* Operators and special symbols */
"("             { return LPAREN; }
")"             { return RPAREN; }
"["             { return LBRACK; }
"]"             { return RBRACK; }
"{"             { return LBRACE; }
"}"             { return RBRACE; }
";"             { return SEMI; }
";;"            { return SEMISEMI; }
","             { return COMMA; }
"."             { return DOT; }
".."            { return DOTDOT; }
":"             { return COLON; }
"::"            { return COLONCOLON; }
":="            { return COLONEQ; }
":>"            { return COLONGT; }
"="             { return EQ; }
"=="            { return EQEQ; }
"<"             { return LT; }
"<="            { return LE; }
">"             { return GT; }
">="            { return GE; }
"<>"            { return NE; }
"!="            { return BANGEQ; }
"+"             { return PLUS; }
"-"             { return MINUS; }
"*"             { return STAR; }
"/"             { return SLASH; }
"%"             { return PERCENT; }
"**"            { return STARSTAR; }
"@"             { return AT; }
"^"             { return CARET; }
"!"             { return BANG; }
"~"             { return TILDE; }
"|"             { return PIPE; }
"||"            { return PIPEPIPE; }
"&"             { return AMP; }
"&&"            { return AMPAMP; }
"->"            { return ARROW; }
"<-"            { return LARROW; }
"=>"            { return FATARROW; }
"?"             { return QUESTION; }
"_"             { return UNDERSCORE; }
"`"             { return BACKQUOTE; }
"#"             { return HASH; }

    /* Integer literals */
{DIGIT}+ {
	yylval.integer = atoi(yytext);
	return INT_LITERAL;
}

0[xX][0-9a-fA-F]+ {
	yylval.integer = strtol(yytext + 2, NULL, 16);
	return INT_LITERAL;
}

0[oO][0-7]+ {
	yylval.integer = strtol(yytext + 2, NULL, 8);
	return INT_LITERAL;
}

0[bB][01]+ {
	yylval.integer = strtol(yytext + 2, NULL, 2);
	return INT_LITERAL;
}

    /* Float literals */
{DIGIT}+"."{DIGIT}* {
	yylval.floating = atof(yytext);
	return FLOAT_LITERAL;
}

{DIGIT}+("."({DIGIT}*)?)?[eE][+-]?{DIGIT}+ {
	yylval.floating = atof(yytext);
	return FLOAT_LITERAL;
}

    /* Character literals */
"'".{1,4}"'" {
	yylval.character = yytext[1];
	return CHAR_LITERAL;
}

    /* String literals */
"\"" {
	string_ptr = string_buf;
	BEGIN(STRING);
}

<STRING>"\"" {
	BEGIN(INITIAL);
	*string_ptr = '\0';
	yylval.string = strdup(string_buf);
	return STRING_LITERAL;
}

<STRING>\n {
	lineno++;
	column = 1;
	*string_ptr++ = '\n';
}

<STRING>\\n     { *string_ptr++ = '\n'; }
<STRING>\\t     { *string_ptr++ = '\t'; }
<STRING>\\r     { *string_ptr++ = '\r'; }
<STRING>\\b     { *string_ptr++ = '\b'; }
<STRING>\\\"    { *string_ptr++ = '\"'; }
<STRING>\\\\    { *string_ptr++ = '\\'; }

<STRING>\\{DIGIT}{3} {
	int val = (yytext[1] - '0') * 64 + (yytext[2] - '0') * 8 + (yytext[3] - '0');
	*string_ptr++ = (char)val;
}

<STRING>. {
	*string_ptr++ = *yytext;
}

    /* Identifiers */
{IDENT_START}{IDENT_CHAR}* {
	yylval.string = strdup(yytext);
	return IDENT;
}

    /* Whitespace */
{WHITESPACE}+   { /* Ignore whitespace */ }

\n {
	lineno++;
	column = 1;
}

    /* Anything else is an error */
. {
	fprintf(stderr, "Lexical error at line %d: unexpected character '%c'\n",
	        lineno, *yytext);
	nerrors++;
}

%%

/* Additional lexer functions can be added here */
