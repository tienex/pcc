/*
 * Copyright (c) 2025 PCC DEC MACRO Compiler
 *
 * Lexical analyzer for DEC MACRO assembly language
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"
#include "y.tab.h"

/* Update line number tracking */
#define YY_USER_ACTION lineno = yylineno;

/* String buffer for string literals */
static char string_buf[4096];
static char *string_ptr;

%}

%option noyywrap
%option yylineno

%x STRING
%x COMMENT

DIGIT           [0-9]
HEXDIGIT        [0-9A-Fa-f]
OCTDIGIT        [0-7]
LETTER          [a-zA-Z]
IDENT_START     ({LETTER}|_|\\$)
IDENT_CHAR      ({LETTER}|{DIGIT}|_|\\$|\\.)
WHITESPACE      [ \t]

%%

    /* Comments - semicolon to end of line */
^[ \t]*;.*\n {
	/* Full-line comment */
	lineno++;
}

;.* {
	/* End-of-line comment */
}

    /* Directives */
\\.TITLE        { return DIR_TITLE; }
\\.IDENT        { return DIR_IDENT; }
\\.PSECT        { return DIR_PSECT; }
\\.ENTRY        { return DIR_ENTRY; }
\\.END          { return DIR_END; }
\\.GLOBL        { return DIR_GLOBL; }
\\.GLOBAL       { return DIR_GLOBL; }
\\.EXTERN       { return DIR_EXTERN; }
\\.EXTRN        { return DIR_EXTERN; }
\\.BYTE         { return DIR_BYTE; }
\\.WORD         { return DIR_WORD; }
\\.LONG         { return DIR_LONG; }
\\.ASCII        { return DIR_ASCII; }
\\.ASCIZ        { return DIR_ASCIZ; }
\\.BLKB         { return DIR_BLKB; }
\\.BLKW         { return DIR_BLKW; }
\\.BLKL         { return DIR_BLKL; }
\\.ALIGN        { return DIR_ALIGN; }
\\.EVEN         { return DIR_EVEN; }
\\.ODD          { return DIR_ODD; }
\\.PAGE         { return DIR_PAGE; }
\\.SBTTL        { return DIR_SBTTL; }

    /* Macro directives */
\\.MACRO        { return DIR_MACRO; }
\\.ENDM         { return DIR_ENDM; }
\\.MEXIT        { return DIR_MEXIT; }
\\.IRP          { return DIR_IRP; }
\\.IRPC         { return DIR_IRPC; }
\\.REPT         { return DIR_REPT; }
\\.ENDR         { return DIR_ENDR; }

    /* Conditional assembly */
\\.IF           { return DIR_IF; }
\\.IFF          { return DIR_IFF; }
\\.IFT          { return DIR_IFT; }
\\.IFTF         { return DIR_IFTF; }
\\.IFB          { return DIR_IFB; }
\\.IFNB         { return DIR_IFNB; }
\\.IFDEF        { return DIR_IFDEF; }
\\.IFNDEF       { return DIR_IFNDEF; }
\\.ENDC         { return DIR_ENDC; }

    /* Listing control */
\\.LIST         { return DIR_LIST; }
\\.NLIST        { return DIR_NLIST; }
\\.ENABLE       { return DIR_ENABLE; }
\\.DISABLE      { return DIR_DISABLE; }

    /* PDP-11/VAX Registers */
R0|r0           { yylval.ival = 0; return REGISTER; }
R1|r1           { yylval.ival = 1; return REGISTER; }
R2|r2           { yylval.ival = 2; return REGISTER; }
R3|r3           { yylval.ival = 3; return REGISTER; }
R4|r4           { yylval.ival = 4; return REGISTER; }
R5|r5           { yylval.ival = 5; return REGISTER; }
R6|r6|SP|sp     { yylval.ival = 6; return REGISTER; }
R7|r7|PC|pc     { yylval.ival = 7; return REGISTER; }
R8|r8           { yylval.ival = 8; return REGISTER; }
R9|r9           { yylval.ival = 9; return REGISTER; }
R10|r10         { yylval.ival = 10; return REGISTER; }
R11|r11         { yylval.ival = 11; return REGISTER; }
R12|r12|AP|ap   { yylval.ival = 12; return REGISTER; }
R13|r13|FP|fp   { yylval.ival = 13; return REGISTER; }
R14|r14         { yylval.ival = 14; return REGISTER; }
R15|r15         { yylval.ival = 15; return REGISTER; }

    /* PDP-10 Accumulators */
AC0|ac0         { yylval.ival = 0; return ACCUMULATOR; }
AC1|ac1         { yylval.ival = 1; return ACCUMULATOR; }
AC2|ac2         { yylval.ival = 2; return ACCUMULATOR; }
AC3|ac3         { yylval.ival = 3; return ACCUMULATOR; }
AC4|ac4         { yylval.ival = 4; return ACCUMULATOR; }
AC5|ac5         { yylval.ival = 5; return ACCUMULATOR; }
AC6|ac6         { yylval.ival = 6; return ACCUMULATOR; }
AC7|ac7         { yylval.ival = 7; return ACCUMULATOR; }
AC8|ac8         { yylval.ival = 8; return ACCUMULATOR; }
AC9|ac9         { yylval.ival = 9; return ACCUMULATOR; }
AC10|ac10       { yylval.ival = 10; return ACCUMULATOR; }
AC11|ac11       { yylval.ival = 11; return ACCUMULATOR; }
AC12|ac12       { yylval.ival = 12; return ACCUMULATOR; }
AC13|ac13       { yylval.ival = 13; return ACCUMULATOR; }
AC14|ac14       { yylval.ival = 14; return ACCUMULATOR; }
AC15|ac15       { yylval.ival = 15; return ACCUMULATOR; }

    /* Numbers - Decimal */
{DIGIT}+\\.?    {
	yylval.lval = strtol(yytext, NULL, 10);
	return NUMBER;
}

    /* Numbers - Hexadecimal (^X or 0x prefix) */
\\^X{HEXDIGIT}+ {
	yylval.lval = strtol(yytext + 2, NULL, 16);
	return NUMBER;
}

0[Xx]{HEXDIGIT}+ {
	yylval.lval = strtol(yytext, NULL, 16);
	return NUMBER;
}

    /* Numbers - Octal (^O prefix or leading 0) */
\\^O{OCTDIGIT}+ {
	yylval.lval = strtol(yytext + 2, NULL, 8);
	return NUMBER;
}

0{OCTDIGIT}+    {
	yylval.lval = strtol(yytext, NULL, 8);
	return NUMBER;
}

    /* Numbers - Binary (^B prefix) */
\\^B[01]+       {
	yylval.lval = strtol(yytext + 2, NULL, 2);
	return NUMBER;
}

    /* String literals - start with angle bracket or quote */
\\<             {
	string_ptr = string_buf;
	BEGIN(STRING);
}

\" {
	string_ptr = string_buf;
	BEGIN(STRING);
}

<STRING>\\>     {
	BEGIN(INITIAL);
	*string_ptr = '\0';
	yylval.sval = strdup(string_buf);
	return STRING_LITERAL;
}

<STRING>\"      {
	BEGIN(INITIAL);
	*string_ptr = '\0';
	yylval.sval = strdup(string_buf);
	return STRING_LITERAL;
}

<STRING>\\n     { *string_ptr++ = '\n'; }
<STRING>\\t     { *string_ptr++ = '\t'; }
<STRING>\\r     { *string_ptr++ = '\r'; }
<STRING>\\\\    { *string_ptr++ = '\\'; }
<STRING>\n      {
	error("unterminated string");
	BEGIN(INITIAL);
}

<STRING>.       { *string_ptr++ = yytext[0]; }

    /* Identifiers and labels */
{IDENT_START}{IDENT_CHAR}*: {
	/* Label definition (ends with colon) */
	yytext[yyleng-1] = '\0';  /* Remove trailing colon */
	yylval.sval = strdup(yytext);
	return LABEL;
}

{DIGIT}+\\$     {
	/* Local label */
	yylval.sval = strdup(yytext);
	return LOCAL_LABEL;
}

{IDENT_START}{IDENT_CHAR}* {
	/* Identifier (instruction, directive, or symbol) */
	yylval.sval = strdup(yytext);
	return IDENTIFIER;
}

    /* Operators and punctuation */
"#"             { return '#'; }
"@"             { return '@'; }
","             { return ','; }
"("             { return '('; }
")"             { return ')'; }
"+"             { return '+'; }
"-"             { return '-'; }
"*"             { return '*'; }
"/"             { return '/'; }
"="             { return '='; }
"<"             { return '<'; }
">"             { return '>'; }
"&"             { return '&'; }
"|"             { return '|'; }
"^"             { return '^'; }
"!"             { return '!'; }
"["             { return '['; }
"]"             { return ']'; }

    /* Whitespace */
{WHITESPACE}+   { /* Ignore */ }

    /* Newline */
\n              { lineno++; return NEWLINE; }

    /* Anything else is an error */
.               {
	error("unexpected character '%c' (0x%02x)", yytext[0], yytext[0]);
}

%%

/*
 * Called when EOF is reached
 */
int
yywrap(void)
{
	return 1;
}
