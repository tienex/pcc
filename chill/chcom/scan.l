/*
 * Copyright (c) 2025 PCC CHILL Compiler
 *
 * Lexical analyzer for CHILL
 * CCITT High Level Language (Z.200)
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
IDENT_START     {LETTER}|_
IDENT_CHAR      ({LETTER}|{DIGIT}|_)
WHITESPACE      [ \t\r]

%%

    /* Comments */
"/*" {
	comment_level = 1;
	BEGIN(COMMENT);
}

<COMMENT>[/][*] {
	comment_level++;
}

<COMMENT>[*][/] {
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

    /* Reserved words - CHILL keywords */
"ACCESS"        { return ACCESS; }
"AND"           { return AND; }
"ARRAY"         { return ARRAY; }
"ASSERT"        { return ASSERT; }
"AT"            { return AT; }
"BASED"         { return BASED; }
"BEGIN"         { return BBEGIN; }
"BIN"           { return BIN; }
"BOOL"          { return BOOL; }
"BY"            { return BY; }
"CALL"          { return CALL; }
"CASE"          { return CASE; }
"CAUSE"         { return CAUSE; }
"CHAR"          { return CHAR; }
"CHARS"         { return CHARS; }
"CONTEXT"       { return CONTEXT; }
"CONTINUE"      { return CONTINUE; }
"CYCLE"         { return CYCLE; }
"DCL"           { return DCL; }
"DELAY"         { return DELAY; }
"DO"            { return DO; }
"DOWN"          { return DOWN; }
"DURATION"      { return DURATION; }
"DYNAMIC"       { return DYNAMIC; }
"ELSE"          { return ELSE; }
"ELSIF"         { return ELSIF; }
"END"           { return END; }
"ESAC"          { return ESAC; }
"EVENT"         { return EVENT; }
"EXCEPTIONS"    { return EXCEPTIONS; }
"EXIT"          { return EXIT; }
"FI"            { return FI; }
"FOR"           { return FOR; }
"FORBID"        { return FORBID; }
"GENERAL"       { return GENERAL; }
"GOTO"          { return GOTO; }
"GRANT"         { return GRANT; }
"IF"            { return IF; }
"IN"            { return IN; }
"INIT"          { return INIT; }
"INLINE"        { return INLINE; }
"INOUT"         { return INOUT; }
"INT"           { return INT; }
"LOC"           { return LOC; }
"MOD"           { return MOD; }
"MODULE"        { return MODULE; }
"NEWMODE"       { return NEWMODE; }
"NONREF"        { return NONREF; }
"NOT"           { return NOT; }
"OD"            { return OD; }
"OF"            { return OF; }
"ON"            { return ON; }
"OR"            { return OR; }
"OUT"           { return OUT; }
"POWERSET"      { return POWERSET; }
"PRIORITY"      { return PRIORITY; }
"PROC"          { return PROC; }
"PROCESS"       { return PROCESS; }
"READ"          { return READ; }
"REAL"          { return REAL; }
"RECEIVE"       { return RECEIVE; }
"RECURSIVE"     { return RECURSIVE; }
"REF"           { return REF; }
"REGION"        { return REGION; }
"REM"           { return REM; }
"REMOTE"        { return REMOTE; }
"RESULT"        { return RESULT; }
"RETURN"        { return RETURN; }
"RETURNS"       { return RETURNS; }
"SEIZE"         { return SEIZE; }
"SEND"          { return SEND; }
"SET"           { return SET; }
"SIGNAL"        { return SIGNAL; }
"SIMPLE"        { return SIMPLE; }
"SPEC"          { return SPEC; }
"START"         { return START; }
"STATIC"        { return STATIC; }
"STEP"          { return STEP; }
"STOP"          { return STOP; }
"STRUCT"        { return STRUCT; }
"SYN"           { return SYN; }
"SYNMODE"       { return SYNMODE; }
"TEXT"          { return TEXT; }
"THEN"          { return THEN; }
"THIS"          { return THIS; }
"TIMEOUT"       { return TIMEOUT; }
"TIME"          { return TIME; }
"TO"            { return TO; }
"TYPE"          { return TYPE; }
"UP"            { return UP; }
"WHILE"         { return WHILE; }
"WITH"          { return WITH; }
"XOR"           { return XOR; }

    /* Predefined modes/types */
"BYTE"          { return BYTE; }
"UBYTE"         { return UBYTE; }
"SHORT"         { return SHORT; }
"USHORT"        { return USHORT; }
"LONG"          { return LONG; }
"ULONG"         { return ULONG; }
"TRUE"          { return CTRUE; }
"FALSE"         { return CFALSE; }
"NULL"          { return CNULL; }

    /* Operators and punctuation */
":="            { return ASSIGN; }
"="             { return EQ; }
"/="            { return NE; }
"<"             { return LT; }
"<="            { return LE; }
">"             { return GT; }
">="            { return GE; }
"+"             { return PLUS; }
"-"             { return MINUS; }
"*"             { return MULT; }
"/"             { return DIV; }
"//"            { return CONCAT; }
"->"            { return ARROW; }
"("             { return '('; }
")"             { return ')'; }
"["             { return '['; }
"]"             { return ']'; }
"{"             { return '{'; }
"}"             { return '}'; }
":"             { return ':'; }
";"             { return ';'; }
","             { return ','; }
"."             { return '.'; }

    /* Identifiers */
{IDENT_START}{IDENT_CHAR}* {
	yylval.sval = strdup(yytext);
	return IDENT;
}

    /* Integer literals */
{DIGIT}+ {
	yylval.ival = atoll(yytext);
	return INTLIT;
}

    /* Floating-point literals */
{DIGIT}+"."{DIGIT}+([eE][+-]?{DIGIT}+)? {
	yylval.dval = atof(yytext);
	return REALLIT;
}

    /* Character literals */
"'"."'" {
	/* Single character in single quotes */
	yylval.cval = yytext[1];
	return CHARLIT;
}

    /* String literals */
\"([^\\\"]|\\.)*\" {
	/* Double-quoted string */
	yylval.sval = strdup(yytext);
	return STRINGLIT;
}

    /* Whitespace */
{WHITESPACE}+   { /* Ignore */ }

    /* Newlines */
\n {
	lineno++;
	column = 1;
}

    /* Anything else is an error */
. {
	error("illegal character '%c' (0x%02x)", yytext[0], (unsigned char)yytext[0]);
}

%%
