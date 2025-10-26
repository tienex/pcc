/*
 * Copyright (c) 2025 PCC BLISS Compiler
 *
 * Lexical analyzer for BLISS (Basic Language for Implementation of System Software)
 * BLISS is a system programming language developed at Carnegie Mellon University
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

%}

%option noyywrap
%option yylineno

%x COMMENT

DIGIT           [0-9]
LETTER          [a-zA-Z]
IDENT_START     ({LETTER}|"_"|"$"|"%")
IDENT_CHAR      ({LETTER}|{DIGIT}|"_"|"$"|"%")
WHITESPACE      [ \t\r]
OCTAL_DIGIT     [0-7]
HEX_DIGIT       [0-9a-fA-F]

%%

    /* Comments - ! ... newline style */
^[ \t]*"!".*\n {
	lineno++;
	column = 1;
}

"!".*\n {
	lineno++;
	column = 1;
}

    /* Reserved words - BLISS control structures */
"BEGIN"         { return BBEGIN; }
"END"           { return END; }
"MODULE"        { return MODULE; }
"ROUTINE"       { return ROUTINE; }
"GLOBAL"        { return GLOBAL; }
"BIND"          { return BIND; }
"LITERAL"       { return LITERAL; }
"LOCAL"         { return LOCAL; }
"FORWARD"       { return FORWARD; }
"EXTERNAL"      { return EXTERNAL; }
"OWN"           { return OWN; }
"REGISTER"      { return REGISTER; }
"STACKLOCAL"    { return STACKLOCAL; }

    /* Conditional structures */
"IF"            { return IF; }
"THEN"          { return THEN; }
"ELSE"          { return ELSE; }
"CASE"          { return CASE; }
"OF"            { return OF; }
"OUTRANGE"      { return OUTRANGE; }
"INRANGE"       { return INRANGE; }
"SELECTONE"     { return SELECTONE; }
"SELECTA"       { return SELECTA; }
"SELECTU"       { return SELECTU; }
"OTHERWISE"     { return OTHERWISE; }
"TES"           { return TES; }

    /* Loop structures */
"DO"            { return DO; }
"WHILE"         { return WHILE; }
"UNTIL"         { return UNTIL; }
"INCR"          { return INCR; }
"DECR"          { return DECR; }
"FROM"          { return FROM; }
"TO"            { return TO; }
"BY"            { return BY; }

    /* Control flow */
"RETURN"        { return RETURN; }
"LEAVE"         { return LEAVE; }
"EXITLOOP"      { return EXITLOOP; }

    /* Block expressions */
"LABEL"         { return LABEL; }

    /* Data types and modifiers */
"VECTOR"        { return VECTOR; }
"BLOCKVECTOR"   { return BLOCKVECTOR; }
"STRUCTURE"     { return STRUCTURE; }
"FIELD"         { return FIELD; }
"SET"           { return SET; }
"PRESET"        { return PRESET; }
"REF"           { return REF; }
"CODECOMMENT"   { return CODECOMMENT; }

    /* Operators and special forms */
"LINKAGE"       { return LINKAGE; }
"PLIT"          { return PLIT; }
"UPLIT"         { return UPLIT; }
"ENABLE"        { return ENABLE; }
"SIGNAL"        { return SIGNAL; }

    /* Comparison operators */
"EQL"           { return EQL; }
"EQLU"          { return EQLU; }
"NEQ"           { return NEQ; }
"NEQU"          { return NEQU; }
"LSS"           { return LSS; }
"LSSU"          { return LSSU; }
"LEQ"           { return LEQ; }
"LEQU"          { return LEQU; }
"GTR"           { return GTR; }
"GTRU"          { return GTRU; }
"GEQ"           { return GEQ; }
"GEQU"          { return GEQU; }

    /* Logical operators */
"NOT"           { return NOT; }
"AND"           { return AND; }
"OR"            { return OR; }
"XOR"           { return XOR; }
"EQV"           { return EQV; }

    /* Operators */
"="             { return EQ; }
":"             { return COLON; }
";"             { return SEMICOLON; }
","             { return COMMA; }
"."             { return DOT; }
"+"             { return PLUS; }
"-"             { return MINUS; }
"*"             { return STAR; }
"/"             { return SLASH; }
"^"             { return UPARROW; }
"<"             { return LANGLE; }
">"             { return RANGLE; }
"["             { return LBRACK; }
"]"             { return RBRACK; }
"("             { return LPAREN; }
")"             { return RPAREN; }
"@"             { return AT; }
"%("            { return PERCENTLP; }
"<"[0-9]+">"    {
	/* Angle bracket constant <n> */
	yylval.ival = atoi(yytext+1);
	return ANGLECONST;
}
"%<"            { return PERCENTLANGLE; }
">%"            { return RANGLEPCENT; }

    /* Integer constants - decimal */
{DIGIT}+ {
	yylval.ival = atoi(yytext);
	return INTCONST;
}

    /* Integer constants - octal (prefixed with %O) */
"%O'"{OCTAL_DIGIT}+|"%o'"{OCTAL_DIGIT}+ {
	yylval.ival = (int)strtol(yytext+3, NULL, 8);
	return INTCONST;
}

    /* Integer constants - hexadecimal (prefixed with %X) */
"%X'"{HEX_DIGIT}+|"%x'"{HEX_DIGIT}+ {
	yylval.ival = (int)strtol(yytext+3, NULL, 16);
	return INTCONST;
}

    /* Integer constants - binary (prefixed with %B) */
"%B'"[01]+|"%b'"[01]+ {
	yylval.ival = (int)strtol(yytext+3, NULL, 2);
	return INTCONST;
}

    /* String literals - single-quoted */
'([^'\n]|'')*' {
	char *s = malloc(strlen(yytext) + 1);
	char *p = s;
	char *q = yytext + 1;  /* Skip opening quote */

	/* Copy string, handling doubled quotes */
	while (*q && *q != '\'') {
		if (*q == '\'' && *(q+1) == '\'') {
			*p++ = '\'';
			q += 2;
		} else {
			*p++ = *q++;
		}
	}
	*p = '\0';

	yylval.sval = s;
	return STRINGCONST;
}

    /* Character constants - %C'x' */
"%C'".|"%c'". {
	yylval.ival = (int)(unsigned char)yytext[3];
	return INTCONST;
}

    /* Identifiers */
{IDENT_START}{IDENT_CHAR}* {
	yylval.sval = strdup(yytext);
	return IDENT;
}

    /* Whitespace */
{WHITESPACE}+ {
	/* Ignore */
}

    /* Newline */
\n {
	lineno++;
	column = 1;
}

    /* Anything else is an error */
. {
	error("unexpected character '%c' (0x%02x)", yytext[0], (unsigned char)yytext[0]);
}

%%

/* Nothing here */
