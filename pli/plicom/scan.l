/*
 * Copyright (c) 2025 PCC PL/I Compiler
 *
 * Lexical analyzer for PL/I, PL/M, and related dialects
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

/* Update location tracking */
#define YY_USER_ACTION \
	current_column = column; \
	column += yyleng;

/* Comment nesting level */
static int comment_level = 0;

/* String buffer for string/bit literals */
static char string_buf[4096];
static char *string_buf_ptr;

/* Helper to normalize keyword (uppercase, check abbreviation) */
static char *normalize_keyword(char *s);
static int keyword_match(const char *input, const char *full, int minlen);

%}

%option noyywrap
%option yylineno
%option case-insensitive

%x COMMENT
%x CPPCOMMENT
%x STRING
%x BITSTRING

DIGIT           [0-9]
LETTER          [a-zA-Z]
IDENT_START     {LETTER}
IDENT_CHAR      ({LETTER}|{DIGIT}|[@#$])
WHITESPACE      [ \t\r]

%%

    /* Comments - /* ... */ style */
"/*" {
	comment_level = 1;
	BEGIN(COMMENT);
}

<COMMENT>"/*" {
	if (ALLOW_NESTED_COMMENTS())
		comment_level++;
}

<COMMENT>"*/" {
	comment_level--;
	if (comment_level == 0)
		BEGIN(INITIAL);
}

<COMMENT>\n {
	lineno++;
	column = 1;
}

<COMMENT><<EOF>> {
	error("unterminated comment");
	yyterminate();
}

<COMMENT>. {
	/* Ignore comment content */
}

    /* C++-style comments (for PL/M and modern dialects) */
"//" {
	if (ALLOW_CPP_COMMENTS()) {
		BEGIN(CPPCOMMENT);
	} else {
		return DIV;  /* Division operator */
	}
}

<CPPCOMMENT>\n {
	lineno++;
	column = 1;
	BEGIN(INITIAL);
}

<CPPCOMMENT>. {
	/* Ignore comment content */
}

    /* String literals */
'([^'\n]|'')* {
	/* Character string - handle '' as escaped quote */
	char *p = yytext + 1;  /* Skip opening ' */
	string_buf_ptr = string_buf;
	while (*p && *p != '\n') {
		if (*p == '\'') {
			if (*(p+1) == '\'') {
				*string_buf_ptr++ = '\'';
				p += 2;
			} else {
				break;  /* End quote */
			}
		} else {
			*string_buf_ptr++ = *p++;
		}
	}
	*string_buf_ptr = '\0';
	yylval.sval = strdup(string_buf);
	return STRING_LITERAL;
}

    /* Bit string literals */
'[01]+'B {
	/* Bit string like '1010'B */
	yytext[yyleng-2] = '\0';  /* Remove 'B */
	yylval.sval = strdup(yytext + 1);  /* Skip opening ' */
	return BIT_LITERAL;
}

\"[01]+\"B {
	/* Bit string like "1010"B */
	yytext[yyleng-2] = '\0';  /* Remove 'B */
	yylval.sval = strdup(yytext + 1);  /* Skip opening " */
	return BIT_LITERAL;
}

    /* Reserved words - PL/I keywords */
    /* Note: PL/I allows abbreviations, handled by keyword_match */

"ALLOCATE"|"ALLOC" { return ALLOCATE; }
"BEGIN" { return BBEGIN; }
"BY" { return BY; }
"CALL" { return CALL; }
"CLOSE" { return CLOSE; }
"DECLARE"|"DCL" { return DECLARE; }
"DO" { return DO; }
"ELSE" { return ELSE; }
"END" { return END; }
"ENTRY"|"ENT" { return ENTRY; }
"EXIT" { return EXIT; }
"FILE" { return FFILE; }
"FORMAT"|"FMT" { return FORMAT; }
"FREE" { return FREE; }
"GET" { return GET; }
"GO" { return GO; }
"GOTO" { return GOTO; }
"IF" { return IF; }
"LEAVE" { return LEAVE; }
"ON" { return ON; }
"OPEN" { return OPEN; }
"PROCEDURE"|"PROC" { return PROCEDURE; }
"PUT" { return PUT; }
"READ" { return READ; }
"RETURN" { return RETURN; }
"REVERT" { return REVERT; }
"SELECT" { return SELECT; }
"SIGNAL" { return SIGNAL; }
"STOP" { return STOP; }
"THEN" { return THEN; }
"TO" { return TO; }
"WAIT" { return WAIT; }
"WHEN" { return WHEN; }
"WHILE" { return WHILE; }
"WRITE" { return WRITE; }

    /* Data types and attributes */
"FIXED"|"FIX" { return FIXED; }
"FLOAT" { return FLOAT; }
"BINARY"|"BIN" { return BINARY; }
"DECIMAL"|"DEC" { return DECIMAL; }
"BIT" { return BIT; }
"CHARACTER"|"CHAR" { return CHARACTER; }
"VARYING"|"VAR" { return VARYING; }
"POINTER"|"PTR" { return POINTER; }
"OFFSET" { return OFFSET; }
"AREA" { return AREA; }
"LABEL"|"LBL" { return LABEL; }
"ENTRY" { return ENTRY; }
"FORMAT" { return FORMAT; }

    /* Storage classes */
"AUTOMATIC"|"AUTO" { return AUTOMATIC; }
"STATIC" { return STATIC; }
"BASED" { return BASED; }
"CONTROLLED"|"CTL" { return CONTROLLED; }
"DEFINED"|"DEF" { return DEFINED; }

    /* Attributes */
"ALIGNED" { return ALIGNED; }
"UNALIGNED"|"UNAL" { return UNALIGNED; }
"SIGNED" { return SIGNED; }
"UNSIGNED" { return UNSIGNED; }
"INITIAL"|"INIT" { return INITIAL; }
"EXTERNAL"|"EXT" { return EXTERNAL; }
"INTERNAL"|"INT" { return INTERNAL; }
"BUILTIN" { return BUILTIN; }
"GENERIC" { return GENERIC; }
"RECURSIVE"|"RECU" { return RECURSIVE; }
"REENTRANT" { return REENTRANT; }
"OPTIONS" { return OPTIONS; }
"MAIN" { return MAIN; }
"RETURNS" { return RETURNS; }

    /* I/O keywords */
"STREAM" { return STREAM; }
"RECORD" { return RECORD; }
"INPUT" { return INPUT; }
"OUTPUT" { return OUTPUT; }
"UPDATE" { return UPDATE; }
"SEQUENTIAL"|"SEQ" { return SEQUENTIAL; }
"DIRECT" { return DIRECT; }
"KEYED" { return KEYED; }
"BACKWARDS"|"BACK" { return BACKWARDS; }
"ENVIRONMENT"|"ENV" { return ENVIRONMENT; }

    /* PL/M specific keywords */
"ADDRESS" {
	if (IS_PLM()) return ADDRESS;
	else return IDENT;
}
"AT" {
	if (IS_PLM() && ALLOW_LITERALLY()) return AT;
	else return IDENT;
}
"BYTE" {
	if (IS_PLM()) return BYTE;
	else return IDENT;
}
"WORD" {
	if (IS_PLM()) return WORD;
	else return IDENT;
}
"DWORD" {
	if (IS_PLM()) return DWORD;
	else return IDENT;
}
"DATA" {
	if (IS_PLM()) return DATA;
	else return IDENT;
}
"INTERRUPT" {
	if (IS_PLM() && ALLOW_INTERRUPT()) return INTERRUPT;
	else return IDENT;
}
"LITERALLY" {
	if (IS_PLM() && ALLOW_LITERALLY()) return LITERALLY;
	else return IDENT;
}
"PUBLIC" {
	if (IS_PLM()) return PUBLIC;
	else return IDENT;
}
"REENTRANT" {
	if (IS_PLM()) return REENTRANT;
	else return IDENT;
}
"REAL" {
	if (IS_PLM()) return REAL;
	else return IDENT;
}
"INTEGER" {
	if (IS_PLM()) return INTEGER;
	else return IDENT;
}

    /* Control flow */
"CASE" { return CASE; }
"OTHERWISE" { return OTHERWISE; }
"ITERATE"|"ITER" { return ITERATE; }

    /* Preprocessor */
"%ACTIVATE"|"%ACT" { return PP_ACTIVATE; }
"%DECLARE"|"%DCL" { return PP_DECLARE; }
"%DEACTIVATE"|"%DEACT" { return PP_DEACTIVATE; }
"%INCLUDE"|"%INC" { return PP_INCLUDE; }
"%REPLACE" { return PP_REPLACE; }

    /* Identifiers */
{IDENT_START}{IDENT_CHAR}* {
	if (ALLOW_UNDERSCORES() || ALLOW_DOLLAR_IDENT()) {
		/* Accept _ and $ in identifiers */
	}
	yylval.sval = strdup(yytext);
	return IDENT;
}

    /* Numeric constants */
{DIGIT}+ {
	/* Integer */
	yylval.ival = atoi(yytext);
	return INTEGER_CONST;
}

{DIGIT}+"."{DIGIT}* {
	/* Floating point */
	yylval.fval = atof(yytext);
	return FLOAT_CONST;
}

{DIGIT}*"."{DIGIT}+ {
	/* Floating point starting with . */
	yylval.fval = atof(yytext);
	return FLOAT_CONST;
}

{DIGIT}+[Ee][+-]?{DIGIT}+ {
	/* Scientific notation */
	yylval.fval = atof(yytext);
	return FLOAT_CONST;
}

{DIGIT}+"."{DIGIT}*[Ee][+-]?{DIGIT}+ {
	/* Scientific notation with decimal */
	yylval.fval = atof(yytext);
	return FLOAT_CONST;
}

    /* Operators and delimiters */
"=" { return ASSIGN; }
"==" { return EQ; }
"^=" { return NE; }
"~=" { return NE; }
"<" { return LT; }
"<=" { return LE; }
">" { return GT; }
">=" { return GE; }
"->" { return ARROW; }
"||" { return CONCAT; }
"**" { return POWER; }
"+" { return PLUS; }
"-" { return MINUS; }
"*" { return STAR; }
"/" { return DIV; }
"&" { return AND; }
"|" { return OR; }
"^" { return NOT; }
"(" { return LPAREN; }
")" { return RPAREN; }
"[" { return LBRACK; }
"]" { return RBRACK; }
"," { return COMMA; }
";" { return SEMI; }
":" { return COLON; }
"." { return DOT; }

    /* Whitespace */
{WHITESPACE}+ {
	/* Ignore whitespace */
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

/* Helper function to normalize keywords for abbreviation matching */
static char *normalize_keyword(char *s) {
	static char buf[256];
	char *p = buf;
	while (*s) {
		*p++ = toupper(*s++);
	}
	*p = '\0';
	return buf;
}

/* Check if input matches full keyword with minimum length */
static int keyword_match(const char *input, const char *full, int minlen) {
	int len = strlen(input);
	if (len < minlen || len > strlen(full))
		return 0;
	return strncasecmp(input, full, len) == 0;
}
