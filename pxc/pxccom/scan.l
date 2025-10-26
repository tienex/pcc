/*
 * Copyright (c) 2025 PCC Xbase++ Compiler
 *
 * Lexical analyzer for Xbase++
 * Supports Xbase++ / Clipper / Harbour / xHarbour syntax
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "pass1.h"
#include "y.tab.h"

/* Track line and column */
int lineno = 1;
static int column = 1;

/* Update column position */
#define YY_USER_ACTION column += yyleng;

/* String buffer for string literals */
static char string_buf[4096];
static int string_len;

%}

%option noyywrap
%option case-insensitive

%x COMMENT
%x LINECOMMENT
%x STRING
%x DSTRING

DIGIT           [0-9]
LETTER          [a-zA-Z_]
IDENT_START     {LETTER}
IDENT_CHAR      ({LETTER}|{DIGIT})
WHITESPACE      [ \t\r]
HEXDIGIT        [0-9a-fA-F]

%%

    /* Comments - C-style /* ... */ */
"/*" {
	BEGIN(COMMENT);
}

<COMMENT>"*/" {
	BEGIN(INITIAL);
}

<COMMENT>\n {
	lineno++;
	column = 1;
}

<COMMENT>. {
	/* Ignore comment content */
}

    /* Comments - C++-style // ... */
"//" {
	BEGIN(LINECOMMENT);
}

    /* Comments - Xbase++ style && ... or * at start of line */
"&&" {
	BEGIN(LINECOMMENT);
}

^"*".* {
	/* Line starting with * is comment in Xbase++ */
	lineno++;
	column = 1;
}

^"NOTE".* {
	/* NOTE at start of line is also comment */
	lineno++;
	column = 1;
}

<LINECOMMENT>\n {
	lineno++;
	column = 1;
	BEGIN(INITIAL);
}

<LINECOMMENT>. {
	/* Ignore comment content */
}

    /* Reserved words - organized by category */

    /* Program structure */
"FUNCTION"      { return FUNCTION; }
"PROCEDURE"     { return PROCEDURE; }
"RETURN"        { return RETURN; }
"PARAMETERS"    { return PARAMETERS; }

    /* Variable declarations */
"LOCAL"         { return LOCAL; }
"STATIC"        { return STATIC; }
"PUBLIC"        { return PUBLIC; }
"PRIVATE"       { return PRIVATE; }
"MEMVAR"        { return MEMVAR; }
"FIELD"         { return FIELD; }

    /* Control structures */
"IF"            { return IF; }
"ELSE"          { return ELSE; }
"ELSEIF"        { return ELSEIF; }
"ENDIF"         { return ENDIF; }
"IIF"           { return IIF; }

"DO"            { return DO; }
"WHILE"         { return WHILE; }
"ENDDO"         { return ENDDO; }
"EXIT"          { return EXIT; }
"LOOP"          { return LOOP; }

"FOR"           { return FOR; }
"TO"            { return TO; }
"STEP"          { return STEP; }
"NEXT"          { return NEXT; }

"CASE"          { return CASE; }
"OTHERWISE"     { return OTHERWISE; }
"ENDCASE"       { return ENDCASE; }

"BEGIN"         { return BBEGIN; }
"SEQUENCE"      { return SEQUENCE; }
"END"           { return END; }
"RECOVER"       { return RECOVER; }
"BREAK"         { return BREAK; }

    /* OOP keywords */
"CLASS"         { return CLASS; }
"ENDCLASS"      { return ENDCLASS; }
"METHOD"        { return METHOD; }
"ENDMETHOD"     { return ENDMETHOD; }
"DATA"          { return DATA; }
"VAR"           { return VAR; }
"INLINE"        { return INLINE; }
"VIRTUAL"       { return VIRTUAL; }
"CONSTRUCTOR"   { return CONSTRUCTOR; }
"DESTRUCTOR"    { return DESTRUCTOR; }
"INHERIT"       { return INHERIT; }
"FROM"          { return FROM; }
"EXPORT"        { return EXPORT; }
"PROTECTED"     { return PROTECTED; }
"HIDDEN"        { return HIDDEN; }
"READONLY"      { return READONLY; }
"SHARED"        { return SHARED; }
"SYNC"          { return SYNC; }

    /* Database commands */
"USE"           { return USE; }
"SELECT"        { return SELECT; }
"GO"            { return GO; }
"GOTO"          { return GOTO; }
"SKIP"          { return SKIP; }
"SEEK"          { return SEEK; }
"LOCATE"        { return LOCATE; }
"CONTINUE"      { return CONTINUE; }
"REPLACE"       { return REPLACE; }
"DELETE"        { return DELETE; }
"RECALL"        { return RECALL; }
"PACK"          { return PACK; }
"ZAP"           { return ZAP; }
"APPEND"        { return APPEND; }
"BLANK"         { return BLANK; }
"INDEX"         { return INDEX; }
"REINDEX"       { return REINDEX; }
"SET"           { return SET; }
"CLOSE"         { return CLOSE; }
"COMMIT"        { return COMMIT; }
"UNLOCK"        { return UNLOCK; }
"ALIAS"         { return ALIAS; }
"IN"            { return IN; }
"EXCLUSIVE"     { return EXCLUSIVE; }
"SHARED"        { return SHARED; }
"NEW"           { return NEW; }
"ADDITIVE"      { return ADDITIVE; }

    /* Other keywords */
"WITH"          { return WITH; }
"REQUEST"       { return REQUEST; }
"EXTERNAL"      { return EXTERNAL; }
"INIT"          { return INIT; }
"EXIT"          { return EXIT; }
"ANNOUNCE"      { return ANNOUNCE; }
"AS"            { return AS; }
"IS"            { return IS; }

    /* Logical constants */
".T."           { return LTRUE; }
".TRUE."        { return LTRUE; }
".Y."           { return LTRUE; }
".F."           { return LFALSE; }
".FALSE."       { return LFALSE; }
".N."           { return LFALSE; }

    /* Logical operators */
".AND."         { return AND; }
".OR."          { return OR; }
".NOT."         { return NOT; }

    /* NIL */
"NIL"           { return NIL; }

    /* Operators */
":="            { return ASSIGN; }
"+="            { return PLUSASSIGN; }
"-="            { return MINUSASSIGN; }
"*="            { return MULASSIGN; }
"/="            { return DIVASSIGN; }
"%="            { return MODASSIGN; }
"^="            { return EXPASSIGN; }

"=="            { return EQ; }
"="             { return EQ; }
"!="            { return NE; }
"<>"            { return NE; }
"#"             { return NE; }
"<"             { return LT; }
"<="            { return LE; }
">"             { return GT; }
">="            { return GE; }
"$"             { return SUBSTR; }

"++"            { return INCR; }
"--"            { return DECR; }

"**"            { return POWER; }
"^"             { return POWER; }

"->"            { return ARROW; }
"::"            { return DCOLON; }

    /* Single character tokens */
"+"             { return '+'; }
"-"             { return '-'; }
"*"             { return '*'; }
"/"             { return '/'; }
"%"             { return '%'; }
"("             { return '('; }
")"             { return ')'; }
"["             { return '['; }
"]"             { return ']'; }
"{"             { return '{'; }
"}"             { return '}'; }
","             { return ','; }
";"             { return ';'; }
":"             { return ':'; }
"."             { return '.'; }
"@"             { return '@'; }
"&"             { return '&'; }
"|"             { return '|'; }

    /* String literals - double quoted */
\" {
	string_len = 0;
	BEGIN(DSTRING);
}

<DSTRING>\" {
	BEGIN(INITIAL);
	string_buf[string_len] = '\0';
	yylval.sval = strdup(string_buf);
	return STRING;
}

<DSTRING>\\n {
	string_buf[string_len++] = '\n';
}

<DSTRING>\\t {
	string_buf[string_len++] = '\t';
}

<DSTRING>\\r {
	string_buf[string_len++] = '\r';
}

<DSTRING>\\\\ {
	string_buf[string_len++] = '\\';
}

<DSTRING>\\\" {
	string_buf[string_len++] = '"';
}

<DSTRING>\n {
	/* Strings can span multiple lines in Xbase++ */
	string_buf[string_len++] = '\n';
	lineno++;
	column = 1;
}

<DSTRING>. {
	string_buf[string_len++] = yytext[0];
}

    /* String literals - single quoted */
\' {
	string_len = 0;
	BEGIN(STRING);
}

<STRING>\' {
	BEGIN(INITIAL);
	string_buf[string_len] = '\0';
	yylval.sval = strdup(string_buf);
	return STRING;
}

<STRING>\\\' {
	string_buf[string_len++] = '\'';
}

<STRING>\n {
	/* Strings can span multiple lines */
	string_buf[string_len++] = '\n';
	lineno++;
	column = 1;
}

<STRING>. {
	string_buf[string_len++] = yytext[0];
}

    /* String literals - bracket delimited [] */
\[\"[^\]]*\"\] {
	/* [" ... "] style strings */
	int len = yyleng - 4;  /* Remove [" and "] */
	char *str = malloc(len + 1);
	strncpy(str, yytext + 2, len);
	str[len] = '\0';
	yylval.sval = str;
	return STRING;
}

    /* Numeric literals */
{DIGIT}+"."{DIGIT}* {
	yylval.fval = atof(yytext);
	return FLOAT;
}

"."{DIGIT}+ {
	yylval.fval = atof(yytext);
	return FLOAT;
}

{DIGIT}+ {
	yylval.ival = atoll(yytext);
	return INTEGER;
}

    /* Hexadecimal numbers */
"0x"{HEXDIGIT}+ {
	yylval.ival = strtoll(yytext + 2, NULL, 16);
	return INTEGER;
}

    /* Date literals { ^ YYYY/MM/DD } or { ^ YYYY-MM-DD } */
"{""^"[ \t]*{DIGIT}{4}"/"{DIGIT}{2}"/"{DIGIT}{2}[ \t]*"}" {
	yylval.sval = strdup(yytext);
	return DATE;
}

"{""^"[ \t]*{DIGIT}{4}"-"{DIGIT}{2}"-"{DIGIT}{2}[ \t]*"}" {
	yylval.sval = strdup(yytext);
	return DATE;
}

    /* Identifiers */
{IDENT_START}{IDENT_CHAR}* {
	yylval.sval = strdup(yytext);
	return IDENT;
}

    /* Ampersand macro variable */
"&"{IDENT_START}{IDENT_CHAR}* {
	yylval.sval = strdup(yytext + 1);  /* Skip & */
	return MACROVAR;
}

    /* Whitespace */
{WHITESPACE}+ {
	/* Ignore whitespace */
}

    /* Newline - statement separator in Xbase++ */
\n {
	lineno++;
	column = 1;
	return EOL;
}

    /* Continuation - semicolon allows multi-statement lines */
";" {
	return ';';
}

    /* Anything else is an error */
. {
	yyerror("Invalid character");
}

%%

/* Additional C code */
void reset_lexer(void) {
	lineno = 1;
	column = 1;
}
