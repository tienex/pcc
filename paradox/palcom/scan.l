/*
 * Copyright (c) 2025 PCC Paradox PAL Compiler
 *
 * Lexical analyzer for PAL and ObjectPAL
 * Supports multiple Paradox/ObjectPAL versions
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "pass1.h"
#include "y.tab.h"

/* Track column position */
int current_column = 1;

/* Update location tracking */
#define YY_USER_ACTION \
	current_loc.column = current_column; \
	current_loc.line = yylineno; \
	current_column += yyleng;

/* Comment nesting level */
static int comment_level = 0;

/* String buffer for building string literals */
static char string_buf[4096];
static char *string_ptr;

/* Case-insensitive keyword lookup */
static int keyword_or_ident(char *text);

%}

%option noyywrap
%option yylineno
%option case-insensitive

%x COMMENT
%x LINECOMMENT
%x STRING

DIGIT           [0-9]
LETTER          [a-zA-Z]
IDENT_START     ({LETTER}|_)
IDENT_CHAR      ({LETTER}|{DIGIT}|_)
WHITESPACE      [ \t\r]
HEXDIGIT        [0-9a-fA-F]

%%

    /* Comments - ; style (PAL classic) */
^[ \t]*";" {
	if (dialect_features->allow_semicolon_comments) {
		BEGIN(LINECOMMENT);
	} else {
		return SEMICOLON;
	}
}

    /* Comments - // style (ObjectPAL) */
"//" {
	if (dialect_features->allow_slash_comments) {
		BEGIN(LINECOMMENT);
	} else {
		error("C++-style comments not allowed in %s",
		      get_dialect_name(current_dialect));
	}
}

<LINECOMMENT>\n {
	lineno++;
	current_column = 1;
	BEGIN(INITIAL);
}

<LINECOMMENT>. {
	/* Ignore comment content */
}

    /* Block comments - { ... } style */
"{" {
	if (dialect_features->allow_block_comments) {
		comment_level = 1;
		BEGIN(COMMENT);
	} else {
		return LBRACE;
	}
}

<COMMENT>"{" {
	comment_level++;
}

<COMMENT>"}" {
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

    /* String literals - double quotes */
\" {
	string_ptr = string_buf;
	BEGIN(STRING);
}

<STRING>\" {
	BEGIN(INITIAL);
	*string_ptr = '\0';
	yylval.sval = strdup(string_buf);
	return STRINGCONST;
}

<STRING>\n {
	error("Unterminated string literal");
	lineno++;
	current_column = 1;
	BEGIN(INITIAL);
}

<STRING>\\n  { *string_ptr++ = '\n'; }
<STRING>\\t  { *string_ptr++ = '\t'; }
<STRING>\\r  { *string_ptr++ = '\r'; }
<STRING>\\\"  { *string_ptr++ = '"'; }
<STRING>\\\\  { *string_ptr++ = '\\'; }
<STRING>\\[0-7]{1,3} {
	/* Octal escape sequence */
	int result;
	sscanf(yytext + 1, "%o", &result);
	*string_ptr++ = result;
}

<STRING>. {
	*string_ptr++ = *yytext;
}

    /* Keywords - PAL/ObjectPAL */
(?i:and)        { return AND; }
(?i:array)      { return ARRAY; }
(?i:as)         { return AS; }
(?i:begin)      { return BBEGIN; }
(?i:break)      { return BREAK; }
(?i:case)       { return CASE; }
(?i:const)      { return CONST; }
(?i:continue)   { return CONTINUE; }
(?i:default)    { return DEFAULT; }
(?i:div)        { return INTDIV; }
(?i:do)         { return DO; }
(?i:downto)     { return DOWNTO; }
(?i:else)       { return ELSE; }
(?i:elsif)      { return ELSIF; }
(?i:end)        { return END; }
(?i:endfor)     { return ENDFOR; }
(?i:endforeach) { return ENDFOREACH; }
(?i:endif)      { return ENDIF; }
(?i:endmethod)  { return ENDMETHOD; }
(?i:endproc)    { return ENDPROC; }
(?i:endscan)    { return ENDSCAN; }
(?i:endswitch)  { return ENDSWITCH; }
(?i:endwhile)   { return ENDWHILE; }
(?i:except)     { return EXCEPT; }
(?i:false)      { yylval.ival = 0; return LOGICALCONST; }
(?i:for)        { return FOR; }
(?i:foreach)    { return FOREACH; }
(?i:from)       { return FROM; }
(?i:function)   { return FUNCTION; }
(?i:if)         { return IF; }
(?i:in)         { return IN; }
(?i:like)       { return LIKE; }
(?i:method)     { return METHOD; }
(?i:mod)        { return MOD; }
(?i:not)        { return NOT; }
(?i:of)         { return OF; }
(?i:or)         { return OR; }
(?i:private)    { return PRIVATE; }
(?i:proc)       { return PROC; }
(?i:protected)  { return PROTECTED; }
(?i:public)     { return PUBLIC; }
(?i:quit)       { return QUIT; }
(?i:record)     { return RECORD; }
(?i:return)     { return RETURN; }
(?i:scan)       { return SCAN; }
(?i:self)       { return SELF; }
(?i:step)       { return STEP; }
(?i:switch)     { return SWITCH; }
(?i:then)       { return THEN; }
(?i:to)         { return TO; }
(?i:true)       { yylval.ival = 1; return LOGICALCONST; }
(?i:try)        { return TRY; }
(?i:type)       { return TYPE; }
(?i:until)      { return UNTIL; }
(?i:uses)       { return USES; }
(?i:var)        { return VAR; }
(?i:while)      { return WHILE; }
(?i:with)       { return WITH; }

    /* Data type keywords */
(?i:SmallInt)   { return TSMALLINT; }
(?i:ShortInt)   { return TSHORTINT; }
(?i:LongInt)    { return TLONGINT; }
(?i:Number)     { return TNUMBER; }
(?i:Currency)   { return TCURRENCY; }
(?i:Logical)    { return TLOGICAL; }
(?i:String)     { return TSTRING; }
(?i:Date)       { return TDATE; }
(?i:Time)       { return TTIME; }
(?i:DateTime)   { return TDATETIME; }
(?i:TimeStamp)  { return TTIMESTAMP; }
(?i:Memo)       { return TMEMO; }
(?i:Blob)       { return TBLOB; }
(?i:Graphic)    { return TGRAPHIC; }
(?i:Variant)    { return TVARIANT; }

    /* Operators */
":="            { return ASSIGN; }
".."            { return DOTDOT; }
"<>"            { return NE; }
"<="            { return LE; }
">="            { return GE; }
"=="            { return EQ; }  /* Alternative equality in ObjectPAL */
"!="            { return NE; }  /* Alternative inequality in ObjectPAL */
"="             { return EQ; }
"<"             { return LT; }
">"             { return GT; }
"+"             { return PLUS; }
"-"             { return MINUS; }
"*"             { return STAR; }
"/"             { return SLASH; }
"^"             { return UPARROW; }
"@"             { return AT; }
"."             { return DOT; }
","             { return COMMA; }
":"             { return COLON; }
";"             { return SEMICOLON; }
"("             { return LPAREN; }
")"             { return RPAREN; }
"["             { return LBRACK; }
"]"             { return RBRACK; }

    /* Numbers - Integer */
{DIGIT}+ {
	yylval.ival = atoi(yytext);
	return INTCONST;
}

    /* Numbers - Hexadecimal (ObjectPAL) */
0[xX]{HEXDIGIT}+ {
	if (IS_OBJECTPAL()) {
		yylval.ival = strtol(yytext + 2, NULL, 16);
		return INTCONST;
	} else {
		error("Hexadecimal literals not supported in %s",
		      get_dialect_name(current_dialect));
	}
}

    /* Numbers - Floating point */
{DIGIT}+"."{DIGIT}* |
{DIGIT}*"."{DIGIT}+ |
{DIGIT}+[eE][+-]?{DIGIT}+ |
{DIGIT}+"."{DIGIT}*[eE][+-]?{DIGIT}+ {
	yylval.dval = atof(yytext);
	return NUMBERCONST;
}

    /* Identifiers */
{IDENT_START}{IDENT_CHAR}* {
	return keyword_or_ident(yytext);
}

    /* Whitespace */
{WHITESPACE}+ {
	/* Skip whitespace */
}

    /* Newline */
\n {
	lineno++;
	current_column = 1;
}

    /* Anything else is an error */
. {
	error("Unexpected character: '%c' (0x%02x)", *yytext, *yytext);
}

%%

/* Case-insensitive keyword lookup */
static int keyword_or_ident(char *text)
{
	/* Already handled by keyword rules above due to case-insensitive mode */
	/* This just handles identifiers */
	if (IS_CASE_SENSITIVE()) {
		yylval.sval = strdup(text);
	} else {
		/* Convert to uppercase for symbol table */
		int i;
		char *buf = strdup(text);
		for (i = 0; buf[i]; i++)
			buf[i] = toupper(buf[i]);
		yylval.sval = buf;
	}
	return IDENT;
}
