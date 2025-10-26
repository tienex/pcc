/*
 * Copyright (c) 2025 PCC Pascal Compiler
 *
 * Lexical analyzer for Pascal
 * Supports multiple Pascal dialects with configurable features
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
%x CPPCOMMENT
%x STRING

DIGIT           [0-9]
LETTER          [a-zA-Z]
IDENT_START     {LETTER}
IDENT_CHAR      ({LETTER}|{DIGIT})
WHITESPACE      [ \t\r]

%%

    /* Comments - (* ... *) style */
"(*" {
	comment_level = 1;
	BEGIN(COMMENT);
}

<COMMENT>"(*" {
	if (ALLOW_NESTED_COMMENTS())
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

    /* Comments - { ... } style */
"{" {
	comment_level = 1;
	BEGIN(COMMENT);
}

<COMMENT>"}" {
	comment_level--;
	if (comment_level == 0)
		BEGIN(INITIAL);
}

    /* C++-style comments (dialect-dependent) */
"//" {
	if (ALLOW_CPP_COMMENTS()) {
		BEGIN(CPPCOMMENT);
	} else {
		error("C++-style comments not allowed in %s",
		      get_dialect_name(current_dialect));
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

    /* Reserved words - standard Pascal */
"and"           { return AND; }
"array"         { return ARRAY; }
"begin"         { return BBEGIN; }
"case"          { return CASE; }
"const"         { return CONST; }
"div"           { return DIV; }
"do"            { return DO; }
"downto"        { return DOWNTO; }
"else"          { return ELSE; }
"end"           { return END; }
"file"          { return FFILE; }
"for"           { return FOR; }
"function"      { return FUNCTION; }
"goto"          { return GOTO; }
"if"            { return IF; }
"in"            { return IN; }
"label"         { return LABEL; }
"mod"           { return MOD; }
"nil"           { return NIL; }
"not"           { return NOT; }
"of"            { return OF; }
"or"            { return OR; }
"packed"        { return PACKED; }
"procedure"     { return PROCEDURE; }
"program"       { return PROGRAM; }
"record"        { return RECORD; }
"repeat"        { return REPEAT; }
"set"           { return SET; }
"then"          { return THEN; }
"to"            { return TO; }
"type"          { return TYPE; }
"until"         { return UNTIL; }
"var"           { return VAR; }
"while"         { return WHILE; }
"with"          { return WITH; }

    /* Extended Pascal / Modular Pascal */
"import"        { if (ALLOW_UNITS()) return IMPORT; else return IDENT; }
"export"        { if (ALLOW_UNITS()) return EXPORT; else return IDENT; }
"module"        { if (ALLOW_UNITS()) return MODULE; else return IDENT; }

    /* Borland/Delphi extensions */
"unit"          { if (ALLOW_UNITS()) return UNIT; else return IDENT; }
"uses"          { if (ALLOW_USES_CLAUSE()) return USES; else return IDENT; }
"interface"     { if (ALLOW_UNITS()) return INTERFACE; else return IDENT; }
"implementation" { if (ALLOW_UNITS()) return IMPLEMENTATION; else return IDENT; }
"object"        { if (ALLOW_OBJECTS()) return OBJECT; else return IDENT; }
"class"         { if (ALLOW_CLASSES()) return CLASS; else return IDENT; }
"property"      { if (ALLOW_PROPERTIES()) return PROPERTY; else return IDENT; }
"inherited"     { if (ALLOW_CLASSES()) return INHERITED; else return IDENT; }
"constructor"   { if (ALLOW_OBJECTS() || ALLOW_CLASSES()) return CONSTRUCTOR; else return IDENT; }
"destructor"    { if (ALLOW_OBJECTS() || ALLOW_CLASSES()) return DESTRUCTOR; else return IDENT; }
"virtual"       { if (ALLOW_OBJECTS() || ALLOW_CLASSES()) return VIRTUAL; else return IDENT; }
"override"      { if (ALLOW_CLASSES()) return OVERRIDE; else return IDENT; }
"break"         { if (ALLOW_BREAK_CONTINUE()) return BREAK; else return IDENT; }
"continue"      { if (ALLOW_BREAK_CONTINUE()) return CONTINUE; else return IDENT; }
"asm"           { if (ALLOW_INLINE_ASM()) return ASM; else return IDENT; }
"inline"        { if (ALLOW_INLINE_DIRECTIVE()) return INLINE; else return IDENT; }
"forward"       { return FORWARD; }
"external"      { if (ALLOW_EXTERNAL_DIRECTIVE()) return EXTERNAL; else return IDENT; }
"absolute"      { if (ALLOW_ABSOLUTE()) return ABSOLUTE; else return IDENT; }
"string"        { if (ALLOW_STRING_TYPE()) return STRING; else return IDENT; }

    /* Operators and delimiters */
":="            { return ASSIGN; }
".."            { return DOTDOT; }
"<>"            { return NE; }
"<="            { return LE; }
">="            { return GE; }
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

    /* Integer constants */
{DIGIT}+ {
	yylval.ival = atoi(yytext);
	return INTCONST;
}

    /* Hexadecimal constants (Borland/Delphi) */
"$"{DIGIT}+ {
	if (ALLOW_DOLLAR_IDENT()) {
		yylval.ival = (int)strtol(yytext+1, NULL, 16);
		return INTCONST;
	} else {
		error("hexadecimal constants not allowed in %s",
		      get_dialect_name(current_dialect));
	}
}

    /* Real constants */
{DIGIT}+"."{DIGIT}+ |
{DIGIT}+"."{DIGIT}+[Ee][+-]?{DIGIT}+ |
{DIGIT}+[Ee][+-]?{DIGIT}+ {
	yylval.dval = atof(yytext);
	return REALCONST;
}

    /* String literals */
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

    /* C-style string literals (dialect-dependent) */
\"([^"\n\\]|\\.)*\" {
	if (ALLOW_CSTRING_LITERALS()) {
		char *s = malloc(strlen(yytext) + 1);
		char *p = s;
		char *q = yytext + 1;  /* Skip opening quote */

		/* Copy string, handling escape sequences */
		while (*q && *q != '"') {
			if (*q == '\\' && *(q+1)) {
				q++;
				switch (*q) {
				case 'n': *p++ = '\n'; break;
				case 't': *p++ = '\t'; break;
				case 'r': *p++ = '\r'; break;
				case '\\': *p++ = '\\'; break;
				case '"': *p++ = '"'; break;
				case '0': *p++ = '\0'; break;
				default: *p++ = *q; break;
				}
				q++;
			} else {
				*p++ = *q++;
			}
		}
		*p = '\0';

		yylval.sval = s;
		return STRINGCONST;
	} else {
		error("C-style string literals not allowed in %s",
		      get_dialect_name(current_dialect));
	}
}

    /* Identifiers */
{IDENT_START}{IDENT_CHAR}* {
	yylval.sval = strdup(yytext);
	return IDENT;
}

    /* Identifiers with underscores (dialect-dependent) */
{IDENT_START}({IDENT_CHAR}|"_")* {
	if (ALLOW_UNDERSCORES()) {
		yylval.sval = strdup(yytext);
		return IDENT;
	} else {
		error("underscores in identifiers not allowed in %s",
		      get_dialect_name(current_dialect));
	}
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
