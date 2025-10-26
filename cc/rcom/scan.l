%{
/*	$Id$	*/
/*
 * Ruby lexer for PCC compiler
 *
 * Tokenizes Ruby source code for parsing
 */

#include "pass1.h"
#include "cgram.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

extern int lineno;
static void line_count(void);
static char *stringbuf(char *);

%}

%option noyywrap

D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
P           [Pp][+-]?{D}+
FS			(f|F|l|L)
IS          ((u|U)|(u|U)?(l|L|ll|LL)|(l|L|ll|LL)(u|U))

%%

"begin"			{ line_count(); return BEGIN_; }
"end"			{ line_count(); return END; }
"class"			{ line_count(); return CLASS; }
"module"		{ line_count(); return MODULE; }
"def"			{ line_count(); return DEF; }
"if"			{ line_count(); return IF; }
"elsif"			{ line_count(); return ELSIF; }
"else"			{ line_count(); return ELSE; }
"unless"		{ line_count(); return UNLESS; }
"case"			{ line_count(); return CASE; }
"when"			{ line_count(); return WHEN; }
"while"			{ line_count(); return WHILE; }
"until"			{ line_count(); return UNTIL; }
"for"			{ line_count(); return FOR; }
"in"			{ line_count(); return IN; }
"do"			{ line_count(); return DO; }
"break"			{ line_count(); return BREAK; }
"next"			{ line_count(); return NEXT; }
"return"		{ line_count(); return RETURN; }
"yield"			{ line_count(); return YIELD; }
"true"			{ line_count(); yylval.intval = 1; return TRUE_; }
"false"			{ line_count(); yylval.intval = 0; return FALSE_; }
"nil"			{ line_count(); return NIL; }
"and"			{ line_count(); return AND; }
"or"			{ line_count(); return OR; }
"not"			{ line_count(); return NOT; }
"self"			{ line_count(); return SELF; }
"super"			{ line_count(); return SUPER; }
"require"		{ line_count(); return REQUIRE; }
"include"		{ line_count(); return INCLUDE; }
"attr_reader"	{ line_count(); return ATTR_READER; }
"attr_writer"	{ line_count(); return ATTR_WRITER; }
"attr_accessor"	{ line_count(); return ATTR_ACCESSOR; }
"private"		{ line_count(); return PRIVATE; }
"protected"		{ line_count(); return PROTECTED; }
"public"		{ line_count(); return PUBLIC; }
"puts"			{ line_count(); return PUTS; }
"print"			{ line_count(); return PRINT; }
"gets"			{ line_count(); return GETS; }

"=="			{ line_count(); return EQ_OP; }
"!="			{ line_count(); return NE_OP; }
"<="			{ line_count(); return LE_OP; }
">="			{ line_count(); return GE_OP; }
"<<"			{ line_count(); return LEFT_SHIFT; }
">>"			{ line_count(); return RIGHT_SHIFT; }
"&&"			{ line_count(); return AND_OP; }
"||"			{ line_count(); return OR_OP; }
"**"			{ line_count(); return POW_OP; }
"=>"			{ line_count(); return ARROW; }
".."			{ line_count(); return RANGE_INCL; }
"..."			{ line_count(); return RANGE_EXCL; }
"+="			{ line_count(); return ADD_ASSIGN; }
"-="			{ line_count(); return SUB_ASSIGN; }
"*="			{ line_count(); return MUL_ASSIGN; }
"/="			{ line_count(); return DIV_ASSIGN; }
"%="			{ line_count(); return MOD_ASSIGN; }
"&="			{ line_count(); return AND_ASSIGN; }
"|="			{ line_count(); return OR_ASSIGN; }
"^="			{ line_count(); return XOR_ASSIGN; }
"<<="			{ line_count(); return LEFT_ASSIGN; }
">>="			{ line_count(); return RIGHT_ASSIGN; }

[a-z_][a-zA-Z0-9_]*	{
	line_count();
	yylval.strval = strdup(yytext);
	return IDENTIFIER;
}

[A-Z][a-zA-Z0-9_]*	{
	line_count();
	yylval.strval = strdup(yytext);
	return CONSTANT;
}

@[a-zA-Z_][a-zA-Z0-9_]*	{
	line_count();
	yylval.strval = strdup(yytext);
	return INSTANCE_VAR;
}

@@[a-zA-Z_][a-zA-Z0-9_]*	{
	line_count();
	yylval.strval = strdup(yytext);
	return CLASS_VAR;
}

\$[a-zA-Z_][a-zA-Z0-9_]*	{
	line_count();
	yylval.strval = strdup(yytext);
	return GLOBAL_VAR;
}

:[a-zA-Z_][a-zA-Z0-9_]*[?!]?	{
	line_count();
	yylval.strval = strdup(yytext + 1);  /* Skip the : */
	return SYMBOL;
}

0[xX]{H}+{IS}?		{ line_count(); yylval.intval = strtol(yytext, NULL, 16); return INTEGER; }
0[0-7]*{IS}?		{ line_count(); yylval.intval = strtol(yytext, NULL, 8); return INTEGER; }
[1-9]{D}*{IS}?		{ line_count(); yylval.intval = atoi(yytext); return INTEGER; }

{D}+{E}{FS}?			{ line_count(); yylval.floatval = atof(yytext); return FLOAT; }
{D}*"."{D}+{E}?{FS}?	{ line_count(); yylval.floatval = atof(yytext); return FLOAT; }
{D}+"."{D}*{E}?{FS}?	{ line_count(); yylval.floatval = atof(yytext); return FLOAT; }

\"([^\\\"]|\\.)*\"	{
	line_count();
	yylval.strval = stringbuf(yytext);
	return STRING;
}

'([^\\']|\\.)*'		{
	line_count();
	yylval.strval = stringbuf(yytext);
	return STRING;
}

#.*$			{ line_count(); /* Comment - ignore */ }

[ \t\v\f]		{ /* Ignore whitespace */ }
\n				{ lineno++; }
\r\n			{ lineno++; }

"("				{ line_count(); return '('; }
")"				{ line_count(); return ')'; }
"{"				{ line_count(); return '{'; }
"}"				{ line_count(); return '}'; }
"["				{ line_count(); return '['; }
"]"				{ line_count(); return ']'; }
"."				{ line_count(); return '.'; }
","				{ line_count(); return ','; }
";"				{ line_count(); return ';'; }
":"				{ line_count(); return ':'; }
"?"				{ line_count(); return '?'; }
"="				{ line_count(); return '='; }
"+"				{ line_count(); return '+'; }
"-"				{ line_count(); return '-'; }
"*"				{ line_count(); return '*'; }
"/"				{ line_count(); return '/'; }
"%"				{ line_count(); return '%'; }
"<"				{ line_count(); return '<'; }
">"				{ line_count(); return '>'; }
"&"				{ line_count(); return '&'; }
"|"				{ line_count(); return '|'; }
"^"				{ line_count(); return '^'; }
"~"				{ line_count(); return '~'; }
"!"				{ line_count(); return '!'; }

.				{ /* Unknown character */ }

%%

static void
line_count(void)
{
	/* Count newlines in matched text for multi-line tokens */
	int i;
	for (i = 0; yytext[i] != '\0'; i++) {
		if (yytext[i] == '\n')
			lineno++;
	}
}

static char *
stringbuf(char *str)
{
	/* Convert string literal to internal representation */
	/* Remove quotes and handle escape sequences */
	int len = strlen(str);
	char *result = malloc(len + 1);
	char *p = result;
	int i;

	/* Skip opening quote */
	for (i = 1; i < len - 1; i++) {
		if (str[i] == '\\' && i + 1 < len - 1) {
			i++;
			switch (str[i]) {
			case 'n': *p++ = '\n'; break;
			case 't': *p++ = '\t'; break;
			case 'r': *p++ = '\r'; break;
			case '\\': *p++ = '\\'; break;
			case '"': *p++ = '"'; break;
			case '\'': *p++ = '\''; break;
			default: *p++ = str[i]; break;
			}
		} else {
			*p++ = str[i];
		}
	}
	*p = '\0';
	return result;
}
