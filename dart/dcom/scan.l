%{
/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart lexical analyzer
 */

#include "pass1.h"
#include "dgram.h"
#include <string.h>

int yylineno = 1;
int yycolumn = 1;

#define YY_USER_ACTION \
	yycolumn += yyleng;

%}

%option noyywrap
%option nounput
%option noinput

DIGIT           [0-9]
HEXDIGIT        [0-9a-fA-F]
LETTER          [a-zA-Z_]
IDENT           {LETTER}({LETTER}|{DIGIT})*
INTEGER         {DIGIT}+
HEX_INTEGER     0[xX]{HEXDIGIT}+
DOUBLE          {DIGIT}+\.{DIGIT}+([eE][+-]?{DIGIT}+)?
STRING_CHAR     [^\\\"\n]
ESCAPE_SEQ      \\[nrtbf\\'\"\\]

%%

    /* Whitespace */
[ \t]+          { /* ignore whitespace */ }
\n              { yylineno++; yycolumn = 1; }
\r\n            { yylineno++; yycolumn = 1; }

    /* Comments */
"//".*          { /* single-line comment */ }
"/*"            {
                    int c;
                    for (;;) {
                        while ((c = input()) != '*' && c != EOF) {
                            if (c == '\n') {
                                yylineno++;
                                yycolumn = 1;
                            }
                        }
                        if (c == '*') {
                            while ((c = input()) == '*')
                                ;
                            if (c == '/')
                                break;
                        }
                        if (c == EOF) {
                            yyerror("unterminated comment");
                            break;
                        }
                    }
                }

    /* Keywords */
"abstract"      { return ABSTRACT; }
"as"            { return AS; }
"assert"        { return ASSERT; }
"async"         { return ASYNC; }
"await"         { return AWAIT; }
"break"         { return BREAK; }
"case"          { return CASE; }
"catch"         { return CATCH; }
"class"         { return CLASS; }
"const"         { return CONST; }
"continue"      { return CONTINUE; }
"default"       { return DEFAULT; }
"deferred"      { return DEFERRED; }
"do"            { return DO; }
"else"          { return ELSE; }
"enum"          { return ENUM; }
"export"        { return EXPORT; }
"extends"       { return EXTENDS; }
"extension"     { return EXTENSION; }
"external"      { return EXTERNAL; }
"factory"       { return FACTORY; }
"false"         { return FALSE; }
"final"         { return FINAL; }
"finally"       { return FINALLY; }
"for"           { return FOR; }
"get"           { return GET; }
"if"            { return IF; }
"implements"    { return IMPLEMENTS; }
"import"        { return IMPORT; }
"in"            { return IN; }
"interface"     { return INTERFACE; }
"is"            { return IS; }
"late"          { return LATE; }
"library"       { return LIBRARY; }
"mixin"         { return MIXIN; }
"new"           { return NEW; }
"null"          { return NULL_LIT; }
"on"            { return ON; }
"operator"      { return OPERATOR; }
"part"          { return PART; }
"required"      { return REQUIRED; }
"rethrow"       { return RETHROW; }
"return"        { return RETURN; }
"set"           { return SET; }
"show"          { return SHOW; }
"static"        { return STATIC; }
"super"         { return SUPER; }
"switch"        { return SWITCH; }
"sync"          { return SYNC; }
"this"          { return THIS; }
"throw"         { return THROW; }
"true"          { return TRUE; }
"try"           { return TRY; }
"typedef"       { return TYPEDEF; }
"var"           { return VAR; }
"void"          { return VOID; }
"while"         { return WHILE; }
"with"          { return WITH; }
"yield"         { return YIELD; }

    /* Built-in types */
"int"           { return INT; }
"double"        { return DOUBLE; }
"bool"          { return BOOL; }
"String"        { return STRING; }
"List"          { return LIST; }
"Map"           { return MAP; }
"Set"           { return SET_TYPE; }
"dynamic"       { return DYNAMIC; }
"Object"        { return OBJECT; }
"Function"      { return FUNCTION; }
"num"           { return NUM; }

    /* Operators */
"+"             { return PLUS; }
"-"             { return MINUS; }
"*"             { return TIMES; }
"/"             { return DIVIDE; }
"%"             { return MODULO; }
"~/"            { return INT_DIVIDE; }

"=="            { return EQ; }
"!="            { return NE; }
"<"             { return LT; }
">"             { return GT; }
"<="            { return LE; }
">="            { return GE; }

"&&"            { return AND; }
"||"            { return OR; }
"!"             { return NOT; }

"&"             { return BIT_AND; }
"|"             { return BIT_OR; }
"^"             { return BIT_XOR; }
"~"             { return BIT_NOT; }
"<<"            { return LSHIFT; }
">>"            { return RSHIFT; }
">>>"           { return URSHIFT; }

"="             { return ASSIGN; }
"+="            { return PLUS_ASSIGN; }
"-="            { return MINUS_ASSIGN; }
"*="            { return TIMES_ASSIGN; }
"/="            { return DIVIDE_ASSIGN; }
"%="            { return MODULO_ASSIGN; }
"~/="           { return INT_DIVIDE_ASSIGN; }
"&="            { return AND_ASSIGN; }
"|="            { return OR_ASSIGN; }
"^="            { return XOR_ASSIGN; }
"<<="           { return LSHIFT_ASSIGN; }
">>="           { return RSHIFT_ASSIGN; }
">>>="          { return URSHIFT_ASSIGN; }

"++"            { return INCREMENT; }
"--"            { return DECREMENT; }

"?"             { return QUESTION; }
":"             { return COLON; }
"??"            { return NULL_COALESCE; }
"??="           { return NULL_COALESCE_ASSIGN; }
"?."            { return NULL_AWARE; }
"..."           { return SPREAD; }
"...?"          { return NULL_SPREAD; }
"=>"            { return ARROW; }

    /* Delimiters */
"("             { return LPAREN; }
")"             { return RPAREN; }
"{"             { return LBRACE; }
"}"             { return RBRACE; }
"["             { return LBRACKET; }
"]"             { return RBRACKET; }
";"             { return SEMICOLON; }
","             { return COMMA; }
"."             { return DOT; }
".."            { return CASCADE; }
"@"             { return AT; }
"#"             { return HASH; }

    /* Literals */
{HEX_INTEGER}   {
                    yylval.intval = (int)strtol(yytext, NULL, 16);
                    return INTEGER_LITERAL;
                }

{INTEGER}       {
                    yylval.intval = atoi(yytext);
                    return INTEGER_LITERAL;
                }

{DOUBLE}        {
                    yylval.doubleval = atof(yytext);
                    return DOUBLE_LITERAL;
                }

\"({STRING_CHAR}|{ESCAPE_SEQ})*\"  {
                    yylval.strval = strdup(yytext + 1);
                    yylval.strval[strlen(yylval.strval) - 1] = '\0';
                    return STRING_LITERAL;
                }

'({STRING_CHAR}|{ESCAPE_SEQ})*'  {
                    yylval.strval = strdup(yytext + 1);
                    yylval.strval[strlen(yylval.strval) - 1] = '\0';
                    return STRING_LITERAL;
                }

r\"[^\"]*\"     {
                    yylval.strval = strdup(yytext + 2);
                    yylval.strval[strlen(yylval.strval) - 1] = '\0';
                    return STRING_LITERAL;
                }

r'[^']*'        {
                    yylval.strval = strdup(yytext + 2);
                    yylval.strval[strlen(yylval.strval) - 1] = '\0';
                    return STRING_LITERAL;
                }

    /* Identifiers */
{IDENT}         {
                    yylval.strval = strdup(yytext);
                    return IDENTIFIER;
                }

    /* Invalid characters */
.               {
                    char buf[100];
                    sprintf(buf, "unexpected character: '%c'", yytext[0]);
                    yyerror(buf);
                }

%%
