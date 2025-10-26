%{
/*
 * Copyright (c) 2025 PCC BASIC Compiler
 *
 * Lexical analyzer for BASIC
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"
#include "y.tab.h"

extern int lineno;
extern char *ftitle;

%}

%option noyywrap
%option yylineno

DIGIT           [0-9]
LETTER          [A-Za-z]
IDENT           {LETTER}({LETTER}|{DIGIT})*
INTEGER         {DIGIT}+
REAL            {DIGIT}+"."{DIGIT}*|{DIGIT}*"."{DIGIT}+
STRING          \"[^\"]*\"

%%

[ \t]+          { /* skip whitespace */ }
\n              { lineno++; return EOL; }
\r\n            { lineno++; return EOL; }

"REM"[^\n]*     { /* comment */ }
"'"[^\n]*       { /* comment */ }

"PRINT"         { return PRINT; }
"INPUT"         { return INPUT; }
"LET"           { return LET; }
"IF"            { return IF; }
"THEN"          { return THEN; }
"ELSE"          { return ELSE; }
"END"           { return END; }
"FOR"           { return FOR; }
"TO"            { return TO; }
"STEP"          { return STEP; }
"NEXT"          { return NEXT; }
"GOTO"          { return GOTO; }
"GOSUB"         { return GOSUB; }
"RETURN"        { return RETURN; }
"WHILE"         { return WHILE; }
"WEND"          { return WEND; }
"DO"            { return DO; }
"LOOP"          { return LOOP; }
"DIM"           { return DIM; }
"AS"            { return AS; }
"INTEGER"       { return TINTEGER_KW; }
"LONG"          { return TLONG_KW; }
"SINGLE"        { return TSINGLE_KW; }
"DOUBLE"        { return TDOUBLE_KW; }
"STRING"        { return TSTRING_KW; }
"SUB"           { return SUB; }
"FUNCTION"      { return FUNCTION; }
"EXIT"          { return EXIT; }
"CALL"          { return CALL; }
"SHARED"        { return SHARED; }
"STATIC"        { return STATIC; }
"COMMON"        { return COMMON; }
"OPTION"        { return OPTION; }
"BASE"          { return BASE; }
"EXPLICIT"      { return EXPLICIT; }

"AND"           { return AND; }
"OR"            { return OR; }
"NOT"           { return NOT; }
"XOR"           { return XOR; }
"MOD"           { return MOD; }

"<>"            { return NE; }
"<="            { return LE; }
">="            { return GE; }
"<"             { return LT; }
">"             { return GT; }
"="             { return EQ; }

{INTEGER}       { yylval.ival = atoi(yytext); return NUMBER; }
{REAL}          { yylval.dval = atof(yytext); return REAL_NUM; }
{STRING}        {
                  yylval.sval = strdup(yytext+1);
                  yylval.sval[strlen(yylval.sval)-1] = '\0';
                  return STRING_LIT;
                }

{IDENT}[$%&!#]? { yylval.sval = strdup(yytext); return IDENT; }

":"             { return COLON; }
";"             { return SEMICOLON; }
","             { return COMMA; }
"("             { return LPAREN; }
")"             { return RPAREN; }
"+"             { return PLUS; }
"-"             { return MINUS; }
"*"             { return MUL; }
"/"             { return DIV; }
"\\"            { return INTDIV; }
"^"             { return POWER; }

.               { warning("unexpected character: %c", yytext[0]); }

%%

/* Nothing else needed here */
