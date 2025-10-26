/*
 * Copyright (c) 2025 PCC Common LISP Compiler
 *
 * Lexical analyzer for Common LISP
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"
#include "lgram.h"

extern int lineno;
%}

%option noyywrap
%option nounput
%option noinput

DIGIT       [0-9]
LETTER      [a-zA-Z]
SYMBOL_CHAR [a-zA-Z0-9_\-+*/<>=!?$%&]

%%

";".*$                  { /* Comment - ignore */ }

[ \t\r]+                { /* Whitespace - ignore */ }

\n                      { lineno++; }

"("                     { return LPAREN; }
")"                     { return RPAREN; }
"'"                     { return QUOTE; }
"`"                     { return BACKQUOTE; }
","                     { return COMMA; }
",@"                    { return COMMA_AT; }
"#'"                    { return FUNCTION_QUOTE; }

"nil"|"NIL"             { return NIL; }
"t"|"T"                 { return T; }

"defun"                 { return DEFUN; }
"defvar"                { return DEFVAR; }
"defparameter"          { return DEFPARAMETER; }
"defconstant"           { return DEFCONSTANT; }
"setq"                  { return SETQ; }
"let"                   { return LET; }
"let*"                  { return LETSTAR; }
"lambda"                { return LAMBDA; }
"if"                    { return IF; }
"cond"                  { return COND; }
"case"                  { return CASE; }
"when"                  { return WHEN; }
"unless"                { return UNLESS; }
"progn"                 { return PROGN; }
"prog1"                 { return PROG1; }
"prog2"                 { return PROG2; }
"and"                   { return AND; }
"or"                    { return OR; }
"not"                   { return NOT; }
"do"                    { return DO; }
"dolist"                { return DOLIST; }
"dotimes"               { return DOTIMES; }
"loop"                  { return LOOP; }
"return"                { return RETURN; }

{DIGIT}+                {
                            yylval.integer = atol(yytext);
                            return INTEGER;
                        }

{DIGIT}+"."{DIGIT}*     |
{DIGIT}*"."{DIGIT}+     {
                            yylval.floating = atof(yytext);
                            return FLOAT;
                        }

\"([^"\\]|\\.)*\"       {
                            /* String literal */
                            yylval.string = strdup(yytext);
                            return STRING;
                        }

{SYMBOL_CHAR}+          {
                            yylval.string = strdup(yytext);
                            return SYMBOL;
                        }

.                       {
                            fprintf(stderr, "Unexpected character: '%c' at line %d\n",
                                yytext[0], lineno);
                        }

%%
