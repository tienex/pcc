/*
 * Copyright (c) 2025 PCC BASIC Compiler
 *
 * Hand-written lexical analyzer for BASIC (since flex/lex is not available)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "pass1.h"
#include "y.tab.h"

FILE *yyin = NULL;
int yylineno = 1;
extern YYSTYPE yylval;

static int getc_wrapper(void) {
    int c = fgetc(yyin);
    if (c == '\n') yylineno++;
    return c;
}

static void ungetc_wrapper(int c) {
    if (c == '\n') yylineno--;
    ungetc(c, yyin);
}

struct keyword {
    const char *name;
    int token;
};

static struct keyword keywords[] = {
    {"PRINT", PRINT},
    {"INPUT", INPUT},
    {"LET", LET},
    {"IF", IF},
    {"THEN", THEN},
    {"ELSE", ELSE},
    {"END", END},
    {"FOR", FOR},
    {"TO", TO},
    {"STEP", STEP},
    {"NEXT", NEXT},
    {"GOTO", GOTO},
    {"GOSUB", GOSUB},
    {"RETURN", RETURN},
    {"WHILE", WHILE},
    {"WEND", WEND},
    {"DO", DO},
    {"LOOP", LOOP},
    {"DIM", DIM},
    {"AS", AS},
    {"INTEGER", TINTEGER_KW},
    {"LONG", TLONG_KW},
    {"SINGLE", TSINGLE_KW},
    {"DOUBLE", TDOUBLE_KW},
    {"STRING", TSTRING_KW},
    {"SUB", SUB},
    {"FUNCTION", FUNCTION},
    {"EXIT", EXIT},
    {"CALL", CALL},
    {"SHARED", SHARED},
    {"STATIC", STATIC},
    {"COMMON", COMMON},
    {"OPTION", OPTION},
    {"BASE", BASE},
    {"EXPLICIT", EXPLICIT},
    {"AND", AND},
    {"OR", OR},
    {"NOT", NOT},
    {"XOR", XOR},
    {"MOD", MOD},
    {NULL, 0}
};

int yylex(void) {
    int c;
    static char buf[1024];
    int i;

    /* Skip whitespace */
    while ((c = getc_wrapper()) != EOF && (c == ' ' || c == '\t'))
        ;

    if (c == EOF)
        return 0;

    /* Newlines */
    if (c == '\n') {
        return EOL;
    }
    if (c == '\r') {
        c = getc_wrapper();
        if (c == '\n')
            return EOL;
        ungetc_wrapper(c);
        return EOL;
    }

    /* REM comments */
    if (c == 'R' || c == 'r') {
        int pos = 0;
        buf[pos++] = c;
        while ((c = getc_wrapper()) != EOF && pos < 3) {
            buf[pos++] = c;
        }
        buf[pos] = '\0';
        if (strcasecmp(buf, "REM") == 0) {
            /* Skip to end of line */
            while ((c = getc_wrapper()) != EOF && c != '\n')
                ;
            if (c == '\n')
                ungetc_wrapper(c);
            return yylex();
        }
        /* Not REM, put it back */
        for (i = pos - 1; i >= 1; i--)
            ungetc_wrapper(buf[i]);
        c = buf[0];
    }

    /* ' comments */
    if (c == '\'') {
        while ((c = getc_wrapper()) != EOF && c != '\n')
            ;
        if (c == '\n')
            ungetc_wrapper(c);
        return yylex();
    }

    /* String literals */
    if (c == '"') {
        i = 0;
        while ((c = getc_wrapper()) != EOF && c != '"' && i < 1023) {
            buf[i++] = c;
        }
        buf[i] = '\0';
        yylval.sval = strdup(buf);
        return STRING_LIT;
    }

    /* Numbers */
    if (isdigit(c)) {
        i = 0;
        int has_dot = 0;
        buf[i++] = c;
        while ((c = getc_wrapper()) != EOF && (isdigit(c) || c == '.')) {
            if (c == '.') {
                if (has_dot) break;
                has_dot = 1;
            }
            buf[i++] = c;
        }
        ungetc_wrapper(c);
        buf[i] = '\0';
        if (has_dot) {
            yylval.dval = atof(buf);
            return REAL_NUM;
        } else {
            yylval.ival = atoi(buf);
            return NUMBER;
        }
    }

    /* Identifiers and keywords */
    if (isalpha(c)) {
        i = 0;
        buf[i++] = c;
        while ((c = getc_wrapper()) != EOF && (isalnum(c) || c == '_')) {
            buf[i++] = c;
        }
        /* Check for type suffix */
        if (c == '$' || c == '%' || c == '&' || c == '!' || c == '#') {
            buf[i++] = c;
        } else {
            ungetc_wrapper(c);
        }
        buf[i] = '\0';

        /* Check keywords */
        for (i = 0; keywords[i].name; i++) {
            if (strcasecmp(buf, keywords[i].name) == 0) {
                return keywords[i].token;
            }
        }

        /* Identifier */
        yylval.sval = strdup(buf);
        return IDENT;
    }

    /* Two-character operators */
    if (c == '<') {
        c = getc_wrapper();
        if (c == '>') return NE;
        if (c == '=') return LE;
        ungetc_wrapper(c);
        return LT;
    }
    if (c == '>') {
        c = getc_wrapper();
        if (c == '=') return GE;
        ungetc_wrapper(c);
        return GT;
    }

    /* Single-character tokens */
    switch (c) {
    case '=': return EQ;
    case ':': return COLON;
    case ';': return SEMICOLON;
    case ',': return COMMA;
    case '(': return LPAREN;
    case ')': return RPAREN;
    case '+': return PLUS;
    case '-': return MINUS;
    case '*': return MUL;
    case '/': return DIV;
    case '\\': return INTDIV;
    case '^': return POWER;
    default:
        warning("unexpected character: %c (0x%02x)", c, c);
        return yylex();
    }
}
