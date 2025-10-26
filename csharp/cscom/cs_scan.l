%{
/*
 * C# 3.0 Lexical Scanner
 * Tokenizes C# source code
 */

#include <stdlib.h>
#include <string.h>
#include "cs_pass1.h"
#include "cs_gram.h"

int cs_lineno = 1;
int cs_column = 1;

#define YY_USER_ACTION cs_column += yyleng;

%}

%option noyywrap
%option prefix="cs_"

DIGIT           [0-9]
HEXDIGIT        [0-9A-Fa-f]
LETTER          [A-Za-z_]
IDENTIFIER      {LETTER}({LETTER}|{DIGIT})*
WHITESPACE      [ \t\r]+
NEWLINE         \n

%%

    /* C# Keywords */
"abstract"      { return ABSTRACT; }
"as"            { return AS; }
"base"          { return BASE; }
"bool"          { return BOOL; }
"break"         { return BREAK; }
"byte"          { return BYTE; }
"case"          { return CASE; }
"catch"         { return CATCH; }
"char"          { return CHAR; }
"checked"       { return CHECKED; }
"class"         { return CLASS; }
"const"         { return CONST; }
"continue"      { return CONTINUE; }
"decimal"       { return DECIMAL; }
"default"       { return DEFAULT; }
"delegate"      { return DELEGATE; }
"do"            { return DO; }
"double"        { return DOUBLE; }
"else"          { return ELSE; }
"enum"          { return ENUM; }
"event"         { return EVENT; }
"explicit"      { return EXPLICIT; }
"extern"        { return EXTERN; }
"false"         { return FALSE; }
"finally"       { return FINALLY; }
"fixed"         { return FIXED; }
"float"         { return FLOAT; }
"for"           { return FOR; }
"foreach"       { return FOREACH; }
"goto"          { return GOTO; }
"if"            { return IF; }
"implicit"      { return IMPLICIT; }
"in"            { return IN; }
"int"           { return INT; }
"interface"     { return INTERFACE; }
"internal"      { return INTERNAL; }
"is"            { return IS; }
"lock"          { return LOCK; }
"long"          { return LONG; }
"namespace"     { return NAMESPACE; }
"new"           { return NEW; }
"null"          { return NULL_LITERAL; }
"object"        { return OBJECT; }
"operator"      { return OPERATOR; }
"out"           { return OUT; }
"override"      { return OVERRIDE; }
"params"        { return PARAMS; }
"partial"       { return PARTIAL; }
"private"       { return PRIVATE; }
"protected"     { return PROTECTED; }
"public"        { return PUBLIC; }
"readonly"      { return READONLY; }
"ref"           { return REF; }
"return"        { return RETURN; }
"sbyte"         { return SBYTE; }
"sealed"        { return SEALED; }
"short"         { return SHORT; }
"sizeof"        { return SIZEOF; }
"stackalloc"    { return STACKALLOC; }
"static"        { return STATIC; }
"string"        { return STRING; }
"struct"        { return STRUCT; }
"switch"        { return SWITCH; }
"this"          { return THIS; }
"throw"         { return THROW; }
"true"          { return TRUE; }
"try"           { return TRY; }
"typeof"        { return TYPEOF; }
"uint"          { return UINT; }
"ulong"         { return ULONG; }
"unchecked"     { return UNCHECKED; }
"unsafe"        { return UNSAFE; }
"ushort"        { return USHORT; }
"using"         { return USING; }
"virtual"       { return VIRTUAL; }
"void"          { return VOID; }
"volatile"      { return VOLATILE; }
"while"         { return WHILE; }

    /* C# 3.0 Contextual Keywords */
"var"           { return VAR; }
"from"          { return FROM; }
"where"         { return WHERE; }
"select"        { return SELECT; }
"group"         { return GROUP; }
"into"          { return INTO; }
"orderby"       { return ORDERBY; }
"join"          { return JOIN; }
"let"           { return LET; }
"on"            { return ON; }
"equals"        { return EQUALS; }
"by"            { return BY; }
"ascending"     { return ASCENDING; }
"descending"    { return DESCENDING; }
"get"           { return GET; }
"set"           { return SET; }
"add"           { return ADD; }
"remove"        { return REMOVE; }
"value"         { return VALUE; }
"yield"         { return YIELD; }

    /* Operators and Punctuation */
"+"             { return PLUS; }
"-"             { return MINUS; }
"*"             { return STAR; }
"/"             { return SLASH; }
"%"             { return PERCENT; }
"&"             { return AMPERSAND; }
"|"             { return PIPE; }
"^"             { return CARET; }
"!"             { return BANG; }
"~"             { return TILDE; }
"="             { return ASSIGN; }
"<"             { return LESS; }
">"             { return GREATER; }
"?"             { return QUESTION; }
":"             { return COLON; }
";"             { return SEMICOLON; }
","             { return COMMA; }
"."             { return DOT; }
"("             { return LPAREN; }
")"             { return RPAREN; }
"["             { return LBRACKET; }
"]"             { return RBRACKET; }
"{"             { return LBRACE; }
"}"             { return RBRACE; }

    /* Compound Operators */
"++"            { return INCREMENT; }
"--"            { return DECREMENT; }
"&&"            { return LOGICAL_AND; }
"||"            { return LOGICAL_OR; }
"<<"            { return LEFT_SHIFT; }
">>"            { return RIGHT_SHIFT; }
"=="            { return EQUAL; }
"!="            { return NOT_EQUAL; }
"<="            { return LESS_EQUAL; }
">="            { return GREATER_EQUAL; }
"+="            { return PLUS_ASSIGN; }
"-="            { return MINUS_ASSIGN; }
"*="            { return STAR_ASSIGN; }
"/="            { return SLASH_ASSIGN; }
"%="            { return PERCENT_ASSIGN; }
"&="            { return AND_ASSIGN; }
"|="            { return OR_ASSIGN; }
"^="            { return XOR_ASSIGN; }
"<<="           { return LEFT_SHIFT_ASSIGN; }
">>="           { return RIGHT_SHIFT_ASSIGN; }
"=>"            { return LAMBDA_ARROW; }
"??"            { return NULL_COALESCE; }

    /* Literals */
{DIGIT}+                                        { cs_lval.ival = atoi(yytext); return INT_LITERAL; }
{DIGIT}+[Ll]                                    { cs_lval.lval = atol(yytext); return LONG_LITERAL; }
{DIGIT}+[Uu]                                    { cs_lval.uival = (unsigned int)atoi(yytext); return UINT_LITERAL; }
{DIGIT}+([Uu][Ll]|[Ll][Uu])                     { cs_lval.ulval = (unsigned long)atol(yytext); return ULONG_LITERAL; }
0[Xx]{HEXDIGIT}+                                { cs_lval.ival = (int)strtol(yytext, NULL, 16); return INT_LITERAL; }
{DIGIT}+"."{DIGIT}*([Ee][+-]?{DIGIT}+)?[Ff]?    { cs_lval.fval = atof(yytext); return FLOAT_LITERAL; }
{DIGIT}+"."{DIGIT}*([Ee][+-]?{DIGIT}+)?[Dd]     { cs_lval.dval = atof(yytext); return DOUBLE_LITERAL; }
{DIGIT}+"."{DIGIT}*([Ee][+-]?{DIGIT}+)?[Mm]     { cs_lval.mval = atof(yytext); return DECIMAL_LITERAL; }

    /* String Literal */
\"([^"\\\n]|\\.)*\"                             { cs_lval.sval = strdup(yytext); return STRING_LITERAL; }
@\"([^"]|\"\")*\"                               { cs_lval.sval = strdup(yytext); return VERBATIM_STRING; }

    /* Character Literal */
'([^'\\\n]|\\.)' {
    cs_lval.cval = yytext[1];
    return CHAR_LITERAL;
}

    /* Identifier */
{IDENTIFIER}    { cs_lval.sval = strdup(yytext); return IDENTIFIER; }

    /* Whitespace */
{WHITESPACE}    { /* ignore */ }
{NEWLINE}       { cs_lineno++; cs_column = 1; }

    /* Comments */
"//".*          { /* single-line comment */ }
"/*"            {
    int c;
    while ((c = input()) != 0) {
        if (c == '\n') {
            cs_lineno++;
            cs_column = 1;
        } else if (c == '*') {
            if ((c = input()) == '/')
                break;
            unput(c);
        }
    }
}

    /* Unknown character */
.               { fprintf(stderr, "Unknown character: %s\n", yytext); }

%%

void cs_scanner_init(const char *filename) {
    cs_lineno = 1;
    cs_column = 1;
    if (filename) {
        yyin = fopen(filename, "r");
        if (!yyin) {
            fprintf(stderr, "Cannot open file: %s\n", filename);
            exit(1);
        }
    }
}

void cs_scanner_cleanup(void) {
    if (yyin && yyin != stdin) {
        fclose(yyin);
    }
}
