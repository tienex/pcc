%{
/*
 * C# Lexical Scanner (C# 1.0-12.0)
 * Tokenizes C# source code with version-aware keyword support
 */

#include <stdlib.h>
#include <string.h>
#include "cs_pass1.h"
#include "cs_version.h"
#include "cs_gram.h"

int cs_lineno = 1;
int cs_column = 1;

#define YY_USER_ACTION cs_column += yyleng;

/* Helper to check if keyword is available in current version */
static int check_keyword_version(enum cs_feature feature);

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

    /* C# 2.0+ Keywords */
"yield"         { if (CS_HAS(CS_FEAT_ITERATORS)) return YIELD; else return IDENTIFIER; }

    /* C# 3.0 Contextual Keywords */
"var"           { if (CS_HAS(CS_FEAT_LINQ)) return VAR; else return IDENTIFIER; }
"from"          { if (CS_HAS(CS_FEAT_LINQ)) return FROM; else return IDENTIFIER; }
"where"         { if (CS_HAS(CS_FEAT_LINQ)) return WHERE; else return IDENTIFIER; }
"select"        { if (CS_HAS(CS_FEAT_LINQ)) return SELECT; else return IDENTIFIER; }
"group"         { if (CS_HAS(CS_FEAT_LINQ)) return GROUP; else return IDENTIFIER; }
"into"          { if (CS_HAS(CS_FEAT_LINQ)) return INTO; else return IDENTIFIER; }
"orderby"       { if (CS_HAS(CS_FEAT_LINQ)) return ORDERBY; else return IDENTIFIER; }
"join"          { if (CS_HAS(CS_FEAT_LINQ)) return JOIN; else return IDENTIFIER; }
"let"           { if (CS_HAS(CS_FEAT_LINQ)) return LET; else return IDENTIFIER; }
"on"            { if (CS_HAS(CS_FEAT_LINQ)) return ON; else return IDENTIFIER; }
"equals"        { if (CS_HAS(CS_FEAT_LINQ)) return EQUALS; else return IDENTIFIER; }
"by"            { if (CS_HAS(CS_FEAT_LINQ)) return BY; else return IDENTIFIER; }
"ascending"     { if (CS_HAS(CS_FEAT_LINQ)) return ASCENDING; else return IDENTIFIER; }
"descending"    { if (CS_HAS(CS_FEAT_LINQ)) return DESCENDING; else return IDENTIFIER; }
"get"           { return GET; }
"set"           { return SET; }
"add"           { return ADD; }
"remove"        { return REMOVE; }
"value"         { return VALUE; }

    /* C# 4.0+ Keywords */
"dynamic"       { if (CS_HAS(CS_FEAT_DYNAMIC)) return DYNAMIC; else return IDENTIFIER; }

    /* C# 5.0+ Keywords */
"async"         { if (CS_HAS(CS_FEAT_ASYNC_AWAIT)) return ASYNC; else return IDENTIFIER; }
"await"         { if (CS_HAS(CS_FEAT_ASYNC_AWAIT)) return AWAIT; else return IDENTIFIER; }

    /* C# 6.0+ Keywords */
"nameof"        { if (CS_HAS(CS_FEAT_NAMEOF)) return NAMEOF; else return IDENTIFIER; }
"when"          { if (CS_HAS(CS_FEAT_EXCEPTION_FILTERS)) return WHEN; else return IDENTIFIER; }

    /* C# 7.0+ Keywords */
"_"             { if (CS_HAS(CS_FEAT_DISCARDS)) return DISCARD; else return IDENTIFIER; }

    /* C# 8.0+ Keywords */
"notnull"       { if (CS_HAS(CS_FEAT_NULLABLE_REFS)) return NOTNULL; else return IDENTIFIER; }
"unmanaged"     { if (CS_HAS(CS_FEAT_NULLABLE_REFS)) return UNMANAGED_KW; else return IDENTIFIER; }

    /* C# 9.0+ Keywords */
"record"        { if (CS_HAS(CS_FEAT_RECORDS)) return RECORD; else return IDENTIFIER; }
"init"          { if (CS_HAS(CS_FEAT_INIT_ONLY)) return INIT; else return IDENTIFIER; }
"with"          { if (CS_HAS(CS_FEAT_RECORDS)) return WITH; else return IDENTIFIER; }

    /* C# 10.0+ Keywords */
"global"        { if (CS_HAS(CS_FEAT_GLOBAL_USING)) return GLOBAL; else return IDENTIFIER; }
"file"          { if (CS_HAS(CS_FEAT_FILE_SCOPED_NS)) return FILE_KW; else return IDENTIFIER; }

    /* C# 11.0+ Keywords */
"required"      { if (CS_HAS(CS_FEAT_REQUIRED_MEMBERS)) return REQUIRED; else return IDENTIFIER; }
"scoped"        { if (cs_language_version >= CS_VERSION_110) return SCOPED; else return IDENTIFIER; }

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
"?."            { if (CS_HAS(CS_FEAT_NULL_CONDITIONAL)) return NULL_CONDITIONAL; else { return QUESTION; unput('.'); } }
"?["            { if (CS_HAS(CS_FEAT_NULL_CONDITIONAL)) return NULL_COND_INDEX; else { return QUESTION; unput('['); } }
".."            { if (CS_HAS(CS_FEAT_RANGES_INDICES)) return RANGE; else return DOT; }
"^"             { if (CS_HAS(CS_FEAT_RANGES_INDICES) && is_index_context()) return INDEX_FROM_END; else return CARET; }

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
\$\"([^"\\\n]|\\.)*\"                           { if (CS_HAS(CS_FEAT_STRING_INTERPOLATION)) { cs_lval.sval = strdup(yytext); return INTERPOLATED_STRING; } }
\"\"\"                                          { if (CS_HAS(CS_FEAT_RAW_STRINGS)) { BEGIN(RAW_STRING); } }

    /* Binary literals (C# 7.0+) */
0[Bb][01][01_]*                                 { if (CS_HAS(CS_FEAT_BINARY_LITERALS)) { cs_lval.ival = (int)strtol(yytext+2, NULL, 2); return INT_LITERAL; } }

    /* Digit separators (C# 7.0+) */
{DIGIT}(_?{DIGIT})+                             { if (CS_HAS(CS_FEAT_DIGIT_SEPARATORS)) { cs_lval.ival = parse_with_separators(yytext); return INT_LITERAL; } }

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

/* Helper function: check if we're in index context */
static int is_index_context(void) {
    /* Would need parser state to determine this properly */
    /* For now, simplified */
    return 1;
}

/* Helper function: parse number with digit separators */
static int parse_with_separators(const char *str) {
    char buf[64];
    int j = 0;
    for (int i = 0; str[i] && j < 63; i++) {
        if (str[i] != '_')
            buf[j++] = str[i];
    }
    buf[j] = '\0';
    return atoi(buf);
}

/* Macro to simplify feature checking */
#define CS_HAS(feat) cs_version_check_feature(feat)

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
