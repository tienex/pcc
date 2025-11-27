%{
/*
 * COBOL lexical analyzer
 * Supports OO COBOL with multi-vendor dialects
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "pass1.h"
#include "gram.h"

extern int lineno;

static char *string_buf;
static int string_len;
static int string_max;

static void add_string_char(char c);
static char *copy_string(void);

%}

%option noyywrap
%option case-insensitive

%x STRING
%x COMMENT

DIGIT   [0-9]
LETTER  [A-Za-z]
ALNUM   [A-Za-z0-9]
IDENT   [A-Za-z][A-Za-z0-9-]*

%%

    /* Skip sequence area (columns 1-6) and indicator (column 7) */
    /* In free-form COBOL (modern), we accept any format */

    /* Comments */
"*>".*          { /* Inline comment */ }
^"*".*          { /* Full-line comment (column 7 = *) */ }

    /* String literals */
\"              { BEGIN(STRING); string_len = 0; }
<STRING>\"      { BEGIN(INITIAL); yylval.string = copy_string(); return STRING_LITERAL; }
<STRING>\n      { yyerror("Unterminated string"); lineno++; }
<STRING>.       { add_string_char(yytext[0]); }

'               { BEGIN(STRING); string_len = 0; }
<STRING>'       { BEGIN(INITIAL); yylval.string = copy_string(); return STRING_LITERAL; }

    /* Division headers */
IDENTIFICATION[ \t]+DIVISION    { return IDENTIFICATION_DIVISION; }
ENVIRONMENT[ \t]+DIVISION       { return ENVIRONMENT_DIVISION; }
DATA[ \t]+DIVISION              { return DATA_DIVISION; }
PROCEDURE[ \t]+DIVISION         { return PROCEDURE_DIVISION; }

    /* Section headers */
CONFIGURATION[ \t]+SECTION      { return CONFIGURATION_SECTION; }
INPUT-OUTPUT[ \t]+SECTION       { return INPUT_OUTPUT_SECTION; }
FILE[ \t]+SECTION               { return FILE_SECTION; }
WORKING-STORAGE[ \t]+SECTION    { return WORKING_STORAGE_SECTION; }
LOCAL-STORAGE[ \t]+SECTION      { return LOCAL_STORAGE_SECTION; }
LINKAGE[ \t]+SECTION            { return LINKAGE_SECTION; }

    /* OO COBOL keywords */
CLASS                   { return CLASS; }
CLASS-ID                { return CLASS_ID; }
METHOD                  { return METHOD; }
METHOD-ID               { return METHOD_ID; }
OBJECT                  { return OBJECT; }
INHERITS                { return INHERITS; }
INTERFACE               { return INTERFACE; }
INTERFACE-ID            { return INTERFACE_ID; }
FACTORY                 { return FACTORY; }
INVOKE                  { return INVOKE; }
NEW                     { return NEW; }
SELF                    { return SELF; }
SUPER                   { return SUPER; }
PROPERTY                { return PROPERTY; }
STATIC                  { return STATIC; }
INSTANCE                { return INSTANCE; }
FINAL                   { return FINAL; }
ABSTRACT                { return ABSTRACT; }
OVERRIDE                { return OVERRIDE; }

    /* Data description keywords */
PIC                     { return PIC; }
PICTURE                 { return PIC; }
VALUE                   { return VALUE; }
REDEFINES               { return REDEFINES; }
OCCURS                  { return OCCURS; }
TIMES                   { return TIMES; }
DEPENDING[ \t]+ON       { return DEPENDING_ON; }
INDEXED[ \t]+BY         { return INDEXED_BY; }
USAGE                   { return USAGE; }
COMP                    { return COMP; }
COMP-1                  { return COMP_1; }
COMP-2                  { return COMP_2; }
COMP-3                  { return COMP_3; }
COMP-4                  { return COMP_4; }
COMP-5                  { return COMP_5; }
BINARY                  { return BINARY; }
PACKED-DECIMAL          { return PACKED_DECIMAL; }
DISPLAY                 { return DISPLAY_USAGE; }
POINTER                 { return POINTER; }
OBJECT[ \t]+REFERENCE   { return OBJECT_REFERENCE; }

    /* Procedure division keywords */
ACCEPT                  { return ACCEPT; }
ADD                     { return ADD; }
CALL                    { return CALL; }
COMPUTE                 { return COMPUTE; }
DELETE                  { return DELETE; }
DISPLAY                 { return DISPLAY; }
DIVIDE                  { return DIVIDE_OP; }
EVALUATE                { return EVALUATE; }
EXIT                    { return EXIT; }
GO[ \t]+TO              { return GOTO; }
IF                      { return IF; }
ELSE                    { return ELSE; }
END-IF                  { return END_IF; }
MOVE                    { return MOVE; }
MULTIPLY                { return MULTIPLY; }
PERFORM                 { return PERFORM; }
END-PERFORM             { return END_PERFORM; }
READ                    { return READ; }
RETURN                  { return RETURN; }
RETURNING               { return RETURNING; }
REWRITE                 { return REWRITE; }
SEARCH                  { return SEARCH; }
SET                     { return SET; }
STOP                    { return STOP; }
STRING                  { return STRING_STMT; }
SUBTRACT                { return SUBTRACT; }
UNSTRING                { return UNSTRING; }
WRITE                   { return WRITE; }

    /* Control flow */
UNTIL                   { return UNTIL; }
VARYING                 { return VARYING; }
WHEN                    { return WHEN; }
OTHER                   { return OTHER; }
THROUGH                 { return THROUGH; }
THRU                    { return THRU; }
BY                      { return BY; }
GIVING                  { return GIVING; }
REMAINDER               { return REMAINDER; }
INTO                    { return INTO; }
FROM                    { return FROM; }

    /* File operations */
OPEN                    { return OPEN; }
CLOSE                   { return CLOSE; }
INPUT                   { return INPUT; }
OUTPUT                  { return OUTPUT; }
I-O                     { return IO; }
EXTEND                  { return EXTEND; }
FILE                    { return FILE; }
FD                      { return FD; }
SD                      { return SD; }
SELECT                  { return SELECT; }
ASSIGN                  { return ASSIGN; }
ORGANIZATION            { return ORGANIZATION; }
ACCESS[ \t]+MODE        { return ACCESS_MODE; }
SEQUENTIAL              { return SEQUENTIAL; }
INDEXED                 { return INDEXED; }
RELATIVE                { return RELATIVE; }
RANDOM                  { return RANDOM; }
DYNAMIC                 { return DYNAMIC; }
RECORD[ \t]+KEY         { return RECORD_KEY; }

    /* Logical operators */
AND                     { return AND; }
OR                      { return OR; }
NOT                     { return NOT; }

    /* Relational operators */
EQUAL[ \t]+TO           { return EQUAL; }
"="                     { return EQUAL; }
GREATER[ \t]+THAN       { return GREATER; }
">"                     { return GREATER; }
LESS[ \t]+THAN          { return LESS; }
"<"                     { return LESS; }
">="                    { return GREATER_EQUAL; }
"<="                    { return LESS_EQUAL; }
NOT[ \t]+EQUAL          { return NOT_EQUAL; }
"<>"                    { return NOT_EQUAL; }

    /* Arithmetic operators */
"+"                     { return PLUS; }
"-"                     { return MINUS; }
"*"                     { return TIMES; }
"/"                     { return DIVIDE; }
"**"                    { return POWER; }

    /* Miscellaneous */
PROGRAM-ID              { return PROGRAM_ID; }
AUTHOR                  { return AUTHOR; }
DATE-WRITTEN            { return DATE_WRITTEN; }
IS                      { return IS; }
ARE                     { return ARE; }
TO                      { return TO; }
OF                      { return OF; }
IN                      { return IN; }
WITH                    { return WITH; }
ALL                     { return ALL; }
USING                   { return USING; }
TRUE                    { return TRUE; }
FALSE                   { return FALSE; }
ZERO                    { return ZERO; }
ZEROS                   { return ZERO; }
ZEROES                  { return ZERO; }
SPACE                   { return SPACE; }
SPACES                  { return SPACE; }
NULL                    { return NULL_TOK; }
NULLS                   { return NULL_TOK; }

    /* Numeric literals */
{DIGIT}+                { yylval.intval = atoi(yytext); return INTEGER; }
{DIGIT}+"."{DIGIT}+     { yylval.floatval = atof(yytext); return FLOAT; }

    /* Identifiers (with hyphens) */
{IDENT}                 { yylval.string = xstrdup(yytext); return IDENTIFIER; }

    /* Level numbers */
^[ \t]*{DIGIT}{1,2}     { yylval.intval = atoi(yytext); return LEVEL_NUMBER; }

    /* Punctuation */
"."                     { return DOT; }
","                     { return COMMA; }
";"                     { return SEMICOLON; }
":"                     { return COLON; }
"("                     { return LPAREN; }
")"                     { return RPAREN; }

    /* Whitespace and newlines */
[ \t]+                  { /* Skip whitespace */ }
\n                      { lineno++; }
\r                      { /* Skip carriage return */ }

    /* Anything else is an error */
.                       { yyerror("Invalid character"); }

%%

static void
add_string_char(char c)
{
	if (string_len >= string_max - 1) {
		string_max = string_max ? string_max * 2 : 128;
		string_buf = realloc(string_buf, string_max);
	}
	string_buf[string_len++] = c;
}

static char *
copy_string(void)
{
	char *s;
	if (string_len == 0)
		return xstrdup("");
	string_buf[string_len] = '\0';
	s = xstrdup(string_buf);
	string_len = 0;
	return s;
}

void
yyerror(const char *s)
{
	extern int nerrors;
	fprintf(stderr, "Line %d: %s near '%s'\n", lineno, s, yytext);
	nerrors++;
}
