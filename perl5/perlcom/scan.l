%{
/*	$Id$	*/
/*
 * Perl 5 Compiler - Lexical Analyzer
 */

#include "pass1.h"
#include "pgram.h"
#include <ctype.h>

int perl_lineno = 1;
char *perl_filename = "<stdin>";
static int in_regex = 0;

static char *string_buf = NULL;
static int string_buf_size = 0;
static int string_buf_len = 0;

static void add_to_string(char c);
static void reset_string_buf(void);
static char *get_string_buf(void);
%}

%option noyywrap
%option never-interactive

%x STRING
%x REGEX
%x COMMENT

DIGIT		[0-9]
HEXDIGIT	[0-9a-fA-F]
OCTDIGIT	[0-7]
BINDIGIT	[01]
LETTER		[a-zA-Z_]
IDENT		{LETTER}({LETTER}|{DIGIT})*

%%

	/* Comments */
^[ \t]*#.*$		{ /* Shell-style comment */ }
#.*$			{ /* Inline comment */ }

	/* Multi-line comments (POD) */
^=pod			{ BEGIN(COMMENT); }
<COMMENT>^=cut		{ BEGIN(INITIAL); }
<COMMENT>.		{ /* Skip POD content */ }
<COMMENT>\n		{ perl_lineno++; }

	/* Keywords */
sub			{ return SUB; }
my			{ return MY; }
our			{ return OUR; }
local			{ return LOCAL; }
state			{ return STATE; }
if			{ return IF; }
elsif			{ return ELSIF; }
else			{ return ELSE; }
unless			{ return UNLESS; }
while			{ return WHILE; }
until			{ return UNTIL; }
for			{ return FOR; }
foreach			{ return FOREACH; }
do			{ return DO; }
next			{ return NEXT; }
last			{ return LAST; }
redo			{ return REDO; }
return			{ return RETURN; }
use			{ return USE; }
require			{ return REQUIRE; }
package			{ return PACKAGE; }
BEGIN			{ return BEGIN_BLOCK; }
END			{ return END_BLOCK; }
eval			{ return EVAL; }
defined			{ return DEFINED; }
undef			{ return UNDEF; }

	/* Built-in functions */
print			{ return PRINT; }
printf			{ return PRINTF; }
say			{ return SAY; }
push			{ return PUSH; }
pop			{ return POP; }
shift			{ return SHIFT; }
unshift			{ return UNSHIFT; }
keys			{ return KEYS; }
values			{ return VALUES; }
each			{ return EACH; }
delete			{ return DELETE; }
exists			{ return EXISTS; }
join			{ return JOIN; }
split			{ return SPLIT; }
grep			{ return GREP; }
map			{ return MAP; }
sort			{ return SORT; }
reverse			{ return REVERSE; }
length			{ return LENGTH; }
substr			{ return SUBSTR; }
index			{ return INDEX; }
rindex			{ return RINDEX; }
chomp			{ return CHOMP; }
chop			{ return CHOP; }
open			{ return OPEN; }
close			{ return CLOSE; }
read			{ return READ; }
write			{ return WRITE; }
die			{ return DIE; }
warn			{ return WARN; }

	/* Variables */
\${IDENT}		{ yylval.string = strdup(yytext + 1); return SCALAR; }
@{IDENT}		{ yylval.string = strdup(yytext + 1); return ARRAY; }
%{IDENT}		{ yylval.string = strdup(yytext + 1); return HASH; }
&{IDENT}		{ yylval.string = strdup(yytext + 1); return SUBNAME; }
\*{IDENT}		{ yylval.string = strdup(yytext + 1); return GLOB; }

	/* Special variables */
\$_			{ yylval.string = strdup("_"); return SCALAR; }
\$@			{ yylval.string = strdup("@"); return SCALAR; }
\$!			{ yylval.string = strdup("!"); return SCALAR; }
\$\?			{ yylval.string = strdup("?"); return SCALAR; }
\$\$			{ yylval.string = strdup("$"); return SCALAR; }
\$0			{ yylval.string = strdup("0"); return SCALAR; }
\$[1-9][0-9]*		{ yylval.string = strdup(yytext + 1); return SCALAR; }

	/* Numbers */
{DIGIT}+		{ yylval.intval = atoi(yytext); return INTEGER; }
{DIGIT}+\.{DIGIT}+	{ yylval.floatval = atof(yytext); return FLOAT; }
\.{DIGIT}+		{ yylval.floatval = atof(yytext); return FLOAT; }
0x{HEXDIGIT}+		{ yylval.intval = strtol(yytext + 2, NULL, 16); return INTEGER; }
0b{BINDIGIT}+		{ yylval.intval = strtol(yytext + 2, NULL, 2); return INTEGER; }
0{OCTDIGIT}+		{ yylval.intval = strtol(yytext + 1, NULL, 8); return INTEGER; }

	/* String literals */
\"			{ reset_string_buf(); BEGIN(STRING); }
<STRING>\"		{ BEGIN(INITIAL); yylval.string = get_string_buf(); return STRING; }
<STRING>\\n		{ add_to_string('\n'); }
<STRING>\\t		{ add_to_string('\t'); }
<STRING>\\r		{ add_to_string('\r'); }
<STRING>\\\"		{ add_to_string('"'); }
<STRING>\\\\		{ add_to_string('\\'); }
<STRING>\\\$		{ add_to_string('$'); }
<STRING>\\x{HEXDIGIT}{2} {
			  char hex[3];
			  hex[0] = yytext[2];
			  hex[1] = yytext[3];
			  hex[2] = '\0';
			  add_to_string((char)strtol(hex, NULL, 16));
			}
<STRING>[^\\\"\n]+	{ for (int i = 0; i < yyleng; i++) add_to_string(yytext[i]); }
<STRING>\n		{ perl_lineno++; add_to_string('\n'); }

'			{ reset_string_buf(); BEGIN(STRING); }
<STRING>'		{ BEGIN(INITIAL); yylval.string = get_string_buf(); return STRING; }

	/* Regular expressions */
\/			{ reset_string_buf(); BEGIN(REGEX); return '/'; }
<REGEX>\/[gimosxc]*	{
			  BEGIN(INITIAL);
			  yylval.string = get_string_buf();
			  return REGEX_PATTERN;
			}
<REGEX>\\.		{ add_to_string(yytext[0]); add_to_string(yytext[1]); }
<REGEX>[^/\\]+		{ for (int i = 0; i < yyleng; i++) add_to_string(yytext[i]); }

	/* Operators */
"=~"			{ return MATCH_OP; }
"!~"			{ return NOT_MATCH_OP; }
"=="			{ return EQ_OP; }
"!="			{ return NE_OP; }
"<="			{ return LE_OP; }
">="			{ return GE_OP; }
"<=>"			{ return CMP_OP; }
"eq"			{ return STR_EQ; }
"ne"			{ return STR_NE; }
"lt"			{ return STR_LT; }
"gt"			{ return STR_GT; }
"le"			{ return STR_LE; }
"ge"			{ return STR_GE; }
"cmp"			{ return STR_CMP; }
"&&"			{ return AND_OP; }
"||"			{ return OR_OP; }
"and"			{ return AND_OP; }
"or"			{ return OR_OP; }
"not"			{ return NOT_OP; }
"xor"			{ return XOR_OP; }
"++"			{ return INC_OP; }
"--"			{ return DEC_OP; }
"+="			{ return ADD_ASSIGN; }
"-="			{ return SUB_ASSIGN; }
"*="			{ return MUL_ASSIGN; }
"/="			{ return DIV_ASSIGN; }
"%="			{ return MOD_ASSIGN; }
"&="			{ return AND_ASSIGN; }
"|="			{ return OR_ASSIGN; }
"^="			{ return XOR_ASSIGN; }
"<<="			{ return SHL_ASSIGN; }
">>="			{ return SHR_ASSIGN; }
".="			{ return CONCAT_ASSIGN; }
"**"			{ return POW_OP; }
"<<"			{ return SHL_OP; }
">>"			{ return SHR_OP; }
"=>"			{ return ARROW; }
"->"			{ return DEREF; }
".."			{ return RANGE; }
"..."			{ return ELLIPSIS; }

	/* Single character operators */
[+\-*/%&|^~<>=!.]	{ return yytext[0]; }
[(){}\[\];,:]		{ return yytext[0]; }
"?"			{ return '?'; }

	/* Bareword identifiers */
{IDENT}			{ yylval.string = strdup(yytext); return IDENT; }

	/* Whitespace */
[ \t\r]+		{ /* Skip whitespace */ }
\n			{ perl_lineno++; }

	/* Anything else is an error */
.			{ perl_error("Unexpected character: %c", yytext[0]); }

%%

/* String buffer management */
static void add_to_string(char c) {
	if (string_buf_len >= string_buf_size - 1) {
		string_buf_size = string_buf_size ? string_buf_size * 2 : 256;
		string_buf = realloc(string_buf, string_buf_size);
		if (!string_buf) {
			perl_fatal("Out of memory");
		}
	}
	string_buf[string_buf_len++] = c;
	string_buf[string_buf_len] = '\0';
}

static void reset_string_buf(void) {
	if (!string_buf) {
		string_buf_size = 256;
		string_buf = malloc(string_buf_size);
		if (!string_buf) {
			perl_fatal("Out of memory");
		}
	}
	string_buf_len = 0;
	string_buf[0] = '\0';
}

static char *get_string_buf(void) {
	char *result = strdup(string_buf);
	if (!result) {
		perl_fatal("Out of memory");
	}
	return result;
}
