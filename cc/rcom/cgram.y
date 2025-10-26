%{
/*	$Id$	*/
/*
 * Ruby parser for PCC compiler
 *
 * Grammar rules for Ruby language
 */

#include "pass1.h"
#include <stdlib.h>
#include <string.h>

extern int lineno;
int yylex(void);
void yyerror(const char *s);

static struct p1node *current_class = NULL;
static int in_def = 0;

/* Helper functions */
static struct p1node *make_method(char *name, struct p1node *params, struct p1node *body);
static struct p1node *make_class(char *name, struct p1node *super, struct p1node *body);
static struct p1node *make_call(struct p1node *receiver, char *method, struct p1node *args);

%}

%union {
	int intval;
	double floatval;
	char *strval;
	struct p1node *node;
	struct symtab *sym;
}

/* Keywords */
%token BEGIN_ END CLASS MODULE DEF IF ELSIF ELSE UNLESS
%token CASE WHEN WHILE UNTIL FOR IN DO BREAK NEXT RETURN YIELD
%token TRUE_ FALSE_ NIL AND OR NOT SELF SUPER
%token REQUIRE INCLUDE ATTR_READER ATTR_WRITER ATTR_ACCESSOR
%token PRIVATE PROTECTED PUBLIC PUTS PRINT GETS

/* Operators */
%token EQ_OP NE_OP LE_OP GE_OP LEFT_SHIFT RIGHT_SHIFT
%token AND_OP OR_OP POW_OP ARROW RANGE_INCL RANGE_EXCL
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token AND_ASSIGN OR_ASSIGN XOR_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN

/* Identifiers and literals */
%token <strval> IDENTIFIER CONSTANT STRING SYMBOL
%token <strval> INSTANCE_VAR CLASS_VAR GLOBAL_VAR
%token <intval> INTEGER
%token <floatval> FLOAT

/* Non-terminals */
%type <node> program statement_list statement
%type <node> expression primary_expr postfix_expr unary_expr
%type <node> multiplicative_expr additive_expr shift_expr
%type <node> relational_expr equality_expr and_expr or_expr
%type <node> conditional_expr assignment_expr
%type <node> method_definition class_definition
%type <node> parameter_list argument_list argument_list_opt
%type <node> if_statement while_statement for_statement
%type <node> case_statement when_clauses block_expr
%type <node> method_call array_literal hash_literal
%type <node> statement_or_expr compound_statement

%start program

%%

program
	: statement_list
		{
			/* Process the complete program */
			if ($1) {
				ecomp($1);
			}
		}
	;

statement_list
	: /* empty */
		{ $$ = NULL; }
	| statement_list statement
		{
			if ($1 == NULL)
				$$ = $2;
			else if ($2 == NULL)
				$$ = $1;
			else
				$$ = block(COMOP, $1, $2);
		}
	;

statement
	: expression ';'
		{ $$ = $1; }
	| expression '\n'
		{ $$ = $1; }
	| method_definition
		{ $$ = $1; }
	| class_definition
		{ $$ = $1; }
	| if_statement
		{ $$ = $1; }
	| while_statement
		{ $$ = $1; }
	| for_statement
		{ $$ = $1; }
	| case_statement
		{ $$ = $1; }
	| RETURN expression
		{ $$ = buildtree(RETURN, $2, NULL); }
	| RETURN
		{ $$ = buildtree(RETURN, NULL, NULL); }
	| BREAK
		{ $$ = buildtree(GOTO, NULL, NULL); /* Use break label */ }
	| NEXT
		{ $$ = buildtree(GOTO, NULL, NULL); /* Use continue label */ }
	| PUTS expression
		{
			/* Call puts function */
			struct symtab *sp = lookup("puts", 0);
			if (!sp) {
				sp = lookup("puts", 0);
				sp->stype = INT;
			}
			$$ = buildtree(CALL, nametree(sp), $2);
		}
	| ';'
		{ $$ = NULL; }
	| '\n'
		{ $$ = NULL; }
	;

method_definition
	: DEF IDENTIFIER '(' parameter_list ')' '\n' statement_list '\n' END
		{ $$ = make_method($2, $4, $7); }
	| DEF IDENTIFIER '\n' statement_list '\n' END
		{ $$ = make_method($2, NULL, $4); }
	;

class_definition
	: CLASS CONSTANT '\n' statement_list '\n' END
		{ $$ = make_class($2, NULL, $4); }
	| CLASS CONSTANT '<' CONSTANT '\n' statement_list '\n' END
		{
			struct symtab *super = lookup($4, 0);
			$$ = make_class($2, nametree(super), $6);
		}
	;

parameter_list
	: IDENTIFIER
		{
			struct symtab *sp = lookup($1, 0);
			sp->sclass = PARAM;
			sp->stype = INT;  /* Default type */
			$$ = nametree(sp);
		}
	| parameter_list ',' IDENTIFIER
		{
			struct symtab *sp = lookup($3, 0);
			sp->sclass = PARAM;
			sp->stype = INT;
			$$ = block(CM, $1, nametree(sp));
		}
	;

if_statement
	: IF expression '\n' statement_list '\n' END
		{ $$ = buildtree(IF, $2, $4); }
	| IF expression '\n' statement_list '\n' ELSE '\n' statement_list '\n' END
		{
			struct p1node *else_part = $8;
			struct p1node *then_part = $4;
			$$ = buildtree(IF, $2, buildtree(COLON, then_part, else_part));
		}
	;

while_statement
	: WHILE expression '\n' statement_list '\n' END
		{ $$ = buildtree(WHILE, $2, $4); }
	| WHILE expression DO '\n' statement_list '\n' END
		{ $$ = buildtree(WHILE, $2, $5); }
	;

for_statement
	: FOR IDENTIFIER IN expression '\n' statement_list '\n' END
		{
			/* Convert for loop to while loop equivalent */
			/* This is simplified - real implementation would be more complex */
			struct symtab *sp = lookup($2, 0);
			sp->sclass = AUTO;
			sp->stype = INT;
			$$ = buildtree(WHILE, $4, $6);
		}
	;

case_statement
	: CASE expression '\n' when_clauses '\n' END
		{
			/* Implement as switch statement */
			/* Simplified - real implementation would build switch table */
			$$ = $4;
		}
	;

when_clauses
	: WHEN expression '\n' statement_list
		{
			/* Build case branch */
			$$ = buildtree(IF, buildtree(EQ, $2, NULL), $4);
		}
	| when_clauses '\n' WHEN expression '\n' statement_list
		{
			struct p1node *new_when = buildtree(IF, buildtree(EQ, $4, NULL), $6);
			$$ = block(COMOP, $1, new_when);
		}
	;

expression
	: conditional_expr
		{ $$ = $1; }
	;

conditional_expr
	: or_expr
		{ $$ = $1; }
	| or_expr '?' expression ':' conditional_expr
		{ $$ = buildtree(QUEST, $1, buildtree(COLON, $3, $5)); }
	;

or_expr
	: and_expr
		{ $$ = $1; }
	| or_expr OR_OP and_expr
		{ $$ = buildtree(OROR, $1, $3); }
	| or_expr OR and_expr
		{ $$ = buildtree(OROR, $1, $3); }
	;

and_expr
	: equality_expr
		{ $$ = $1; }
	| and_expr AND_OP equality_expr
		{ $$ = buildtree(ANDAND, $1, $3); }
	| and_expr AND equality_expr
		{ $$ = buildtree(ANDAND, $1, $3); }
	;

equality_expr
	: relational_expr
		{ $$ = $1; }
	| equality_expr EQ_OP relational_expr
		{ $$ = buildtree(EQ, $1, $3); }
	| equality_expr NE_OP relational_expr
		{ $$ = buildtree(NE, $1, $3); }
	;

relational_expr
	: shift_expr
		{ $$ = $1; }
	| relational_expr '<' shift_expr
		{ $$ = buildtree(LT, $1, $3); }
	| relational_expr '>' shift_expr
		{ $$ = buildtree(GT, $1, $3); }
	| relational_expr LE_OP shift_expr
		{ $$ = buildtree(LE, $1, $3); }
	| relational_expr GE_OP shift_expr
		{ $$ = buildtree(GE, $1, $3); }
	;

shift_expr
	: additive_expr
		{ $$ = $1; }
	| shift_expr LEFT_SHIFT additive_expr
		{ $$ = buildtree(LS, $1, $3); }
	| shift_expr RIGHT_SHIFT additive_expr
		{ $$ = buildtree(RS, $1, $3); }
	;

additive_expr
	: multiplicative_expr
		{ $$ = $1; }
	| additive_expr '+' multiplicative_expr
		{ $$ = buildtree(PLUS, $1, $3); }
	| additive_expr '-' multiplicative_expr
		{ $$ = buildtree(MINUS, $1, $3); }
	;

multiplicative_expr
	: unary_expr
		{ $$ = $1; }
	| multiplicative_expr '*' unary_expr
		{ $$ = buildtree(MUL, $1, $3); }
	| multiplicative_expr '/' unary_expr
		{ $$ = buildtree(DIV, $1, $3); }
	| multiplicative_expr '%' unary_expr
		{ $$ = buildtree(MOD, $1, $3); }
	| multiplicative_expr POW_OP unary_expr
		{
			/* Implement power as function call */
			struct symtab *sp = lookup("pow", 0);
			$$ = buildtree(CALL, nametree(sp), block(CM, $1, $3));
		}
	;

unary_expr
	: postfix_expr
		{ $$ = $1; }
	| '+' unary_expr
		{ $$ = buildtree(PLUS, bcon(0), $2); }
	| '-' unary_expr
		{ $$ = buildtree(MINUS, bcon(0), $2); }
	| '!' unary_expr
		{ $$ = buildtree(NOT, $2, NULL); }
	| '~' unary_expr
		{ $$ = buildtree(COMPL, $2, NULL); }
	| NOT unary_expr
		{ $$ = buildtree(NOT, $2, NULL); }
	;

postfix_expr
	: primary_expr
		{ $$ = $1; }
	| postfix_expr '(' argument_list_opt ')'
		{
			/* Function/method call */
			$$ = buildtree(CALL, $1, $3);
		}
	| postfix_expr '.' IDENTIFIER
		{
			/* Method call without arguments */
			$$ = make_call($1, $3, NULL);
		}
	| postfix_expr '.' IDENTIFIER '(' argument_list_opt ')'
		{
			/* Method call with arguments */
			$$ = make_call($1, $3, $5);
		}
	| postfix_expr '[' expression ']'
		{
			/* Array indexing */
			$$ = buildtree(PLUS, $1, $3);
			$$ = buildtree(UMUL, $$, NULL);
		}
	;

primary_expr
	: IDENTIFIER
		{
			struct symtab *sp = lookup($1, 0);
			if (!sp) {
				sp = lookup($1, 0);
				sp->stype = INT;
				sp->sclass = AUTO;
			}
			$$ = nametree(sp);
		}
	| INTEGER
		{ $$ = bcon($1); }
	| FLOAT
		{
			char buf[64];
			snprintf(buf, sizeof(buf), "%f", $1);
			$$ = fcon(buf);
		}
	| STRING
		{ $$ = string($1); }
	| SYMBOL
		{
			/* Treat symbols as string constants for now */
			$$ = string($1);
		}
	| TRUE_
		{ $$ = bcon(1); }
	| FALSE_
		{ $$ = bcon(0); }
	| NIL
		{ $$ = bcon(0); }
	| SELF
		{
			/* Reference to self */
			struct symtab *sp = lookup("self", 0);
			$$ = nametree(sp);
		}
	| INSTANCE_VAR
		{
			/* Instance variable */
			struct symtab *sp = lookup($1, 0);
			sp->sclass = RUBY_IVAR;
			sp->stype = INT;
			$$ = nametree(sp);
		}
	| CLASS_VAR
		{
			/* Class variable */
			struct symtab *sp = lookup($1, 0);
			sp->sclass = RUBY_CVAR;
			sp->stype = INT;
			$$ = nametree(sp);
		}
	| GLOBAL_VAR
		{
			/* Global variable */
			struct symtab *sp = lookup($1, 0);
			sp->sclass = RUBY_GVAR;
			sp->stype = INT;
			$$ = nametree(sp);
		}
	| '(' expression ')'
		{ $$ = $2; }
	| array_literal
		{ $$ = $1; }
	| block_expr
		{ $$ = $1; }
	;

array_literal
	: '[' argument_list_opt ']'
		{
			/* Create array - simplified implementation */
			$$ = $2;
		}
	;

block_expr
	: '{' statement_list '}'
		{ $$ = $2; }
	| DO '\n' statement_list '\n' END
		{ $$ = $3; }
	;

argument_list_opt
	: /* empty */
		{ $$ = NULL; }
	| argument_list
		{ $$ = $1; }
	;

argument_list
	: expression
		{ $$ = $1; }
	| argument_list ',' expression
		{ $$ = block(CM, $1, $3); }
	;

assignment_expr
	: IDENTIFIER '=' expression
		{
			struct symtab *sp = lookup($1, 0);
			if (!sp) {
				sp = lookup($1, 0);
				sp->stype = INT;
				sp->sclass = AUTO;
			}
			$$ = buildtree(ASSIGN, nametree(sp), $3);
		}
	| postfix_expr '=' expression
		{ $$ = buildtree(ASSIGN, $1, $3); }
	;

%%

void
yyerror(const char *s)
{
	extern int nerrors;
	fprintf(stderr, "%s:%d: %s\n", ftitle ? ftitle : "<stdin>", lineno, s);
	nerrors++;
}

static struct p1node *
make_method(char *name, struct p1node *params, struct p1node *body)
{
	/* Create a method definition as a function */
	struct symtab *sp = lookup(name, 0);
	sp->stype = INT | FTN;
	sp->sclass = EXTDEF;

	/* Store in symbol table */
	defid(sp, EXTDEF);

	/* Build function body */
	return body;
}

static struct p1node *
make_class(char *name, struct p1node *super, struct p1node *body)
{
	/* Create a class definition */
	/* For now, treat as a struct with methods */
	struct symtab *sp = lookup(name, 0);
	sp->stype = STRTY;
	sp->sclass = STNAME;

	current_class = nametree(sp);

	/* Process class body */
	return body;
}

static struct p1node *
make_call(struct p1node *receiver, char *method, struct p1node *args)
{
	/* Create a method call */
	/* Receiver is implicit first argument */
	struct symtab *sp = lookup(method, 0);
	struct p1node *fn = nametree(sp);

	if (args) {
		struct p1node *all_args = block(CM, receiver, args);
		return buildtree(CALL, fn, all_args);
	} else {
		return buildtree(CALL, fn, receiver);
	}
}
