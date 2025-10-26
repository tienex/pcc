/*
 * Copyright (c) 2025 PCC DEC MACRO Compiler
 *
 * Parser grammar for DEC MACRO assembly language
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Parser interface */
extern int yylex(void);
extern void yyerror(const char *s);

/* Current instruction being built */
static INSTRUCTION current_inst;

/* Macro being defined */
static char *macro_name = NULL;
static char **macro_params = NULL;
static int macro_nparam = 0;
static char *macro_body = NULL;
static int in_macro = 0;

%}

%union {
	long lval;           /* Integer value */
	int ival;            /* Integer (for registers) */
	char *sval;          /* String value */
	OPERAND operand;     /* Operand */
}

/* Token types */
%token <sval> IDENTIFIER LABEL LOCAL_LABEL STRING_LITERAL
%token <lval> NUMBER
%token <ival> REGISTER ACCUMULATOR

/* Directives */
%token DIR_TITLE DIR_IDENT DIR_PSECT DIR_ENTRY DIR_END
%token DIR_GLOBL DIR_EXTERN
%token DIR_BYTE DIR_WORD DIR_LONG
%token DIR_ASCII DIR_ASCIZ
%token DIR_BLKB DIR_BLKW DIR_BLKL
%token DIR_ALIGN DIR_EVEN DIR_ODD
%token DIR_PAGE DIR_SBTTL
%token DIR_MACRO DIR_ENDM DIR_MEXIT
%token DIR_IRP DIR_IRPC DIR_REPT DIR_ENDR
%token DIR_IF DIR_IFF DIR_IFT DIR_IFTF
%token DIR_IFB DIR_IFNB DIR_IFDEF DIR_IFNDEF DIR_ENDC
%token DIR_LIST DIR_NLIST DIR_ENABLE DIR_DISABLE

%token NEWLINE

/* Non-terminals */
%type <operand> operand
%type <lval> expression

%%

program:
	  /* empty */
	| program line
	;

line:
	  NEWLINE
	| statement NEWLINE
	| LABEL statement NEWLINE {
		emit_label_ir($1);
		free($1);
	}
	| LABEL NEWLINE {
		emit_label_ir($1);
		free($1);
	}
	| error NEWLINE {
		yyerrok;
	}
	;

statement:
	  directive
	| instruction
	| macro_call
	;

/* ========== Directives ========== */

directive:
	  DIR_TITLE STRING_LITERAL {
		emit_directive_ir(DIR_TITLE, $2);
		free($2);
	}
	| DIR_IDENT STRING_LITERAL {
		emit_directive_ir(DIR_IDENT, $2);
		free($2);
	}
	| DIR_PSECT IDENTIFIER {
		emit_directive_ir(DIR_PSECT, $2);
		free($2);
	}
	| DIR_ENTRY IDENTIFIER {
		SYMTAB *sym = install($2, SYM_LABEL);
		sym->sflags |= SF_ENTRY;
		emit_directive_ir(DIR_ENTRY, $2);
		free($2);
	}
	| DIR_END {
		emit_directive_ir(DIR_END);
	}
	| DIR_GLOBL symbol_list {
		/* Symbols marked global in symbol_list production */
	}
	| DIR_EXTERN symbol_list {
		/* Symbols marked extern in symbol_list production */
	}
	| DIR_BYTE data_list {
		/* Data emitted in data_list production */
	}
	| DIR_WORD data_list {
		/* Data emitted in data_list production */
	}
	| DIR_LONG data_list {
		/* Data emitted in data_list production */
	}
	| DIR_ASCII STRING_LITERAL {
		emit_string_ir($2, 0);
		free($2);
	}
	| DIR_ASCIZ STRING_LITERAL {
		emit_string_ir($2, 1);
		free($2);
	}
	| DIR_BLKB expression {
		location_counter += $2;
	}
	| DIR_BLKW expression {
		location_counter += $2 * 2;
	}
	| DIR_BLKL expression {
		location_counter += $2 * 4;
	}
	| DIR_ALIGN expression {
		emit_directive_ir(DIR_ALIGN, $2);
	}
	| DIR_EVEN {
		emit_directive_ir(DIR_EVEN);
	}
	| DIR_MACRO IDENTIFIER {
		/* Start macro definition */
		macro_name = $2;
		macro_nparam = 0;
		macro_params = NULL;
		macro_body = strdup("");
		in_macro = 1;
	}
	| DIR_ENDM {
		/* End macro definition */
		if (in_macro) {
			define_macro(macro_name, macro_params, macro_nparam, macro_body);
			free(macro_name);
			free(macro_body);
			in_macro = 0;
		} else {
			error(".ENDM without .MACRO");
		}
	}
	;

symbol_list:
	  IDENTIFIER {
		SYMTAB *sym = install($1, SYM_GLOBAL);
		export_symbol(sym);
		free($1);
	}
	| symbol_list ',' IDENTIFIER {
		SYMTAB *sym = install($3, SYM_GLOBAL);
		export_symbol(sym);
		free($3);
	}
	;

data_list:
	  expression {
		emit_data_ir(1, $1);  /* Emit byte by default */
	}
	| data_list ',' expression {
		emit_data_ir(1, $3);
	}
	;

/* ========== Instructions ========== */

instruction:
	  IDENTIFIER {
		/* Instruction with no operands */
		current_inst.mnemonic = $1;
		current_inst.noperands = 0;
		current_inst.lineno = lineno;
		emit_instruction_ir(&current_inst);
		free($1);
	}
	| IDENTIFIER operand {
		/* Instruction with one operand */
		current_inst.mnemonic = $1;
		current_inst.operands[0] = $2;
		current_inst.noperands = 1;
		current_inst.lineno = lineno;
		emit_instruction_ir(&current_inst);
		free($1);
	}
	| IDENTIFIER operand ',' operand {
		/* Instruction with two operands */
		current_inst.mnemonic = $1;
		current_inst.operands[0] = $2;
		current_inst.operands[1] = $4;
		current_inst.noperands = 2;
		current_inst.lineno = lineno;
		emit_instruction_ir(&current_inst);
		free($1);
	}
	| IDENTIFIER operand ',' operand ',' operand {
		/* Instruction with three operands */
		current_inst.mnemonic = $1;
		current_inst.operands[0] = $2;
		current_inst.operands[1] = $4;
		current_inst.operands[2] = $6;
		current_inst.noperands = 3;
		current_inst.lineno = lineno;
		emit_instruction_ir(&current_inst);
		free($1);
	}
	;

/* ========== Operands ========== */

operand:
	  REGISTER {
		/* Register direct */
		$$.type = OP_REGISTER;
		$$.reg = $1;
		$$.value = 0;
		$$.symbol = NULL;
		$$.flags = 0;
	}
	| ACCUMULATOR {
		/* PDP-10 accumulator */
		$$.type = OP_REGISTER;
		$$.reg = $1;
		$$.value = 0;
		$$.symbol = NULL;
		$$.flags = 0;
	}
	| '#' expression {
		/* Immediate mode */
		$$.type = OP_IMMEDIATE;
		$$.reg = 0;
		$$.value = $2;
		$$.symbol = NULL;
		$$.flags = 0;
	}
	| '@' REGISTER {
		/* Register indirect (deferred) */
		$$.type = OP_INDIRECT;
		$$.reg = $2;
		$$.value = 0;
		$$.symbol = NULL;
		$$.flags = OF_DEFERRED;
	}
	| '@' IDENTIFIER {
		/* Indirect through symbol */
		$$.type = OP_INDIRECT;
		$$.reg = 0;
		$$.value = 0;
		$$.symbol = $2;
		$$.flags = OF_DEFERRED;
	}
	| '-' '(' REGISTER ')' {
		/* Autodecrement */
		$$.type = OP_AUTODEC;
		$$.reg = $3;
		$$.value = 0;
		$$.symbol = NULL;
		$$.flags = OF_AUTODEC;
	}
	| '(' REGISTER ')' '+' {
		/* Autoincrement */
		$$.type = OP_AUTOINC;
		$$.reg = $2;
		$$.value = 0;
		$$.symbol = NULL;
		$$.flags = OF_AUTOINC;
	}
	| expression '(' REGISTER ')' {
		/* Indexed (displacement) */
		$$.type = OP_INDEXED;
		$$.reg = $3;
		$$.value = $1;
		$$.symbol = NULL;
		$$.flags = OF_INDEXED;
	}
	| IDENTIFIER '(' REGISTER ')' {
		/* Indexed with symbol */
		$$.type = OP_INDEXED;
		$$.reg = $3;
		$$.value = 0;
		$$.symbol = $1;
		$$.flags = OF_INDEXED;
	}
	| IDENTIFIER {
		/* Symbol reference (direct addressing) */
		$$.type = OP_SYMBOL;
		$$.reg = 0;
		$$.value = 0;
		$$.symbol = $1;
		$$.flags = 0;

		/* Mark symbol as referenced */
		SYMTAB *sym = lookup($1);
		if (sym == NULL)
			sym = install($1, SYM_LABEL);
		sym->sflags |= SF_REFERENCED;
	}
	| expression {
		/* Numeric literal */
		$$.type = OP_LITERAL;
		$$.reg = 0;
		$$.value = $1;
		$$.symbol = NULL;
		$$.flags = 0;
	}
	;

/* ========== Expressions ========== */

expression:
	  NUMBER {
		$$ = $1;
	}
	| expression '+' expression {
		$$ = $1 + $3;
	}
	| expression '-' expression {
		$$ = $1 - $3;
	}
	| expression '*' expression {
		$$ = $1 * $3;
	}
	| expression '/' expression {
		if ($3 == 0) {
			error("division by zero");
			$$ = 0;
		} else {
			$$ = $1 / $3;
		}
	}
	| expression '&' expression {
		$$ = $1 & $3;
	}
	| expression '|' expression {
		$$ = $1 | $3;
	}
	| expression '^' expression {
		$$ = $1 ^ $3;
	}
	| '(' expression ')' {
		$$ = $2;
	}
	| '-' expression {
		$$ = -$2;
	}
	;

/* ========== Macro calls ========== */

macro_call:
	  IDENTIFIER {
		/* Check if this is a macro call */
		MACRO *m = lookup_macro($1);
		if (m != NULL) {
			/* Expand macro with no arguments */
			char *expanded = expand_macro(m, NULL, 0);
			/* TODO: Re-parse expanded text */
			free(expanded);
		}
		free($1);
	}
	;

%%

/*
 * Error handler
 */
void
yyerror(const char *s)
{
	error("%s", s);
}
