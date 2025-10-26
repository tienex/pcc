/*
 * Copyright (c) 2025 PCC BASIC Compiler
 *
 * Built-in functions and statements
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "pass1.h"

/* Line number table */
static LINE_ENTRY *line_table_head = NULL;
static int next_label_id = 1;

/* Built-in function names */
static const char *builtin_funcs[] = {
	"ABS", "ASC", "ATN", "CDBL", "CHR$", "CINT", "COS",
	"CSNG", "CVD", "CVI", "CVS", "EOF", "EXP", "FIX",
	"FRE", "HEX$", "INKEY$", "INP", "INPUT$", "INSTR",
	"INT", "LEFT$", "LEN", "LOC", "LOF", "LOG", "LPOS",
	"MID$", "MKD$", "MKI$", "MKS$", "OCT$", "PEEK",
	"POS", "RIGHT$", "RND", "SGN", "SIN", "SPACE$",
	"SPC", "SQR", "STR$", "STRING$", "TAB", "TAN",
	"USR", "VAL", "VARPTR", NULL
};

/* Built-in statement names */
static const char *builtin_stmts[] = {
	"BEEP", "BLOAD", "BSAVE", "CALL", "CHAIN", "CHDIR",
	"CIRCLE", "CLEAR", "CLOSE", "CLS", "COLOR", "COM",
	"COMMON", "CONST", "DATA", "DATE$", "DECLARE", "DEF",
	"DEFDBL", "DEFINT", "DEFLNG", "DEFSNG", "DEFSTR", "DIM",
	"DRAW", "END", "ENVIRON", "ERASE", "ERROR", "EXIT",
	"FIELD", "FILES", "FOR", "FUNCTION", "GET", "GOSUB",
	"GOTO", "IF", "INPUT", "KEY", "KILL", "LET", "LINE",
	"LOCATE", "LOCK", "LPRINT", "LSET", "MKDIR", "NAME",
	"NEXT", "ON", "OPEN", "OPTION", "OUT", "PAINT", "PALETTE",
	"PCOPY", "PLAY", "POKE", "PRESET", "PRINT", "PSET",
	"PUT", "RANDOMIZE", "READ", "REDIM", "REM", "RESET",
	"RESTORE", "RESUME", "RETURN", "RMDIR", "RSET", "RUN",
	"SCREEN", "SEEK", "SELECT", "SHARED", "SHELL", "SLEEP",
	"SOUND", "STATIC", "STOP", "STRIG", "SUB", "SWAP",
	"SYSTEM", "TIME$", "TIMER", "TROFF", "TRON", "TYPE",
	"UEVENT", "UNLOCK", "VIEW", "WAIT", "WEND", "WHILE",
	"WIDTH", "WINDOW", "WRITE", NULL
};

/*
 * Initialize built-in functions and line number table
 */
void
init_builtins(void)
{
	/* Built-ins are predefined, no initialization needed */
}

/*
 * Initialize line number table
 */
void
init_line_table(void)
{
	line_table_head = NULL;
	next_label_id = 1;
}

/*
 * Add line number and return its label ID
 */
int
add_line_number(int line_num)
{
	LINE_ENTRY *entry = malloc(sizeof(LINE_ENTRY));
	entry->line_num = line_num;
	entry->label_id = next_label_id++;
	entry->next = line_table_head;
	line_table_head = entry;
	return entry->label_id;
}

/*
 * Find label ID for a line number
 */
int
find_line_label(int line_num)
{
	LINE_ENTRY *entry;

	for (entry = line_table_head; entry != NULL; entry = entry->next) {
		if (entry->line_num == line_num)
			return entry->label_id;
	}

	return -1;  /* Not found */
}

/*
 * Check if name is a built-in function
 */
int
is_builtin_func(char *name)
{
	int i;

	for (i = 0; builtin_funcs[i] != NULL; i++) {
		if (strcasecmp(name, builtin_funcs[i]) == 0)
			return 1;
	}

	return 0;
}

/*
 * Check if name is a built-in statement
 */
int
is_builtin_stmt(char *name)
{
	int i;

	for (i = 0; builtin_stmts[i] != NULL; i++) {
		if (strcasecmp(name, builtin_stmts[i]) == 0)
			return 1;
	}

	return 0;
}

/*
 * Emit IR code (placeholder)
 */
void
emit_code(int op, ...)
{
	/* TODO: Implement IR generation */
}

/*
 * Emit label
 */
void
emit_label(int label)
{
	fprintf(outfile, "L%d:\n", label);
}

/*
 * Allocate new label
 */
int
new_label(void)
{
	static int label_counter = 1000;
	return label_counter++;
}

/*
 * Allocate new temporary
 */
int
new_temp(void)
{
	static int temp_counter = 1;
	return temp_counter++;
}
