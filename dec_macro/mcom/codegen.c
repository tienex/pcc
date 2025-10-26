/*
 * Copyright (c) 2025 PCC DEC MACRO Compiler
 *
 * Code generation - emit assembly instructions
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "pass1.h"

/* Current program section */
int current_psect = 0;

/* Location counter (current address) */
long location_counter = 0;

/* Assembly output file */
FILE *outfile = NULL;

/*
 * Initialize code generator
 */
void
codegen_init(void)
{
	location_counter = 0;
	current_psect = 0;
}

/*
 * Emit a label definition
 */
void
emit_label(const char *label)
{
	fprintf(outfile, "%s:\n", label);
}

/*
 * Emit an operand (forward declaration)
 */
static void emit_operand(OPERAND *op);

/*
 * Emit an instruction
 */
void
emit_instruction(INSTRUCTION *inst)
{
	int i;

	fprintf(outfile, "\t%s", inst->mnemonic);

	/* Emit operands */
	for (i = 0; i < inst->noperands; i++) {
		if (i > 0)
			fprintf(outfile, ",");
		else
			fprintf(outfile, "\t");

		emit_operand(&inst->operands[i]);
	}

	fprintf(outfile, "\n");
}

/*
 * Emit an operand
 */
static void
emit_operand(OPERAND *op)
{
	switch (op->type) {
	case OP_REGISTER:
		fprintf(outfile, "r%d", op->reg);
		break;

	case OP_IMMEDIATE:
		fprintf(outfile, "#%ld", op->value);
		break;

	case OP_DIRECT:
		if (op->symbol)
			fprintf(outfile, "%s", op->symbol);
		else
			fprintf(outfile, "%ld", op->value);
		break;

	case OP_INDIRECT:
		fprintf(outfile, "@");
		if (op->symbol)
			fprintf(outfile, "%s", op->symbol);
		else
			fprintf(outfile, "%ld", op->value);
		break;

	case OP_INDEXED:
		if (op->value != 0)
			fprintf(outfile, "%ld", op->value);
		fprintf(outfile, "(r%d)", op->reg);
		break;

	case OP_AUTODEC:
		fprintf(outfile, "-(r%d)", op->reg);
		break;

	case OP_AUTOINC:
		fprintf(outfile, "(r%d)+", op->reg);
		break;

	case OP_SYMBOL:
		fprintf(outfile, "%s", op->symbol);
		if (op->value != 0)
			fprintf(outfile, "+%ld", op->value);
		break;

	case OP_LITERAL:
		fprintf(outfile, "#%ld", op->value);
		break;

	default:
		error("unknown operand type %d", op->type);
		break;
	}
}

/*
 * Emit data (byte, word, long)
 */
void
emit_data(int size, long value)
{
	switch (size) {
	case 1:
		fprintf(outfile, "\t.byte\t0x%02lx\n", value & 0xff);
		location_counter += 1;
		break;
	case 2:
		fprintf(outfile, "\t.word\t0x%04lx\n", value & 0xffff);
		location_counter += 2;
		break;
	case 4:
		fprintf(outfile, "\t.long\t0x%08lx\n", value);
		location_counter += 4;
		break;
	default:
		error("invalid data size %d", size);
		break;
	}
}

/*
 * Emit string data
 */
void
emit_string(const char *str, int null_term)
{
	if (null_term)
		fprintf(outfile, "\t.asciz\t\"%s\"\n", str);
	else
		fprintf(outfile, "\t.ascii\t\"%s\"\n", str);

	location_counter += strlen(str);
	if (null_term)
		location_counter++;
}

/*
 * Emit a directive
 */
void
emit_directive(int directive, ...)
{
	va_list ap;
	const char *str;
	long value;

	va_start(ap, directive);

	switch (directive) {
	case DIR_TITLE:
		str = va_arg(ap, const char *);
		fprintf(outfile, "\t.title\t\"%s\"\n", str);
		break;

	case DIR_IDENT:
		str = va_arg(ap, const char *);
		fprintf(outfile, "\t.ident\t\"%s\"\n", str);
		break;

	case DIR_PSECT:
		str = va_arg(ap, const char *);
		fprintf(outfile, "\t.psect\t%s\n", str);
		break;

	case DIR_ENTRY:
		str = va_arg(ap, const char *);
		fprintf(outfile, "\t.entry\t%s\n", str);
		break;

	case DIR_END:
		fprintf(outfile, "\t.end\n");
		break;

	case DIR_GLOBL:
		str = va_arg(ap, const char *);
		fprintf(outfile, "\t.globl\t%s\n", str);
		break;

	case DIR_EXTERN:
		str = va_arg(ap, const char *);
		fprintf(outfile, "\t.extern\t%s\n", str);
		break;

	case DIR_ALIGN:
		value = va_arg(ap, long);
		fprintf(outfile, "\t.align\t%ld\n", value);
		break;

	case DIR_EVEN:
		fprintf(outfile, "\t.even\n");
		if (location_counter & 1)
			location_counter++;
		break;

	case DIR_PAGE:
		fprintf(outfile, "\t.page\n");
		break;

	default:
		error("unknown directive %d", directive);
		break;
	}

	va_end(ap);
}
