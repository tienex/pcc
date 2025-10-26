/*
 * Copyright (c) 2025 PCC DEC MACRO Compiler
 *
 * Code generation - using PCC IR (Intermediate Representation)
 * Properly integrated with PCC's interpass mechanism
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "pass1.h"
#include "y.tab.h"  /* For DIR_* token definitions */

/* Current program section */
int current_psect = 0;

/* Location counter (current address) */
long location_counter = 0;

/* Block nesting level (for PCC) */
int blevel = 0;

/* Label counter for PCC */
static int label_counter = 100;

/*
 * Initialize code generator
 */
void
codegen_init(void)
{
	location_counter = 0;
	current_psect = 0;
	blevel = 0;
	label_counter = 100;
}

/*
 * Get a new PCC label number
 */
int
get_label(void)
{
	return label_counter++;
}

/*
 * Emit a label definition using PCC IR
 */
void
emit_label_ir(const char *label)
{
	SYMTAB *sym;

	/* Look up or create symbol */
	sym = lookup(label);
	if (sym == NULL) {
		sym = install(label, SYM_LABEL);
		sym->label_num = get_label();
	}

	/* Define label using PCC interpass */
	send_passt(IP_DEFLAB, sym->label_num);
	define_symbol(sym, location_counter);
}

/*
 * Build assembly string from operand
 */
static void
format_operand(char *buf, size_t bufsize, OPERAND *op)
{
	switch (op->type) {
	case OP_REGISTER:
		snprintf(buf, bufsize, "r%d", op->reg);
		break;

	case OP_IMMEDIATE:
		snprintf(buf, bufsize, "#%ld", op->value);
		break;

	case OP_DIRECT:
		if (op->symbol)
			snprintf(buf, bufsize, "%s", op->symbol);
		else
			snprintf(buf, bufsize, "%ld", op->value);
		break;

	case OP_INDIRECT:
		if (op->symbol)
			snprintf(buf, bufsize, "@%s", op->symbol);
		else
			snprintf(buf, bufsize, "@%ld", op->value);
		break;

	case OP_INDEXED:
		if (op->value != 0)
			snprintf(buf, bufsize, "%ld(r%d)", op->value, op->reg);
		else
			snprintf(buf, bufsize, "(r%d)", op->reg);
		break;

	case OP_AUTODEC:
		snprintf(buf, bufsize, "-(r%d)", op->reg);
		break;

	case OP_AUTOINC:
		snprintf(buf, bufsize, "(r%d)+", op->reg);
		break;

	case OP_SYMBOL:
		if (op->value != 0)
			snprintf(buf, bufsize, "%s+%ld", op->symbol, op->value);
		else
			snprintf(buf, bufsize, "%s", op->symbol);
		break;

	case OP_LITERAL:
		snprintf(buf, bufsize, "#%ld", op->value);
		break;

	default:
		snprintf(buf, bufsize, "<unknown>");
		error("unknown operand type %d", op->type);
		break;
	}
}

/*
 * Emit an instruction using PCC IR (IP_ASM)
 */
void
emit_instruction_ir(INSTRUCTION *inst)
{
	char asm_line[1024];
	char operand_buf[256];
	int i;

	/* Build assembly string */
	snprintf(asm_line, sizeof(asm_line), "\t%s", inst->mnemonic);

	/* Add operands */
	for (i = 0; i < inst->noperands; i++) {
		if (i == 0)
			strcat(asm_line, "\t");
		else
			strcat(asm_line, ",");

		format_operand(operand_buf, sizeof(operand_buf), &inst->operands[i]);
		strcat(asm_line, operand_buf);
	}

	strcat(asm_line, "\n");

	/* Send to PCC backend as inline assembly */
	send_passt(IP_ASM, asm_line);

	/* Update location counter (rough estimate) */
	location_counter += 2;  /* Most instructions are ~2 bytes */
}

/*
 * Emit data using PCC IR
 */
void
emit_data_ir(int size, long value)
{
	char asm_line[256];

	switch (size) {
	case 1:
		snprintf(asm_line, sizeof(asm_line), "\t.byte\t0x%02lx\n",
		         value & 0xff);
		location_counter += 1;
		break;
	case 2:
		snprintf(asm_line, sizeof(asm_line), "\t.word\t0x%04lx\n",
		         value & 0xffff);
		location_counter += 2;
		break;
	case 4:
		snprintf(asm_line, sizeof(asm_line), "\t.long\t0x%08lx\n", value);
		location_counter += 4;
		break;
	default:
		error("invalid data size %d", size);
		return;
	}

	send_passt(IP_ASM, asm_line);
}

/*
 * Emit string data using PCC IR
 */
void
emit_string_ir(const char *str, int null_term)
{
	char asm_line[1024];

	if (null_term)
		snprintf(asm_line, sizeof(asm_line), "\t.asciz\t\"%s\"\n", str);
	else
		snprintf(asm_line, sizeof(asm_line), "\t.ascii\t\"%s\"\n", str);

	send_passt(IP_ASM, asm_line);

	location_counter += strlen(str);
	if (null_term)
		location_counter++;
}

/*
 * Emit a directive using PCC IR
 */
void
emit_directive_ir(int directive, ...)
{
	va_list ap;
	const char *str;
	long value;
	char asm_line[1024];

	va_start(ap, directive);

	switch (directive) {
	case DIR_TITLE:
		str = va_arg(ap, const char *);
		snprintf(asm_line, sizeof(asm_line), "\t.title\t\"%s\"\n", str);
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_IDENT:
		str = va_arg(ap, const char *);
		snprintf(asm_line, sizeof(asm_line), "\t.ident\t\"%s\"\n", str);
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_PSECT:
		str = va_arg(ap, const char *);
		snprintf(asm_line, sizeof(asm_line), "\t.psect\t%s\n", str);
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_ENTRY:
		str = va_arg(ap, const char *);
		snprintf(asm_line, sizeof(asm_line), "\t.entry\t%s\n", str);
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_END:
		snprintf(asm_line, sizeof(asm_line), "\t.end\n");
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_GLOBL:
		str = va_arg(ap, const char *);
		snprintf(asm_line, sizeof(asm_line), "\t.globl\t%s\n", str);
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_EXTERN:
		str = va_arg(ap, const char *);
		snprintf(asm_line, sizeof(asm_line), "\t.extern\t%s\n", str);
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_ALIGN:
		value = va_arg(ap, long);
		snprintf(asm_line, sizeof(asm_line), "\t.align\t%ld\n", value);
		send_passt(IP_ASM, asm_line);
		break;

	case DIR_EVEN:
		snprintf(asm_line, sizeof(asm_line), "\t.even\n");
		send_passt(IP_ASM, asm_line);
		if (location_counter & 1)
			location_counter++;
		break;

	case DIR_PAGE:
		snprintf(asm_line, sizeof(asm_line), "\t.page\n");
		send_passt(IP_ASM, asm_line);
		break;

	default:
		error("unknown directive %d", directive);
		break;
	}

	va_end(ap);
}

/*
 * PCC backend interface - begin compilation unit
 */
void
bjobcode(void)
{
	/* Mark beginning of compilation unit */
	/* This would normally set up function prologue, but for assembly
	 * we just need to initialize our state */
	codegen_init();
}

/*
 * PCC backend interface - end compilation unit
 */
void
ejobcode(int retlab)
{
	/* Mark end of compilation unit */
	/* For assembly, we just finalize */
	(void)retlab;  /* Unused for assembly */
}

/* ========== PCC IR Node Building Functions ========== */

/*
 * Build an integer constant node
 */
P1ND *
build_icon(long value)
{
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	if (p == NULL)
		fatal("out of memory");

	memset(p, 0, sizeof(P1ND));
	p->n_op = ICON;
	p->n_type = INT;
	setlval(p, value);

	return p;
}

/*
 * Build a register node
 */
P1ND *
build_reg(int reg)
{
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	if (p == NULL)
		fatal("out of memory");

	memset(p, 0, sizeof(P1ND));
	p->n_op = REG;
	p->n_type = INT;
	p->n_rval = reg;

	return p;
}

/*
 * Build an offset register node (OREG)
 */
P1ND *
build_oreg(int reg, long offset)
{
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	if (p == NULL)
		fatal("out of memory");

	memset(p, 0, sizeof(P1ND));
	p->n_op = OREG;
	p->n_type = INT;
	p->n_rval = reg;
	setlval(p, offset);

	return p;
}

/*
 * Build an assignment node
 */
P1ND *
build_assign(P1ND *left, P1ND *right)
{
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	if (p == NULL)
		fatal("out of memory");

	memset(p, 0, sizeof(P1ND));
	p->n_op = ASSIGN;
	p->n_type = left->n_type;
	p->n_left = left;
	p->n_right = right;

	return p;
}

/*
 * Build a binary operation node
 */
P1ND *
build_binop(int op, P1ND *left, P1ND *right)
{
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	if (p == NULL)
		fatal("out of memory");

	memset(p, 0, sizeof(P1ND));
	p->n_op = op;
	p->n_type = left->n_type;
	p->n_left = left;
	p->n_right = right;

	return p;
}

/*
 * Build a unary operation node
 */
P1ND *
build_unop(int op, P1ND *child)
{
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	if (p == NULL)
		fatal("out of memory");

	memset(p, 0, sizeof(P1ND));
	p->n_op = op;
	p->n_type = child->n_type;
	p->n_left = child;

	return p;
}
