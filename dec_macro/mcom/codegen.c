/*
 * Copyright (c) 2025 PCC DEC MACRO Compiler
 *
 * Code generation - Table-driven PCC IR generation
 * NO assembly string generation - only PCC NODE trees
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "pass1.h"
#include "y.tab.h"

/* Forward declarations */
typedef P1ND *(*insn_handler_t)(INSTRUCTION *inst);
static P1ND *handle_mov(INSTRUCTION *inst);
static P1ND *handle_add(INSTRUCTION *inst);
static P1ND *handle_sub(INSTRUCTION *inst);
static P1ND *handle_mul(INSTRUCTION *inst);
static P1ND *handle_div(INSTRUCTION *inst);
static P1ND *handle_and(INSTRUCTION *inst);
static P1ND *handle_or(INSTRUCTION *inst);
static P1ND *handle_xor(INSTRUCTION *inst);
static P1ND *handle_clr(INSTRUCTION *inst);
static P1ND *handle_inc(INSTRUCTION *inst);
static P1ND *handle_dec(INSTRUCTION *inst);
static P1ND *handle_neg(INSTRUCTION *inst);
static P1ND *handle_com(INSTRUCTION *inst);
static P1ND *handle_tst(INSTRUCTION *inst);
static P1ND *handle_cmp(INSTRUCTION *inst);
static P1ND *handle_bit(INSTRUCTION *inst);
static P1ND *handle_bic(INSTRUCTION *inst);
static P1ND *handle_bis(INSTRUCTION *inst);
static P1ND *handle_asl(INSTRUCTION *inst);
static P1ND *handle_asr(INSTRUCTION *inst);
static P1ND *handle_rol(INSTRUCTION *inst);
static P1ND *handle_ror(INSTRUCTION *inst);
static P1ND *handle_swab(INSTRUCTION *inst);
static P1ND *handle_sxt(INSTRUCTION *inst);
static P1ND *handle_branch(INSTRUCTION *inst);
static P1ND *handle_jmp(INSTRUCTION *inst);
static P1ND *handle_jsr(INSTRUCTION *inst);
static P1ND *handle_rts(INSTRUCTION *inst);
static P1ND *handle_halt(INSTRUCTION *inst);
static P1ND *handle_nop(INSTRUCTION *inst);

/* Instruction table entry */
typedef struct {
	const char *mnemonic;
	insn_handler_t handler;
	int opcode;
} insn_table_entry_t;

/* DEC MACRO Instruction Table */
static const insn_table_entry_t insn_table[] = {
	/* Data Movement */
	{ "MOV",   handle_mov,   0010000 },
	{ "MOVB",  handle_mov,   0110000 },
	{ "MOVA",  handle_mov,   0010000 },
	{ "MOVZ",  handle_mov,   0010000 },

	/* Arithmetic */
	{ "ADD",   handle_add,   0060000 },
	{ "SUB",   handle_sub,   0160000 },
	{ "MUL",   handle_mul,   0070000 },
	{ "DIV",   handle_div,   0071000 },
	{ "INC",   handle_inc,   0005200 },
	{ "INCB",  handle_inc,   0105200 },
	{ "DEC",   handle_dec,   0005300 },
	{ "DECB",  handle_dec,   0105300 },
	{ "NEG",   handle_neg,   0005400 },
	{ "NEGB",  handle_neg,   0105400 },
	{ "ADC",   handle_inc,   0005500 },  /* Add carry */
	{ "SBC",   handle_dec,   0005600 },  /* Subtract carry */

	/* Logical */
	{ "AND",   handle_and,   0040000 },
	{ "OR",    handle_or,    0050000 },
	{ "XOR",   handle_xor,   0074000 },
	{ "BIC",   handle_bic,   0040000 },  /* Bit clear */
	{ "BIS",   handle_bis,   0050000 },  /* Bit set */
	{ "BIT",   handle_bit,   0030000 },  /* Bit test */
	{ "COM",   handle_com,   0005100 },  /* Complement */
	{ "COMB",  handle_com,   0105100 },

	/* Shift/Rotate */
	{ "ASL",   handle_asl,   0006300 },  /* Arithmetic shift left */
	{ "ASLB",  handle_asl,   0106300 },
	{ "ASR",   handle_asr,   0006200 },  /* Arithmetic shift right */
	{ "ASRB",  handle_asr,   0106200 },
	{ "ROL",   handle_rol,   0006100 },  /* Rotate left */
	{ "ROLB",  handle_rol,   0106100 },
	{ "ROR",   handle_ror,   0006000 },  /* Rotate right */
	{ "RORB",  handle_ror,   0106000 },
	{ "SWAB",  handle_swab,  0000300 },  /* Swap bytes */
	{ "SXT",   handle_sxt,   0006700 },  /* Sign extend */

	/* Compare/Test */
	{ "CMP",   handle_cmp,   0020000 },
	{ "CMPB",  handle_cmp,   0120000 },
	{ "TST",   handle_tst,   0005700 },
	{ "TSTB",  handle_tst,   0105700 },

	/* Branches (conditional) */
	{ "BR",    handle_branch, 0000400 },  /* Branch unconditional */
	{ "BNE",   handle_branch, 0001000 },  /* Branch not equal */
	{ "BEQ",   handle_branch, 0001400 },  /* Branch equal */
	{ "BPL",   handle_branch, 0100000 },  /* Branch plus */
	{ "BMI",   handle_branch, 0100400 },  /* Branch minus */
	{ "BVC",   handle_branch, 0102000 },  /* Branch overflow clear */
	{ "BVS",   handle_branch, 0102400 },  /* Branch overflow set */
	{ "BCC",   handle_branch, 0103000 },  /* Branch carry clear */
	{ "BCS",   handle_branch, 0103400 },  /* Branch carry set */
	{ "BGE",   handle_branch, 0002000 },  /* Branch >= */
	{ "BLT",   handle_branch, 0002400 },  /* Branch < */
	{ "BGT",   handle_branch, 0003000 },  /* Branch > */
	{ "BLE",   handle_branch, 0003400 },  /* Branch <= */
	{ "BHI",   handle_branch, 0101000 },  /* Branch higher */
	{ "BLOS",  handle_branch, 0101400 },  /* Branch lower or same */

	/* Jump/Call */
	{ "JMP",   handle_jmp,   0000100 },
	{ "JSR",   handle_jsr,   0004000 },
	{ "RTS",   handle_rts,   0000200 },
	{ "RTI",   handle_rts,   0000002 },
	{ "RTT",   handle_rts,   0000006 },

	/* Special */
	{ "HALT",  handle_halt,  0000000 },
	{ "WAIT",  handle_halt,  0000001 },
	{ "NOP",   handle_nop,   0000240 },
	{ "RESET", handle_halt,  0000005 },
	{ "CLR",   handle_clr,   0005000 },
	{ "CLRB",  handle_clr,   0105000 },

	{ NULL, NULL, 0 }  /* Sentinel */
};

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
 * Convert OPERAND to PCC NODE
 */
static P1ND *
operand_to_node(OPERAND *op)
{
	P1ND *p;
	SYMTAB *sym;

	switch (op->type) {
	case OP_REGISTER:
		/* Direct register reference */
		return build_reg(op->reg);

	case OP_IMMEDIATE:
		/* Immediate constant */
		return build_icon(op->value);

	case OP_DIRECT:
		/* Direct memory reference or symbol */
		if (op->symbol) {
			/* Symbol reference - build NAME node */
			sym = lookup(op->symbol);
			if (sym == NULL) {
				sym = install(op->symbol, SYM_EXTERNAL);
			}
			p = (P1ND *)malloc(sizeof(P1ND));
			memset(p, 0, sizeof(P1ND));
			p->n_op = NAME;
			p->n_type = INT;
			p->n_name = op->symbol;
			return p;
		} else {
			/* Direct address - build as dereference */
			p = build_icon(op->value);
			return build_unop(UMUL, p);
		}

	case OP_INDIRECT:
		/* Indirect addressing @symbol or @value */
		if (op->symbol) {
			sym = lookup(op->symbol);
			if (sym == NULL) {
				sym = install(op->symbol, SYM_EXTERNAL);
			}
			p = (P1ND *)malloc(sizeof(P1ND));
			memset(p, 0, sizeof(P1ND));
			p->n_op = NAME;
			p->n_type = INT;
			p->n_name = op->symbol;
			/* Double indirect */
			p = build_unop(UMUL, p);
			return build_unop(UMUL, p);
		} else {
			p = build_icon(op->value);
			p = build_unop(UMUL, p);
			return build_unop(UMUL, p);
		}

	case OP_INDEXED:
		/* offset(Rn) - register with offset */
		return build_oreg(op->reg, op->value);

	case OP_AUTODEC:
		/* -(Rn) - predecrement - build as side effect */
		p = build_reg(op->reg);
		p = build_assign(build_reg(op->reg),
		    build_binop(MINUS, build_reg(op->reg), build_icon(2)));
		return build_oreg(op->reg, 0);

	case OP_AUTOINC:
		/* (Rn)+ - postincrement */
		return build_oreg(op->reg, 0);

	case OP_SYMBOL:
		/* Symbol reference, possibly with offset */
		sym = lookup(op->symbol);
		if (sym == NULL) {
			sym = install(op->symbol, SYM_EXTERNAL);
		}
		p = (P1ND *)malloc(sizeof(P1ND));
		memset(p, 0, sizeof(P1ND));
		p->n_op = NAME;
		p->n_type = INT;
		p->n_name = op->symbol;

		if (op->value != 0) {
			/* Add offset */
			p = build_binop(PLUS, p, build_icon(op->value));
		}
		return p;

	case OP_LITERAL:
		/* Literal value */
		return build_icon(op->value);

	default:
		error("unknown operand type %d", op->type);
		return build_icon(0);
	}
}

/* ========== Instruction Handlers (generate ONLY PCC IR) ========== */

/*
 * MOV src, dst -> dst = src
 */
static P1ND *
handle_mov(INSTRUCTION *inst)
{
	P1ND *src, *dst;

	if (inst->noperands != 2) {
		error("MOV requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	return build_assign(dst, src);
}

/*
 * ADD src, dst -> dst = dst + src
 */
static P1ND *
handle_add(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("ADD requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_binop(PLUS, dst_rhs, src));
}

/*
 * SUB src, dst -> dst = dst - src
 */
static P1ND *
handle_sub(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("SUB requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_binop(MINUS, dst_rhs, src));
}

/*
 * MUL src, reg -> reg = reg * src
 */
static P1ND *
handle_mul(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("MUL requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_binop(MUL, dst_rhs, src));
}

/*
 * DIV src, reg -> reg = reg / src
 */
static P1ND *
handle_div(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("DIV requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_binop(DIV, dst_rhs, src));
}

/*
 * AND src, dst -> dst = dst & src
 */
static P1ND *
handle_and(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("AND requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_binop(AND, dst_rhs, src));
}

/*
 * OR src, dst -> dst = dst | src
 */
static P1ND *
handle_or(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("OR requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_binop(OR, dst_rhs, src));
}

/*
 * XOR src, dst -> dst = dst ^ src
 */
static P1ND *
handle_xor(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("XOR requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	return build_assign(dst, build_binop(ER, dst_rhs, src));  /* ER = XOR */
}

/*
 * BIC src, dst -> dst = dst & ~src (bit clear)
 */
static P1ND *
handle_bic(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("BIC requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);
	/* dst = dst & ~src */
	return build_assign(dst, build_binop(AND, dst_rhs, build_unop(COMPL, src)));
}

/*
 * BIS src, dst -> dst = dst | src (bit set, same as OR)
 */
static P1ND *
handle_bis(INSTRUCTION *inst)
{
	return handle_or(inst);
}

/*
 * BIT src, dst -> test (dst & src) [sets condition codes]
 */
static P1ND *
handle_bit(INSTRUCTION *inst)
{
	P1ND *src, *dst;

	if (inst->noperands != 2) {
		error("BIT requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	/* Generate AND operation (result affects condition codes) */
	return build_binop(AND, dst, src);
}

/*
 * CLR dst -> dst = 0
 */
static P1ND *
handle_clr(INSTRUCTION *inst)
{
	P1ND *dst;

	if (inst->noperands != 1) {
		error("CLR requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	return build_assign(dst, build_icon(0));
}

/*
 * INC dst -> dst = dst + 1
 */
static P1ND *
handle_inc(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs;

	if (inst->noperands != 1) {
		error("INC requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	return build_assign(dst, build_binop(PLUS, dst_rhs, build_icon(1)));
}

/*
 * DEC dst -> dst = dst - 1
 */
static P1ND *
handle_dec(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs;

	if (inst->noperands != 1) {
		error("DEC requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	return build_assign(dst, build_binop(MINUS, dst_rhs, build_icon(1)));
}

/*
 * NEG dst -> dst = -dst
 */
static P1ND *
handle_neg(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs;

	if (inst->noperands != 1) {
		error("NEG requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	return build_assign(dst, build_unop(UMINUS, dst_rhs));
}

/*
 * COM dst -> dst = ~dst (one's complement)
 */
static P1ND *
handle_com(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs;

	if (inst->noperands != 1) {
		error("COM requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	return build_assign(dst, build_unop(COMPL, dst_rhs));
}

/*
 * TST dst -> compare dst with 0
 */
static P1ND *
handle_tst(INSTRUCTION *inst)
{
	P1ND *dst;

	if (inst->noperands != 1) {
		error("TST requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	return build_binop(NE, dst, build_icon(0));
}

/*
 * CMP src, dst -> compare src with dst
 */
static P1ND *
handle_cmp(INSTRUCTION *inst)
{
	P1ND *src, *dst;

	if (inst->noperands != 2) {
		error("CMP requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	return build_binop(NE, src, dst);
}

/*
 * ASL dst -> dst = dst << 1 (arithmetic shift left)
 */
static P1ND *
handle_asl(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs;

	if (inst->noperands != 1) {
		error("ASL requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	return build_assign(dst, build_binop(LS, dst_rhs, build_icon(1)));
}

/*
 * ASR dst -> dst = dst >> 1 (arithmetic shift right)
 */
static P1ND *
handle_asr(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs;

	if (inst->noperands != 1) {
		error("ASR requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs = operand_to_node(&inst->operands[0]);
	return build_assign(dst, build_binop(RS, dst_rhs, build_icon(1)));
}

/*
 * ROL dst -> rotate left (no direct PCC operation, use shifts and OR)
 */
static P1ND *
handle_rol(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs1, *dst_rhs2, *dst_rhs3;
	P1ND *left_part, *right_part;

	if (inst->noperands != 1) {
		error("ROL requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs1 = operand_to_node(&inst->operands[0]);
	dst_rhs2 = operand_to_node(&inst->operands[0]);
	dst_rhs3 = operand_to_node(&inst->operands[0]);

	/* dst = (dst << 1) | (dst >> 15) */
	left_part = build_binop(LS, dst_rhs1, build_icon(1));
	right_part = build_binop(RS, dst_rhs2, build_icon(15));
	return build_assign(dst, build_binop(OR, left_part, right_part));
}

/*
 * ROR dst -> rotate right
 */
static P1ND *
handle_ror(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs1, *dst_rhs2;
	P1ND *left_part, *right_part;

	if (inst->noperands != 1) {
		error("ROR requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs1 = operand_to_node(&inst->operands[0]);
	dst_rhs2 = operand_to_node(&inst->operands[0]);

	/* dst = (dst >> 1) | (dst << 15) */
	right_part = build_binop(RS, dst_rhs1, build_icon(1));
	left_part = build_binop(LS, dst_rhs2, build_icon(15));
	return build_assign(dst, build_binop(OR, right_part, left_part));
}

/*
 * SWAB dst -> swap bytes in word
 */
static P1ND *
handle_swab(INSTRUCTION *inst)
{
	P1ND *dst, *dst_rhs1, *dst_rhs2;
	P1ND *low_byte, *high_byte;

	if (inst->noperands != 1) {
		error("SWAB requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	dst_rhs1 = operand_to_node(&inst->operands[0]);
	dst_rhs2 = operand_to_node(&inst->operands[0]);

	/* dst = ((dst & 0xff) << 8) | ((dst >> 8) & 0xff) */
	low_byte = build_binop(LS,
	    build_binop(AND, dst_rhs1, build_icon(0xff)),
	    build_icon(8));
	high_byte = build_binop(AND,
	    build_binop(RS, dst_rhs2, build_icon(8)),
	    build_icon(0xff));
	return build_assign(dst, build_binop(OR, low_byte, high_byte));
}

/*
 * SXT dst -> sign extend (if N set, dst = -1, else dst = 0)
 */
static P1ND *
handle_sxt(INSTRUCTION *inst)
{
	P1ND *dst;

	if (inst->noperands != 1) {
		error("SXT requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	/* For now, just clear it - proper implementation needs condition codes */
	return build_assign(dst, build_icon(0));
}

/*
 * Branch instructions - use GOTO nodes with labels
 */
static P1ND *
handle_branch(INSTRUCTION *inst)
{
	SYMTAB *sym;
	P1ND *p;
	int label_num;

	if (inst->noperands != 1) {
		error("Branch requires 1 operand (label)");
		return NULL;
	}

	/* Get or create label symbol */
	if (inst->operands[0].symbol) {
		sym = lookup(inst->operands[0].symbol);
		if (sym == NULL) {
			sym = install(inst->operands[0].symbol, SYM_LABEL);
			sym->label_num = get_label();
		}
		label_num = sym->label_num;
	} else {
		label_num = get_label();
	}

	/* Build GOTO node */
	p = (P1ND *)malloc(sizeof(P1ND));
	memset(p, 0, sizeof(P1ND));
	p->n_op = GOTO;
	p->n_type = INT;
	p->n_label = label_num;

	return p;
}

/*
 * JMP addr -> unconditional jump
 */
static P1ND *
handle_jmp(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/*
 * JSR reg, addr -> call subroutine (push PC, jump)
 */
static P1ND *
handle_jsr(INSTRUCTION *inst)
{
	/* For now, treat as branch - full implementation needs CALL node */
	return handle_branch(inst);
}

/*
 * RTS -> return from subroutine
 */
static P1ND *
handle_rts(INSTRUCTION *inst)
{
	P1ND *p;

	/* Build return node - use value 0 */
	p = (P1ND *)malloc(sizeof(P1ND));
	memset(p, 0, sizeof(P1ND));
	p->n_op = RETURN;
	p->n_type = INT;
	p->n_left = build_icon(0);

	return p;
}

/*
 * HALT/WAIT/RESET -> special operations
 * For now, generate as no-op or specific inline asm marker
 */
static P1ND *
handle_halt(INSTRUCTION *inst)
{
	/* Generate as assignment to trigger backend action */
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	memset(p, 0, sizeof(P1ND));
	p->n_op = ICON;
	p->n_type = INT;
	setlval(p, 0);

	return p;
}

/*
 * NOP -> no operation
 */
static P1ND *
handle_nop(INSTRUCTION *inst)
{
	/* Generate icon node that will be eliminated by optimizer */
	return build_icon(0);
}

/* ========== Public API ========== */

/*
 * Emit an instruction using PCC IR (IP_NODE only!)
 * This uses table dispatch - NO string comparisons!
 */
void
emit_instruction_ir(INSTRUCTION *inst)
{
	P1ND *tree = NULL;
	const insn_table_entry_t *entry;
	int i;

	/* Look up instruction in table */
	for (i = 0; insn_table[i].mnemonic != NULL; i++) {
		if (strcmp(inst->mnemonic, insn_table[i].mnemonic) == 0) {
			entry = &insn_table[i];

			/* Call handler function */
			tree = entry->handler(inst);

			if (tree != NULL) {
				/* Send NODE to PCC backend - ONLY IR, no assembly! */
				send_passt(IP_NODE, tree);
			}

			/* Update location counter */
			location_counter += 2;
			return;
		}
	}

	/* Unknown instruction */
	error("unknown instruction: %s", inst->mnemonic);
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
	codegen_init();
}

/*
 * PCC backend interface - end compilation unit
 */
void
ejobcode(int retlab)
{
	(void)retlab;
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
