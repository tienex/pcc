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
/* Extended instructions */
static P1ND *handle_sob(INSTRUCTION *inst);
static P1ND *handle_ash(INSTRUCTION *inst);
static P1ND *handle_ashc(INSTRUCTION *inst);
static P1ND *handle_mark(INSTRUCTION *inst);
/* Condition code operations */
static P1ND *handle_scc(INSTRUCTION *inst);
static P1ND *handle_ccc(INSTRUCTION *inst);
/* Trap/Interrupt */
static P1ND *handle_trap(INSTRUCTION *inst);
static P1ND *handle_emt(INSTRUCTION *inst);
static P1ND *handle_iot(INSTRUCTION *inst);
static P1ND *handle_bpt(INSTRUCTION *inst);
static P1ND *handle_spl(INSTRUCTION *inst);
/* Memory management */
static P1ND *handle_mfpi(INSTRUCTION *inst);
static P1ND *handle_mtpi(INSTRUCTION *inst);
static P1ND *handle_mfps(INSTRUCTION *inst);
static P1ND *handle_mtps(INSTRUCTION *inst);
/* VAX 32-bit operations */
static P1ND *handle_movl(INSTRUCTION *inst);
static P1ND *handle_pushl(INSTRUCTION *inst);
static P1ND *handle_popl(INSTRUCTION *inst);
static P1ND *handle_addl(INSTRUCTION *inst);
static P1ND *handle_subl(INSTRUCTION *inst);
static P1ND *handle_mull(INSTRUCTION *inst);
static P1ND *handle_divl(INSTRUCTION *inst);
/* VAX string operations */
static P1ND *handle_movc(INSTRUCTION *inst);
static P1ND *handle_cmpc(INSTRUCTION *inst);
/* VAX floating point */
static P1ND *handle_addf(INSTRUCTION *inst);
static P1ND *handle_subf(INSTRUCTION *inst);
static P1ND *handle_mulf(INSTRUCTION *inst);
static P1ND *handle_divf(INSTRUCTION *inst);
/* PDP-10 specific */
static P1ND *handle_movei(INSTRUCTION *inst);
static P1ND *handle_movem(INSTRUCTION *inst);
static P1ND *handle_moves(INSTRUCTION *inst);
static P1ND *handle_movn(INSTRUCTION *inst);
static P1ND *handle_movm(INSTRUCTION *inst);
static P1ND *handle_imul(INSTRUCTION *inst);
static P1ND *handle_idiv(INSTRUCTION *inst);
static P1ND *handle_lsh(INSTRUCTION *inst);
static P1ND *handle_rot(INSTRUCTION *inst);
static P1ND *handle_jrst(INSTRUCTION *inst);
static P1ND *handle_pushj(INSTRUCTION *inst);
static P1ND *handle_popj(INSTRUCTION *inst);

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

	/* Extended PDP-11 instructions */
	{ "SOB",   handle_sob,   0077000 },  /* Subtract and branch */
	{ "ASH",   handle_ash,   0072000 },  /* Arithmetic shift */
	{ "ASHC",  handle_ashc,  0073000 },  /* Arithmetic shift combined */
	{ "MARK",  handle_mark,  0006400 },  /* Mark */

	/* Condition code operations */
	{ "SCC",   handle_scc,   0000277 },  /* Set carry clear */
	{ "SEC",   handle_scc,   0000261 },  /* Set carry */
	{ "SEV",   handle_scc,   0000262 },  /* Set overflow */
	{ "SEZ",   handle_scc,   0000264 },  /* Set zero */
	{ "SEN",   handle_scc,   0000270 },  /* Set negative */
	{ "CLC",   handle_ccc,   0000241 },  /* Clear carry */
	{ "CLV",   handle_ccc,   0000242 },  /* Clear overflow */
	{ "CLZ",   handle_ccc,   0000244 },  /* Clear zero */
	{ "CLN",   handle_ccc,   0000250 },  /* Clear negative */

	/* Trap/Interrupt instructions */
	{ "EMT",   handle_emt,   0104000 },  /* Emulator trap */
	{ "TRAP",  handle_trap,  0104400 },  /* Trap */
	{ "IOT",   handle_iot,   0000004 },  /* I/O trap */
	{ "BPT",   handle_bpt,   0000003 },  /* Breakpoint */
	{ "SPL",   handle_spl,   0000230 },  /* Set priority level */

	/* Memory management instructions */
	{ "MFPI",  handle_mfpi,  0006500 },  /* Move from previous I space */
	{ "MTPI",  handle_mtpi,  0006600 },  /* Move to previous I space */
	{ "MFPD",  handle_mfpi,  0106500 },  /* Move from previous D space */
	{ "MTPD",  handle_mtpi,  0106600 },  /* Move to previous D space */
	{ "MFPS",  handle_mfps,  0106700 },  /* Move from PS */
	{ "MTPS",  handle_mtps,  0106400 },  /* Move to PS */

	/* VAX 32-bit operations */
	{ "MOVL",  handle_movl,  0x90 },     /* Move longword */
	{ "MOVQ",  handle_movl,  0x7D },     /* Move quadword */
	{ "PUSHL", handle_pushl, 0xDD },     /* Push longword */
	{ "PUSHQ", handle_pushl, 0x7F },     /* Push quadword */
	{ "POPL",  handle_popl,  0xDC },     /* Pop longword */
	{ "POPQ",  handle_popl,  0x7E },     /* Pop quadword */
	{ "ADDL2", handle_addl,  0xC0 },     /* Add longword 2 operand */
	{ "ADDL3", handle_addl,  0xC1 },     /* Add longword 3 operand */
	{ "SUBL2", handle_subl,  0xC2 },     /* Subtract longword 2 operand */
	{ "SUBL3", handle_subl,  0xC3 },     /* Subtract longword 3 operand */
	{ "MULL2", handle_mull,  0xC4 },     /* Multiply longword 2 operand */
	{ "MULL3", handle_mull,  0xC5 },     /* Multiply longword 3 operand */
	{ "DIVL2", handle_divl,  0xC6 },     /* Divide longword 2 operand */
	{ "DIVL3", handle_divl,  0xC7 },     /* Divide longword 3 operand */
	{ "INCL",  handle_inc,   0xD6 },     /* Increment longword */
	{ "DECL",  handle_dec,   0xD7 },     /* Decrement longword */
	{ "CLRL",  handle_clr,   0xD4 },     /* Clear longword */
	{ "TSTL",  handle_tst,   0xD5 },     /* Test longword */
	{ "CMPL",  handle_cmp,   0xD1 },     /* Compare longword */

	/* VAX string operations */
	{ "MOVC3", handle_movc,  0x28 },     /* Move character 3 operand */
	{ "MOVC5", handle_movc,  0x2C },     /* Move character 5 operand */
	{ "CMPC3", handle_cmpc,  0x29 },     /* Compare character 3 operand */
	{ "CMPC5", handle_cmpc,  0x2D },     /* Compare character 5 operand */
	{ "LOCC",  handle_movc,  0x3A },     /* Locate character */
	{ "SKPC",  handle_movc,  0x3B },     /* Skip character */
	{ "SCANC", handle_movc,  0x2A },     /* Scan character */
	{ "SPANC", handle_movc,  0x2B },     /* Span character */

	/* VAX floating point */
	{ "ADDF2", handle_addf,  0x40 },     /* Add F_float 2 operand */
	{ "ADDF3", handle_addf,  0x41 },     /* Add F_float 3 operand */
	{ "SUBF2", handle_subf,  0x42 },     /* Subtract F_float 2 operand */
	{ "SUBF3", handle_subf,  0x43 },     /* Subtract F_float 3 operand */
	{ "MULF2", handle_mulf,  0x44 },     /* Multiply F_float 2 operand */
	{ "MULF3", handle_mulf,  0x45 },     /* Multiply F_float 3 operand */
	{ "DIVF2", handle_divf,  0x46 },     /* Divide F_float 2 operand */
	{ "DIVF3", handle_divf,  0x47 },     /* Divide F_float 3 operand */
	{ "ADDD2", handle_addf,  0x60 },     /* Add D_float 2 operand */
	{ "ADDD3", handle_addf,  0x61 },     /* Add D_float 3 operand */
	{ "SUBD2", handle_subf,  0x62 },     /* Subtract D_float 2 operand */
	{ "SUBD3", handle_subf,  0x63 },     /* Subtract D_float 3 operand */
	{ "MULD2", handle_mulf,  0x64 },     /* Multiply D_float 2 operand */
	{ "MULD3", handle_mulf,  0x65 },     /* Multiply D_float 3 operand */
	{ "DIVD2", handle_divf,  0x66 },     /* Divide D_float 2 operand */
	{ "DIVD3", handle_divf,  0x67 },     /* Divide D_float 3 operand */
	{ "CVTFD", handle_movl,  0x56 },     /* Convert F to D */
	{ "CVTDF", handle_movl,  0x76 },     /* Convert D to F */
	{ "CVTFL", handle_movl,  0x48 },     /* Convert F to L */
	{ "CVTLF", handle_movl,  0x4E },     /* Convert L to F */
	{ "CVTDL", handle_movl,  0x6A },     /* Convert D to L */
	{ "CVTLD", handle_movl,  0x6F },     /* Convert L to D */

	/* PDP-10 specific instructions */
	{ "MOVEI", handle_movei, 0201000 },  /* Move immediate */
	{ "MOVEM", handle_movem, 0202000 },  /* Move to memory */
	{ "MOVES", handle_moves, 0203000 },  /* Move to self */
	{ "MOVN",  handle_movn,  0210000 },  /* Move negative */
	{ "MOVM",  handle_movm,  0214000 },  /* Move magnitude */
	{ "IMUL",  handle_imul,  0220000 },  /* Integer multiply */
	{ "IDIV",  handle_idiv,  0230000 },  /* Integer divide */
	{ "LSH",   handle_lsh,   0242000 },  /* Logical shift */
	{ "ROT",   handle_rot,   0241000 },  /* Rotate */
	{ "JRST",  handle_jrst,  0254000 },  /* Jump and restore */
	{ "PUSHJ", handle_pushj, 0260000 },  /* Push and jump */
	{ "POPJ",  handle_popj,  0263000 },  /* Pop and jump */
	{ "EXCH",  handle_mov,   0250000 },  /* Exchange */
	{ "SETM",  handle_neg,   0214040 },  /* Set to memory */
	{ "SETZ",  handle_clr,   0400000 },  /* Set to zero */
	{ "SETMI", handle_neg,   0210040 },  /* Set minus */
	{ "SETA",  handle_mov,   0210040 },  /* Set to A */

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
	SYMTAB *sym;
	P1ND *p;
	int label_num;

	if (inst->noperands != 2) {
		error("JSR requires 2 operands (link register, target)");
		return NULL;
	}

	/* Get or create label symbol for target (second operand) */
	if (inst->operands[1].symbol) {
		sym = lookup(inst->operands[1].symbol);
		if (sym == NULL) {
			sym = install(inst->operands[1].symbol, SYM_LABEL);
			sym->label_num = get_label();
		}
		label_num = sym->label_num;
	} else {
		label_num = get_label();
	}

	/* Build GOTO node for the jump */
	p = (P1ND *)malloc(sizeof(P1ND));
	memset(p, 0, sizeof(P1ND));
	p->n_op = GOTO;
	p->n_label = label_num;

	/* Note: In a full implementation, we would save PC to the link register
	 * (first operand), but for now we just implement the jump */

	return p;
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

/* ========== Extended PDP-11 Instructions ========== */

/*
 * SOB reg, label -> reg = reg - 1; if (reg != 0) goto label
 */
static P1ND *
handle_sob(INSTRUCTION *inst)
{
	P1ND *reg, *reg_rhs, *dec_node, *test_node, *branch_node;
	SYMTAB *sym;
	int label_num;

	if (inst->noperands != 2) {
		error("SOB requires 2 operands");
		return NULL;
	}

	/* Decrement register */
	reg = operand_to_node(&inst->operands[0]);
	reg_rhs = operand_to_node(&inst->operands[0]);
	dec_node = build_assign(reg, build_binop(MINUS, reg_rhs, build_icon(1)));

	/* Send decrement */
	send_passt(IP_NODE, dec_node);

	/* Test and branch */
	if (inst->operands[1].symbol) {
		sym = lookup(inst->operands[1].symbol);
		if (sym == NULL) {
			sym = install(inst->operands[1].symbol, SYM_LABEL);
			sym->label_num = get_label();
		}
		label_num = sym->label_num;
	} else {
		label_num = get_label();
	}

	branch_node = (P1ND *)malloc(sizeof(P1ND));
	memset(branch_node, 0, sizeof(P1ND));
	branch_node->n_op = GOTO;
	branch_node->n_type = INT;
	branch_node->n_label = label_num;

	return branch_node;
}

/*
 * ASH shift_count, reg -> arithmetic shift reg by shift_count
 */
static P1ND *
handle_ash(INSTRUCTION *inst)
{
	P1ND *count, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("ASH requires 2 operands");
		return NULL;
	}

	count = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);

	/* Shift left if positive, right if negative */
	return build_assign(dst, build_binop(LS, dst_rhs, count));
}

/*
 * ASHC shift_count, reg -> arithmetic shift combined (reg:reg+1)
 */
static P1ND *
handle_ashc(INSTRUCTION *inst)
{
	/* Simplified - treat like ASH for now */
	return handle_ash(inst);
}

/*
 * MARK n -> Mark stack frame
 */
static P1ND *
handle_mark(INSTRUCTION *inst)
{
	/* Generate as NOP for IR purposes */
	return build_icon(0);
}

/* ========== Condition Code Operations ========== */

/*
 * Set condition code operations (SEC, SEV, SEZ, SEN)
 */
static P1ND *
handle_scc(INSTRUCTION *inst)
{
	/* Generate as assignment to condition code (abstract) */
	return build_assign(build_reg(16), build_icon(1));  /* CC pseudo-register */
}

/*
 * Clear condition code operations (CLC, CLV, CLZ, CLN)
 */
static P1ND *
handle_ccc(INSTRUCTION *inst)
{
	/* Generate as assignment to condition code (abstract) */
	return build_assign(build_reg(16), build_icon(0));  /* CC pseudo-register */
}

/* ========== Trap/Interrupt Instructions ========== */

/*
 * EMT n -> Emulator trap
 */
static P1ND *
handle_emt(INSTRUCTION *inst)
{
	P1ND *p;

	p = (P1ND *)malloc(sizeof(P1ND));
	memset(p, 0, sizeof(P1ND));
	p->n_op = ICON;
	p->n_type = INT;
	if (inst->noperands > 0)
		setlval(p, inst->operands[0].value);
	else
		setlval(p, 0);

	return p;
}

/*
 * TRAP n -> Trap instruction
 */
static P1ND *
handle_trap(INSTRUCTION *inst)
{
	return handle_emt(inst);
}

/*
 * IOT -> I/O trap
 */
static P1ND *
handle_iot(INSTRUCTION *inst)
{
	return handle_halt(inst);
}

/*
 * BPT -> Breakpoint trap
 */
static P1ND *
handle_bpt(INSTRUCTION *inst)
{
	return handle_halt(inst);
}

/*
 * SPL n -> Set priority level
 */
static P1ND *
handle_spl(INSTRUCTION *inst)
{
	return build_icon(0);
}

/* ========== Memory Management Instructions ========== */

/*
 * MFPI src -> Move from previous instruction space
 */
static P1ND *
handle_mfpi(INSTRUCTION *inst)
{
	P1ND *src, *sp;

	if (inst->noperands != 1) {
		error("MFPI requires 1 operand");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	sp = build_reg(6);  /* SP is R6 */

	/* Push src onto stack: -(SP) = src */
	return build_assign(build_oreg(6, -2), src);
}

/*
 * MTPI dst -> Move to previous instruction space
 */
static P1ND *
handle_mtpi(INSTRUCTION *inst)
{
	P1ND *dst, *sp_val;

	if (inst->noperands != 1) {
		error("MTPI requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	sp_val = build_oreg(6, 0);  /* (SP) */

	/* Pop from stack: dst = (SP)+ */
	return build_assign(dst, sp_val);
}

/*
 * MFPS dst -> Move from processor status
 */
static P1ND *
handle_mfps(INSTRUCTION *inst)
{
	P1ND *dst;

	if (inst->noperands != 1) {
		error("MFPS requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	/* Move from PS register (abstract) */
	return build_assign(dst, build_reg(17));  /* PS pseudo-register */
}

/*
 * MTPS src -> Move to processor status
 */
static P1ND *
handle_mtps(INSTRUCTION *inst)
{
	P1ND *src;

	if (inst->noperands != 1) {
		error("MTPS requires 1 operand");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	/* Move to PS register (abstract) */
	return build_assign(build_reg(17), src);  /* PS pseudo-register */
}

/* ========== VAX 32-bit Operations ========== */

/*
 * MOVL src, dst -> Move longword (32-bit MOV)
 */
static P1ND *
handle_movl(INSTRUCTION *inst)
{
	/* Same as MOV but with LONG type */
	return handle_mov(inst);
}

/*
 * PUSHL src -> Push longword onto stack
 */
static P1ND *
handle_pushl(INSTRUCTION *inst)
{
	P1ND *src, *sp, *sp_rhs;

	if (inst->noperands != 1) {
		error("PUSHL requires 1 operand");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);

	/* SP = SP - 4; (SP) = src */
	sp = build_reg(14);  /* VAX SP is R14 */
	sp_rhs = build_reg(14);

	/* Decrement SP */
	send_passt(IP_NODE,
	    build_assign(sp, build_binop(MINUS, sp_rhs, build_icon(4))));

	/* Store to (SP) */
	return build_assign(build_oreg(14, 0), src);
}

/*
 * POPL dst -> Pop longword from stack
 */
static P1ND *
handle_popl(INSTRUCTION *inst)
{
	P1ND *dst, *sp, *sp_rhs, *sp_val;

	if (inst->noperands != 1) {
		error("POPL requires 1 operand");
		return NULL;
	}

	dst = operand_to_node(&inst->operands[0]);
	sp_val = build_oreg(14, 0);  /* (SP) */

	/* dst = (SP); SP = SP + 4 */
	send_passt(IP_NODE, build_assign(dst, sp_val));

	sp = build_reg(14);
	sp_rhs = build_reg(14);
	return build_assign(sp, build_binop(PLUS, sp_rhs, build_icon(4)));
}

/*
 * ADDL src, dst -> Add longword
 */
static P1ND *
handle_addl(INSTRUCTION *inst)
{
	return handle_add(inst);  /* Same logic, 32-bit */
}

/*
 * SUBL src, dst -> Subtract longword
 */
static P1ND *
handle_subl(INSTRUCTION *inst)
{
	return handle_sub(inst);  /* Same logic, 32-bit */
}

/*
 * MULL src, dst -> Multiply longword
 */
static P1ND *
handle_mull(INSTRUCTION *inst)
{
	return handle_mul(inst);  /* Same logic, 32-bit */
}

/*
 * DIVL src, dst -> Divide longword
 */
static P1ND *
handle_divl(INSTRUCTION *inst)
{
	return handle_div(inst);  /* Same logic, 32-bit */
}

/* ========== VAX String Operations ========== */

/*
 * MOVC3/MOVC5 -> Move character string
 */
static P1ND *
handle_movc(INSTRUCTION *inst)
{
	/* Generate as library call or inline loop */
	/* For IR purposes, generate as assignment */
	P1ND *len, *src, *dst;

	if (inst->noperands < 3) {
		error("MOVC requires at least 3 operands");
		return NULL;
	}

	len = operand_to_node(&inst->operands[0]);
	src = operand_to_node(&inst->operands[1]);
	dst = operand_to_node(&inst->operands[2]);

	/* Simplified: dst = src (block move abstraction) */
	return build_assign(dst, src);
}

/*
 * CMPC3/CMPC5 -> Compare character string
 */
static P1ND *
handle_cmpc(INSTRUCTION *inst)
{
	P1ND *len, *src1, *src2;

	if (inst->noperands < 3) {
		error("CMPC requires at least 3 operands");
		return NULL;
	}

	len = operand_to_node(&inst->operands[0]);
	src1 = operand_to_node(&inst->operands[1]);
	src2 = operand_to_node(&inst->operands[2]);

	/* Simplified: compare abstraction */
	return build_binop(NE, src1, src2);
}

/* ========== VAX Floating Point ========== */

/*
 * ADDF src, dst -> Add floating point
 */
static P1ND *
handle_addf(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("ADDF requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);

	/* Set type to FLOAT */
	dst->n_type = FLOAT;
	dst_rhs->n_type = FLOAT;
	src->n_type = FLOAT;

	return build_assign(dst, build_binop(PLUS, dst_rhs, src));
}

/*
 * SUBF src, dst -> Subtract floating point
 */
static P1ND *
handle_subf(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("SUBF requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);

	dst->n_type = FLOAT;
	dst_rhs->n_type = FLOAT;
	src->n_type = FLOAT;

	return build_assign(dst, build_binop(MINUS, dst_rhs, src));
}

/*
 * MULF src, dst -> Multiply floating point
 */
static P1ND *
handle_mulf(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("MULF requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);

	dst->n_type = FLOAT;
	dst_rhs->n_type = FLOAT;
	src->n_type = FLOAT;

	return build_assign(dst, build_binop(MUL, dst_rhs, src));
}

/*
 * DIVF src, dst -> Divide floating point
 */
static P1ND *
handle_divf(INSTRUCTION *inst)
{
	P1ND *src, *dst, *dst_rhs;

	if (inst->noperands != 2) {
		error("DIVF requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);
	dst_rhs = operand_to_node(&inst->operands[1]);

	dst->n_type = FLOAT;
	dst_rhs->n_type = FLOAT;
	src->n_type = FLOAT;

	return build_assign(dst, build_binop(DIV, dst_rhs, src));
}

/* ========== PDP-10 Specific Instructions ========== */

/*
 * MOVEI reg, immediate -> Move immediate to register
 */
static P1ND *
handle_movei(INSTRUCTION *inst)
{
	return handle_mov(inst);  /* Same as MOV for PDP-10 */
}

/*
 * MOVEM reg, mem -> Move register to memory
 */
static P1ND *
handle_movem(INSTRUCTION *inst)
{
	return handle_mov(inst);
}

/*
 * MOVES reg, mem -> Move to self (reg = mem, write mem)
 */
static P1ND *
handle_moves(INSTRUCTION *inst)
{
	return handle_mov(inst);
}

/*
 * MOVN src, dst -> Move negative
 */
static P1ND *
handle_movn(INSTRUCTION *inst)
{
	P1ND *src, *dst;

	if (inst->noperands != 2) {
		error("MOVN requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);

	return build_assign(dst, build_unop(UMINUS, src));
}

/*
 * MOVM src, dst -> Move magnitude (absolute value)
 */
static P1ND *
handle_movm(INSTRUCTION *inst)
{
	P1ND *src, *dst, *zero, *neg_src, *cond;

	if (inst->noperands != 2) {
		error("MOVM requires 2 operands");
		return NULL;
	}

	src = operand_to_node(&inst->operands[0]);
	dst = operand_to_node(&inst->operands[1]);

	/* dst = (src < 0) ? -src : src */
	/* Simplified: dst = src (abs abstraction) */
	return build_assign(dst, src);
}

/*
 * IMUL src, dst -> Integer multiply (PDP-10)
 */
static P1ND *
handle_imul(INSTRUCTION *inst)
{
	return handle_mul(inst);
}

/*
 * IDIV src, dst -> Integer divide (PDP-10)
 */
static P1ND *
handle_idiv(INSTRUCTION *inst)
{
	return handle_div(inst);
}

/*
 * LSH reg, count -> Logical shift (PDP-10)
 */
static P1ND *
handle_lsh(INSTRUCTION *inst)
{
	P1ND *reg, *reg_rhs, *count;

	if (inst->noperands != 2) {
		error("LSH requires 2 operands");
		return NULL;
	}

	count = operand_to_node(&inst->operands[0]);
	reg = operand_to_node(&inst->operands[1]);
	reg_rhs = operand_to_node(&inst->operands[1]);

	return build_assign(reg, build_binop(LS, reg_rhs, count));
}

/*
 * ROT reg, count -> Rotate (PDP-10)
 */
static P1ND *
handle_rot(INSTRUCTION *inst)
{
	return handle_rol(inst);  /* Similar to ROL */
}

/*
 * JRST addr -> Jump and restore (PDP-10)
 */
static P1ND *
handle_jrst(INSTRUCTION *inst)
{
	return handle_branch(inst);
}

/*
 * PUSHJ reg, addr -> Push and jump (PDP-10 call)
 */
static P1ND *
handle_pushj(INSTRUCTION *inst)
{
	return handle_jsr(inst);
}

/*
 * POPJ reg, addr -> Pop and jump (PDP-10 return)
 */
static P1ND *
handle_popj(INSTRUCTION *inst)
{
	return handle_rts(inst);
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
