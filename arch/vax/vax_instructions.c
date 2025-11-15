/*
 * Copyright (c) 2025 PCC Project
 *
 * VAX Instruction Set - Implementation
 *
 * Provides instruction mnemonics, encoding/decoding,  and helper functions
 * for all VAX instructions.
 */

#include "vax_instructions.h"
#include <stdio.h>
#include <string.h>

/*
 * VAX Instruction Mnemonic Table
 * Maps opcodes to assembly mnemonics
 */
const char *vax_mnemonic[] = {
	[HALT]    = "halt",
	[NOP]     = "nop",
	[REI]     = "rei",
	[BPT]     = "bpt",
	[RET]     = "ret",
	[RSB]     = "rsb",
	[LDPCTX]  = "ldpctx",
	[SVPCTX]  = "svpctx",
	[CVTPS]   = "cvtps",
	[CVTSP]   = "cvtsp",
	[INDEX]   = "index",
	[CRC]     = "crc",
	[PROBER]  = "prober",
	[PROBEW]  = "probew",
	[INSQUE]  = "insque",
	[REMQUE]  = "remque",
	[BSBB]    = "bsbb",
	[BRB]     = "brb",
	[BNEQ]    = "bneq",
	[BEQL]    = "beql",
	[BGTR]    = "bgtr",
	[BLEQ]    = "bleq",
	[JSB]     = "jsb",
	[JMP]     = "jmp",
	[BGEQ]    = "bgeq",
	[BLSS]    = "blss",
	[BGTRU]   = "bgtru",
	[BLEQU]   = "blequ",
	[BVC]     = "bvc",
	[BVS]     = "bvs",
	[BGEQU]   = "bgequ",
	[BLSSU]   = "blssu",
	[ADDP4]   = "addp4",
	[ADDP6]   = "addp6",
	[SUBP4]   = "subp4",
	[SUBP6]   = "subp6",
	[CVTPT]   = "cvtpt",
	[MULP]    = "mulp",
	[CVTTP]   = "cvttp",
	[DIVP]    = "divp",
	[MOVC3]   = "movc3",
	[CMPC3]   = "cmpc3",
	[SCANC]   = "scanc",
	[SPANC]   = "spanc",
	[MOVC5]   = "movc5",
	[CMPC5]   = "cmpc5",
	[MOVP]    = "movp",
	[CMPP3]   = "cmpp3",
	[CVTPL]   = "cvtpl",
	[CMPP4]   = "cmpp4",
	[EDITPC]  = "editpc",
	[LOCC]    = "locc",
	[SKPC]    = "skpc",
	[MOVZWL]  = "movzwl",
	[BSBW]    = "bsbw",
	[BRW]     = "brw",
	[CVTWL]   = "cvtwl",
	[CVTWB]   = "cvtwb",
	[MOVAW]   = "movaw",
	[PUSHAW]  = "pushaw",
	[ADDF2]   = "addf2",
	[ADDF3]   = "addf3",
	[SUBF2]   = "subf2",
	[SUBF3]   = "subf3",
	[MULF2]   = "mulf2",
	[MULF3]   = "mulf3",
	[DIVF2]   = "divf2",
	[DIVF3]   = "divf3",
	[CVTFB]   = "cvtfb",
	[CVTFW]   = "cvtfw",
	[CVTFL]   = "cvtfl",
	[CVTRFL]  = "cvtrfl",
	[CVTBF]   = "cvtbf",
	[CVTWD]   = "cvtwd",
	[CVTLF]   = "cvtlf",
	[ACBF]    = "acbf",
	[MOVF]    = "movf",
	[CMPF]    = "cmpf",
	[MNEGF]   = "mnegf",
	[TSTF]    = "tstf",
	[EMOD]    = "emod",
	[POLYF]   = "polyf",
	[CVTFD]   = "cvtfd",
	[ADAWI]   = "adawi",
	[INSQHI]  = "insqhi",
	[INSQTI]  = "insqti",
	[REMQHI]  = "remqhi",
	[REMQTI]  = "remqti",
	[ADDD2]   = "addd2",
	[ADDD3]   = "addd3",
	[SUBD2]   = "subd2",
	[SUBD3]   = "subd3",
	[MULD2]   = "muld2",
	[MULD3]   = "muld3",
	[DIVD2]   = "divd2",
	[DIVD3]   = "divd3",
	[CVTDB]   = "cvtdb",
	[CVTDW]   = "cvtdw",
	[CVTDL]   = "cvtdl",
	[CVTRDL]  = "cvtrdl",
	[CVTBD]   = "cvtbd",
	[CVTWD]   = "cvtwd",
	[CVTLD]   = "cvtld",
	[ACBD]    = "acbd",
	[MOVD]    = "movd",
	[CMPD]    = "cmpd",
	[MNEGD]   = "mnegd",
	[TSTD]    = "tstd",
	[EMODD]   = "emodd",
	[POLYD]   = "polyd",
	[CVTDF]   = "cvtdf",
	[ASHL]    = "ashl",
	[ASHQ]    = "ashq",
	[EMUL]    = "emul",
	[EDIV]    = "ediv",
	[CLRQ]    = "clrq",
	[MOVQ]    = "movq",
	[MOVAQ]   = "movaq",
	[PUSHAQ]  = "pushaq",
	[ADDB2]   = "addb2",
	[ADDB3]   = "addb3",
	[SUBB2]   = "subb2",
	[SUBB3]   = "subb3",
	[MULB2]   = "mulb2",
	[MULB3]   = "mulb3",
	[DIVB2]   = "divb2",
	[DIVB3]   = "divb3",
	[BISB2]   = "bisb2",
	[BISB3]   = "bisb3",
	[BICB2]   = "bicb2",
	[BICB3]   = "bicb3",
	[XORB2]   = "xorb2",
	[XORB3]   = "xorb3",
	[MNEGB]   = "mnegb",
	[CASEB]   = "caseb",
	[MOVB]    = "movb",
	[CMPB]    = "cmpb",
	[MCOMB]   = "mcomb",
	[BITB]    = "bitb",
	[CLRB]    = "clrb",
	[TSTB]    = "tstb",
	[INCB]    = "incb",
	[DECB]    = "decb",
	[CVTBL]   = "cvtbl",
	[CVTBW]   = "cvtbw",
	[MOVZBL]  = "movzbl",
	[MOVZBW]  = "movzbw",
	[ROTL]    = "rotl",
	[ACBB]    = "acbb",
	[MOVAB]   = "movab",
	[PUSHAB]  = "pushab",
	[ADDW2]   = "addw2",
	[ADDW3]   = "addw3",
	[SUBW2]   = "subw2",
	[SUBW3]   = "subw3",
	[MULW2]   = "mulw2",
	[MULW3]   = "mulw3",
	[DIVW2]   = "divw2",
	[DIVW3]   = "divw3",
	[BISW2]   = "bisw2",
	[BISW3]   = "bisw3",
	[BICW2]   = "bicw2",
	[BICW3]   = "bicw3",
	[XORW2]   = "xorw2",
	[XORW3]   = "xorw3",
	[MNEGW]   = "mnegw",
	[CASEW]   = "casew",
	[MOVW]    = "movw",
	[CMPW]    = "cmpw",
	[MCOMW]   = "mcomw",
	[BITW]    = "bitw",
	[CLRW]    = "clrw",
	[TSTW]    = "tstw",
	[INCW]    = "incw",
	[DECW]    = "decw",
	[BISPSW]  = "bispsw",
	[BICPSW]  = "bicpsw",
	[POPR]    = "popr",
	[PUSHR]   = "pushr",
	[CHMK]    = "chmk",
	[CHME]    = "chme",
	[CHMS]    = "chms",
	[CHMU]    = "chmu",
	[ADDL2]   = "addl2",
	[ADDL3]   = "addl3",
	[SUBL2]   = "subl2",
	[SUBL3]   = "subl3",
	[MULL2]   = "mull2",
	[MULL3]   = "mull3",
	[DIVL2]   = "divl2",
	[DIVL3]   = "divl3",
	[BISL2]   = "bisl2",
	[BISL3]   = "bisl3",
	[BICL2]   = "bicl2",
	[BICL3]   = "bicl3",
	[XORL2]   = "xorl2",
	[XORL3]   = "xorl3",
	[MNEGL]   = "mnegl",
	[CASEL]   = "casel",
	[MOVL]    = "movl",
	[CMPL]    = "cmpl",
	[MCOML]   = "mcoml",
	[BITL]    = "bitl",
	[CLRL]    = "clrl",
	[TSTL]    = "tstl",
	[INCL]    = "incl",
	[DECL]    = "decl",
	[ADWC]    = "adwc",
	[SBWC]    = "sbwc",
	[MTPR]    = "mtpr",
	[MFPR]    = "mfpr",
	[MOVPSL]  = "movpsl",
	[PUSHL]   = "pushl",
	[MOVAL]   = "moval",
	[PUSHAL]  = "pushal",
	[BBS]     = "bbs",
	[BBC]     = "bbc",
	[BBSS]    = "bbss",
	[BBCS]    = "bbcs",
	[BBSC]    = "bbsc",
	[BBCC]    = "bbcc",
	[BBSSI]   = "bbssi",
	[BBCCI]   = "bbcci",
	[BLBS]    = "blbs",
	[BLBC]    = "blbc",
	[FFS]     = "ffs",
	[FFC]     = "ffc",
	[CMPV]    = "cmpv",
	[CMPZV]   = "cmpzv",
	[EXTV]    = "extv",
	[EXTZV]   = "extzv",
	[INSV]    = "insv",
	[ACBL]    = "acbl",
	[AOBLSS]  = "aoblss",
	[AOBLEQ]  = "aobleq",
	[SOBGEQ]  = "sobgeq",
	[SOBGTR]  = "sobgtr",
	[CVTLB]   = "cvtlb",
	[CVTLW]   = "cvtlw",
	[ASHP]    = "ashp",
	[CVTLP]   = "cvtlp",
	[CALLG]   = "callg",
	[CALLS]   = "calls",
	[XFC]     = "xfc",
};

/*
 * Get instruction mnemonic string
 */
const char *vax_get_mnemonic(unsigned char opcode) {
	if (opcode < sizeof(vax_mnemonic) / sizeof(vax_mnemonic[0])) {
		return vax_mnemonic[opcode] ? vax_mnemonic[opcode] : "???";
	}
	return "???";
}

/*
 * Determine instruction length
 * VAX instructions are variable length from 1 to many bytes
 */
int vax_instruction_length(unsigned char opcode) {
	/* This is simplified - actual length depends on operand specifiers */
	switch (opcode) {
	case HALT:
	case NOP:
	case REI:
	case BPT:
	case RET:
	case RSB:
		return 1;  /* No operands */

	case BRB:
	case BSBB:
		return 2;  /* Opcode + byte displacement */

	case BRW:
	case BSBW:
		return 3;  /* Opcode + word displacement */

	/* Most instructions need operand analysis */
	default:
		return -1;  /* Variable, depends on operands */
	}
}

/*
 * Classify instruction by type
 */
typedef enum {
	VAX_INSN_ARITHMETIC,
	VAX_INSN_LOGICAL,
	VAX_INSN_MOVE,
	VAX_INSN_COMPARE,
	VAX_INSN_BRANCH,
	VAX_INSN_JUMP,
	VAX_INSN_CALL,
	VAX_INSN_FLOAT,
	VAX_INSN_STRING,
	VAX_INSN_DECIMAL,
	VAX_INSN_QUEUE,
	VAX_INSN_BITFIELD,
	VAX_INSN_SYSTEM,
	VAX_INSN_MISC
} vax_insn_class_t;

vax_insn_class_t vax_classify_instruction(unsigned char opcode) {
	/* Arithmetic */
	if ((opcode >= ADDB2 && opcode <= DIVB3) ||
	    (opcode >= ADDW2 && opcode <= DIVW3) ||
	    (opcode >= ADDL2 && opcode <= DIVL3) ||
	    opcode == EMUL || opcode == EDIV ||
	    opcode == ADWC || opcode == SBWC ||
	    opcode == INCB || opcode == INCW || opcode == INCL ||
	    opcode == DECB || opcode == DECW || opcode == DECL) {
		return VAX_INSN_ARITHMETIC;
	}

	/* Logical */
	if ((opcode >= BISB2 && opcode <= BICB3) ||
	    (opcode >= BISW2 && opcode <= BICW3) ||
	    (opcode >= BISL2 && opcode <= BICL3) ||
	    (opcode >= XORB2 && opcode <= XORB3) ||
	    (opcode >= XORW2 && opcode <= XORW3) ||
	    (opcode >= XORL2 && opcode <= XORL3) ||
	    opcode == BITB || opcode == BITW || opcode == BITL ||
	    opcode == ROTL || opcode == ASHL || opcode == ASHQ) {
		return VAX_INSN_LOGICAL;
	}

	/* Move */
	if (opcode == MOVB || opcode == MOVW || opcode == MOVL ||
	    opcode == MOVQ || opcode == MOVAB || opcode == MOVAW ||
	    opcode == MOVAL || opcode == MOVAQ ||
	    opcode == MOVZBL || opcode == MOVZBW || opcode == MOVZWL ||
	    opcode == CVTBW || opcode == CVTBL || opcode == CVTWL ||
	    opcode == CVTLB || opcode == CVTLW || opcode == CVTWB ||
	    opcode == PUSHL || opcode == PUSHAB || opcode == PUSHAW ||
	    opcode == PUSHAL || opcode == PUSHAQ ||
	    opcode == PUSHR || opcode == POPR) {
		return VAX_INSN_MOVE;
	}

	/* Compare */
	if (opcode == CMPB || opcode == CMPW || opcode == CMPL ||
	    opcode == TSTB || opcode == TSTW || opcode == TSTL) {
		return VAX_INSN_COMPARE;
	}

	/* Branches */
	if ((opcode >= BRB && opcode <= BLSSU) ||
	    (opcode >= BBS && opcode <= BLBC) ||
	    opcode == AOBLEQ || opcode == AOBLSS ||
	    opcode == SOBGEQ || opcode == SOBGTR ||
	    opcode == ACBB || opcode == ACBW || opcode == ACBL) {
		return VAX_INSN_BRANCH;
	}

	/* Jumps */
	if (opcode == JMP) {
		return VAX_INSN_JUMP;
	}

	/* Calls */
	if (opcode == JSB || opcode == BSBB || opcode == BSBW ||
	    opcode == CALLG || opcode == CALLS ||
	    opcode == RET || opcode == RSB) {
		return VAX_INSN_CALL;
	}

	/* Floating point */
	if ((opcode >= ADDF2 && opcode <= DIVF3) ||
	    (opcode >= ADDD2 && opcode <= DIVD3) ||
	    opcode == MOVF || opcode == MOVD ||
	    opcode == CMPF || opcode == CMPD ||
	    opcode == TSTF || opcode == TSTD ||
	    opcode == MNEGF || opcode == MNEGD ||
	    opcode == POLYF || opcode == POLYD ||
	    opcode == EMOD || opcode == EMODD ||
	    (opcode >= CVTBF && opcode <= CVTRFL) ||
	    (opcode >= CVTBD && opcode <= CVTRDL) ||
	    opcode == CVTFD || opcode == CVTDF ||
	    opcode == ACBF || opcode == ACBD) {
		return VAX_INSN_FLOAT;
	}

	/* String */
	if (opcode == MOVC3 || opcode == MOVC5 ||
	    opcode == CMPC3 || opcode == CMPC5 ||
	    opcode == LOCC || opcode == SKPC ||
	    opcode == SCANC || opcode == SPANC) {
		return VAX_INSN_STRING;
	}

	/* Decimal */
	if ((opcode >= ADDP4 && opcode <= DIVP) ||
	    opcode == MOVP || opcode == CMPP3 || opcode == CMPP4 ||
	    opcode == CVTLP || opcode == CVTPL ||
	    opcode == CVTPT || opcode == CVTTP ||
	    opcode == CVTPS || opcode == CVTSP ||
	    opcode == ASHP || opcode == EDITPC) {
		return VAX_INSN_DECIMAL;
	}

	/* Queue */
	if (opcode == INSQUE || opcode == REMQUE ||
	    opcode == INSQHI || opcode == INSQTI ||
	    opcode == REMQHI || opcode == REMQTI) {
		return VAX_INSN_QUEUE;
	}

	/* Bitfield */
	if (opcode == CMPV || opcode == CMPZV ||
	    opcode == EXTV || opcode == EXTZV ||
	    opcode == INSV || opcode == FFS || opcode == FFC) {
		return VAX_INSN_BITFIELD;
	}

	/* System/Privileged */
	if (opcode == CHMK || opcode == CHME ||
	    opcode == CHMS || opcode == CHMU ||
	    opcode == MTPR || opcode == MFPR ||
	    opcode == LDPCTX || opcode == SVPCTX ||
	    opcode == PROBER || opcode == PROBEW ||
	    opcode == REI || opcode == XFC) {
		return VAX_INSN_SYSTEM;
	}

	return VAX_INSN_MISC;
}

/*
 * Get condition code effects
 * Returns bitmask of which condition codes are modified
 */
unsigned char vax_cc_effects(unsigned char opcode) {
	unsigned char cc = 0;

	switch (vax_classify_instruction(opcode)) {
	case VAX_INSN_ARITHMETIC:
	case VAX_INSN_LOGICAL:
	case VAX_INSN_COMPARE:
	case VAX_INSN_FLOAT:
		/* Most arithmetic/logical operations set all 4 CCs */
		cc = VAX_CC_N | VAX_CC_Z | VAX_CC_V | VAX_CC_C;
		break;

	case VAX_INSN_MOVE:
		/* Move operations typically set N and Z, clear V */
		cc = VAX_CC_N | VAX_CC_Z | VAX_CC_V;
		break;

	case VAX_INSN_BRANCH:
	case VAX_INSN_JUMP:
	case VAX_INSN_CALL:
		/* Control transfer doesn't modify CCs */
		cc = 0;
		break;

	default:
		/* Varies by instruction */
		cc = VAX_CC_N | VAX_CC_Z | VAX_CC_V | VAX_CC_C;
		break;
	}

	return cc;
}

/*
 * Helper: Format operand for disassembly
 */
void vax_format_operand(char *buf, size_t bufsize, vax_operand_mode_t mode,
                        int reg, int value) {
	switch (mode) {
	case VAX_OP_REG:
		snprintf(buf, bufsize, "r%d", reg);
		break;
	case VAX_OP_REGDEF:
		snprintf(buf, bufsize, "(r%d)", reg);
		break;
	case VAX_OP_AUTODEC:
		snprintf(buf, bufsize, "-(r%d)", reg);
		break;
	case VAX_OP_AUTOINC:
		snprintf(buf, bufsize, "(r%d)+", reg);
		break;
	case VAX_OP_AUTOINC_DEF:
		snprintf(buf, bufsize, "@(r%d)+", reg);
		break;
	case VAX_OP_BYTE_DISP:
		snprintf(buf, bufsize, "%d(r%d)", (char)value, reg);
		break;
	case VAX_OP_BYTE_DISP_DEF:
		snprintf(buf, bufsize, "@%d(r%d)", (char)value, reg);
		break;
	case VAX_OP_LITERAL:
		snprintf(buf, bufsize, "$%d", value);
		break;
	case VAX_OP_IMMEDIATE:
		snprintf(buf, bufsize, "#%d", value);
		break;
	case VAX_OP_ABSOLUTE:
		snprintf(buf, bufsize, "@#%d", value);
		break;
	case VAX_OP_RELATIVE:
		snprintf(buf, bufsize, "%d(pc)", value);
		break;
	default:
		snprintf(buf, bufsize, "???");
		break;
	}
}
