/*
 * Copyright (c) 2025 PCC Project
 *
 * VAX Instruction Set - Complete Definition
 *
 * This file defines ALL VAX instructions from the VAX-11 architecture.
 * The VAX (Virtual Address eXtension) is a CISC architecture with 300+ instructions.
 *
 * Instruction Categories:
 * - Integer Arithmetic & Logical
 * - Address Arithmetic
 * - Variable-Length Bit Field
 * - Control Transfer (Branches & Jumps)
 * - Procedure Call
 * - Queue & Interlock
 * - Floating-Point
 * - Character String
 * - Decimal String
 * - Edit
 * - Cyclic Redundancy Check
 * - Specialized
 */

#ifndef _VAX_INSTRUCTIONS_H_
#define _VAX_INSTRUCTIONS_H_

/*
 * VAX Instruction Opcodes (Hexadecimal)
 */

/* ========================================================================
 * INTEGER ARITHMETIC AND LOGICAL INSTRUCTIONS
 * ======================================================================== */

/* Addition Instructions */
#define ADDB2   0x80    /* Add Byte 2 operand */
#define ADDB3   0x81    /* Add Byte 3 operand */
#define ADDW2   0xA0    /* Add Word 2 operand */
#define ADDW3   0xA1    /* Add Word 3 operand */
#define ADDL2   0xC0    /* Add Long 2 operand */
#define ADDL3   0xC1    /* Add Long 3 operand */

/* Add with Carry */
#define ADWC    0xD8    /* Add With Carry */
#define ADAWI   0x58    /* Add Aligned Word Interlocked */

/* Subtraction Instructions */
#define SUBB2   0x82    /* Subtract Byte 2 operand */
#define SUBB3   0x83    /* Subtract Byte 3 operand */
#define SUBW2   0xA2    /* Subtract Word 2 operand */
#define SUBW3   0xA3    /* Subtract Word 3 operand */
#define SUBL2   0xC2    /* Subtract Long 2 operand */
#define SUBL3   0xC3    /* Subtract Long 3 operand */

/* Subtract with Carry */
#define SBWC    0xD9    /* Subtract With Carry */

/* Multiplication Instructions */
#define MULB2   0x84    /* Multiply Byte 2 operand */
#define MULB3   0x85    /* Multiply Byte 3 operand */
#define MULW2   0xA4    /* Multiply Word 2 operand */
#define MULW3   0xA5    /* Multiply Word 3 operand */
#define MULL2   0xC4    /* Multiply Long 2 operand */
#define MULL3   0xC5    /* Multiply Long 3 operand */

/* Extended Multiply */
#define EMUL    0x7A    /* Extended Multiply */

/* Division Instructions */
#define DIVB2   0x86    /* Divide Byte 2 operand */
#define DIVB3   0x87    /* Divide Byte 3 operand */
#define DIVW2   0xA6    /* Divide Word 2 operand */
#define DIVW3   0xA7    /* Divide Word 3 operand */
#define DIVL2   0xC6    /* Divide Long 2 operand */
#define DIVL3   0xC7    /* Divide Long 3 operand */

/* Extended Divide */
#define EDIV    0x7B    /* Extended Divide */

/* Increment and Decrement */
#define INCB    0x96    /* Increment Byte */
#define INCW    0xB6    /* Increment Word */
#define INCL    0xD6    /* Increment Long */
#define DECB    0x97    /* Decrement Byte */
#define DECW    0xB7    /* Decrement Word */
#define DECL    0xD7    /* Decrement Long */

/* Negate */
#define MNEGB   0x8E    /* Move Negated Byte */
#define MNEGW   0xAE    /* Move Negated Word */
#define MNEGL   0xCE    /* Move Negated Long */

/* ========================================================================
 * LOGICAL INSTRUCTIONS
 * ======================================================================== */

/* Bitwise AND */
#define BICB2   0x8A    /* Bit Clear Byte 2 operand */
#define BICB3   0x8B    /* Bit Clear Byte 3 operand */
#define BICW2   0xAA    /* Bit Clear Word 2 operand */
#define BICW3   0xAB    /* Bit Clear Word 3 operand */
#define BICL2   0xCA    /* Bit Clear Long 2 operand */
#define BICL3   0xCB    /* Bit Clear Long 3 operand */

/* Bitwise OR */
#define BISB2   0x88    /* Bit Set Byte 2 operand */
#define BISB3   0x89    /* Bit Set Byte 3 operand */
#define BISW2   0xA8    /* Bit Set Word 2 operand */
#define BISW3   0xA9    /* Bit Set Word 3 operand */
#define BISL2   0xC8    /* Bit Set Long 2 operand */
#define BISL3   0xC9    /* Bit Set Long 3 operand */

/* Bitwise XOR */
#define XORB2   0x8C    /* XOR Byte 2 operand */
#define XORB3   0x8D    /* XOR Byte 3 operand */
#define XORW2   0xAC    /* XOR Word 2 operand */
#define XORW3   0xAD    /* XOR Word 3 operand */
#define XORL2   0xCC    /* XOR Long 2 operand */
#define XORL3   0xCD    /* XOR Long 3 operand */

/* Bit Test */
#define BIT     0x93    /* Bit Test Byte */
#define BITW    0xB3    /* Bit Test Word */
#define BITL    0xD3    /* Bit Test Long */

/* Rotate and Shift */
#define ROTL    0x9C    /* Rotate Long */
#define ASHL    0x78    /* Arithmetic Shift Long */
#define ASHQ    0x79    /* Arithmetic Shift Quad */

/* ========================================================================
 * COMPARISON INSTRUCTIONS
 * ======================================================================== */

#define CMPB    0x91    /* Compare Byte */
#define CMPW    0xB1    /* Compare Word */
#define CMPL    0xD1    /* Compare Long */

/* Test (Compare with Zero) */
#define TSTB    0x95    /* Test Byte */
#define TSTW    0xB5    /* Test Word */
#define TSTL    0xD5    /* Test Long */

/* ========================================================================
 * MOVE INSTRUCTIONS
 * ======================================================================== */

/* Move */
#define MOVB    0x90    /* Move Byte */
#define MOVW    0xB0    /* Move Word */
#define MOVL    0xD0    /* Move Long */
#define MOVQ    0x7D    /* Move Quad */
#define MOVO    0x7DFD  /* Move Octaword */

/* Move Address */
#define MOVAB   0x9E    /* Move Address of Byte */
#define MOVAW   0x3E    /* Move Address of Word */
#define MOVAL   0xDE    /* Move Address of Long */
#define MOVAQ   0x7E    /* Move Address of Quad */
#define MOVAO   0x7EFD  /* Move Address of Octaword */

/* Move Complemented */
#define MCOMB   0x92    /* Move Complemented Byte */
#define MCOMW   0xB2    /* Move Complemented Word */
#define MCOML   0xD2    /* Move Complemented Long */

/* Move Zero-Extended */
#define MOVZBW  0x9B    /* Move Zero-Extended Byte to Word */
#define MOVZBL  0x9A    /* Move Zero-Extended Byte to Long */
#define MOVZWL  0x3C    /* Move Zero-Extended Word to Long */

/* Convert (Sign-Extended Move) */
#define CVTBW   0x99    /* Convert Byte to Word */
#define CVTBL   0x98    /* Convert Byte to Long */
#define CVTWL   0x32    /* Convert Word to Long */
#define CVTLB   0xF6    /* Convert Long to Byte */
#define CVTLW   0xF7    /* Convert Long to Word */
#define CVTWB   0x33    /* Convert Word to Byte */

/* ========================================================================
 * PUSH AND POP INSTRUCTIONS
 * ======================================================================== */

#define PUSHL   0xDD    /* Push Long */
#define PUSHAB  0x9F    /* Push Address of Byte */
#define PUSHAW  0x3F    /* Push Address of Word */
#define PUSHAL  0xDF    /* Push Address of Long */
#define PUSHAQ  0x7F    /* Push Address of Quad */
#define PUSHAO  0x7FFD  /* Push Address of Octaword */
#define PUSHR   0xBB    /* Push Registers */
#define POPR    0xBA    /* Pop Registers */

/* ========================================================================
 * CONTROL TRANSFER INSTRUCTIONS (Branches)
 * ======================================================================== */

/* Unconditional Branch */
#define BRB     0x11    /* Branch (byte displacement) */
#define BRW     0x31    /* Branch (word displacement) */

/* Conditional Branches */
#define BEQL    0x13    /* Branch if Equal (Z=1) */
#define BEQLU   0x13    /* Branch if Equal Unsigned */
#define BNEQ    0x12    /* Branch if Not Equal (Z=0) */
#define BNEQU   0x12    /* Branch if Not Equal Unsigned */

#define BGTR    0x14    /* Branch if Greater (signed) */
#define BLEQ    0x15    /* Branch if Less or Equal (signed) */
#define BGEQ    0x18    /* Branch if Greater or Equal (signed) */
#define BLSS    0x19    /* Branch if Less (signed) */

#define BGTRU   0x1A    /* Branch if Greater Unsigned */
#define BLEQU   0x1B    /* Branch if Less or Equal Unsigned */
#define BGEQU   0x1E    /* Branch if Greater or Equal Unsigned */
#define BLSSU   0x1F    /* Branch if Less Unsigned */

/* Condition Code Branches */
#define BVC     0x1C    /* Branch if Overflow Clear */
#define BVS     0x1D    /* Branch if Overflow Set */
#define BCC     0x1E    /* Branch if Carry Clear (BGEQU) */
#define BCS     0x1F    /* Branch if Carry Set (BLSSU) */

/* Bit Test Branches */
#define BBS     0xE0    /* Branch on Bit Set */
#define BBC     0xE1    /* Branch on Bit Clear */
#define BBSS    0xE2    /* Branch on Bit Set and Set */
#define BBCS    0xE3    /* Branch on Bit Clear and Set */
#define BBSC    0xE4    /* Branch on Bit Set and Clear */
#define BBCC    0xE5    /* Branch on Bit Clear and Clear */
#define BBSSI   0xE6    /* Branch on Bit Set and Set Interlocked */
#define BBCCI   0xE7    /* Branch on Bit Clear and Clear Interlocked */

/* BLBS/BLBC - Branch on Low Bit */
#define BLBS    0xE8    /* Branch if Low Bit Set */
#define BLBC    0xE9    /* Branch if Low Bit Clear */

/* ========================================================================
 * JUMP INSTRUCTIONS
 * ======================================================================== */

#define JMP     0x17    /* Jump */

/* ========================================================================
 * SUBROUTINE CALL INSTRUCTIONS
 * ======================================================================== */

#define BSBB    0x10    /* Branch to Subroutine (byte) */
#define BSBW    0x30    /* Branch to Subroutine (word) */
#define JSB     0x16    /* Jump to Subroutine */
#define RSB     0x05    /* Return from Subroutine */

/* Call Frame Instructions */
#define CALLG   0xFA    /* Call with General Argument List */
#define CALLS   0xFB    /* Call with Stack */
#define RET     0x04    /* Return from Procedure */

/* ========================================================================
 * LOOP CONTROL INSTRUCTIONS
 * ======================================================================== */

#define AOBLEQ  0xF3    /* Add One and Branch if Less or Equal */
#define AOBLSS  0xF2    /* Add One and Branch if Less */
#define SOBGEQ  0xF4    /* Subtract One and Branch if Greater or Equal */
#define SOBGTR  0xF5    /* Subtract One and Branch if Greater */

/* ACB - Add Compare and Branch */
#define ACBB    0x9D    /* ACB Byte */
#define ACBW    0x3D    /* ACB Word */
#define ACBL    0xF1    /* ACB Long */
#define ACBF    0x4F    /* ACB F_floating */
#define ACBD    0x6F    /* ACB D_floating */
#define ACBG    0x4FFD  /* ACB G_floating */
#define ACBH    0x6FFD  /* ACB H_floating */

/* ========================================================================
 * CASE INSTRUCTIONS
 * ======================================================================== */

#define CASEB   0x8F    /* Case Byte */
#define CASEW   0xAF    /* Case Word */
#define CASEL   0xCF    /* Case Long */

/* ========================================================================
 * FLOATING-POINT INSTRUCTIONS (F_floating and D_floating)
 * ======================================================================== */

/* F_floating (Single Precision - 32 bit) */
#define ADDF2   0x40    /* Add F_floating 2 operand */
#define ADDF3   0x41    /* Add F_floating 3 operand */
#define SUBF2   0x42    /* Subtract F_floating 2 operand */
#define SUBF3   0x43    /* Subtract F_floating 3 operand */
#define MULF2   0x44    /* Multiply F_floating 2 operand */
#define MULF3   0x45    /* Multiply F_floating 3 operand */
#define DIVF2   0x46    /* Divide F_floating 2 operand */
#define DIVF3   0x47    /* Divide F_floating 3 operand */

/* D_floating (Double Precision - 64 bit) */
#define ADDD2   0x60    /* Add D_floating 2 operand */
#define ADDD3   0x61    /* Add D_floating 3 operand */
#define SUBD2   0x62    /* Subtract D_floating 2 operand */
#define SUBD3   0x63    /* Subtract D_floating 3 operand */
#define MULD2   0x64    /* Multiply D_floating 2 operand */
#define MULD3   0x65    /* Multiply D_floating 3 operand */
#define DIVD2   0x66    /* Divide D_floating 2 operand */
#define DIVD3   0x67    /* Divide D_floating 3 operand */

/* G_floating (Extended Precision - 64 bit) */
#define ADDG2   0x40FD  /* Add G_floating 2 operand */
#define ADDG3   0x41FD  /* Add G_floating 3 operand */
#define SUBG2   0x42FD  /* Subtract G_floating 2 operand */
#define SUBG3   0x43FD  /* Subtract G_floating 3 operand */
#define MULG2   0x44FD  /* Multiply G_floating 2 operand */
#define MULG3   0x45FD  /* Multiply G_floating 3 operand */
#define DIVG2   0x46FD  /* Divide G_floating 2 operand */
#define DIVG3   0x47FD  /* Divide G_floating 3 operand */

/* H_floating (High Precision - 128 bit) */
#define ADDH2   0x60FD  /* Add H_floating 2 operand */
#define ADDH3   0x61FD  /* Add H_floating 3 operand */
#define SUBH2   0x62FD  /* Subtract H_floating 2 operand */
#define SUBH3   0x63FD  /* Subtract H_floating 3 operand */
#define MULH2   0x64FD  /* Multiply H_floating 2 operand */
#define MULH3   0x65FD  /* Multiply H_floating 3 operand */
#define DIVH2   0x66FD  /* Divide H_floating 2 operand */
#define DIVH3   0x67FD  /* Divide H_floating 3 operand */

/* Floating Comparison */
#define CMPF    0x51    /* Compare F_floating */
#define CMPD    0x71    /* Compare D_floating */
#define CMPG    0x51FD  /* Compare G_floating */
#define CMPH    0x71FD  /* Compare H_floating */

/* Floating Test */
#define TSTF    0x53    /* Test F_floating */
#define TSTD    0x73    /* Test D_floating */
#define TSTG    0x53FD  /* Test G_floating */
#define TSTH    0x73FD  /* Test H_floating */

/* Floating Move */
#define MOVF    0x50    /* Move F_floating */
#define MOVD    0x70    /* Move D_floating */
#define MOVG    0x50FD  /* Move G_floating */
#define MOVH    0x70FD  /* Move H_floating */

/* Floating Negate */
#define MNEGF   0x52    /* Move Negated F_floating */
#define MNEGD   0x72    /* Move Negated D_floating */
#define MNEGG   0x52FD  /* Move Negated G_floating */
#define MNEGH   0x72FD  /* Move Negated H_floating */

/* Floating Convert */
#define CVTBF   0x4C    /* Convert Byte to F_floating */
#define CVTWF   0x4D    /* Convert Word to F_floating */
#define CVTLF   0x4E    /* Convert Long to F_floating */
#define CVTFB   0x48    /* Convert F_floating to Byte */
#define CVTFW   0x49    /* Convert F_floating to Word */
#define CVTFL   0x4A    /* Convert F_floating to Long */
#define CVTRFL  0x4B    /* Convert Rounded F_floating to Long */

#define CVTBD   0x6C    /* Convert Byte to D_floating */
#define CVTWD   0x6D    /* Convert Word to D_floating */
#define CVTLD   0x6E    /* Convert Long to D_floating */
#define CVTDB   0x68    /* Convert D_floating to Byte */
#define CVTDW   0x69    /* Convert D_floating to Word */
#define CVTDL   0x6A    /* Convert D_floating to Long */
#define CVTRDL  0x6B    /* Convert Rounded D_floating to Long */

#define CVTDF   0x76    /* Convert D_floating to F_floating */
#define CVTFD   0x56    /* Convert F_floating to D_floating */

/* Polynomial Evaluation */
#define POLYF   0x55    /* Polynomial Evaluation F_floating */
#define POLYD   0x75    /* Polynomial Evaluation D_floating */
#define POLYG   0x55FD  /* Polynomial Evaluation G_floating */
#define POLYH   0x75FD  /* Polynomial Evaluation H_floating */

/* Extended Modulo */
#define EMOD    0x54    /* Extended Modulo F_floating */
#define EMODD   0x74    /* Extended Modulo D_floating */
#define EMODG   0x54FD  /* Extended Modulo G_floating */
#define EMODH   0x74FD  /* Extended Modulo H_floating */

/* ========================================================================
 * CHARACTER STRING INSTRUCTIONS
 * ======================================================================== */

#define MOVC3   0x28    /* Move Character 3 operand */
#define MOVC5   0x2C    /* Move Character 5 operand */
#define CMPC3   0x29    /* Compare Character 3 operand */
#define CMPC5   0x2D    /* Compare Character 5 operand */
#define LOCC    0x3A    /* Locate Character */
#define SKPC    0x3B    /* Skip Character */
#define SCANC   0x2A    /* Scan Characters */
#define SPANC   0x2B    /* Span Characters */

/* ========================================================================
 * DECIMAL STRING INSTRUCTIONS
 * ======================================================================== */

#define MOVP    0x34    /* Move Packed */
#define CMPP3   0x35    /* Compare Packed 3 operand */
#define CMPP4   0x37    /* Compare Packed 4 operand */
#define CVTLP   0xF9    /* Convert Long to Packed */
#define CVTPL   0x36    /* Convert Packed to Long */
#define CVTPT   0x24    /* Convert Packed to Trailing */
#define CVTTP   0x26    /* Convert Trailing to Packed */
#define CVTPS   0x08    /* Convert Packed to Separate */
#define CVTSP   0x09    /* Convert Separate to Packed */

#define ADDP4   0x20    /* Add Packed 4 operand */
#define ADDP6   0x21    /* Add Packed 6 operand */
#define SUBP4   0x22    /* Subtract Packed 4 operand */
#define SUBP6   0x23    /* Subtract Packed 6 operand */
#define MULP    0x25    /* Multiply Packed */
#define DIVP    0x27    /* Divide Packed */

#define ASHP    0xF8    /* Arithmetic Shift and Round Packed */

/* ========================================================================
 * EDIT INSTRUCTION
 * ======================================================================== */

#define EDITPC  0x38    /* Edit Packed to Character */

/* ========================================================================
 * QUEUE INSTRUCTIONS
 * ======================================================================== */

#define INSQUE  0x0E    /* Insert into Queue */
#define REMQUE  0x0F    /* Remove from Queue */

#define INSQHI  0x5C    /* Insert into Queue at Head, Interlocked */
#define INSQTI  0x5D    /* Insert into Queue at Tail, Interlocked */
#define REMQHI  0x5E    /* Remove from Queue at Head, Interlocked */
#define REMQTI  0x5F    /* Remove from Queue at Tail, Interlocked */

/* ========================================================================
 * VARIABLE-LENGTH BIT FIELD INSTRUCTIONS
 * ======================================================================== */

#define CMPV    0xEC    /* Compare Field */
#define CMPZV   0xED    /* Compare Zero-Extended Field */
#define EXTV    0xEE    /* Extract Field */
#define EXTZV   0xEF    /* Extract Zero-Extended Field */
#define INSV    0xF0    /* Insert Field */

/* Find First */
#define FFC     0xEB    /* Find First Clear Bit */
#define FFS     0xEA    /* Find First Set Bit */

/* ========================================================================
 * MISCELLANEOUS INSTRUCTIONS
 * ======================================================================== */

/* Clear */
#define CLRB    0x94    /* Clear Byte */
#define CLRW    0xB4    /* Clear Word */
#define CLRL    0xD4    /* Clear Long */
#define CLRQ    0x7C    /* Clear Quad */
#define CLRO    0x7CFD  /* Clear Octaword */

/* No Operation */
#define NOP     0x01    /* No Operation */

/* Halt */
#define HALT    0x00    /* Halt */

/* Index */
#define INDEX   0x0A    /* Compute Index */

/* ========================================================================
 * PRIVILEGED AND OPERATING SYSTEM INSTRUCTIONS
 * ======================================================================== */

#define CHMK    0xBC    /* Change Mode to Kernel */
#define CHME    0xBD    /* Change Mode to Executive */
#define CHMS    0xBE    /* Change Mode to Supervisor */
#define CHMU    0xBF    /* Change Mode to User */

#define REI     0x02    /* Return from Exception or Interrupt */

#define LDPCTX  0x06    /* Load Process Context */
#define SVPCTX  0x07    /* Save Process Context */

#define PROBER  0x0C    /* Probe Read Access */
#define PROBEW  0x0D    /* Probe Write Access */

#define MTPR    0xDA    /* Move To Processor Register */
#define MFPR    0xDB    /* Move From Processor Register */

#define BPT     0x03    /* Breakpoint */
#define XFC     0xFC    /* Extended Function Call */

/* ========================================================================
 * CYCLIC REDUNDANCY CHECK
 * ======================================================================== */

#define CRC     0x0B    /* Calculate CRC */

/* ========================================================================
 * COMPATIBILITY MODE INSTRUCTIONS (PDP-11 Compatibility)
 * ======================================================================== */

#define COMPAT  0xFD    /* Compatibility Mode Prefix */

/* ========================================================================
 * INSTRUCTION MNEMONICS AS STRINGS (for assembler output)
 * ======================================================================== */

extern const char *vax_mnemonic[];

/* ========================================================================
 * INSTRUCTION FORMAT DEFINITIONS
 * ======================================================================== */

/* Operand specifier formats */
typedef enum {
	VAX_OP_REG = 0,          /* Register */
	VAX_OP_REGDEF = 1,       /* Register deferred */
	VAX_OP_AUTODEC = 2,      /* Autodecrement */
	VAX_OP_AUTOINC = 3,      /* Autoincrement */
	VAX_OP_AUTOINC_DEF = 4,  /* Autoincrement deferred */
	VAX_OP_BYTE_DISP = 5,    /* Byte displacement */
	VAX_OP_BYTE_DISP_DEF = 6,/* Byte displacement deferred */
	VAX_OP_WORD_DISP = 7,    /* Word displacement */
	VAX_OP_WORD_DISP_DEF = 8,/* Word displacement deferred */
	VAX_OP_LONG_DISP = 9,    /* Long displacement */
	VAX_OP_LONG_DISP_DEF = 10,/* Long displacement deferred */
	VAX_OP_LITERAL = 11,     /* Short literal */
	VAX_OP_INDEX = 12,       /* Indexed */
	VAX_OP_ABSOLUTE = 13,    /* Absolute */
	VAX_OP_IMMEDIATE = 14,   /* Immediate */
	VAX_OP_RELATIVE = 15,    /* Relative */
	VAX_OP_RELATIVE_DEF = 16 /* Relative deferred */
} vax_operand_mode_t;

/* Instruction data types */
typedef enum {
	VAX_DT_BYTE = 0,         /* 8-bit signed */
	VAX_DT_WORD = 1,         /* 16-bit signed */
	VAX_DT_LONG = 2,         /* 32-bit signed */
	VAX_DT_QUAD = 3,         /* 64-bit signed */
	VAX_DT_OCTA = 4,         /* 128-bit */
	VAX_DT_F_FLOAT = 5,      /* F_floating (32-bit) */
	VAX_DT_D_FLOAT = 6,      /* D_floating (64-bit) */
	VAX_DT_G_FLOAT = 7,      /* G_floating (64-bit) */
	VAX_DT_H_FLOAT = 8,      /* H_floating (128-bit) */
	VAX_DT_PACKED = 9,       /* Packed decimal */
	VAX_DT_CHAR = 10         /* Character string */
} vax_data_type_t;

/* Condition codes */
#define VAX_CC_N  0x08   /* Negative */
#define VAX_CC_Z  0x04   /* Zero */
#define VAX_CC_V  0x02   /* Overflow */
#define VAX_CC_C  0x01   /* Carry */

#endif /* _VAX_INSTRUCTIONS_H_ */
