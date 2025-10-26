/*
 * x86asm_util.c - Utility functions for x86 assembly emitter library
 *
 * Copyright (c) 2025
 * BSD Licensed - See COPYING
 */

#include "x86asm_internal.h"
#include <string.h>

/* Register name table mapping to AT&T and Intel syntax */
const x86asm_regname_t x86asm_reg_table[] = {
    /* 8-bit registers */
    { REG_AL,   "%al",   "al"   }, { REG_CL,   "%cl",   "cl"   },
    { REG_DL,   "%dl",   "dl"   }, { REG_BL,   "%bl",   "bl"   },
    { REG_AH,   "%ah",   "ah"   }, { REG_CH,   "%ch",   "ch"   },
    { REG_DH,   "%dh",   "dh"   }, { REG_BH,   "%bh",   "bh"   },
    { REG_SPL,  "%spl",  "spl"  }, { REG_BPL,  "%bpl",  "bpl"  },
    { REG_SIL,  "%sil",  "sil"  }, { REG_DIL,  "%dil",  "dil"  },
    { REG_R8B,  "%r8b",  "r8b"  }, { REG_R9B,  "%r9b",  "r9b"  },
    { REG_R10B, "%r10b", "r10b" }, { REG_R11B, "%r11b", "r11b" },
    { REG_R12B, "%r12b", "r12b" }, { REG_R13B, "%r13b", "r13b" },
    { REG_R14B, "%r14b", "r14b" }, { REG_R15B, "%r15b", "r15b" },

    /* 16-bit registers */
    { REG_AX,   "%ax",   "ax"   }, { REG_CX,   "%cx",   "cx"   },
    { REG_DX,   "%dx",   "dx"   }, { REG_BX,   "%bx",   "bx"   },
    { REG_SP,   "%sp",   "sp"   }, { REG_BP,   "%bp",   "bp"   },
    { REG_SI,   "%si",   "si"   }, { REG_DI,   "%di",   "di"   },
    { REG_R8W,  "%r8w",  "r8w"  }, { REG_R9W,  "%r9w",  "r9w"  },
    { REG_R10W, "%r10w", "r10w" }, { REG_R11W, "%r11w", "r11w" },
    { REG_R12W, "%r12w", "r12w" }, { REG_R13W, "%r13w", "r13w" },
    { REG_R14W, "%r14w", "r14w" }, { REG_R15W, "%r15w", "r15w" },

    /* 32-bit registers */
    { REG_EAX,  "%eax",  "eax"  }, { REG_ECX,  "%ecx",  "ecx"  },
    { REG_EDX,  "%edx",  "edx"  }, { REG_EBX,  "%ebx",  "ebx"  },
    { REG_ESP,  "%esp",  "esp"  }, { REG_EBP,  "%ebp",  "ebp"  },
    { REG_ESI,  "%esi",  "esi"  }, { REG_EDI,  "%edi",  "edi"  },
    { REG_R8D,  "%r8d",  "r8d"  }, { REG_R9D,  "%r9d",  "r9d"  },
    { REG_R10D, "%r10d", "r10d" }, { REG_R11D, "%r11d", "r11d" },
    { REG_R12D, "%r12d", "r12d" }, { REG_R13D, "%r13d", "r13d" },
    { REG_R14D, "%r14d", "r14d" }, { REG_R15D, "%r15d", "r15d" },

    /* 64-bit registers */
    { REG_RAX,  "%rax",  "rax"  }, { REG_RCX,  "%rcx",  "rcx"  },
    { REG_RDX,  "%rdx",  "rdx"  }, { REG_RBX,  "%rbx",  "rbx"  },
    { REG_RSP,  "%rsp",  "rsp"  }, { REG_RBP,  "%rbp",  "rbp"  },
    { REG_RSI,  "%rsi",  "rsi"  }, { REG_RDI,  "%rdi",  "rdi"  },
    { REG_R8,   "%r8",   "r8"   }, { REG_R9,   "%r9",   "r9"   },
    { REG_R10,  "%r10",  "r10"  }, { REG_R11,  "%r11",  "r11"  },
    { REG_R12,  "%r12",  "r12"  }, { REG_R13,  "%r13",  "r13"  },
    { REG_R14,  "%r14",  "r14"  }, { REG_R15,  "%r15",  "r15"  },

    /* APX Extended 64-bit registers (R16-R31) */
    { REG_R16,  "%r16",  "r16"  }, { REG_R17,  "%r17",  "r17"  },
    { REG_R18,  "%r18",  "r18"  }, { REG_R19,  "%r19",  "r19"  },
    { REG_R20,  "%r20",  "r20"  }, { REG_R21,  "%r21",  "r21"  },
    { REG_R22,  "%r22",  "r22"  }, { REG_R23,  "%r23",  "r23"  },
    { REG_R24,  "%r24",  "r24"  }, { REG_R25,  "%r25",  "r25"  },
    { REG_R26,  "%r26",  "r26"  }, { REG_R27,  "%r27",  "r27"  },
    { REG_R28,  "%r28",  "r28"  }, { REG_R29,  "%r29",  "r29"  },
    { REG_R30,  "%r30",  "r30"  }, { REG_R31,  "%r31",  "r31"  },

    /* APX Extended 32-bit registers (R16D-R31D) */
    { REG_R16D, "%r16d", "r16d" }, { REG_R17D, "%r17d", "r17d" },
    { REG_R18D, "%r18d", "r18d" }, { REG_R19D, "%r19d", "r19d" },
    { REG_R20D, "%r20d", "r20d" }, { REG_R21D, "%r21d", "r21d" },
    { REG_R22D, "%r22d", "r22d" }, { REG_R23D, "%r23d", "r23d" },
    { REG_R24D, "%r24d", "r24d" }, { REG_R25D, "%r25d", "r25d" },
    { REG_R26D, "%r26d", "r26d" }, { REG_R27D, "%r27d", "r27d" },
    { REG_R28D, "%r28d", "r28d" }, { REG_R29D, "%r29d", "r29d" },
    { REG_R30D, "%r30d", "r30d" }, { REG_R31D, "%r31d", "r31d" },

    /* APX Extended 16-bit registers (R16W-R31W) */
    { REG_R16W, "%r16w", "r16w" }, { REG_R17W, "%r17w", "r17w" },
    { REG_R18W, "%r18w", "r18w" }, { REG_R19W, "%r19w", "r19w" },
    { REG_R20W, "%r20w", "r20w" }, { REG_R21W, "%r21w", "r21w" },
    { REG_R22W, "%r22w", "r22w" }, { REG_R23W, "%r23w", "r23w" },
    { REG_R24W, "%r24w", "r24w" }, { REG_R25W, "%r25w", "r25w" },
    { REG_R26W, "%r26w", "r26w" }, { REG_R27W, "%r27w", "r27w" },
    { REG_R28W, "%r28w", "r28w" }, { REG_R29W, "%r29w", "r29w" },
    { REG_R30W, "%r30w", "r30w" }, { REG_R31W, "%r31w", "r31w" },

    /* APX Extended 8-bit registers (R16B-R31B) */
    { REG_R16B, "%r16b", "r16b" }, { REG_R17B, "%r17b", "r17b" },
    { REG_R18B, "%r18b", "r18b" }, { REG_R19B, "%r19b", "r19b" },
    { REG_R20B, "%r20b", "r20b" }, { REG_R21B, "%r21b", "r21b" },
    { REG_R22B, "%r22b", "r22b" }, { REG_R23B, "%r23b", "r23b" },
    { REG_R24B, "%r24b", "r24b" }, { REG_R25B, "%r25b", "r25b" },
    { REG_R26B, "%r26b", "r26b" }, { REG_R27B, "%r27b", "r27b" },
    { REG_R28B, "%r28b", "r28b" }, { REG_R29B, "%r29b", "r29b" },
    { REG_R30B, "%r30b", "r30b" }, { REG_R31B, "%r31b", "r31b" },

    /* Segment registers */
    { REG_ES,   "%es",   "es"   }, { REG_CS,   "%cs",   "cs"   },
    { REG_SS,   "%ss",   "ss"   }, { REG_DS,   "%ds",   "ds"   },
    { REG_FS,   "%fs",   "fs"   }, { REG_GS,   "%gs",   "gs"   },

    /* FPU registers */
    { REG_ST0,  "%st",   "st(0)" }, { REG_ST1,  "%st(1)", "st(1)" },
    { REG_ST2,  "%st(2)", "st(2)" }, { REG_ST3,  "%st(3)", "st(3)" },
    { REG_ST4,  "%st(4)", "st(4)" }, { REG_ST5,  "%st(5)", "st(5)" },
    { REG_ST6,  "%st(6)", "st(6)" }, { REG_ST7,  "%st(7)", "st(7)" },

    /* MMX registers */
    { REG_MM0,  "%mm0",  "mm0"  }, { REG_MM1,  "%mm1",  "mm1"  },
    { REG_MM2,  "%mm2",  "mm2"  }, { REG_MM3,  "%mm3",  "mm3"  },
    { REG_MM4,  "%mm4",  "mm4"  }, { REG_MM5,  "%mm5",  "mm5"  },
    { REG_MM6,  "%mm6",  "mm6"  }, { REG_MM7,  "%mm7",  "mm7"  },

    /* XMM registers */
    { REG_XMM0,  "%xmm0",  "xmm0"  }, { REG_XMM1,  "%xmm1",  "xmm1"  },
    { REG_XMM2,  "%xmm2",  "xmm2"  }, { REG_XMM3,  "%xmm3",  "xmm3"  },
    { REG_XMM4,  "%xmm4",  "xmm4"  }, { REG_XMM5,  "%xmm5",  "xmm5"  },
    { REG_XMM6,  "%xmm6",  "xmm6"  }, { REG_XMM7,  "%xmm7",  "xmm7"  },
    { REG_XMM8,  "%xmm8",  "xmm8"  }, { REG_XMM9,  "%xmm9",  "xmm9"  },
    { REG_XMM10, "%xmm10", "xmm10" }, { REG_XMM11, "%xmm11", "xmm11" },
    { REG_XMM12, "%xmm12", "xmm12" }, { REG_XMM13, "%xmm13", "xmm13" },
    { REG_XMM14, "%xmm14", "xmm14" }, { REG_XMM15, "%xmm15", "xmm15" },

    /* APX/AVX-512 Extended XMM registers (XMM16-XMM31) */
    { REG_XMM16, "%xmm16", "xmm16" }, { REG_XMM17, "%xmm17", "xmm17" },
    { REG_XMM18, "%xmm18", "xmm18" }, { REG_XMM19, "%xmm19", "xmm19" },
    { REG_XMM20, "%xmm20", "xmm20" }, { REG_XMM21, "%xmm21", "xmm21" },
    { REG_XMM22, "%xmm22", "xmm22" }, { REG_XMM23, "%xmm23", "xmm23" },
    { REG_XMM24, "%xmm24", "xmm24" }, { REG_XMM25, "%xmm25", "xmm25" },
    { REG_XMM26, "%xmm26", "xmm26" }, { REG_XMM27, "%xmm27", "xmm27" },
    { REG_XMM28, "%xmm28", "xmm28" }, { REG_XMM29, "%xmm29", "xmm29" },
    { REG_XMM30, "%xmm30", "xmm30" }, { REG_XMM31, "%xmm31", "xmm31" },

    /* YMM registers */
    { REG_YMM0,  "%ymm0",  "ymm0"  }, { REG_YMM1,  "%ymm1",  "ymm1"  },
    { REG_YMM2,  "%ymm2",  "ymm2"  }, { REG_YMM3,  "%ymm3",  "ymm3"  },
    { REG_YMM4,  "%ymm4",  "ymm4"  }, { REG_YMM5,  "%ymm5",  "ymm5"  },
    { REG_YMM6,  "%ymm6",  "ymm6"  }, { REG_YMM7,  "%ymm7",  "ymm7"  },
    { REG_YMM8,  "%ymm8",  "ymm8"  }, { REG_YMM9,  "%ymm9",  "ymm9"  },
    { REG_YMM10, "%ymm10", "ymm10" }, { REG_YMM11, "%ymm11", "ymm11" },
    { REG_YMM12, "%ymm12", "ymm12" }, { REG_YMM13, "%ymm13", "ymm13" },
    { REG_YMM14, "%ymm14", "ymm14" }, { REG_YMM15, "%ymm15", "ymm15" },

    /* APX/AVX-512 Extended YMM registers (YMM16-YMM31) */
    { REG_YMM16, "%ymm16", "ymm16" }, { REG_YMM17, "%ymm17", "ymm17" },
    { REG_YMM18, "%ymm18", "ymm18" }, { REG_YMM19, "%ymm19", "ymm19" },
    { REG_YMM20, "%ymm20", "ymm20" }, { REG_YMM21, "%ymm21", "ymm21" },
    { REG_YMM22, "%ymm22", "ymm22" }, { REG_YMM23, "%ymm23", "ymm23" },
    { REG_YMM24, "%ymm24", "ymm24" }, { REG_YMM25, "%ymm25", "ymm25" },
    { REG_YMM26, "%ymm26", "ymm26" }, { REG_YMM27, "%ymm27", "ymm27" },
    { REG_YMM28, "%ymm28", "ymm28" }, { REG_YMM29, "%ymm29", "ymm29" },
    { REG_YMM30, "%ymm30", "ymm30" }, { REG_YMM31, "%ymm31", "ymm31" },

    /* ZMM registers (AVX-512) */
    { REG_ZMM0,  "%zmm0",  "zmm0"  }, { REG_ZMM1,  "%zmm1",  "zmm1"  },
    { REG_ZMM2,  "%zmm2",  "zmm2"  }, { REG_ZMM3,  "%zmm3",  "zmm3"  },
    { REG_ZMM4,  "%zmm4",  "zmm4"  }, { REG_ZMM5,  "%zmm5",  "zmm5"  },
    { REG_ZMM6,  "%zmm6",  "zmm6"  }, { REG_ZMM7,  "%zmm7",  "zmm7"  },
    { REG_ZMM8,  "%zmm8",  "zmm8"  }, { REG_ZMM9,  "%zmm9",  "zmm9"  },
    { REG_ZMM10, "%zmm10", "zmm10" }, { REG_ZMM11, "%zmm11", "zmm11" },
    { REG_ZMM12, "%zmm12", "zmm12" }, { REG_ZMM13, "%zmm13", "zmm13" },
    { REG_ZMM14, "%zmm14", "zmm14" }, { REG_ZMM15, "%zmm15", "zmm15" },
    { REG_ZMM16, "%zmm16", "zmm16" }, { REG_ZMM17, "%zmm17", "zmm17" },
    { REG_ZMM18, "%zmm18", "zmm18" }, { REG_ZMM19, "%zmm19", "zmm19" },
    { REG_ZMM20, "%zmm20", "zmm20" }, { REG_ZMM21, "%zmm21", "zmm21" },
    { REG_ZMM22, "%zmm22", "zmm22" }, { REG_ZMM23, "%zmm23", "zmm23" },
    { REG_ZMM24, "%zmm24", "zmm24" }, { REG_ZMM25, "%zmm25", "zmm25" },
    { REG_ZMM26, "%zmm26", "zmm26" }, { REG_ZMM27, "%zmm27", "zmm27" },
    { REG_ZMM28, "%zmm28", "zmm28" }, { REG_ZMM29, "%zmm29", "zmm29" },
    { REG_ZMM30, "%zmm30", "zmm30" }, { REG_ZMM31, "%zmm31", "zmm31" },

    /* Mask registers (AVX-512) */
    { REG_K0,    "%k0",    "k0"    }, { REG_K1,    "%k1",    "k1"    },
    { REG_K2,    "%k2",    "k2"    }, { REG_K3,    "%k3",    "k3"    },
    { REG_K4,    "%k4",    "k4"    }, { REG_K5,    "%k5",    "k5"    },
    { REG_K6,    "%k6",    "k6"    }, { REG_K7,    "%k7",    "k7"    },

    { REG_NONE, NULL, NULL }
};

const int x86asm_reg_table_size = sizeof(x86asm_reg_table) / sizeof(x86asm_reg_table[0]);

/*
 * Check if format uses AT&T syntax
 */
int
x86asm_is_att_syntax(x86asm_format_t fmt)
{
    return (fmt == ASM_FMT_GNU_AS || fmt == ASM_FMT_APPLE_AS);
}

/*
 * Get register name for a specific format
 */
const char *
x86asm_reg_name_internal(x86asm_format_t fmt, x86asm_reg_t reg, int bits)
{
    int i;
    int use_att = x86asm_is_att_syntax(fmt);

    for (i = 0; i < x86asm_reg_table_size; i++) {
        if (x86asm_reg_table[i].reg == reg) {
            return use_att ? x86asm_reg_table[i].att_name :
                             x86asm_reg_table[i].intel_name;
        }
    }

    return NULL;
}

/*
 * Get size prefix for different formats
 */
const char *
x86asm_size_prefix(x86asm_format_t fmt, int size)
{
    int use_att = x86asm_is_att_syntax(fmt);

    if (use_att) {
        /* AT&T syntax uses suffixes: b, w, l, q */
        switch (size) {
        case 8:  return "b";
        case 16: return "w";
        case 32: return "l";
        case 64: return "q";
        default: return "";
        }
    } else {
        /* Intel syntax uses prefixes: byte ptr, word ptr, etc. */
        switch (size) {
        case 8:   return "byte ptr";
        case 16:  return "word ptr";
        case 32:  return "dword ptr";
        case 64:  return "qword ptr";
        case 80:  return "tbyte ptr";
        case 128: return "xmmword ptr";
        case 256: return "ymmword ptr";
        default:  return "";
        }
    }
}

/*
 * Get comment prefix for different formats
 */
const char *
x86asm_comment_prefix(x86asm_format_t fmt)
{
    switch (fmt) {
    case ASM_FMT_GNU_AS:
    case ASM_FMT_APPLE_AS:
    case ASM_FMT_NASM:
    case ASM_FMT_YASM:
        return "#";
    case ASM_FMT_MASM:
    case ASM_FMT_ML:
    case ASM_FMT_TASM:
    case ASM_FMT_WASM:
    case ASM_FMT_OWASM:
        return ";";
    default:
        return "#";
    }
}

/*
 * Get immediate prefix for different formats
 */
const char *
x86asm_immediate_prefix(x86asm_format_t fmt)
{
    if (x86asm_is_att_syntax(fmt))
        return "$";
    return "";
}

/*
 * Get register prefix for different formats
 */
const char *
x86asm_register_prefix(x86asm_format_t fmt)
{
    if (x86asm_is_att_syntax(fmt))
        return "%";
    return "";
}
