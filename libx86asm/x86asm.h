/*
 * x86asm.h - Universal x86 Assembly Emitter Library
 *
 * Copyright (c) 2025
 * BSD Licensed - See COPYING
 *
 * This library provides a unified interface for emitting x86 assembly
 * code in multiple assembler formats:
 *  - GNU AS (AT&T syntax)
 *  - Apple AS (Darwin/Mach-O)
 *  - MASM 1.x-6.11 (Microsoft Macro Assembler)
 *  - ML (Microsoft Macro Assembler 6.x+)
 *  - TASM (Borland Turbo Assembler)
 *  - WASM (Watcom Assembler)
 *  - OWASM (Open Watcom Assembler)
 *  - NASM (Netwide Assembler)
 *  - YASM (Yet Another Assembler)
 */

#ifndef X86ASM_H
#define X86ASM_H

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>

/* Assembler format types */
typedef enum {
    ASM_FMT_GNU_AS,      /* GNU assembler (gas) - AT&T syntax */
    ASM_FMT_APPLE_AS,    /* Apple's assembler (Darwin/Mach-O) */
    ASM_FMT_MASM,        /* Microsoft Macro Assembler 1.x-6.11 */
    ASM_FMT_ML,          /* Microsoft ML 6.x+ */
    ASM_FMT_TASM,        /* Borland Turbo Assembler */
    ASM_FMT_WASM,        /* Watcom Assembler */
    ASM_FMT_OWASM,       /* Open Watcom Assembler */
    ASM_FMT_NASM,        /* Netwide Assembler */
    ASM_FMT_YASM,        /* Yet Another Assembler */
    ASM_FMT_FASM,        /* Flat Assembler */
    ASM_FMT_JWASM,       /* JWasm (MASM-compatible) */
    ASM_FMT_UASM         /* UASM (JWasm successor) */
} x86asm_format_t;

/* Register types */
typedef enum {
    /* 8-bit registers */
    REG_AL, REG_CL, REG_DL, REG_BL, REG_AH, REG_CH, REG_DH, REG_BH,
    REG_SPL, REG_BPL, REG_SIL, REG_DIL,
    REG_R8B, REG_R9B, REG_R10B, REG_R11B, REG_R12B, REG_R13B, REG_R14B, REG_R15B,

    /* 16-bit registers */
    REG_AX, REG_CX, REG_DX, REG_BX, REG_SP, REG_BP, REG_SI, REG_DI,
    REG_R8W, REG_R9W, REG_R10W, REG_R11W, REG_R12W, REG_R13W, REG_R14W, REG_R15W,

    /* 32-bit registers */
    REG_EAX, REG_ECX, REG_EDX, REG_EBX, REG_ESP, REG_EBP, REG_ESI, REG_EDI,
    REG_R8D, REG_R9D, REG_R10D, REG_R11D, REG_R12D, REG_R13D, REG_R14D, REG_R15D,

    /* 64-bit registers */
    REG_RAX, REG_RCX, REG_RDX, REG_RBX, REG_RSP, REG_RBP, REG_RSI, REG_RDI,
    REG_R8, REG_R9, REG_R10, REG_R11, REG_R12, REG_R13, REG_R14, REG_R15,

    /* APX Extended 64-bit registers (R16-R31) */
    REG_R16, REG_R17, REG_R18, REG_R19, REG_R20, REG_R21, REG_R22, REG_R23,
    REG_R24, REG_R25, REG_R26, REG_R27, REG_R28, REG_R29, REG_R30, REG_R31,

    /* APX Extended 32-bit registers (R16D-R31D) */
    REG_R16D, REG_R17D, REG_R18D, REG_R19D, REG_R20D, REG_R21D, REG_R22D, REG_R23D,
    REG_R24D, REG_R25D, REG_R26D, REG_R27D, REG_R28D, REG_R29D, REG_R30D, REG_R31D,

    /* APX Extended 16-bit registers (R16W-R31W) */
    REG_R16W, REG_R17W, REG_R18W, REG_R19W, REG_R20W, REG_R21W, REG_R22W, REG_R23W,
    REG_R24W, REG_R25W, REG_R26W, REG_R27W, REG_R28W, REG_R29W, REG_R30W, REG_R31W,

    /* APX Extended 8-bit registers (R16B-R31B) */
    REG_R16B, REG_R17B, REG_R18B, REG_R19B, REG_R20B, REG_R21B, REG_R22B, REG_R23B,
    REG_R24B, REG_R25B, REG_R26B, REG_R27B, REG_R28B, REG_R29B, REG_R30B, REG_R31B,

    /* Segment registers */
    REG_ES, REG_CS, REG_SS, REG_DS, REG_FS, REG_GS,

    /* FPU registers */
    REG_ST0, REG_ST1, REG_ST2, REG_ST3, REG_ST4, REG_ST5, REG_ST6, REG_ST7,

    /* MMX registers */
    REG_MM0, REG_MM1, REG_MM2, REG_MM3, REG_MM4, REG_MM5, REG_MM6, REG_MM7,

    /* XMM registers */
    REG_XMM0, REG_XMM1, REG_XMM2, REG_XMM3, REG_XMM4, REG_XMM5, REG_XMM6, REG_XMM7,
    REG_XMM8, REG_XMM9, REG_XMM10, REG_XMM11, REG_XMM12, REG_XMM13, REG_XMM14, REG_XMM15,

    /* APX Extended XMM registers (XMM16-XMM31) */
    REG_XMM16, REG_XMM17, REG_XMM18, REG_XMM19, REG_XMM20, REG_XMM21, REG_XMM22, REG_XMM23,
    REG_XMM24, REG_XMM25, REG_XMM26, REG_XMM27, REG_XMM28, REG_XMM29, REG_XMM30, REG_XMM31,

    /* YMM registers (AVX) */
    REG_YMM0, REG_YMM1, REG_YMM2, REG_YMM3, REG_YMM4, REG_YMM5, REG_YMM6, REG_YMM7,
    REG_YMM8, REG_YMM9, REG_YMM10, REG_YMM11, REG_YMM12, REG_YMM13, REG_YMM14, REG_YMM15,

    /* APX Extended YMM registers (YMM16-YMM31) */
    REG_YMM16, REG_YMM17, REG_YMM18, REG_YMM19, REG_YMM20, REG_YMM21, REG_YMM22, REG_YMM23,
    REG_YMM24, REG_YMM25, REG_YMM26, REG_YMM27, REG_YMM28, REG_YMM29, REG_YMM30, REG_YMM31,

    /* ZMM registers (AVX-512) */
    REG_ZMM0, REG_ZMM1, REG_ZMM2, REG_ZMM3, REG_ZMM4, REG_ZMM5, REG_ZMM6, REG_ZMM7,
    REG_ZMM8, REG_ZMM9, REG_ZMM10, REG_ZMM11, REG_ZMM12, REG_ZMM13, REG_ZMM14, REG_ZMM15,
    REG_ZMM16, REG_ZMM17, REG_ZMM18, REG_ZMM19, REG_ZMM20, REG_ZMM21, REG_ZMM22, REG_ZMM23,
    REG_ZMM24, REG_ZMM25, REG_ZMM26, REG_ZMM27, REG_ZMM28, REG_ZMM29, REG_ZMM30, REG_ZMM31,

    /* Mask registers (AVX-512) */
    REG_K0, REG_K1, REG_K2, REG_K3, REG_K4, REG_K5, REG_K6, REG_K7,

    REG_NONE
} x86asm_reg_t;

/* Operand types */
typedef enum {
    OP_NONE,
    OP_REG,          /* Register operand */
    OP_IMM,          /* Immediate value */
    OP_MEM,          /* Memory reference */
    OP_LABEL         /* Label/symbol reference */
} x86asm_optype_t;

/* Memory operand structure */
typedef struct {
    x86asm_reg_t base;       /* Base register */
    x86asm_reg_t index;      /* Index register */
    int scale;               /* Scale factor (1, 2, 4, 8) */
    int64_t disp;            /* Displacement */
    const char *symbol;      /* Symbol name (if any) */
    x86asm_reg_t segment;    /* Segment override */
} x86asm_mem_t;

/* Operand structure */
typedef struct {
    x86asm_optype_t type;
    int size;                /* Size in bits (8, 16, 32, 64, 80, 128, 256) */
    union {
        x86asm_reg_t reg;
        int64_t imm;
        x86asm_mem_t mem;
        const char *label;
    } u;
} x86asm_operand_t;

/* Instruction prefix flags */
typedef enum {
    PREFIX_NONE   = 0,
    PREFIX_LOCK   = (1 << 0),
    PREFIX_REP    = (1 << 1),
    PREFIX_REPE   = (1 << 2),
    PREFIX_REPNE  = (1 << 3),
    PREFIX_REPZ   = (1 << 4),
    PREFIX_REPNZ  = (1 << 5)
} x86asm_prefix_t;

/* Segment/Section types */
typedef enum {
    /* Common sections (all formats) */
    SEG_TEXT,        /* Code section (.text) */
    SEG_DATA,        /* Initialized data (.data) */
    SEG_BSS,         /* Uninitialized data (.bss) */
    SEG_RODATA,      /* Read-only data (.rodata) */
    SEG_CONST,       /* Constants (.const) */

    /* ELF-specific sections */
    SEG_INIT,        /* Initialization code (.init) */
    SEG_FINI,        /* Finalization code (.fini) */
    SEG_INIT_ARRAY,  /* Array of init function pointers (.init_array) */
    SEG_FINI_ARRAY,  /* Array of fini function pointers (.fini_array) */
    SEG_PREINIT_ARRAY, /* Array of preinit function pointers (.preinit_array) */
    SEG_CTORS,       /* Global constructors (.ctors) */
    SEG_DTORS,       /* Global destructors (.dtors) */
    SEG_PLT,         /* Procedure linkage table (.plt) */
    SEG_GOT,         /* Global offset table (.got) */
    SEG_GOT_PLT,     /* GOT for PLT (.got.plt) */
    SEG_DYNAMIC,     /* Dynamic linking info (.dynamic) */
    SEG_DYNSYM,      /* Dynamic symbol table (.dynsym) */
    SEG_DYNSTR,      /* Dynamic string table (.dynstr) */
    SEG_HASH,        /* Symbol hash table (.hash) */
    SEG_GNU_HASH,    /* GNU-style hash table (.gnu.hash) */
    SEG_INTERP,      /* Program interpreter (.interp) */
    SEG_NOTE,        /* Note section (.note) */
    SEG_EH_FRAME,    /* Exception handling frames (.eh_frame) */
    SEG_EH_FRAME_HDR,/* Exception handling header (.eh_frame_hdr) */
    SEG_GCC_EXCEPT_TABLE, /* GCC exception table (.gcc_except_table) */
    SEG_STRTAB,      /* String table (.strtab) */
    SEG_SYMTAB,      /* Symbol table (.symtab) */
    SEG_SHSTRTAB,    /* Section header string table (.shstrtab) */
    SEG_REL,         /* Relocations without addends (.rel.*) */
    SEG_RELA,        /* Relocations with addends (.rela.*) */
    SEG_COMMENT,     /* Comments (.comment) */

    /* Thread-Local Storage (TLS) */
    SEG_TDATA,       /* TLS initialized data (.tdata) */
    SEG_TBSS,        /* TLS uninitialized data (.tbss) */

    /* Position Independent Code (PIC) */
    SEG_PIC_DATA,    /* PIC data (.data.rel.rw) */
    SEG_PIC_RODATA,  /* PIC read-only data (.data.rel.ro) */
    SEG_PIC_LOCAL,   /* PIC local data (.data.rel.local) */

    /* Debug sections (DWARF) */
    SEG_DEBUG_INFO,      /* Debug information (.debug_info) */
    SEG_DEBUG_ABBREV,    /* Debug abbreviations (.debug_abbrev) */
    SEG_DEBUG_LINE,      /* Line number info (.debug_line) */
    SEG_DEBUG_STR,       /* Debug strings (.debug_str) */
    SEG_DEBUG_LOC,       /* Location lists (.debug_loc) */
    SEG_DEBUG_RANGES,    /* Address ranges (.debug_ranges) */
    SEG_DEBUG_FRAME,     /* Call frame info (.debug_frame) */
    SEG_DEBUG_MACINFO,   /* Macro info (.debug_macinfo) */
    SEG_DEBUG_PUBNAMES,  /* Public names (.debug_pubnames) */
    SEG_DEBUG_PUBTYPES,  /* Public types (.debug_pubtypes) */
    SEG_DEBUG_ARANGES,   /* Address ranges (.debug_aranges) */

    /* PE/COFF-specific sections */
    SEG_IDATA,       /* Import data (.idata) */
    SEG_EDATA,       /* Export data (.edata) */
    SEG_PDATA,       /* Exception handler data (.pdata) */
    SEG_XDATA,       /* Exception handler data (.xdata) */
    SEG_RELOC,       /* Base relocations (.reloc) */
    SEG_RSRC,        /* Resources (.rsrc) */
    SEG_TLS,         /* Thread-local storage (.tls) */
    SEG_RDATA,       /* Read-only data (.rdata) */
    SEG_DRECTVE,     /* Linker directives (.drectve) */
    SEG_DEBUG,       /* Debug data (.debug) */

    /* Mach-O sections */
    SEG_CSTRING,     /* C string literals (__cstring) */
    SEG_LITERAL4,    /* 4-byte literals (__literal4) */
    SEG_LITERAL8,    /* 8-byte literals (__literal8) */
    SEG_LITERAL16,   /* 16-byte literals (__literal16) */
    SEG_MOD_INIT_FUNC, /* Module init functions (__mod_init_func) */
    SEG_MOD_TERM_FUNC, /* Module term functions (__mod_term_func) */
    SEG_OBJC_CLASSLIST,  /* Objective-C class list */
    SEG_OBJC_CATLIST,    /* Objective-C category list */
    SEG_OBJC_PROTOLIST,  /* Objective-C protocol list */
    SEG_OBJC_IMAGEINFO,  /* Objective-C image info */
    SEG_OBJC_CONST,      /* Objective-C constants */
    SEG_OBJC_DATA,       /* Objective-C data */

    /* Special sections */
    SEG_STACK,       /* Stack section */
    SEG_HEAP,        /* Heap section */
    SEG_CUSTOM       /* Custom section (use string name) */
} x86asm_segment_t;

/* Data size types */
typedef enum {
    DATA_BYTE,       /* 8-bit */
    DATA_WORD,       /* 16-bit */
    DATA_DWORD,      /* 32-bit */
    DATA_QWORD,      /* 64-bit */
    DATA_TBYTE,      /* 80-bit (10 bytes) */
    DATA_OWORD,      /* 128-bit (16 bytes) */
    DATA_YWORD,      /* 256-bit (32 bytes) */
    DATA_FLOAT,      /* 32-bit float */
    DATA_DOUBLE,     /* 64-bit double */
    DATA_LDOUBLE,    /* 80-bit long double */
    DATA_STRING,     /* String/array of bytes */
    DATA_ASCIZ       /* Null-terminated string */
} x86asm_datasize_t;

/* Emitter context */
typedef struct x86asm_ctx x86asm_ctx_t;

/* Function pointer types for emitter operations */
typedef void (*x86asm_emit_fn)(x86asm_ctx_t *ctx, const char *mnemonic,
                               x86asm_operand_t *ops, int nops,
                               x86asm_prefix_t prefix);
typedef void (*x86asm_directive_fn)(x86asm_ctx_t *ctx, const char *name,
                                    const char *value);
typedef void (*x86asm_label_fn)(x86asm_ctx_t *ctx, const char *name, int global);
typedef void (*x86asm_segment_fn)(x86asm_ctx_t *ctx, x86asm_segment_t seg,
                                  const char *name);
typedef void (*x86asm_data_fn)(x86asm_ctx_t *ctx, x86asm_datasize_t size,
                               const void *data, size_t count);
typedef void (*x86asm_comment_fn)(x86asm_ctx_t *ctx, const char *text);
typedef void (*x86asm_align_fn)(x86asm_ctx_t *ctx, int alignment);

/* Emitter operations table */
typedef struct {
    x86asm_emit_fn      emit_insn;
    x86asm_directive_fn emit_directive;
    x86asm_label_fn     emit_label;
    x86asm_segment_fn   emit_segment;
    x86asm_data_fn      emit_data;
    x86asm_comment_fn   emit_comment;
    x86asm_align_fn     emit_align;
} x86asm_ops_t;

/* Emitter context structure */
struct x86asm_ctx {
    x86asm_format_t format;     /* Target assembler format */
    FILE *output;               /* Output stream */
    const x86asm_ops_t *ops;    /* Emitter operations */
    void *user_data;            /* User-defined data */
    int bits;                   /* Target mode: 16, 32, or 64 */
    int indent;                 /* Indentation level */
    int flags;                  /* Format-specific flags */
};

/* API Functions */

/* Initialize emitter context */
x86asm_ctx_t *x86asm_create(x86asm_format_t format, FILE *output, int bits);

/* Destroy emitter context */
void x86asm_destroy(x86asm_ctx_t *ctx);

/* Set format-specific flags */
void x86asm_set_flags(x86asm_ctx_t *ctx, int flags);

/* Emit an instruction */
void x86asm_insn(x86asm_ctx_t *ctx, const char *mnemonic,
                 x86asm_operand_t *ops, int nops, x86asm_prefix_t prefix);

/* Emit a label */
void x86asm_label(x86asm_ctx_t *ctx, const char *name, int global);

/* Emit a segment/section directive */
void x86asm_segment(x86asm_ctx_t *ctx, x86asm_segment_t seg, const char *name);

/* Emit data */
void x86asm_data(x86asm_ctx_t *ctx, x86asm_datasize_t size,
                 const void *data, size_t count);

/* Emit a comment */
void x86asm_comment(x86asm_ctx_t *ctx, const char *text);

/* Emit an alignment directive */
void x86asm_align(x86asm_ctx_t *ctx, int alignment);

/* Emit a raw directive */
void x86asm_directive(x86asm_ctx_t *ctx, const char *name, const char *value);

/* High-level directive functions */

/* Symbol type (for ELF: .type symbol,@function or @object) */
typedef enum {
	SYMBOL_TYPE_FUNCTION,
	SYMBOL_TYPE_OBJECT,
	SYMBOL_TYPE_NOTYPE
} x86asm_symbol_type_t;

void x86asm_symbol_type(x86asm_ctx_t *ctx, const char *symbol,
                        x86asm_symbol_type_t type);

/* Symbol size (for ELF: .size symbol,size) */
void x86asm_symbol_size(x86asm_ctx_t *ctx, const char *symbol, size_t size);

/* Ident/version string (e.g., .ident "compiler version") */
void x86asm_ident(x86asm_ctx_t *ctx, const char *ident_string);

/* Indirect symbol (for Mach-O: .indirect_symbol symbol) */
void x86asm_indirect_symbol(x86asm_ctx_t *ctx, const char *symbol);

/* End directive (for some formats like ELF: .end) */
void x86asm_end(x86asm_ctx_t *ctx);

/* Common/local common directives (for BSS allocation) */
void x86asm_comm(x86asm_ctx_t *ctx, const char *symbol, size_t size, int alignment);
void x86asm_lcomm(x86asm_ctx_t *ctx, const char *symbol, size_t size, int alignment);

/* Symbol visibility directives */
void x86asm_local(x86asm_ctx_t *ctx, const char *symbol);
void x86asm_hidden(x86asm_ctx_t *ctx, const char *symbol);
void x86asm_weak(x86asm_ctx_t *ctx, const char *symbol);
void x86asm_weakref(x86asm_ctx_t *ctx, const char *symbol, const char *target);

/* Symbol aliasing directive */
void x86asm_set(x86asm_ctx_t *ctx, const char *symbol, const char *value);

/* Section management directives */
void x86asm_previous(x86asm_ctx_t *ctx);
void x86asm_p2align(x86asm_ctx_t *ctx, int power);

/* Helper functions for creating operands */
x86asm_operand_t x86asm_op_reg(x86asm_reg_t reg, int size);
x86asm_operand_t x86asm_op_imm(int64_t value, int size);
x86asm_operand_t x86asm_op_mem(x86asm_reg_t base, x86asm_reg_t index,
                               int scale, int64_t disp, int size);
x86asm_operand_t x86asm_op_mem_symbol(const char *symbol, int64_t disp, int size);
x86asm_operand_t x86asm_op_label(const char *label);

/* Get register name for a given format */
const char *x86asm_reg_name(x86asm_ctx_t *ctx, x86asm_reg_t reg);

/* Convenience macros for common operations */
#define X86ASM_INSN0(ctx, mnem) \
    x86asm_insn(ctx, mnem, NULL, 0, PREFIX_NONE)

#define X86ASM_INSN1(ctx, mnem, op1) \
    do { \
        x86asm_operand_t ops[] = { op1 }; \
        x86asm_insn(ctx, mnem, ops, 1, PREFIX_NONE); \
    } while(0)

#define X86ASM_INSN2(ctx, mnem, op1, op2) \
    do { \
        x86asm_operand_t ops[] = { op1, op2 }; \
        x86asm_insn(ctx, mnem, ops, 2, PREFIX_NONE); \
    } while(0)

#define X86ASM_INSN3(ctx, mnem, op1, op2, op3) \
    do { \
        x86asm_operand_t ops[] = { op1, op2, op3 }; \
        x86asm_insn(ctx, mnem, ops, 3, PREFIX_NONE); \
    } while(0)

#endif /* X86ASM_H */
