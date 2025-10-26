/*
 * emit_gnu_as.c - GNU Assembler (gas) emitter
 *
 * Copyright (c) 2025
 * BSD Licensed - See COPYING
 *
 * Emits x86 assembly in GNU AS (AT&T syntax) format
 */

#include "x86asm_internal.h"
#include <stdio.h>
#include <string.h>

/* Forward declarations */
static void gnu_as_emit_insn(x86asm_ctx_t *ctx, const char *mnemonic,
                             x86asm_operand_t *ops, int nops,
                             x86asm_prefix_t prefix);
static void gnu_as_emit_directive(x86asm_ctx_t *ctx, const char *name,
                                  const char *value);
static void gnu_as_emit_label(x86asm_ctx_t *ctx, const char *name, int global);
static void gnu_as_emit_segment(x86asm_ctx_t *ctx, x86asm_segment_t seg,
                                const char *name);
static void gnu_as_emit_data(x86asm_ctx_t *ctx, x86asm_datasize_t size,
                             const void *data, size_t count);
static void gnu_as_emit_comment(x86asm_ctx_t *ctx, const char *text);
static void gnu_as_emit_align(x86asm_ctx_t *ctx, int alignment);

/* Operations table for GNU AS */
const x86asm_ops_t x86asm_ops_gnu_as = {
    gnu_as_emit_insn,
    gnu_as_emit_directive,
    gnu_as_emit_label,
    gnu_as_emit_segment,
    gnu_as_emit_data,
    gnu_as_emit_comment,
    gnu_as_emit_align
};

/*
 * Format an operand for GNU AS (AT&T syntax)
 */
static void
gnu_as_format_operand(x86asm_ctx_t *ctx, x86asm_operand_t *op, char *buf, size_t bufsize)
{
    const char *regname;

    switch (op->type) {
    case OP_REG:
        regname = x86asm_reg_name_internal(ASM_FMT_GNU_AS, op->u.reg, ctx->bits);
        snprintf(buf, bufsize, "%s", regname ? regname : "???");
        break;

    case OP_IMM:
        snprintf(buf, bufsize, "$%lld", (long long)op->u.imm);
        break;

    case OP_MEM:
        /* AT&T syntax: disp(base, index, scale) or symbol+disp(base, index, scale) */
        if (op->u.mem.symbol) {
            if (op->u.mem.base != REG_NONE || op->u.mem.index != REG_NONE) {
                /* Complex: symbol+disp(base, index, scale) */
                char base_str[32] = "";
                char index_str[64] = "";

                if (op->u.mem.base != REG_NONE) {
                    regname = x86asm_reg_name_internal(ASM_FMT_GNU_AS, op->u.mem.base, ctx->bits);
                    snprintf(base_str, sizeof(base_str), "%s", regname);
                }

                if (op->u.mem.index != REG_NONE) {
                    const char *idx_name = x86asm_reg_name_internal(ASM_FMT_GNU_AS, op->u.mem.index, ctx->bits);
                    snprintf(index_str, sizeof(index_str), ",%s,%d", idx_name, op->u.mem.scale);
                }

                if (op->u.mem.disp != 0) {
                    snprintf(buf, bufsize, "%s%+lld(%s%s)", op->u.mem.symbol,
                             (long long)op->u.mem.disp, base_str, index_str);
                } else {
                    snprintf(buf, bufsize, "%s(%s%s)", op->u.mem.symbol, base_str, index_str);
                }
            } else {
                /* Simple: symbol or symbol+disp */
                if (op->u.mem.disp != 0) {
                    snprintf(buf, bufsize, "%s%+lld", op->u.mem.symbol, (long long)op->u.mem.disp);
                } else {
                    snprintf(buf, bufsize, "%s", op->u.mem.symbol);
                }
            }
        } else {
            /* Numeric memory reference */
            char base_str[32] = "";
            char index_str[64] = "";

            if (op->u.mem.base != REG_NONE) {
                regname = x86asm_reg_name_internal(ASM_FMT_GNU_AS, op->u.mem.base, ctx->bits);
                snprintf(base_str, sizeof(base_str), "%s", regname);
            }

            if (op->u.mem.index != REG_NONE) {
                const char *idx_name = x86asm_reg_name_internal(ASM_FMT_GNU_AS, op->u.mem.index, ctx->bits);
                snprintf(index_str, sizeof(index_str), ",%s,%d", idx_name, op->u.mem.scale);
            }

            if (op->u.mem.base != REG_NONE || op->u.mem.index != REG_NONE) {
                if (op->u.mem.disp != 0) {
                    snprintf(buf, bufsize, "%lld(%s%s)", (long long)op->u.mem.disp, base_str, index_str);
                } else {
                    snprintf(buf, bufsize, "(%s%s)", base_str, index_str);
                }
            } else {
                snprintf(buf, bufsize, "%lld", (long long)op->u.mem.disp);
            }
        }
        break;

    case OP_LABEL:
        snprintf(buf, bufsize, "%s", op->u.label);
        break;

    default:
        snprintf(buf, bufsize, "???");
        break;
    }
}

/*
 * Emit an instruction
 */
static void
gnu_as_emit_insn(x86asm_ctx_t *ctx, const char *mnemonic,
                 x86asm_operand_t *ops, int nops, x86asm_prefix_t prefix)
{
    char op_bufs[3][128];
    int i;

    /* Emit prefix if present */
    if (prefix & PREFIX_LOCK)
        fprintf(ctx->output, "\tlock\n");
    if (prefix & (PREFIX_REP | PREFIX_REPE | PREFIX_REPZ))
        fprintf(ctx->output, "\trep\n");
    if (prefix & (PREFIX_REPNE | PREFIX_REPNZ))
        fprintf(ctx->output, "\trepnz\n");

    /* Format operands */
    for (i = 0; i < nops && i < 3; i++) {
        gnu_as_format_operand(ctx, &ops[i], op_bufs[i], sizeof(op_bufs[i]));
    }

    /* Emit instruction (AT&T syntax is source, destination) */
    fprintf(ctx->output, "\t%s", mnemonic);

    if (nops > 0) {
        fprintf(ctx->output, " %s", op_bufs[0]);
        for (i = 1; i < nops; i++) {
            fprintf(ctx->output, ", %s", op_bufs[i]);
        }
    }

    fprintf(ctx->output, "\n");
}

/*
 * Emit a directive
 */
static void
gnu_as_emit_directive(x86asm_ctx_t *ctx, const char *name, const char *value)
{
    if (value)
        fprintf(ctx->output, "\t.%s %s\n", name, value);
    else
        fprintf(ctx->output, "\t.%s\n", name);
}

/*
 * Emit a label
 */
static void
gnu_as_emit_label(x86asm_ctx_t *ctx, const char *name, int global)
{
    if (global)
        fprintf(ctx->output, "\t.globl %s\n", name);
    fprintf(ctx->output, "%s:\n", name);
}

/*
 * Emit a segment/section directive
 */
static void
gnu_as_emit_segment(x86asm_ctx_t *ctx, x86asm_segment_t seg, const char *name)
{
    const char *seg_name;

    if (name) {
        fprintf(ctx->output, "\t.section %s\n", name);
        return;
    }

    switch (seg) {
    /* Common sections */
    case SEG_TEXT:        seg_name = ".text"; break;
    case SEG_DATA:        seg_name = ".data"; break;
    case SEG_BSS:         seg_name = ".bss"; break;
    case SEG_RODATA:      seg_name = ".section .rodata"; break;
    case SEG_CONST:       seg_name = ".section .rodata"; break;

    /* ELF initialization/finalization */
    case SEG_INIT:        seg_name = ".section .init"; break;
    case SEG_FINI:        seg_name = ".section .fini"; break;
    case SEG_INIT_ARRAY:  seg_name = ".section .init_array,\"aw\""; break;
    case SEG_FINI_ARRAY:  seg_name = ".section .fini_array,\"aw\""; break;
    case SEG_PREINIT_ARRAY: seg_name = ".section .preinit_array,\"aw\""; break;
    case SEG_CTORS:       seg_name = ".section .ctors,\"aw\",@progbits"; break;
    case SEG_DTORS:       seg_name = ".section .dtors,\"aw\",@progbits"; break;

    /* Dynamic linking sections */
    case SEG_PLT:         seg_name = ".section .plt,\"ax\",@progbits"; break;
    case SEG_GOT:         seg_name = ".section .got,\"aw\",@progbits"; break;
    case SEG_GOT_PLT:     seg_name = ".section .got.plt,\"aw\",@progbits"; break;
    case SEG_DYNAMIC:     seg_name = ".section .dynamic,\"aw\",@progbits"; break;
    case SEG_DYNSYM:      seg_name = ".section .dynsym,\"a\",@progbits"; break;
    case SEG_DYNSTR:      seg_name = ".section .dynstr,\"aMS\",@progbits,1"; break;
    case SEG_HASH:        seg_name = ".section .hash,\"a\",@hash"; break;
    case SEG_GNU_HASH:    seg_name = ".section .gnu.hash,\"a\",@gnu.hash"; break;
    case SEG_INTERP:      seg_name = ".section .interp,\"a\",@progbits"; break;

    /* Exception handling and debugging */
    case SEG_EH_FRAME:    seg_name = ".section .eh_frame,\"a\",@progbits"; break;
    case SEG_EH_FRAME_HDR: seg_name = ".section .eh_frame_hdr,\"a\",@progbits"; break;
    case SEG_GCC_EXCEPT_TABLE: seg_name = ".section .gcc_except_table,\"a\",@progbits"; break;

    /* Symbol and relocation tables */
    case SEG_SYMTAB:      seg_name = ".section .symtab,\"a\",@progbits"; break;
    case SEG_STRTAB:      seg_name = ".section .strtab,\"aMS\",@progbits,1"; break;
    case SEG_SHSTRTAB:    seg_name = ".section .shstrtab,\"aMS\",@progbits,1"; break;
    case SEG_REL:         seg_name = ".section .rel,\"a\",@progbits"; break;
    case SEG_RELA:        seg_name = ".section .rela,\"a\",@progbits"; break;

    /* Miscellaneous */
    case SEG_NOTE:        seg_name = ".section .note,\"a\",@note"; break;
    case SEG_COMMENT:     seg_name = ".section .comment,\"MS\",@progbits,1"; break;

    /* Thread-Local Storage */
    case SEG_TDATA:       seg_name = ".section .tdata,\"awT\",@progbits"; break;
    case SEG_TBSS:        seg_name = ".section .tbss,\"awT\",@nobits"; break;

    /* Position Independent Code */
    case SEG_PIC_DATA:    seg_name = ".section .data.rel.rw,\"aw\",@progbits"; break;
    case SEG_PIC_RODATA:  seg_name = ".section .data.rel.ro,\"aw\",@progbits"; break;
    case SEG_PIC_LOCAL:   seg_name = ".section .data.rel.local,\"aw\",@progbits"; break;

    /* DWARF debug sections */
    case SEG_DEBUG_INFO:     seg_name = ".section .debug_info,\"\",@progbits"; break;
    case SEG_DEBUG_ABBREV:   seg_name = ".section .debug_abbrev,\"\",@progbits"; break;
    case SEG_DEBUG_LINE:     seg_name = ".section .debug_line,\"\",@progbits"; break;
    case SEG_DEBUG_STR:      seg_name = ".section .debug_str,\"MS\",@progbits,1"; break;
    case SEG_DEBUG_LOC:      seg_name = ".section .debug_loc,\"\",@progbits"; break;
    case SEG_DEBUG_RANGES:   seg_name = ".section .debug_ranges,\"\",@progbits"; break;
    case SEG_DEBUG_FRAME:    seg_name = ".section .debug_frame,\"\",@progbits"; break;
    case SEG_DEBUG_MACINFO:  seg_name = ".section .debug_macinfo,\"\",@progbits"; break;
    case SEG_DEBUG_PUBNAMES: seg_name = ".section .debug_pubnames,\"\",@progbits"; break;
    case SEG_DEBUG_PUBTYPES: seg_name = ".section .debug_pubtypes,\"\",@progbits"; break;
    case SEG_DEBUG_ARANGES:  seg_name = ".section .debug_aranges,\"\",@progbits"; break;

    /* PE/COFF sections (limited support in ELF tools) */
    case SEG_RDATA:       seg_name = ".section .rdata,\"a\",@progbits"; break;

    /* Default */
    default:
        seg_name = ".text";
        break;
    }

    fprintf(ctx->output, "\t%s\n", seg_name);
}

/*
 * Emit data
 */
static void
gnu_as_emit_data(x86asm_ctx_t *ctx, x86asm_datasize_t size,
                 const void *data, size_t count)
{
    const char *directive;
    const uint8_t *bytes = (const uint8_t *)data;
    const uint16_t *words;
    const uint32_t *dwords;
    const uint64_t *qwords;
    size_t i;

    switch (size) {
    case DATA_BYTE:
        directive = ".byte";
        fprintf(ctx->output, "\t%s ", directive);
        for (i = 0; i < count; i++) {
            fprintf(ctx->output, "%s0x%02x", i > 0 ? ", " : "", bytes[i]);
        }
        fprintf(ctx->output, "\n");
        break;

    case DATA_WORD:
        directive = ".word";
        words = (const uint16_t *)data;
        fprintf(ctx->output, "\t%s ", directive);
        for (i = 0; i < count; i++) {
            fprintf(ctx->output, "%s0x%04x", i > 0 ? ", " : "", words[i]);
        }
        fprintf(ctx->output, "\n");
        break;

    case DATA_DWORD:
        directive = ".long";
        dwords = (const uint32_t *)data;
        fprintf(ctx->output, "\t%s ", directive);
        for (i = 0; i < count; i++) {
            fprintf(ctx->output, "%s0x%08x", i > 0 ? ", " : "", dwords[i]);
        }
        fprintf(ctx->output, "\n");
        break;

    case DATA_QWORD:
        directive = ".quad";
        qwords = (const uint64_t *)data;
        fprintf(ctx->output, "\t%s ", directive);
        for (i = 0; i < count; i++) {
            fprintf(ctx->output, "%s0x%016llx", i > 0 ? ", " : "",
                    (unsigned long long)qwords[i]);
        }
        fprintf(ctx->output, "\n");
        break;

    case DATA_STRING:
        fprintf(ctx->output, "\t.byte ");
        for (i = 0; i < count; i++) {
            fprintf(ctx->output, "%s0x%02x", i > 0 ? ", " : "", bytes[i]);
        }
        fprintf(ctx->output, "\n");
        break;

    case DATA_ASCIZ:
        fprintf(ctx->output, "\t.asciz \"");
        for (i = 0; i < count; i++) {
            char c = bytes[i];
            if (c == '"' || c == '\\')
                fprintf(ctx->output, "\\%c", c);
            else if (c >= 32 && c < 127)
                fprintf(ctx->output, "%c", c);
            else
                fprintf(ctx->output, "\\x%02x", (unsigned char)c);
        }
        fprintf(ctx->output, "\"\n");
        break;

    default:
        break;
    }
}

/*
 * Emit a comment
 */
static void
gnu_as_emit_comment(x86asm_ctx_t *ctx, const char *text)
{
    fprintf(ctx->output, "\t# %s\n", text);
}

/*
 * Emit an alignment directive
 */
static void
gnu_as_emit_align(x86asm_ctx_t *ctx, int alignment)
{
    fprintf(ctx->output, "\t.align %d\n", alignment);
}
