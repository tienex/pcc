/*
 * emit_apple_as.c - Apple Assembler (Mach-O) emitter
 *
 * Copyright (c) 2025
 * BSD Licensed - See COPYING
 *
 * Emits x86 assembly in Apple AS (AT&T syntax) format for Mach-O
 */

#include "x86asm_internal.h"
#include <stdio.h>
#include <string.h>

/* Forward declarations */
static void apple_as_emit_insn(x86asm_ctx_t *ctx, const char *mnemonic,
                               x86asm_operand_t *ops, int nops,
                               x86asm_prefix_t prefix);
static void apple_as_emit_directive(x86asm_ctx_t *ctx, const char *name,
                                    const char *value);
static void apple_as_emit_label(x86asm_ctx_t *ctx, const char *name, int global);
static void apple_as_emit_segment(x86asm_ctx_t *ctx, x86asm_segment_t seg,
                                  const char *name);
static void apple_as_emit_data(x86asm_ctx_t *ctx, x86asm_datasize_t size,
                               const void *data, size_t count);
static void apple_as_emit_comment(x86asm_ctx_t *ctx, const char *text);
static void apple_as_emit_align(x86asm_ctx_t *ctx, int alignment);

/* Operations table for Apple AS */
const x86asm_ops_t x86asm_ops_apple_as = {
    apple_as_emit_insn,
    apple_as_emit_directive,
    apple_as_emit_label,
    apple_as_emit_segment,
    apple_as_emit_data,
    apple_as_emit_comment,
    apple_as_emit_align
};

/*
 * Format an operand for Apple AS (AT&T syntax, similar to GNU AS)
 */
static void
apple_as_format_operand(x86asm_ctx_t *ctx, x86asm_operand_t *op, char *buf, size_t bufsize)
{
    const char *regname;

    switch (op->type) {
    case OP_REG:
        regname = x86asm_reg_name_internal(ASM_FMT_APPLE_AS, op->u.reg, ctx->bits);
        snprintf(buf, bufsize, "%s", regname ? regname : "???");
        break;

    case OP_IMM:
        snprintf(buf, bufsize, "$%lld", (long long)op->u.imm);
        break;

    case OP_MEM:
        /* AT&T syntax: disp(base, index, scale) */
        if (op->u.mem.symbol) {
            if (op->u.mem.base != REG_NONE || op->u.mem.index != REG_NONE) {
                char base_str[32] = "";
                char index_str[64] = "";

                if (op->u.mem.base != REG_NONE) {
                    regname = x86asm_reg_name_internal(ASM_FMT_APPLE_AS, op->u.mem.base, ctx->bits);
                    snprintf(base_str, sizeof(base_str), "%s", regname);
                }

                if (op->u.mem.index != REG_NONE) {
                    const char *idx_name = x86asm_reg_name_internal(ASM_FMT_APPLE_AS, op->u.mem.index, ctx->bits);
                    snprintf(index_str, sizeof(index_str), ",%s,%d", idx_name, op->u.mem.scale);
                }

                if (op->u.mem.disp != 0) {
                    snprintf(buf, bufsize, "%s%+lld(%s%s)", op->u.mem.symbol,
                             (long long)op->u.mem.disp, base_str, index_str);
                } else {
                    snprintf(buf, bufsize, "%s(%s%s)", op->u.mem.symbol, base_str, index_str);
                }
            } else {
                if (op->u.mem.disp != 0) {
                    snprintf(buf, bufsize, "%s%+lld", op->u.mem.symbol, (long long)op->u.mem.disp);
                } else {
                    snprintf(buf, bufsize, "%s", op->u.mem.symbol);
                }
            }
        } else {
            char base_str[32] = "";
            char index_str[64] = "";

            if (op->u.mem.base != REG_NONE) {
                regname = x86asm_reg_name_internal(ASM_FMT_APPLE_AS, op->u.mem.base, ctx->bits);
                snprintf(base_str, sizeof(base_str), "%s", regname);
            }

            if (op->u.mem.index != REG_NONE) {
                const char *idx_name = x86asm_reg_name_internal(ASM_FMT_APPLE_AS, op->u.mem.index, ctx->bits);
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
apple_as_emit_insn(x86asm_ctx_t *ctx, const char *mnemonic,
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
        apple_as_format_operand(ctx, &ops[i], op_bufs[i], sizeof(op_bufs[i]));
    }

    /* Emit instruction */
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
apple_as_emit_directive(x86asm_ctx_t *ctx, const char *name, const char *value)
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
apple_as_emit_label(x86asm_ctx_t *ctx, const char *name, int global)
{
    if (global)
        fprintf(ctx->output, "\t.globl _%s\n", name);  /* Apple adds underscore prefix */
    fprintf(ctx->output, "_%s:\n", name);
}

/*
 * Emit a segment/section directive (Mach-O specific)
 */
static void
apple_as_emit_segment(x86asm_ctx_t *ctx, x86asm_segment_t seg, const char *name)
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
    case SEG_BSS:         seg_name = ".section __DATA,__bss"; break;
    case SEG_RODATA:      seg_name = ".const"; break;
    case SEG_CONST:       seg_name = ".const"; break;

    /* Mach-O specific sections */
    case SEG_CSTRING:     seg_name = ".cstring"; break;
    case SEG_LITERAL4:    seg_name = ".literal4"; break;
    case SEG_LITERAL8:    seg_name = ".literal8"; break;
    case SEG_LITERAL16:   seg_name = ".literal16"; break;
    case SEG_MOD_INIT_FUNC: seg_name = ".mod_init_func"; break;
    case SEG_MOD_TERM_FUNC: seg_name = ".mod_term_func"; break;

    /* Objective-C sections */
    case SEG_OBJC_CLASSLIST: seg_name = ".section __DATA,__objc_classlist,regular,no_dead_strip"; break;
    case SEG_OBJC_CATLIST:   seg_name = ".section __DATA,__objc_catlist,regular,no_dead_strip"; break;
    case SEG_OBJC_PROTOLIST: seg_name = ".section __DATA,__objc_protolist,coalesced,no_dead_strip"; break;
    case SEG_OBJC_IMAGEINFO: seg_name = ".section __DATA,__objc_imageinfo,regular,no_dead_strip"; break;
    case SEG_OBJC_CONST:     seg_name = ".section __DATA,__objc_const"; break;
    case SEG_OBJC_DATA:      seg_name = ".section __DATA,__objc_data"; break;

    /* Constructor/Destructor (Mach-O style) */
    case SEG_CTORS:       seg_name = ".mod_init_func"; break;
    case SEG_DTORS:       seg_name = ".mod_term_func"; break;

    /* Thread-Local Storage */
    case SEG_TDATA:       seg_name = ".section __DATA,__thread_data,thread_local_regular"; break;
    case SEG_TBSS:        seg_name = ".section __DATA,__thread_bss,thread_local_zerofill"; break;

    /* Exception handling */
    case SEG_EH_FRAME:    seg_name = ".section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support"; break;

    /* Debug sections (DWARF for Mach-O) */
    case SEG_DEBUG_INFO:     seg_name = ".section __DWARF,__debug_info,regular,debug"; break;
    case SEG_DEBUG_ABBREV:   seg_name = ".section __DWARF,__debug_abbrev,regular,debug"; break;
    case SEG_DEBUG_LINE:     seg_name = ".section __DWARF,__debug_line,regular,debug"; break;
    case SEG_DEBUG_STR:      seg_name = ".section __DWARF,__debug_str,regular,debug"; break;
    case SEG_DEBUG_LOC:      seg_name = ".section __DWARF,__debug_loc,regular,debug"; break;
    case SEG_DEBUG_RANGES:   seg_name = ".section __DWARF,__debug_ranges,regular,debug"; break;
    case SEG_DEBUG_FRAME:    seg_name = ".section __DWARF,__debug_frame,regular,debug"; break;
    case SEG_DEBUG_MACINFO:  seg_name = ".section __DWARF,__debug_macinfo,regular,debug"; break;
    case SEG_DEBUG_PUBNAMES: seg_name = ".section __DWARF,__debug_pubnames,regular,debug"; break;
    case SEG_DEBUG_PUBTYPES: seg_name = ".section __DWARF,__debug_pubtypes,regular,debug"; break;
    case SEG_DEBUG_ARANGES:  seg_name = ".section __DWARF,__debug_aranges,regular,debug"; break;

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
apple_as_emit_data(x86asm_ctx_t *ctx, x86asm_datasize_t size,
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
apple_as_emit_comment(x86asm_ctx_t *ctx, const char *text)
{
    fprintf(ctx->output, "\t# %s\n", text);
}

/*
 * Emit an alignment directive (Apple uses power-of-2 for alignment)
 */
static void
apple_as_emit_align(x86asm_ctx_t *ctx, int alignment)
{
    /* Apple AS uses .align with log2 of alignment */
    int log2 = 0;
    int temp = alignment;
    while (temp > 1) {
        temp >>= 1;
        log2++;
    }
    fprintf(ctx->output, "\t.align %d\n", log2);
}
