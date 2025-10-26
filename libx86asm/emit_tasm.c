/*
 * emit_tasm.c - Borland Turbo Assembler (TASM) emitter
 *
 * Copyright (c) 2025
 * BSD Licensed - See COPYING
 *
 * Emits x86 assembly in TASM (Intel syntax) format
 * TASM is largely compatible with MASM but has some differences
 */

#include "x86asm_internal.h"
#include <stdio.h>
#include <string.h>

/* Forward declarations */
static void tasm_emit_insn(x86asm_ctx_t *ctx, const char *mnemonic,
                           x86asm_operand_t *ops, int nops,
                           x86asm_prefix_t prefix);
static void tasm_emit_directive(x86asm_ctx_t *ctx, const char *name,
                                const char *value);
static void tasm_emit_label(x86asm_ctx_t *ctx, const char *name, int global);
static void tasm_emit_segment(x86asm_ctx_t *ctx, x86asm_segment_t seg,
                              const char *name);
static void tasm_emit_data(x86asm_ctx_t *ctx, x86asm_datasize_t size,
                           const void *data, size_t count);
static void tasm_emit_comment(x86asm_ctx_t *ctx, const char *text);
static void tasm_emit_align(x86asm_ctx_t *ctx, int alignment);

/* Operations table for TASM */
const x86asm_ops_t x86asm_ops_tasm = {
    tasm_emit_insn,
    tasm_emit_directive,
    tasm_emit_label,
    tasm_emit_segment,
    tasm_emit_data,
    tasm_emit_comment,
    tasm_emit_align
};

/*
 * Get size specifier for TASM
 */
static const char *
tasm_size_spec(int size)
{
    switch (size) {
    case 8:   return "BYTE PTR";
    case 16:  return "WORD PTR";
    case 32:  return "DWORD PTR";
    case 64:  return "QWORD PTR";
    case 80:  return "TBYTE PTR";
    default:  return "";
    }
}

/*
 * Format an operand for TASM (Intel syntax, similar to MASM)
 */
static void
tasm_format_operand(x86asm_ctx_t *ctx, x86asm_operand_t *op, char *buf, size_t bufsize)
{
    const char *regname;

    switch (op->type) {
    case OP_REG:
        regname = x86asm_reg_name_internal(ASM_FMT_TASM, op->u.reg, ctx->bits);
        snprintf(buf, bufsize, "%s", regname ? regname : "???");
        break;

    case OP_IMM:
        snprintf(buf, bufsize, "%lld", (long long)op->u.imm);
        break;

    case OP_MEM:
        /* Intel syntax: [base + index*scale + disp] */
        if (op->size > 0) {
            snprintf(buf, bufsize, "%s ", tasm_size_spec(op->size));
        } else {
            buf[0] = '\0';
        }

        strcat(buf, "[");

        if (op->u.mem.symbol) {
            strcat(buf, op->u.mem.symbol);
        }

        if (op->u.mem.base != REG_NONE) {
            regname = x86asm_reg_name_internal(ASM_FMT_TASM, op->u.mem.base, ctx->bits);
            if (op->u.mem.symbol)
                strcat(buf, " + ");
            strcat(buf, regname);
        }

        if (op->u.mem.index != REG_NONE) {
            const char *idx_name = x86asm_reg_name_internal(ASM_FMT_TASM, op->u.mem.index, ctx->bits);
            if (op->u.mem.symbol || op->u.mem.base != REG_NONE)
                strcat(buf, " + ");
            strcat(buf, idx_name);
            if (op->u.mem.scale > 1) {
                char scale_buf[16];
                snprintf(scale_buf, sizeof(scale_buf), "*%d", op->u.mem.scale);
                strcat(buf, scale_buf);
            }
        }

        if (op->u.mem.disp != 0) {
            char disp_buf[32];
            if (op->u.mem.disp > 0 && (op->u.mem.symbol || op->u.mem.base != REG_NONE || op->u.mem.index != REG_NONE)) {
                snprintf(disp_buf, sizeof(disp_buf), " + %lld", (long long)op->u.mem.disp);
            } else if (op->u.mem.disp < 0) {
                snprintf(disp_buf, sizeof(disp_buf), " - %lld", (long long)-op->u.mem.disp);
            } else {
                snprintf(disp_buf, sizeof(disp_buf), "%lld", (long long)op->u.mem.disp);
            }
            strcat(buf, disp_buf);
        }

        strcat(buf, "]");
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
tasm_emit_insn(x86asm_ctx_t *ctx, const char *mnemonic,
               x86asm_operand_t *ops, int nops, x86asm_prefix_t prefix)
{
    char op_bufs[3][256];
    int i;

    /* Emit instruction with prefix */
    fprintf(ctx->output, "\t");

    if (prefix & PREFIX_LOCK)
        fprintf(ctx->output, "LOCK ");
    if (prefix & (PREFIX_REP | PREFIX_REPE | PREFIX_REPZ))
        fprintf(ctx->output, "REP ");
    if (prefix & (PREFIX_REPNE | PREFIX_REPNZ))
        fprintf(ctx->output, "REPNE ");

    fprintf(ctx->output, "%s", mnemonic);

    /* Format operands */
    if (nops > 0) {
        for (i = 0; i < nops && i < 3; i++) {
            tasm_format_operand(ctx, &ops[i], op_bufs[i], sizeof(op_bufs[i]));
        }

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
tasm_emit_directive(x86asm_ctx_t *ctx, const char *name, const char *value)
{
    if (value)
        fprintf(ctx->output, "\t%s %s\n", name, value);
    else
        fprintf(ctx->output, "\t%s\n", name);
}

/*
 * Emit a label
 */
static void
tasm_emit_label(x86asm_ctx_t *ctx, const char *name, int global)
{
    if (global)
        fprintf(ctx->output, "PUBLIC %s\n", name);
    fprintf(ctx->output, "%s:\n", name);
}

/*
 * Emit a segment/section directive
 */
static void
tasm_emit_segment(x86asm_ctx_t *ctx, x86asm_segment_t seg, const char *name)
{
    const char *seg_name;

    if (name) {
        fprintf(ctx->output, "%s SEGMENT\n", name);
        return;
    }

    /* TASM uses MASM-compatible segment definitions */
    switch (seg) {
    /* Common sections */
    case SEG_TEXT:        seg_name = "_TEXT"; break;
    case SEG_DATA:        seg_name = "_DATA"; break;
    case SEG_BSS:         seg_name = "_BSS"; break;
    case SEG_RODATA:      seg_name = "CONST"; break;
    case SEG_CONST:       seg_name = "CONST"; break;

    /* PE/COFF sections */
    case SEG_RDATA:       seg_name = ".rdata"; break;
    case SEG_IDATA:       seg_name = ".idata"; break;
    case SEG_EDATA:       seg_name = ".edata"; break;
    case SEG_PDATA:       seg_name = ".pdata"; break;
    case SEG_XDATA:       seg_name = ".xdata"; break;
    case SEG_RELOC:       seg_name = ".reloc"; break;
    case SEG_RSRC:        seg_name = ".rsrc"; break;
    case SEG_TLS:         seg_name = ".tls$"; break;
    case SEG_DRECTVE:     seg_name = ".drectve"; break;
    case SEG_DEBUG:       seg_name = ".debug"; break;

    /* Thread-Local Storage */
    case SEG_TDATA:       seg_name = ".tls$"; break;
    case SEG_TBSS:        seg_name = ".tls$"; break;

    /* Constructors/Destructors */
    case SEG_INIT:
    case SEG_CTORS:       seg_name = ".CRT$XCU"; break;
    case SEG_FINI:
    case SEG_DTORS:       seg_name = ".CRT$XTU"; break;

    /* Debug sections */
    case SEG_DEBUG_INFO:
    case SEG_DEBUG_ABBREV:
    case SEG_DEBUG_LINE:
    case SEG_DEBUG_STR:
    case SEG_DEBUG_LOC:
    case SEG_DEBUG_RANGES:
    case SEG_DEBUG_FRAME:
    case SEG_DEBUG_MACINFO:
    case SEG_DEBUG_PUBNAMES:
    case SEG_DEBUG_PUBTYPES:
    case SEG_DEBUG_ARANGES:
        seg_name = ".debug"; break;

    /* Default */
    default:
        seg_name = "_TEXT";
        break;
    }

    fprintf(ctx->output, "%s SEGMENT\n", seg_name);
}

/*
 * Emit data
 */
static void
tasm_emit_data(x86asm_ctx_t *ctx, x86asm_datasize_t size,
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
        directive = "DB";
        fprintf(ctx->output, "\t%s ", directive);
        for (i = 0; i < count; i++) {
            fprintf(ctx->output, "%s%02Xh", i > 0 ? ", " : "", bytes[i]);
        }
        fprintf(ctx->output, "\n");
        break;

    case DATA_WORD:
        directive = "DW";
        words = (const uint16_t *)data;
        fprintf(ctx->output, "\t%s ", directive);
        for (i = 0; i < count; i++) {
            fprintf(ctx->output, "%s%04Xh", i > 0 ? ", " : "", words[i]);
        }
        fprintf(ctx->output, "\n");
        break;

    case DATA_DWORD:
        directive = "DD";
        dwords = (const uint32_t *)data;
        fprintf(ctx->output, "\t%s ", directive);
        for (i = 0; i < count; i++) {
            fprintf(ctx->output, "%s%08Xh", i > 0 ? ", " : "", dwords[i]);
        }
        fprintf(ctx->output, "\n");
        break;

    case DATA_QWORD:
        directive = "DQ";
        qwords = (const uint64_t *)data;
        fprintf(ctx->output, "\t%s ", directive);
        for (i = 0; i < count; i++) {
            fprintf(ctx->output, "%s%016llXh", i > 0 ? ", " : "",
                    (unsigned long long)qwords[i]);
        }
        fprintf(ctx->output, "\n");
        break;

    case DATA_STRING:
        fprintf(ctx->output, "\tDB ");
        for (i = 0; i < count; i++) {
            fprintf(ctx->output, "%s%02Xh", i > 0 ? ", " : "", bytes[i]);
        }
        fprintf(ctx->output, "\n");
        break;

    case DATA_ASCIZ:
        fprintf(ctx->output, "\tDB \"");
        for (i = 0; i < count; i++) {
            char c = bytes[i];
            if (c == '"')
                fprintf(ctx->output, "\", 22h, \"");
            else if (c >= 32 && c < 127)
                fprintf(ctx->output, "%c", c);
            else
                fprintf(ctx->output, "\", %02Xh, \"", (unsigned char)c);
        }
        fprintf(ctx->output, "\", 0\n");
        break;

    default:
        break;
    }
}

/*
 * Emit a comment
 */
static void
tasm_emit_comment(x86asm_ctx_t *ctx, const char *text)
{
    fprintf(ctx->output, "\t; %s\n", text);
}

/*
 * Emit an alignment directive
 */
static void
tasm_emit_align(x86asm_ctx_t *ctx, int alignment)
{
    fprintf(ctx->output, "\tALIGN %d\n", alignment);
}
