/*
 * emit_nasm.c - NASM (Netwide Assembler) emitter
 *
 * Copyright (c) 2025
 * BSD Licensed - See COPYING
 *
 * Emits x86 assembly in NASM (Intel syntax) format
 */

#include "x86asm_internal.h"
#include <stdio.h>
#include <string.h>

/* Forward declarations - non-static to allow YASM to reuse them */
void nasm_emit_insn(x86asm_ctx_t *ctx, const char *mnemonic,
                    x86asm_operand_t *ops, int nops,
                    x86asm_prefix_t prefix);
void nasm_emit_directive(x86asm_ctx_t *ctx, const char *name,
                         const char *value);
void nasm_emit_label(x86asm_ctx_t *ctx, const char *name, int global);
void nasm_emit_segment(x86asm_ctx_t *ctx, x86asm_segment_t seg,
                       const char *name);
void nasm_emit_data(x86asm_ctx_t *ctx, x86asm_datasize_t size,
                    const void *data, size_t count);
void nasm_emit_comment(x86asm_ctx_t *ctx, const char *text);
void nasm_emit_align(x86asm_ctx_t *ctx, int alignment);

/* Operations table for NASM */
const x86asm_ops_t x86asm_ops_nasm = {
    nasm_emit_insn,
    nasm_emit_directive,
    nasm_emit_label,
    nasm_emit_segment,
    nasm_emit_data,
    nasm_emit_comment,
    nasm_emit_align
};

/*
 * Get size specifier for NASM
 */
static const char *
nasm_size_spec(int size)
{
    switch (size) {
    case 8:   return "byte";
    case 16:  return "word";
    case 32:  return "dword";
    case 64:  return "qword";
    case 80:  return "tword";
    case 128: return "oword";
    case 256: return "yword";
    default:  return "";
    }
}

/*
 * Format an operand for NASM (Intel syntax: dest, source)
 * Made static as it's internal to this file
 */
static void
nasm_format_operand(x86asm_ctx_t *ctx, x86asm_operand_t *op, char *buf, size_t bufsize)
{
    const char *regname;

    switch (op->type) {
    case OP_REG:
        regname = x86asm_reg_name_internal(ASM_FMT_NASM, op->u.reg, ctx->bits);
        snprintf(buf, bufsize, "%s", regname ? regname : "???");
        break;

    case OP_IMM:
        snprintf(buf, bufsize, "%lld", (long long)op->u.imm);
        break;

    case OP_MEM:
        /* Intel syntax: [base + index*scale + disp] */
        if (op->size > 0) {
            snprintf(buf, bufsize, "%s ", nasm_size_spec(op->size));
        } else {
            buf[0] = '\0';
        }

        strcat(buf, "[");

        if (op->u.mem.symbol) {
            strcat(buf, op->u.mem.symbol);
        }

        if (op->u.mem.base != REG_NONE) {
            regname = x86asm_reg_name_internal(ASM_FMT_NASM, op->u.mem.base, ctx->bits);
            if (op->u.mem.symbol)
                strcat(buf, " + ");
            strcat(buf, regname);
        }

        if (op->u.mem.index != REG_NONE) {
            const char *idx_name = x86asm_reg_name_internal(ASM_FMT_NASM, op->u.mem.index, ctx->bits);
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
void
nasm_emit_insn(x86asm_ctx_t *ctx, const char *mnemonic,
               x86asm_operand_t *ops, int nops, x86asm_prefix_t prefix)
{
    char op_bufs[3][256];
    int i;

    /* Emit instruction with prefix */
    fprintf(ctx->output, "\t");

    if (prefix & PREFIX_LOCK)
        fprintf(ctx->output, "lock ");
    if (prefix & (PREFIX_REP | PREFIX_REPE | PREFIX_REPZ))
        fprintf(ctx->output, "rep ");
    if (prefix & (PREFIX_REPNE | PREFIX_REPNZ))
        fprintf(ctx->output, "repnz ");

    fprintf(ctx->output, "%s", mnemonic);

    /* Format operands (Intel syntax: destination, source) */
    if (nops > 0) {
        for (i = 0; i < nops && i < 3; i++) {
            nasm_format_operand(ctx, &ops[i], op_bufs[i], sizeof(op_bufs[i]));
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
void
nasm_emit_directive(x86asm_ctx_t *ctx, const char *name, const char *value)
{
    if (value)
        fprintf(ctx->output, "[%s %s]\n", name, value);
    else
        fprintf(ctx->output, "[%s]\n", name);
}

/*
 * Emit a label
 */
void
nasm_emit_label(x86asm_ctx_t *ctx, const char *name, int global)
{
    if (global)
        fprintf(ctx->output, "global %s\n", name);
    fprintf(ctx->output, "%s:\n", name);
}

/*
 * Emit a segment/section directive
 */
void
nasm_emit_segment(x86asm_ctx_t *ctx, x86asm_segment_t seg, const char *name)
{
    const char *seg_name;

    if (name) {
        fprintf(ctx->output, "section %s\n", name);
        return;
    }

    switch (seg) {
    case SEG_TEXT:
        seg_name = ".text";
        break;
    case SEG_DATA:
        seg_name = ".data";
        break;
    case SEG_BSS:
        seg_name = ".bss";
        break;
    case SEG_RODATA:
    case SEG_CONST:
        seg_name = ".rodata";
        break;
    default:
        seg_name = ".text";
        break;
    }

    fprintf(ctx->output, "section %s\n", seg_name);
}

/*
 * Emit data
 */
void
nasm_emit_data(x86asm_ctx_t *ctx, x86asm_datasize_t size,
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
        directive = "db";
        fprintf(ctx->output, "\t%s ", directive);
        for (i = 0; i < count; i++) {
            fprintf(ctx->output, "%s0x%02x", i > 0 ? ", " : "", bytes[i]);
        }
        fprintf(ctx->output, "\n");
        break;

    case DATA_WORD:
        directive = "dw";
        words = (const uint16_t *)data;
        fprintf(ctx->output, "\t%s ", directive);
        for (i = 0; i < count; i++) {
            fprintf(ctx->output, "%s0x%04x", i > 0 ? ", " : "", words[i]);
        }
        fprintf(ctx->output, "\n");
        break;

    case DATA_DWORD:
        directive = "dd";
        dwords = (const uint32_t *)data;
        fprintf(ctx->output, "\t%s ", directive);
        for (i = 0; i < count; i++) {
            fprintf(ctx->output, "%s0x%08x", i > 0 ? ", " : "", dwords[i]);
        }
        fprintf(ctx->output, "\n");
        break;

    case DATA_QWORD:
        directive = "dq";
        qwords = (const uint64_t *)data;
        fprintf(ctx->output, "\t%s ", directive);
        for (i = 0; i < count; i++) {
            fprintf(ctx->output, "%s0x%016llx", i > 0 ? ", " : "",
                    (unsigned long long)qwords[i]);
        }
        fprintf(ctx->output, "\n");
        break;

    case DATA_FLOAT:
        directive = "dd";
        fprintf(ctx->output, "\t%s ", directive);
        /* Note: would need float interpretation here */
        break;

    case DATA_DOUBLE:
        directive = "dq";
        fprintf(ctx->output, "\t%s ", directive);
        /* Note: would need double interpretation here */
        break;

    case DATA_STRING:
        fprintf(ctx->output, "\tdb ");
        for (i = 0; i < count; i++) {
            fprintf(ctx->output, "%s0x%02x", i > 0 ? ", " : "", bytes[i]);
        }
        fprintf(ctx->output, "\n");
        break;

    case DATA_ASCIZ:
        fprintf(ctx->output, "\tdb \"");
        for (i = 0; i < count; i++) {
            char c = bytes[i];
            if (c == '"' || c == '\\')
                fprintf(ctx->output, "\\%c", c);
            else if (c >= 32 && c < 127)
                fprintf(ctx->output, "%c", c);
            else
                fprintf(ctx->output, "\", 0x%02x, \"", (unsigned char)c);
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
void
nasm_emit_comment(x86asm_ctx_t *ctx, const char *text)
{
    fprintf(ctx->output, "\t; %s\n", text);
}

/*
 * Emit an alignment directive
 */
void
nasm_emit_align(x86asm_ctx_t *ctx, int alignment)
{
    fprintf(ctx->output, "\talign %d\n", alignment);
}
