/*
 * x86asm.c - Universal x86 Assembly Emitter Library - Core Implementation
 *
 * Copyright (c) 2025
 * BSD Licensed - See COPYING
 */

#include "x86asm.h"
#include "x86asm_internal.h"
#include <stdlib.h>
#include <string.h>

/* Forward declarations for format-specific ops */
extern const x86asm_ops_t x86asm_ops_gnu_as;
extern const x86asm_ops_t x86asm_ops_apple_as;
extern const x86asm_ops_t x86asm_ops_masm;
extern const x86asm_ops_t x86asm_ops_ml;
extern const x86asm_ops_t x86asm_ops_tasm;
extern const x86asm_ops_t x86asm_ops_wasm;
extern const x86asm_ops_t x86asm_ops_owasm;
extern const x86asm_ops_t x86asm_ops_nasm;
extern const x86asm_ops_t x86asm_ops_yasm;
extern const x86asm_ops_t x86asm_ops_fasm;

/*
 * Create a new emitter context
 */
x86asm_ctx_t *
x86asm_create(x86asm_format_t format, FILE *output, int bits)
{
    x86asm_ctx_t *ctx;

    if (!output || (bits != 16 && bits != 32 && bits != 64))
        return NULL;

    ctx = calloc(1, sizeof(x86asm_ctx_t));
    if (!ctx)
        return NULL;

    ctx->format = format;
    ctx->output = output;
    ctx->bits = bits;
    ctx->indent = 1;
    ctx->flags = 0;

    /* Select appropriate operations table */
    switch (format) {
    case ASM_FMT_GNU_AS:
        ctx->ops = &x86asm_ops_gnu_as;
        break;
    case ASM_FMT_APPLE_AS:
        ctx->ops = &x86asm_ops_apple_as;
        break;
    case ASM_FMT_MASM:
        ctx->ops = &x86asm_ops_masm;
        break;
    case ASM_FMT_ML:
        ctx->ops = &x86asm_ops_ml;
        break;
    case ASM_FMT_TASM:
        ctx->ops = &x86asm_ops_tasm;
        break;
    case ASM_FMT_WASM:
        ctx->ops = &x86asm_ops_wasm;
        break;
    case ASM_FMT_OWASM:
        ctx->ops = &x86asm_ops_owasm;
        break;
    case ASM_FMT_NASM:
        ctx->ops = &x86asm_ops_nasm;
        break;
    case ASM_FMT_YASM:
        ctx->ops = &x86asm_ops_yasm;
        break;
    case ASM_FMT_FASM:
        ctx->ops = &x86asm_ops_fasm;
        break;
    case ASM_FMT_JWASM:
    case ASM_FMT_UASM:
        /* JWasm and UASM are MASM-compatible */
        ctx->ops = &x86asm_ops_masm;
        break;
    default:
        free(ctx);
        return NULL;
    }

    return ctx;
}

/*
 * Destroy emitter context
 */
void
x86asm_destroy(x86asm_ctx_t *ctx)
{
    if (ctx) {
        free(ctx->user_data);
        free(ctx);
    }
}

/*
 * Set format-specific flags
 */
void
x86asm_set_flags(x86asm_ctx_t *ctx, int flags)
{
    if (ctx)
        ctx->flags = flags;
}

/*
 * Emit an instruction
 */
void
x86asm_insn(x86asm_ctx_t *ctx, const char *mnemonic,
            x86asm_operand_t *ops, int nops, x86asm_prefix_t prefix)
{
    if (ctx && ctx->ops && ctx->ops->emit_insn)
        ctx->ops->emit_insn(ctx, mnemonic, ops, nops, prefix);
}

/*
 * Emit a label
 */
void
x86asm_label(x86asm_ctx_t *ctx, const char *name, int global)
{
    if (ctx && ctx->ops && ctx->ops->emit_label)
        ctx->ops->emit_label(ctx, name, global);
}

/*
 * Emit a segment/section directive
 */
void
x86asm_segment(x86asm_ctx_t *ctx, x86asm_segment_t seg, const char *name)
{
    if (ctx && ctx->ops && ctx->ops->emit_segment)
        ctx->ops->emit_segment(ctx, seg, name);
}

/*
 * Emit data
 */
void
x86asm_data(x86asm_ctx_t *ctx, x86asm_datasize_t size,
            const void *data, size_t count)
{
    if (ctx && ctx->ops && ctx->ops->emit_data)
        ctx->ops->emit_data(ctx, size, data, count);
}

/*
 * Emit a comment
 */
void
x86asm_comment(x86asm_ctx_t *ctx, const char *text)
{
    if (ctx && ctx->ops && ctx->ops->emit_comment)
        ctx->ops->emit_comment(ctx, text);
}

/*
 * Emit an alignment directive
 */
void
x86asm_align(x86asm_ctx_t *ctx, int alignment)
{
    if (ctx && ctx->ops && ctx->ops->emit_align)
        ctx->ops->emit_align(ctx, alignment);
}

/*
 * Emit a raw directive
 */
void
x86asm_directive(x86asm_ctx_t *ctx, const char *name, const char *value)
{
    if (ctx && ctx->ops && ctx->ops->emit_directive)
        ctx->ops->emit_directive(ctx, name, value);
}

/*
 * Helper: Create register operand
 */
x86asm_operand_t
x86asm_op_reg(x86asm_reg_t reg, int size)
{
    x86asm_operand_t op = {0};
    op.type = OP_REG;
    op.size = size;
    op.u.reg = reg;
    return op;
}

/*
 * Helper: Create immediate operand
 */
x86asm_operand_t
x86asm_op_imm(int64_t value, int size)
{
    x86asm_operand_t op = {0};
    op.type = OP_IMM;
    op.size = size;
    op.u.imm = value;
    return op;
}

/*
 * Helper: Create memory operand
 */
x86asm_operand_t
x86asm_op_mem(x86asm_reg_t base, x86asm_reg_t index,
              int scale, int64_t disp, int size)
{
    x86asm_operand_t op = {0};
    op.type = OP_MEM;
    op.size = size;
    op.u.mem.base = base;
    op.u.mem.index = index;
    op.u.mem.scale = scale;
    op.u.mem.disp = disp;
    op.u.mem.symbol = NULL;
    op.u.mem.segment = REG_NONE;
    return op;
}

/*
 * Helper: Create memory operand with symbol
 */
x86asm_operand_t
x86asm_op_mem_symbol(const char *symbol, int64_t disp, int size)
{
    x86asm_operand_t op = {0};
    op.type = OP_MEM;
    op.size = size;
    op.u.mem.base = REG_NONE;
    op.u.mem.index = REG_NONE;
    op.u.mem.scale = 0;
    op.u.mem.disp = disp;
    op.u.mem.symbol = symbol;
    op.u.mem.segment = REG_NONE;
    return op;
}

/*
 * Helper: Create label operand
 */
x86asm_operand_t
x86asm_op_label(const char *label)
{
    x86asm_operand_t op = {0};
    op.type = OP_LABEL;
    op.size = 0;
    op.u.label = label;
    return op;
}

/*
 * Get register name (delegates to format-specific implementation)
 */
const char *
x86asm_reg_name(x86asm_ctx_t *ctx, x86asm_reg_t reg)
{
    if (!ctx)
        return NULL;

    return x86asm_reg_name_internal(ctx->format, reg, ctx->bits);
}

/*
 * High-level directive: Symbol type
 * Emits format-specific symbol type directive
 */
void
x86asm_symbol_type(x86asm_ctx_t *ctx, const char *symbol,
                   x86asm_symbol_type_t type)
{
    char directive[512];

    if (!ctx || !symbol)
        return;

    /* Only emit for formats that support it (ELF-based formats) */
    switch (ctx->format) {
    case ASM_FMT_GNU_AS:
    case ASM_FMT_NASM:
    case ASM_FMT_YASM:
        if (type == SYMBOL_TYPE_FUNCTION) {
            snprintf(directive, sizeof(directive), "%s,@function", symbol);
        } else if (type == SYMBOL_TYPE_OBJECT) {
            snprintf(directive, sizeof(directive), "%s,@object", symbol);
        } else {
            return; /* NOTYPE - don't emit */
        }
        x86asm_directive(ctx, "type", directive);
        break;

    default:
        /* Other formats don't use .type directive */
        break;
    }
}

/*
 * High-level directive: Symbol size
 * Emits format-specific symbol size directive
 */
void
x86asm_symbol_size(x86asm_ctx_t *ctx, const char *symbol, size_t size)
{
    char directive[512];

    if (!ctx || !symbol)
        return;

    /* Only emit for formats that support it (ELF-based formats) */
    switch (ctx->format) {
    case ASM_FMT_GNU_AS:
    case ASM_FMT_NASM:
    case ASM_FMT_YASM:
        snprintf(directive, sizeof(directive), "%s,%zu", symbol, size);
        x86asm_directive(ctx, "size", directive);
        break;

    default:
        /* Other formats don't use .size directive */
        break;
    }
}

/*
 * High-level directive: Ident/version string
 * Emits compiler identification string
 */
void
x86asm_ident(x86asm_ctx_t *ctx, const char *ident_string)
{
    char directive[512];

    if (!ctx || !ident_string)
        return;

    switch (ctx->format) {
    case ASM_FMT_GNU_AS:
    case ASM_FMT_APPLE_AS:
        snprintf(directive, sizeof(directive), "\"%s\"", ident_string);
        x86asm_directive(ctx, "ident", directive);
        break;

    case ASM_FMT_NASM:
    case ASM_FMT_YASM:
        /* NASM/YASM don't have .ident, but we can use a comment */
        fprintf(ctx->output, "\t; %s\n", ident_string);
        break;

    case ASM_FMT_MASM:
    case ASM_FMT_ML:
    case ASM_FMT_TASM:
        /* MASM-family: use comment */
        fprintf(ctx->output, "; %s\n", ident_string);
        break;

    case ASM_FMT_WASM:
    case ASM_FMT_OWASM:
        /* Watcom: use comment */
        fprintf(ctx->output, "; %s\n", ident_string);
        break;
    }
}

/*
 * High-level directive: Indirect symbol (Mach-O)
 * Emits .indirect_symbol directive for Mach-O stub tables
 */
void
x86asm_indirect_symbol(x86asm_ctx_t *ctx, const char *symbol)
{
    if (!ctx || !symbol)
        return;

    /* This is primarily for Mach-O (Apple AS) */
    switch (ctx->format) {
    case ASM_FMT_APPLE_AS:
        x86asm_directive(ctx, "indirect_symbol", symbol);
        break;

    default:
        /* Other formats don't use .indirect_symbol */
        break;
    }
}

/*
 * High-level directive: End
 * Emits end-of-file directive for formats that require it
 */
void
x86asm_end(x86asm_ctx_t *ctx)
{
    if (!ctx)
        return;

    switch (ctx->format) {
    case ASM_FMT_GNU_AS:
        /* ELF format uses .end */
        x86asm_directive(ctx, "end", NULL);
        break;

    default:
        /* Most formats don't need an explicit end directive */
        break;
    }
}

/*
 * High-level directive: Common symbol
 * Emits .comm directive for uninitialized global data
 */
void
x86asm_comm(x86asm_ctx_t *ctx, const char *symbol, size_t size, int alignment)
{
    char directive[512];

    if (!ctx || !symbol)
        return;

    switch (ctx->format) {
    case ASM_FMT_GNU_AS:
    case ASM_FMT_APPLE_AS:
        /* GNU AS and Apple AS: .comm symbol,size,alignment */
        if (alignment > 0) {
            snprintf(directive, sizeof(directive), "%s,%zu,%d", symbol, size, alignment);
        } else {
            snprintf(directive, sizeof(directive), "%s,%zu", symbol, size);
        }
        x86asm_directive(ctx, "comm", directive);
        break;

    case ASM_FMT_NASM:
    case ASM_FMT_YASM:
        /* NASM/YASM: common symbol size */
        if (alignment > 0) {
            snprintf(directive, sizeof(directive), "%s %zu:%d", symbol, size, alignment);
        } else {
            snprintf(directive, sizeof(directive), "%s %zu", symbol, size);
        }
        x86asm_directive(ctx, "common", directive);
        break;

    default:
        /* Other formats: emit as comment */
        snprintf(directive, sizeof(directive), "COMM %s,%zu,%d", symbol, size, alignment);
        x86asm_comment(ctx, directive);
        break;
    }
}

/*
 * High-level directive: Local common symbol
 * Emits .lcomm directive for uninitialized local data
 * Note: On ELF (GNU_AS), this emits .local + .comm instead of .lcomm
 */
void
x86asm_lcomm(x86asm_ctx_t *ctx, const char *symbol, size_t size, int alignment)
{
    char directive[512];

    if (!ctx || !symbol)
        return;

    switch (ctx->format) {
    case ASM_FMT_GNU_AS:
        /* ELF: Use .local + .comm (not .lcomm) */
        x86asm_local(ctx, symbol);
        x86asm_comm(ctx, symbol, size, alignment);
        break;

    case ASM_FMT_APPLE_AS:
        /* Apple AS (Mach-O): .lcomm symbol,size,alignment */
        if (alignment > 0) {
            snprintf(directive, sizeof(directive), "%s,%zu,%d", symbol, size, alignment);
        } else {
            snprintf(directive, sizeof(directive), "%s,%zu", symbol, size);
        }
        x86asm_directive(ctx, "lcomm", directive);
        break;

    case ASM_FMT_NASM:
    case ASM_FMT_YASM:
        /* NASM/YASM don't have .lcomm, use BSS section instead */
        x86asm_segment(ctx, SEG_BSS, NULL);
        if (alignment > 0) {
            x86asm_align(ctx, alignment);
        }
        x86asm_label(ctx, symbol, 0);
        /* Reserve space using resb directive */
        snprintf(directive, sizeof(directive), "%zu", size);
        x86asm_directive(ctx, "resb", directive);
        break;

    default:
        /* Other formats: emit as comment */
        snprintf(directive, sizeof(directive), "LCOMM %s,%zu,%d", symbol, size, alignment);
        x86asm_comment(ctx, directive);
        break;
    }
}

/*
 * High-level directive: Local symbol visibility
 * Emits .local directive to mark symbol as local
 */
void
x86asm_local(x86asm_ctx_t *ctx, const char *symbol)
{
    if (!ctx || !symbol)
        return;

    switch (ctx->format) {
    case ASM_FMT_GNU_AS:
        /* ELF: .local symbol */
        x86asm_directive(ctx, "local", symbol);
        break;

    default:
        /* Other formats don't typically use .local */
        break;
    }
}

/*
 * High-level directive: Hidden symbol visibility
 * Emits .hidden directive to mark symbol as hidden (not exported)
 */
void
x86asm_hidden(x86asm_ctx_t *ctx, const char *symbol)
{
    if (!ctx || !symbol)
        return;

    switch (ctx->format) {
    case ASM_FMT_GNU_AS:
        /* ELF: .hidden symbol */
        x86asm_directive(ctx, "hidden", symbol);
        break;

    case ASM_FMT_APPLE_AS:
        /* Mach-O uses .private_extern instead */
        x86asm_directive(ctx, "private_extern", symbol);
        break;

    default:
        /* Other formats don't typically use visibility directives */
        break;
    }
}

/*
 * High-level directive: Weak symbol
 * Emits .weak directive to mark symbol as weak
 */
void
x86asm_weak(x86asm_ctx_t *ctx, const char *symbol)
{
    if (!ctx || !symbol)
        return;

    switch (ctx->format) {
    case ASM_FMT_GNU_AS:
    case ASM_FMT_APPLE_AS:
        /* ELF/Mach-O: .weak symbol */
        x86asm_directive(ctx, "weak", symbol);
        break;

    case ASM_FMT_NASM:
    case ASM_FMT_YASM:
        /* NASM/YASM: weak symbol */
        x86asm_directive(ctx, "weak", symbol);
        break;

    default:
        /* Other formats: emit as comment */
        {
            char buf[512];
            snprintf(buf, sizeof(buf), "WEAK %s", symbol);
            x86asm_comment(ctx, buf);
        }
        break;
    }
}

/*
 * High-level directive: Weak reference
 * Emits .weakref directive to create a weak reference to another symbol
 */
void
x86asm_weakref(x86asm_ctx_t *ctx, const char *symbol, const char *target)
{
    char directive[512];

    if (!ctx || !symbol || !target)
        return;

    switch (ctx->format) {
    case ASM_FMT_GNU_AS:
        /* ELF: .weakref symbol,target */
        snprintf(directive, sizeof(directive), "%s,%s", symbol, target);
        x86asm_directive(ctx, "weakref", directive);
        break;

    default:
        /* Other formats: use weak + set */
        x86asm_weak(ctx, symbol);
        x86asm_set(ctx, symbol, target);
        break;
    }
}

/*
 * High-level directive: Set (symbol aliasing)
 * Emits .set directive to create a symbol alias
 */
void
x86asm_set(x86asm_ctx_t *ctx, const char *symbol, const char *value)
{
    char directive[512];

    if (!ctx || !symbol || !value)
        return;

    switch (ctx->format) {
    case ASM_FMT_GNU_AS:
    case ASM_FMT_APPLE_AS:
        /* ELF/Mach-O: .set symbol,value */
        snprintf(directive, sizeof(directive), "%s,%s", symbol, value);
        x86asm_directive(ctx, "set", directive);
        break;

    case ASM_FMT_NASM:
    case ASM_FMT_YASM:
        /* NASM/YASM: %define or equ */
        fprintf(ctx->output, "%%define %s %s\n", symbol, value);
        break;

    case ASM_FMT_MASM:
    case ASM_FMT_ML:
    case ASM_FMT_TASM:
        /* MASM: symbol EQU value */
        fprintf(ctx->output, "%s EQU %s\n", symbol, value);
        break;

    default:
        /* Other formats: emit as comment */
        snprintf(directive, sizeof(directive), "SET %s = %s", symbol, value);
        x86asm_comment(ctx, directive);
        break;
    }
}

/*
 * High-level directive: Previous section
 * Emits .previous directive to return to previous section
 */
void
x86asm_previous(x86asm_ctx_t *ctx)
{
    if (!ctx)
        return;

    switch (ctx->format) {
    case ASM_FMT_GNU_AS:
        /* ELF: .previous */
        x86asm_directive(ctx, "previous", NULL);
        break;

    case ASM_FMT_APPLE_AS:
        /* Mach-O also supports .previous */
        x86asm_directive(ctx, "previous", NULL);
        break;

    default:
        /* Other formats don't typically support .previous */
        break;
    }
}

/*
 * High-level directive: Power-of-2 alignment
 * Emits .p2align directive for power-of-2 alignment
 */
void
x86asm_p2align(x86asm_ctx_t *ctx, int power)
{
    char directive[32];

    if (!ctx)
        return;

    switch (ctx->format) {
    case ASM_FMT_GNU_AS:
    case ASM_FMT_APPLE_AS:
        /* ELF/Mach-O: .p2align power */
        snprintf(directive, sizeof(directive), "%d", power);
        x86asm_directive(ctx, "p2align", directive);
        break;

    case ASM_FMT_NASM:
    case ASM_FMT_YASM:
        /* NASM/YASM: align (1 << power) */
        {
            int alignment = 1 << power;
            x86asm_align(ctx, alignment);
        }
        break;

    default:
        /* Other formats: use regular align */
        {
            int alignment = 1 << power;
            x86asm_align(ctx, alignment);
        }
        break;
    }
}
