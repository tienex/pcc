/*
 * emit_yasm.c - YASM (Yet Another Assembler) emitter
 *
 * Copyright (c) 2025
 * BSD Licensed - See COPYING
 *
 * Emits x86 assembly in YASM format (largely compatible with NASM)
 */

#include "x86asm_internal.h"
#include <stdio.h>
#include <string.h>

/* Forward declarations - YASM is largely compatible with NASM */
extern void nasm_emit_insn(x86asm_ctx_t *ctx, const char *mnemonic,
                           x86asm_operand_t *ops, int nops,
                           x86asm_prefix_t prefix);
extern void nasm_emit_directive(x86asm_ctx_t *ctx, const char *name,
                                const char *value);
extern void nasm_emit_label(x86asm_ctx_t *ctx, const char *name, int global);
extern void nasm_emit_segment(x86asm_ctx_t *ctx, x86asm_segment_t seg,
                              const char *name);
extern void nasm_emit_data(x86asm_ctx_t *ctx, x86asm_datasize_t size,
                           const void *data, size_t count);
extern void nasm_emit_comment(x86asm_ctx_t *ctx, const char *text);
extern void nasm_emit_align(x86asm_ctx_t *ctx, int alignment);

/* Operations table for YASM - reuses NASM functions */
const x86asm_ops_t x86asm_ops_yasm = {
    nasm_emit_insn,       /* YASM uses same instruction format as NASM */
    nasm_emit_directive,  /* Same directive format */
    nasm_emit_label,      /* Same label format */
    nasm_emit_segment,    /* Same section/segment format */
    nasm_emit_data,       /* Same data directives */
    nasm_emit_comment,    /* Same comment style */
    nasm_emit_align       /* Same align directive */
};
