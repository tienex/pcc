/*
 * x86asm_internal.h - Internal definitions for x86 assembly emitter library
 *
 * Copyright (c) 2025
 * BSD Licensed - See COPYING
 */

#ifndef X86ASM_INTERNAL_H
#define X86ASM_INTERNAL_H

#include "x86asm.h"

/* Internal helper functions */

/* Get register name for a specific format */
const char *x86asm_reg_name_internal(x86asm_format_t fmt, x86asm_reg_t reg, int bits);

/* Check if format uses AT&T syntax (source, dest) vs Intel syntax (dest, source) */
int x86asm_is_att_syntax(x86asm_format_t fmt);

/* Get size prefix for different formats */
const char *x86asm_size_prefix(x86asm_format_t fmt, int size);

/* Format-specific string utilities */
const char *x86asm_comment_prefix(x86asm_format_t fmt);
const char *x86asm_immediate_prefix(x86asm_format_t fmt);
const char *x86asm_register_prefix(x86asm_format_t fmt);

/* Common register name tables */
typedef struct {
    x86asm_reg_t reg;
    const char *att_name;    /* AT&T syntax name (GNU AS, Apple AS) */
    const char *intel_name;  /* Intel syntax name (MASM, NASM, TASM, etc.) */
} x86asm_regname_t;

extern const x86asm_regname_t x86asm_reg_table[];
extern const int x86asm_reg_table_size;

#endif /* X86ASM_INTERNAL_H */
