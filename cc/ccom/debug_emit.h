/*	$Id$	*/

/*
 * Copyright (c) 2025
 * All rights reserved.
 *
 * Debug Symbol Emission Wrapper
 *
 * This header provides architecture-independent debug symbol emission
 * that uses libx86asm for x86 targets and falls back to direct printf()
 * for other architectures.
 */

#ifndef DEBUG_EMIT_H
#define DEBUG_EMIT_H

#include <stdio.h>
#include <stdint.h>

/* Check if we're on an x86 target with libx86asm available */
#if defined(ARCH_I86) || defined(ARCH_I386) || defined(ARCH_AMD64)
#define DEBUG_USE_X86ASM 1
#include "x86asm.h"
#else
#define DEBUG_USE_X86ASM 0
#endif

/*
 * Emit a debug section directive
 * Example: debug_emit_section(".debug_info", "", "@progbits")
 */
void debug_emit_section(const char *name, const char *flags, const char *type);

/*
 * Emit a label
 * Example: debug_emit_label(".Ldebug_info0")
 */
void debug_emit_label(const char *name);

/*
 * Emit data bytes
 * Example: debug_emit_byte(0x04)
 */
void debug_emit_byte(uint8_t value);
void debug_emit_bytes(const uint8_t *data, size_t count);

/*
 * Emit data words (16-bit)
 */
void debug_emit_word(uint16_t value);
void debug_emit_words(const uint16_t *data, size_t count);

/*
 * Emit data long/dword (32-bit)
 */
void debug_emit_long(uint32_t value);
void debug_emit_longs(const uint32_t *data, size_t count);

/*
 * Emit data quad (64-bit)
 */
void debug_emit_quad(uint64_t value);
void debug_emit_quads(const uint64_t *data, size_t count);

/*
 * Emit a null-terminated string
 * Example: debug_emit_asciz("hello.c")
 */
void debug_emit_asciz(const char *str);

/*
 * Emit a string without null terminator
 */
void debug_emit_string(const char *str, size_t len);

/*
 * Emit a label reference (address)
 * Example: debug_emit_ref(".Ldebug_abbrev0")
 */
void debug_emit_ref(const char *label, int size);

/*
 * Emit an offset expression
 * Example: debug_emit_offset(".Lcu_end", ".Lcu_start")
 */
void debug_emit_offset(const char *end_label, const char *start_label, int size);

/*
 * Get the x86asm context (only available on x86 targets)
 */
#if DEBUG_USE_X86ASM
extern x86asm_ctx_t *debug_get_asm_ctx(void);
#endif

#endif /* DEBUG_EMIT_H */
