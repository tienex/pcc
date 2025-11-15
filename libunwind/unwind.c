/*
 * Copyright (c) 2025 PCC Project
 * Stack Unwinding Library - Implementation
 */

#include "unwind.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#if defined(__x86_64__) || defined(_M_X64)
#include <setjmp.h>

struct unw_context {
	uintptr_t regs[UNW_REG_MAX];
};

struct unw_cursor {
	unw_context_t ctx;
	int depth;
};

int unw_getcontext(unw_context_t *ctx) {
	if (!ctx) return -1;
	memset(ctx, 0, sizeof(*ctx));
	
	/* Capture registers using inline assembly or builtin */
#ifdef __GNUC__
	ctx->regs[UNW_REG_IP] = (uintptr_t)__builtin_return_address(0);
	ctx->regs[UNW_REG_SP] = (uintptr_t)__builtin_frame_address(0);
#endif
	return 0;
}

int unw_init_local(unw_cursor_t *cursor, unw_context_t *ctx) {
	if (!cursor || !ctx) return -1;
	cursor->ctx = *ctx;
	cursor->depth = 0;
	return 0;
}

int unw_step(unw_cursor_t *cursor) {
	if (!cursor) return 0;
	
	/* Simple frame walking */
	uintptr_t *fp = (uintptr_t *)cursor->ctx.regs[UNW_REG_FP];
	if (!fp || cursor->depth > 100) return 0;
	
	cursor->ctx.regs[UNW_REG_IP] = fp[1];
	cursor->ctx.regs[UNW_REG_FP] = fp[0];
	cursor->depth++;
	
	return (cursor->ctx.regs[UNW_REG_IP] != 0) ? 1 : 0;
}

int unw_get_reg(unw_cursor_t *cursor, unw_regnum_t reg, uintptr_t *val) {
	if (!cursor || !val || reg >= UNW_REG_MAX) return -1;
	*val = cursor->ctx.regs[reg];
	return 0;
}

int unw_set_reg(unw_cursor_t *cursor, unw_regnum_t reg, uintptr_t val) {
	if (!cursor || reg >= UNW_REG_MAX) return -1;
	cursor->ctx.regs[reg] = val;
	return 0;
}

int unw_get_proc_name(unw_cursor_t *cursor, char *buf, size_t len, uintptr_t *off) {
	if (!cursor || !buf) return -1;
	snprintf(buf, len, "0x%lx", (unsigned long)cursor->ctx.regs[UNW_REG_IP]);
	if (off) *off = 0;
	return 0;
}

unw_reason_code_t unw_raise_exception(unw_exception_t *exception) {
	(void)exception;
	return UNW_REASON_END_OF_STACK;
}

unw_reason_code_t unw_force_unwind(unw_exception_t *exception,
                                   void *stop, void *stop_arg) {
	(void)exception; (void)stop; (void)stop_arg;
	return UNW_REASON_END_OF_STACK;
}

void unw_resume(unw_cursor_t *cursor) {
	(void)cursor;
}

int unw_backtrace(void **buffer, int size) {
	if (!buffer || size <= 0) return 0;
	
	int i = 0;
#ifdef __GNUC__
	for (i = 0; i < size; i++) {
		void *addr = __builtin_return_address(i);
		if (!addr) break;
		buffer[i] = addr;
	}
#endif
	return i;
}

#else
/* Stub implementations for unsupported architectures */
int unw_getcontext(unw_context_t *ctx) { (void)ctx; return -1; }
int unw_init_local(unw_cursor_t *cursor, unw_context_t *ctx) { (void)cursor; (void)ctx; return -1; }
int unw_step(unw_cursor_t *cursor) { (void)cursor; return 0; }
int unw_get_reg(unw_cursor_t *cursor, unw_regnum_t reg, uintptr_t *val) {
	(void)cursor; (void)reg; (void)val; return -1;
}
int unw_set_reg(unw_cursor_t *cursor, unw_regnum_t reg, uintptr_t val) {
	(void)cursor; (void)reg; (void)val; return -1;
}
int unw_get_proc_name(unw_cursor_t *cursor, char *buf, size_t len, uintptr_t *off) {
	(void)cursor; (void)buf; (void)len; (void)off; return -1;
}
unw_reason_code_t unw_raise_exception(unw_exception_t *exception) {
	(void)exception; return UNW_REASON_END_OF_STACK;
}
unw_reason_code_t unw_force_unwind(unw_exception_t *exception, void *stop, void *stop_arg) {
	(void)exception; (void)stop; (void)stop_arg; return UNW_REASON_END_OF_STACK;
}
void unw_resume(unw_cursor_t *cursor) { (void)cursor; }
int unw_backtrace(void **buffer, int size) { (void)buffer; (void)size; return 0; }
#endif
