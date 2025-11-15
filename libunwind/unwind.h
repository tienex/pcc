/*
 * Copyright (c) 2025 PCC Project
 * Stack Unwinding Library - Cross-platform unwinding for exception handling
 */

#ifndef _PCC_UNWIND_H_
#define _PCC_UNWIND_H_

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Unwinding reason codes */
typedef enum {
	UNW_REASON_NO_REASON = 0,
	UNW_REASON_FOREIGN_EXCEPTION = 1,
	UNW_REASON_FATAL_PHASE2_ERROR = 2,
	UNW_REASON_FATAL_PHASE1_ERROR = 3,
	UNW_REASON_NORMAL_STOP = 4,
	UNW_REASON_END_OF_STACK = 5,
	UNW_REASON_HANDLER_FOUND = 6,
	UNW_REASON_INSTALL_CONTEXT = 7,
	UNW_REASON_CONTINUE_UNWIND = 8
} unw_reason_code_t;

/* Exception class (8-byte identifier) */
typedef uint64_t unw_exception_class_t;

/* Exception object */
typedef struct {
	unw_exception_class_t exception_class;
	void (*exception_cleanup)(unw_reason_code_t, struct unw_exception *);
	uintptr_t private_1;
	uintptr_t private_2;
} unw_exception_t;

/* Context (platform-specific registers) */
typedef struct unw_context unw_context_t;
typedef struct unw_cursor unw_cursor_t;

/* Register numbers (generic) */
typedef enum {
	UNW_REG_IP,    /* Instruction pointer */
	UNW_REG_SP,    /* Stack pointer */
	UNW_REG_FP,    /* Frame pointer */
	UNW_REG_R0, UNW_REG_R1, UNW_REG_R2, UNW_REG_R3,
	UNW_REG_R4, UNW_REG_R5, UNW_REG_R6, UNW_REG_R7,
	UNW_REG_MAX = 256
} unw_regnum_t;

/* API functions */
int unw_getcontext(unw_context_t *ctx);
int unw_init_local(unw_cursor_t *cursor, unw_context_t *ctx);
int unw_step(unw_cursor_t *cursor);
int unw_get_reg(unw_cursor_t *cursor, unw_regnum_t reg, uintptr_t *val);
int unw_set_reg(unw_cursor_t *cursor, unw_regnum_t reg, uintptr_t val);
int unw_get_proc_name(unw_cursor_t *cursor, char *buf, size_t len, uintptr_t *off);

/* Raise exception */
unw_reason_code_t unw_raise_exception(unw_exception_t *exception);

/* Force unwind */
unw_reason_code_t unw_force_unwind(unw_exception_t *exception,
                                   void *stop, void *stop_arg);

/* Resume execution */
void unw_resume(unw_cursor_t *cursor);

/* Backtrace */
int unw_backtrace(void **buffer, int size);

#ifdef __cplusplus
}
#endif

#endif /* _PCC_UNWIND_H_ */
