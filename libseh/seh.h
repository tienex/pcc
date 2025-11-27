/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef _SEH_H_
#define _SEH_H_

#include <setjmp.h>

/*
 * Structured Exception Handling (SEH) Runtime Library
 *
 * This library provides cross-platform SEH support for PCC using DWARF
 * exception handling on non-Windows platforms.
 */

/*
 * Exception disposition codes (filter expression return values)
 */
#define EXCEPTION_EXECUTE_HANDLER      1
#define EXCEPTION_CONTINUE_SEARCH      0
#define EXCEPTION_CONTINUE_EXECUTION  (-1)

/*
 * Exception codes (common Windows exceptions)
 */
#define EXCEPTION_ACCESS_VIOLATION          0xC0000005L
#define EXCEPTION_DATATYPE_MISALIGNMENT     0x80000002L
#define EXCEPTION_BREAKPOINT                0x80000003L
#define EXCEPTION_SINGLE_STEP               0x80000004L
#define EXCEPTION_ARRAY_BOUNDS_EXCEEDED     0xC000008CL
#define EXCEPTION_FLT_DENORMAL_OPERAND      0xC000008DL
#define EXCEPTION_FLT_DIVIDE_BY_ZERO        0xC000008EL
#define EXCEPTION_FLT_INEXACT_RESULT        0xC000008FL
#define EXCEPTION_FLT_INVALID_OPERATION     0xC0000090L
#define EXCEPTION_FLT_OVERFLOW              0xC0000091L
#define EXCEPTION_FLT_STACK_CHECK           0xC0000092L
#define EXCEPTION_FLT_UNDERFLOW             0xC0000093L
#define EXCEPTION_INT_DIVIDE_BY_ZERO        0xC0000094L
#define EXCEPTION_INT_OVERFLOW              0xC0000095L
#define EXCEPTION_PRIV_INSTRUCTION          0xC0000096L
#define EXCEPTION_IN_PAGE_ERROR             0xC0000006L
#define EXCEPTION_ILLEGAL_INSTRUCTION       0xC000001DL
#define EXCEPTION_NONCONTINUABLE_EXCEPTION  0xC0000025L
#define EXCEPTION_STACK_OVERFLOW            0xC00000FDL
#define EXCEPTION_INVALID_DISPOSITION       0xC0000026L
#define EXCEPTION_GUARD_PAGE                0x80000001L
#define EXCEPTION_INVALID_HANDLE            0xC0000008L

/*
 * Exception record - describes the exception
 */
struct _seh_exception_record {
	unsigned long ExceptionCode;
	unsigned long ExceptionFlags;
	struct _seh_exception_record *ExceptionRecord;
	void *ExceptionAddress;
	unsigned long NumberParameters;
	unsigned long ExceptionInformation[15];
};

/*
 * Exception pointers - passed to filter expression
 */
struct _seh_exception_pointers {
	struct _seh_exception_record *ExceptionRecord;
	void *ContextRecord;	/* Platform-specific context */
};

/*
 * Exception registration record - one per __try block
 * This structure is placed on the stack in each function with __try.
 */
struct _seh_registration {
	struct _seh_registration *prev;
	void *handler;			/* Exception handler function */
	void *filter;			/* Exception filter function */
	jmp_buf jmpbuf;			/* For longjmp on exception */
	int state;			/* Current state within try block */
	int finally_executed;		/* Has __finally been executed? */
};

/*
 * Global exception chain head
 * On Windows, this is at FS:[0] (x86) or GS:[0] (x64)
 * On Unix, we maintain it in TLS
 */
#ifdef _WIN32
/* Windows uses FS:[0] or GS:[0] - handled by OS */
#else
extern __thread struct _seh_registration *_seh_chain_head;
#endif

/*
 * Runtime functions
 */

/* Register an exception handler (called at __try entry) */
void _seh_register(struct _seh_registration *reg,
                   void *handler, void *filter);

/* Unregister an exception handler (called at __try exit) */
void _seh_unregister(struct _seh_registration *reg);

/* Execute finally block (if it hasn't been executed yet) */
void _seh_execute_finally(struct _seh_registration *reg,
                          void (*finally_block)(void));

/* Raise an exception */
void _seh_raise_exception(unsigned long code, unsigned long flags,
                          unsigned long nparams, unsigned long *params);

/* Get exception code (for use in filter/handler) */
unsigned long _seh_get_exception_code(void);

/* Get exception information (for use in filter/handler) */
struct _seh_exception_pointers *_seh_get_exception_info(void);

/*
 * Internal functions (used by compiler-generated code)
 */

/* Try-except handler wrapper */
int _seh_try_except_handler(struct _seh_registration *reg,
                             struct _seh_exception_record *record,
                             void *context);

/* Try-finally handler wrapper */
int _seh_try_finally_handler(struct _seh_registration *reg,
                              struct _seh_exception_record *record,
                              void *context);

/*
 * Signal-to-exception mapping (for Unix platforms)
 */
#ifndef _WIN32
void _seh_init_signals(void);
void _seh_signal_handler(int signo, void *info, void *context);
#endif

/*
 * DWARF exception handling integration
 * These functions interface with the DWARF unwinder
 */
#ifdef __ELF__
/* Register DWARF frame information */
void _seh_register_dwarf_frame(void *frame_info);

/* Unregister DWARF frame information */
void _seh_unregister_dwarf_frame(void *frame_info);

/* Personality routine for DWARF unwinding */
int _seh_dwarf_personality(int version, int actions,
                           unsigned long exception_class,
                           void *exception_object,
                           void *context);
#endif

/*
 * Platform-specific context manipulation
 */
void _seh_save_context(void *context);
void _seh_restore_context(void *context);
void _seh_extract_signal_context(void *sigcontext, struct _seh_exception_record *exc);
void *_seh_get_ip(void *context);
void *_seh_get_sp(void *context);
void _seh_set_ip(void *context, void *ip);
unsigned long _seh_get_register(void *context, int reg_index);

/*
 * C++ exception interoperability
 */
#ifdef __cplusplus
extern "C" {
#endif

/* Translate C++ exception to SEH exception */
void _seh_translate_cxx_exception(void *cxx_exception);

/* Catch-all handler for C++ exceptions in SEH context */
void _seh_cxx_catch_all(void);

/* C++-aware DWARF personality routine */
int _seh_dwarf_personality_cxx(int version, int actions,
                               unsigned long exception_class,
                               void *exception_object,
                               void *context);

/* Execute finally block with C++ exception safety */
void _seh_execute_finally_cxx(struct _seh_registration *reg,
                              void (*finally_block)(void));

/* Check if current exception is a C++ exception */
int _seh_is_cxx_exception(void);

/* Get C++ exception object (NULL if not a C++ exception) */
void *_seh_get_cxx_exception(void);

#ifdef __cplusplus
}
#endif

/*
 * Helper macros for compiler code generation
 */
#define SEH_PROLOG(reg, handler, filter) \
	_seh_register(&(reg), (handler), (filter))

#define SEH_EPILOG(reg) \
	_seh_unregister(&(reg))

#define SEH_ENTER_TRY(reg) \
	if (setjmp((reg).jmpbuf) == 0)

#define SEH_ENTER_EXCEPT(reg) \
	else

#define SEH_ENTER_FINALLY(reg, finally_func) \
	_seh_execute_finally(&(reg), (finally_func))

#define SEH_ENTER_FINALLY_CXX(reg, finally_func) \
	_seh_execute_finally_cxx(&(reg), (finally_func))

#endif /* _SEH_H_ */
