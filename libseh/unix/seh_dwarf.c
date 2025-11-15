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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "seh.h"
#include "seh_helpers.h"

/*
 * DWARF Exception Handling Support for SEH
 *
 * This module provides integration with DWARF-based exception handling
 * on ELF platforms (Linux, *BSD, etc.). It allows SEH constructs to
 * work with the native DWARF unwinder.
 */

#ifdef __ELF__

/*
 * DWARF unwinder interface
 * These are typically provided by libgcc or libunwind
 */

/* Action codes for personality routine */
#define _UA_SEARCH_PHASE    1
#define _UA_CLEANUP_PHASE   2
#define _UA_HANDLER_FRAME   4
#define _UA_FORCE_UNWIND    8

/* Return codes for personality routine */
#define _URC_NO_REASON              0
#define _URC_FOREIGN_EXCEPTION_CAUGHT  1
#define _URC_FATAL_PHASE2_ERROR     2
#define _URC_FATAL_PHASE1_ERROR     3
#define _URC_NORMAL_STOP            4
#define _URC_END_OF_STACK           5
#define _URC_HANDLER_FOUND          6
#define _URC_INSTALL_CONTEXT        7
#define _URC_CONTINUE_UNWIND        8

/*
 * Exception object (standard Itanium ABI)
 */
struct _Unwind_Exception {
	unsigned long exception_class;
	void (*exception_cleanup)(int, struct _Unwind_Exception *);
	unsigned long private_1;
	unsigned long private_2;
};

/*
 * Unwind context (opaque)
 */
struct _Unwind_Context;

/*
 * DWARF unwinder functions (provided by libgcc/libunwind)
 */
extern void _Unwind_Resume(struct _Unwind_Exception *);
extern unsigned long _Unwind_GetIP(struct _Unwind_Context *);
extern unsigned long _Unwind_GetLanguageSpecificData(struct _Unwind_Context *);
extern unsigned long _Unwind_GetRegionStart(struct _Unwind_Context *);
extern void _Unwind_SetIP(struct _Unwind_Context *, unsigned long);
extern void _Unwind_SetGR(struct _Unwind_Context *, int, unsigned long);

/*
 * SEH exception class (8 characters)
 * "PCCSEH\0\0" = 0x5043435345480000
 */
#define SEH_EXCEPTION_CLASS 0x5043435345480000UL

/*
 * SEH personality routine for DWARF unwinding
 *
 * This is called by the DWARF unwinder during exception propagation.
 * It determines whether this frame can handle the exception.
 */
int
_seh_dwarf_personality(int version, int actions,
                       unsigned long exception_class,
                       void *exception_object,
                       void *context)
{
	struct _Unwind_Exception *ue = (struct _Unwind_Exception *)exception_object;
	struct _Unwind_Context *ctx = (struct _Unwind_Context *)context;
	unsigned long lsda, ip, func_start;
	struct _seh_registration *reg;
	int disposition;

	/* Only handle our own exceptions */
	if (exception_class != SEH_EXCEPTION_CLASS)
		return _URC_CONTINUE_UNWIND;

	/* Get instruction pointer and language-specific data area */
	ip = _Unwind_GetIP(ctx);
	lsda = _Unwind_GetLanguageSpecificData(ctx);
	func_start = _Unwind_GetRegionStart(ctx);

	if (actions & _UA_SEARCH_PHASE) {
		/*
		 * Phase 1: Search for a handler
		 * Walk the SEH chain to see if any filter wants to handle this
		 */
		for (reg = _seh_chain_head; reg != NULL; reg = reg->prev) {
			if (reg->filter == NULL)
				continue;  /* Try-finally, skip in search phase */

			/* Call filter expression */
			disposition = ((int (*)(void))reg->filter)();

			if (disposition == EXCEPTION_EXECUTE_HANDLER) {
				/* Found a handler */
				return _URC_HANDLER_FOUND;
			} else if (disposition == EXCEPTION_CONTINUE_EXECUTION) {
				/* Don't unwind */
				return _URC_NO_REASON;
			}
			/* EXCEPTION_CONTINUE_SEARCH - keep looking */
		}

		/* No handler found in this frame */
		return _URC_CONTINUE_UNWIND;
	}

	if (actions & _UA_CLEANUP_PHASE) {
		/*
		 * Phase 2: Cleanup and handler execution
		 * Execute finally blocks and/or exception handler
		 */
		for (reg = _seh_chain_head; reg != NULL; reg = reg->prev) {
			if (reg->filter == NULL) {
				/* Try-finally: execute the finally block */
				if (reg->handler && !reg->finally_executed) {
					((void (*)(void))reg->handler)();
					reg->finally_executed = 1;
				}
			} else {
				/* Try-except: check if this is the handling frame */
				disposition = ((int (*)(void))reg->filter)();

				if (disposition == EXCEPTION_EXECUTE_HANDLER) {
					/* Install context to jump to handler */
					if (actions & _UA_HANDLER_FRAME) {
						/* Set up to execute handler code */
						longjmp(reg->jmpbuf, 1);
						/* Does not return */
					}
				}
			}
		}

		return _URC_INSTALL_CONTEXT;
	}

	/* Unknown action */
	return _URC_CONTINUE_UNWIND;
}

/*
 * Raise an exception using DWARF unwinder
 */
void
_seh_raise_exception_dwarf(unsigned long code, unsigned long flags,
                           unsigned long nparams, unsigned long *params)
{
	struct _Unwind_Exception *ue;

	/* Allocate exception object */
	ue = malloc(sizeof(struct _Unwind_Exception));
	if (ue == NULL)
		abort();

	/* Initialize exception object */
	ue->exception_class = SEH_EXCEPTION_CLASS;
	ue->exception_cleanup = NULL;

	/* Set up current exception info for filters */
	_seh_current_exception.ExceptionCode = code;
	_seh_current_exception.ExceptionFlags = flags;
	_seh_current_exception.ExceptionRecord = NULL;
	_seh_current_exception.ExceptionAddress = __builtin_return_address(0);
	_seh_current_exception.NumberParameters = nparams;

	if (nparams > 15)
		nparams = 15;
	if (params)
		memcpy(_seh_current_exception.ExceptionInformation, params,
		       nparams * sizeof(unsigned long));

	/* Trigger unwinding */
	_Unwind_Resume(ue);

	/* Should not return */
	abort();
}

/*
 * Register DWARF frame information
 *
 * On ELF platforms, frame information is typically in .eh_frame section
 * and is automatically registered. This is a placeholder for explicit
 * registration if needed.
 */
void
_seh_register_dwarf_frame(void *frame_info)
{
	/* Usually automatic via .eh_frame section */
	/* Can use __register_frame() if needed */
}

/*
 * Unregister DWARF frame information
 */
void
_seh_unregister_dwarf_frame(void *frame_info)
{
	/* Usually automatic via .eh_frame section */
	/* Can use __deregister_frame() if needed */
}

#else /* !__ELF__ */

/*
 * Non-ELF platforms - DWARF not available
 * Provide stub implementations
 */

int
_seh_dwarf_personality(int version, int actions,
                       unsigned long exception_class,
                       void *exception_object,
                       void *context)
{
	return 8; /* _URC_CONTINUE_UNWIND */
}

void
_seh_register_dwarf_frame(void *frame_info)
{
}

void
_seh_unregister_dwarf_frame(void *frame_info)
{
}

#endif /* __ELF__ */
