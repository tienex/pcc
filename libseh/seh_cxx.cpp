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
 *    distribution.
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

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "seh.h"
#include "seh_helpers.h"

#ifdef __cplusplus
}
#endif

/*
 * C++ Exception Interoperability for SEH
 *
 * This module provides integration between SEH and C++ exceptions,
 * allowing SEH __try/__except blocks to catch C++ exceptions and
 * vice versa on platforms where both coexist (macOS, Linux, etc.).
 *
 * Strategy:
 * - SEH uses setjmp/longjmp for flow control
 * - C++ uses DWARF unwinder with exception objects
 * - We bridge between them by:
 *   1. Recognizing C++ exception class in DWARF personality routine
 *   2. Converting C++ exceptions to SEH exceptions when needed
 *   3. Ensuring proper cleanup order (SEH finally, then C++ destructors)
 */

#ifdef __cplusplus

#include <exception>
#include <cstdint>

/*
 * C++ exception class identifier (defined by Itanium ABI)
 * "GNUCC++\0" = 0x474E5543432B2B00
 */
#define CXX_EXCEPTION_CLASS 0x474E5543432B2B00ULL

/*
 * SEH exception code for C++ exceptions
 * Use Microsoft-compatible code for C++ exception
 */
#define EXCEPTION_CXX_EXCEPTION 0xE06D7363UL  /* "msc" | 0xE0000000 */

/*
 * Map C++ exception to SEH exception
 *
 * This is called when a C++ exception propagates through a SEH __try block.
 */
extern "C" void
_seh_translate_cxx_exception(void *cxx_exception)
{
	/*
	 * In a real implementation, we would:
	 * 1. Extract the C++ exception type information
	 * 2. Create an SEH exception record
	 * 3. Store pointer to C++ exception in ExceptionInformation
	 * 4. Trigger SEH exception chain walk
	 */

	_seh_current_exception.ExceptionCode = EXCEPTION_CXX_EXCEPTION;
	_seh_current_exception.ExceptionFlags = 0;
	_seh_current_exception.ExceptionRecord = NULL;
	_seh_current_exception.ExceptionAddress = __builtin_return_address(0);
	_seh_current_exception.NumberParameters = 1;
	_seh_current_exception.ExceptionInformation[0] = (unsigned long)cxx_exception;

	/* Walk SEH chain looking for handler */
	struct _seh_registration *reg;
	for (reg = _seh_chain_head; reg != NULL; reg = reg->prev) {
		if (reg->filter == NULL)
			continue;

		int disposition = ((int (*)(void))reg->filter)();

		if (disposition == EXCEPTION_EXECUTE_HANDLER) {
			/* SEH handler wants to catch this C++ exception */
			longjmp(reg->jmpbuf, 1);
		}
		/* EXCEPTION_CONTINUE_SEARCH or EXCEPTION_CONTINUE_EXECUTION */
	}

	/* No SEH handler - let C++ exception continue */
}

/*
 * C++ exception handler that wraps SEH-aware code
 *
 * This catch(...) handler can be installed to catch any C++ exception
 * and convert it to an SEH exception if needed.
 */
extern "C" void
_seh_cxx_catch_all(void)
{
	try {
		throw;  /* Re-throw current exception */
	} catch (std::exception &e) {
		/* Standard C++ exception */
		_seh_translate_cxx_exception((void*)&e);
	} catch (...) {
		/* Unknown C++ exception */
		_seh_translate_cxx_exception(NULL);
	}
}

/*
 * Enhanced DWARF personality routine with C++ awareness
 *
 * This version of the personality routine recognizes both SEH and C++
 * exception classes and handles them appropriately.
 */
extern "C" int
_seh_dwarf_personality_cxx(int version, int actions,
                           uint64_t exception_class,
                           void *exception_object,
                           void *context)
{
	/*
	 * Check if this is a C++ exception propagating through SEH code
	 */
	if (exception_class == CXX_EXCEPTION_CLASS) {
		/* C++ exception - let SEH filters decide if they want to handle it */
		_seh_translate_cxx_exception(exception_object);

		/* If we get here, no SEH handler wanted it - continue C++ unwinding */
		return 8; /* _URC_CONTINUE_UNWIND */
	}

	/* For SEH exceptions, use normal SEH personality routine */
	return _seh_dwarf_personality(version, actions, exception_class,
	                              exception_object, context);
}

namespace {
	/*
	 * RAII guard to ensure SEH finally blocks execute even during C++ unwinding
	 */
	class SehFinallyGuard {
	private:
		struct _seh_registration *reg;
		void (*finally_block)(void);

	public:
		SehFinallyGuard(struct _seh_registration *r, void (*f)(void))
		    : reg(r), finally_block(f) {}

		~SehFinallyGuard() {
			if (finally_block && !reg->finally_executed) {
				reg->finally_executed = 1;
				finally_block();
			}
		}
	};
}

/*
 * Execute SEH finally block with C++ exception safety
 *
 * This ensures that finally blocks are executed even if C++ exceptions
 * occur during unwinding.
 */
extern "C" void
_seh_execute_finally_cxx(struct _seh_registration *reg,
                         void (*finally_block)(void))
{
	SehFinallyGuard guard(reg, finally_block);
	/* Destructor will execute finally block */
}

#else /* !__cplusplus */

/*
 * C-only stubs
 * These are used when compiling without C++ support
 */

void
_seh_translate_cxx_exception(void *cxx_exception)
{
	/* Not in C++ mode - should never be called */
}

void
_seh_cxx_catch_all(void)
{
	/* Not in C++ mode - should never be called */
}

int
_seh_dwarf_personality_cxx(int version, int actions,
                           unsigned long exception_class,
                           void *exception_object,
                           void *context)
{
	/* In C mode, just use regular SEH personality */
	return _seh_dwarf_personality(version, actions, exception_class,
	                              exception_object, context);
}

void
_seh_execute_finally_cxx(struct _seh_registration *reg,
                         void (*finally_block)(void))
{
	/* In C mode, use regular finally execution */
	_seh_execute_finally(reg, finally_block);
}

#endif /* __cplusplus */

/*
 * Helper function to check if an exception is a C++ exception
 */
int
_seh_is_cxx_exception(void)
{
	return _seh_current_exception.ExceptionCode == EXCEPTION_CXX_EXCEPTION;
}

/*
 * Get the C++ exception object if this is a C++ exception
 * Returns NULL if not a C++ exception
 */
void *
_seh_get_cxx_exception(void)
{
	if (_seh_is_cxx_exception() &&
	    _seh_current_exception.NumberParameters >= 1) {
		return (void *)_seh_current_exception.ExceptionInformation[0];
	}
	return NULL;
}
