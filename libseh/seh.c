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
#include <signal.h>
#include <setjmp.h>
#include "seh.h"

/*
 * SEH Runtime Implementation for Non-Windows Platforms
 *
 * This implementation uses setjmp/longjmp for exception handling
 * on platforms without native SEH support (e.g., Unix with DWARF).
 */

#ifndef _WIN32

/*
 * Thread-local storage for exception chain
 */
__thread struct _seh_registration *_seh_chain_head = NULL;

/*
 * Current exception information (for use in filters/handlers)
 */
__thread struct _seh_exception_record _seh_current_exception;
__thread void *_seh_current_context = NULL;

/*
 * Initialize signal handlers to convert signals to exceptions
 */
static int _seh_signals_initialized = 0;

void
_seh_init_signals(void)
{
	struct sigaction sa;

	if (_seh_signals_initialized)
		return;

	memset(&sa, 0, sizeof(sa));
	sa.sa_sigaction = (void (*)(int, siginfo_t *, void *))_seh_signal_handler;
	sa.sa_flags = SA_SIGINFO;

	/* Install signal handlers for common faults */
	sigaction(SIGSEGV, &sa, NULL);  /* Access violation */
	sigaction(SIGFPE, &sa, NULL);   /* Floating point exception */
	sigaction(SIGILL, &sa, NULL);   /* Illegal instruction */
	sigaction(SIGBUS, &sa, NULL);   /* Bus error */
	sigaction(SIGTRAP, &sa, NULL);  /* Breakpoint */

	_seh_signals_initialized = 1;
}

/*
 * Convert signal number to exception code
 */
static unsigned long
signal_to_exception_code(int signo)
{
	switch (signo) {
	case SIGSEGV:
		return EXCEPTION_ACCESS_VIOLATION;
	case SIGFPE:
		return EXCEPTION_FLT_DIVIDE_BY_ZERO;
	case SIGILL:
		return EXCEPTION_ILLEGAL_INSTRUCTION;
	case SIGBUS:
		return EXCEPTION_DATATYPE_MISALIGNMENT;
	case SIGTRAP:
		return EXCEPTION_BREAKPOINT;
	default:
		return 0xC0000000L;  /* Generic exception */
	}
}

/*
 * Signal handler - converts signals to SEH exceptions
 */
void
_seh_signal_handler(int signo, void *info, void *context)
{
	struct _seh_registration *reg;
	int disposition;
	int (*filter)(void);

	/* Initialize current exception */
	_seh_current_exception.ExceptionCode = signal_to_exception_code(signo);
	_seh_current_exception.ExceptionFlags = 0;
	_seh_current_exception.ExceptionRecord = NULL;
	_seh_current_exception.ExceptionAddress = NULL;
	_seh_current_exception.NumberParameters = 0;
	_seh_current_context = context;

	/* Extract detailed context information (IP, fault address, etc.) */
	_seh_extract_signal_context(context, &_seh_current_exception);

	/* Walk the exception chain looking for a handler */
	for (reg = _seh_chain_head; reg != NULL; reg = reg->prev) {
		if (reg->filter == NULL)
			continue;  /* Try-finally, not try-except */

		/* Call the filter expression */
		filter = (int (*)(void))reg->filter;
		disposition = filter();

		if (disposition == EXCEPTION_EXECUTE_HANDLER) {
			/* Handler will execute - longjmp to it */
			longjmp(reg->jmpbuf, 1);
		} else if (disposition == EXCEPTION_CONTINUE_EXECUTION) {
			/* Continue execution at faulting instruction */
			return;
		}
		/* EXCEPTION_CONTINUE_SEARCH - keep looking */
	}

	/* No handler found - restore default handler and re-raise */
	signal(signo, SIG_DFL);
	raise(signo);
}

/*
 * Register an exception handler
 */
void
_seh_register(struct _seh_registration *reg, void *handler, void *filter)
{
	if (!_seh_signals_initialized)
		_seh_init_signals();

	reg->handler = handler;
	reg->filter = filter;
	reg->state = 0;
	reg->finally_executed = 0;

	/* Link into chain */
	reg->prev = _seh_chain_head;
	_seh_chain_head = reg;
}

/*
 * Unregister an exception handler
 */
void
_seh_unregister(struct _seh_registration *reg)
{
	/* Unlink from chain */
	if (_seh_chain_head == reg)
		_seh_chain_head = reg->prev;
}

/*
 * Execute finally block
 */
void
_seh_execute_finally(struct _seh_registration *reg, void (*finally_block)(void))
{
	if (reg->finally_executed)
		return;

	reg->finally_executed = 1;
	if (finally_block)
		finally_block();
}

/*
 * Raise an exception manually
 */
void
_seh_raise_exception(unsigned long code, unsigned long flags,
                     unsigned long nparams, unsigned long *params)
{
	struct _seh_registration *reg;
	int disposition;
	int (*filter)(void);

	/* Initialize exception record */
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

	/* Walk the exception chain */
	for (reg = _seh_chain_head; reg != NULL; reg = reg->prev) {
		if (reg->filter == NULL)
			continue;

		filter = (int (*)(void))reg->filter;
		disposition = filter();

		if (disposition == EXCEPTION_EXECUTE_HANDLER) {
			longjmp(reg->jmpbuf, 1);
		} else if (disposition == EXCEPTION_CONTINUE_EXECUTION) {
			return;
		}
	}

	/* No handler - abort */
	fprintf(stderr, "Unhandled exception: 0x%08lX\n", code);
	abort();
}

/*
 * Get exception code
 */
unsigned long
_seh_get_exception_code(void)
{
	return _seh_current_exception.ExceptionCode;
}

/*
 * Get exception information
 */
struct _seh_exception_pointers *
_seh_get_exception_info(void)
{
	static __thread struct _seh_exception_pointers ptrs;

	ptrs.ExceptionRecord = &_seh_current_exception;
	ptrs.ContextRecord = _seh_current_context;

	return &ptrs;
}

#else /* _WIN32 */

/*
 * Windows implementation uses native SEH
 * These are stubs - Windows compiler will use native SEH
 */

void
_seh_register(struct _seh_registration *reg, void *handler, void *filter)
{
	/* Windows uses native SEH registration */
}

void
_seh_unregister(struct _seh_registration *reg)
{
	/* Windows uses native SEH unregistration */
}

void
_seh_execute_finally(struct _seh_registration *reg, void (*finally_block)(void))
{
	if (finally_block)
		finally_block();
}

#endif /* _WIN32 */
