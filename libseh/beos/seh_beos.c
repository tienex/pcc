/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * SEH Runtime Implementation for BeOS
 *
 * BeOS is POSIX-like, similar to Unix implementation.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <setjmp.h>
#include "../seh.h"

__thread struct _seh_registration *_seh_chain_head = NULL;
__thread struct _seh_exception_record _seh_current_exception;
__thread void *_seh_current_context = NULL;

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

	sigaction(SIGSEGV, &sa, NULL);
	sigaction(SIGFPE, &sa, NULL);
	sigaction(SIGILL, &sa, NULL);
	sigaction(SIGBUS, &sa, NULL);

	_seh_signals_initialized = 1;
}

static unsigned long
signal_to_exception_code(int signo)
{
	switch (signo) {
	case SIGSEGV: return EXCEPTION_ACCESS_VIOLATION;
	case SIGFPE: return EXCEPTION_FLT_DIVIDE_BY_ZERO;
	case SIGILL: return EXCEPTION_ILLEGAL_INSTRUCTION;
	case SIGBUS: return EXCEPTION_DATATYPE_MISALIGNMENT;
	default: return 0xC0000000L;
	}
}

void
_seh_signal_handler(int signo, void *info, void *context)
{
	struct _seh_registration *reg;
	int disposition;
	int (*filter)(void);

	_seh_current_exception.ExceptionCode = signal_to_exception_code(signo);
	_seh_current_exception.ExceptionFlags = 0;
	_seh_current_exception.ExceptionRecord = NULL;
	_seh_current_exception.ExceptionAddress = NULL;
	_seh_current_exception.NumberParameters = 0;
	_seh_current_context = context;

	_seh_extract_signal_context(context, &_seh_current_exception);

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

	signal(signo, SIG_DFL);
	raise(signo);
}

void
_seh_register(struct _seh_registration *reg, void *handler, void *filter)
{
	if (!_seh_signals_initialized)
		_seh_init_signals();
	reg->handler = handler;
	reg->filter = filter;
	reg->state = 0;
	reg->finally_executed = 0;
	reg->prev = _seh_chain_head;
	_seh_chain_head = reg;
}

void
_seh_unregister(struct _seh_registration *reg)
{
	if (_seh_chain_head == reg)
		_seh_chain_head = reg->prev;
}

void
_seh_execute_finally(struct _seh_registration *reg, void (*finally_block)(void))
{
	if (reg->finally_executed)
		return;
	reg->finally_executed = 1;
	if (finally_block)
		finally_block();
}

void
_seh_raise_exception(unsigned long code, unsigned long flags,
                     unsigned long nparams, unsigned long *params)
{
	struct _seh_registration *reg;
	int disposition;
	int (*filter)(void);

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

	fprintf(stderr, "Unhandled exception: 0x%08lX\n", code);
	abort();
}

unsigned long
_seh_get_exception_code(void)
{
	return _seh_current_exception.ExceptionCode;
}

struct _seh_exception_pointers *
_seh_get_exception_info(void)
{
	static __thread struct _seh_exception_pointers ptrs;
	ptrs.ExceptionRecord = &_seh_current_exception;
	ptrs.ContextRecord = _seh_current_context;
	return &ptrs;
}
