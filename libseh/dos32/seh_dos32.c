/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * SEH Runtime Implementation for DOS 32-bit (DJGPP/Watcom)
 *
 * DOS32 has better exception support than DOS16 via DPMI.
 * Can hook CPU exceptions in protected mode.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include "../seh.h"

/* Exception chain (no threads in DOS32) */
static struct _seh_registration *_seh_chain_head = NULL;

/* Current exception information */
static struct _seh_exception_record _seh_current_exception;
static void *_seh_current_context = NULL;

void
_seh_register(struct _seh_registration *reg, void *handler, void *filter)
{
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
	static struct _seh_exception_pointers ptrs;

	ptrs.ExceptionRecord = &_seh_current_exception;
	ptrs.ContextRecord = _seh_current_context;

	return &ptrs;
}
