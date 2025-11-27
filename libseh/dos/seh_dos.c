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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include "../seh.h"

/*
 * SEH Runtime Implementation for DOS
 *
 * DOS doesn't have signals or advanced exception handling,
 * so we use a simplified setjmp/longjmp approach with interrupt hooks.
 */

/*
 * Exception chain (no threads on DOS, so no need for TLS)
 */
static struct _seh_registration *_seh_chain_head = NULL;

/*
 * Current exception information
 */
static struct _seh_exception_record _seh_current_exception;
static void *_seh_current_context = NULL;

/*
 * Register an exception handler
 */
void
_seh_register(struct _seh_registration *reg, void *handler, void *filter)
{
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
	static struct _seh_exception_pointers ptrs;

	ptrs.ExceptionRecord = &_seh_current_exception;
	ptrs.ContextRecord = _seh_current_context;

	return &ptrs;
}
