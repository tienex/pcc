/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * SEH Runtime Implementation for AmigaOS
 *
 * AmigaOS uses native exception handling via Exec signals and traps.
 * Supports both 68k and PowerPC (OS4) versions.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include "../seh.h"

/* AmigaOS uses Exec tasks, not POSIX threads */
#ifdef __amigaos4__
  /* OS4 has some thread support */
  __thread struct _seh_registration *_seh_chain_head = NULL;
  __thread struct _seh_exception_record _seh_current_exception;
  __thread void *_seh_current_context = NULL;
#else
  /* Classic AmigaOS - single task context */
  static struct _seh_registration *_seh_chain_head = NULL;
  static struct _seh_exception_record _seh_current_exception;
  static void *_seh_current_context = NULL;
#endif

/* Exec signal numbers for exceptions */
#define SIGBREAKF_CTRL_C   (1L<<12)
#define SIGBREAKF_CTRL_D   (1L<<13)
#define SIGBREAKF_CTRL_E   (1L<<14)
#define SIGBREAKF_CTRL_F   (1L<<15)

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

void
_seh_unregister(struct _seh_registration *reg)
{
	/* Unlink from chain */
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

	/* No handler - display alert and abort */
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
#ifdef __amigaos4__
	static __thread struct _seh_exception_pointers ptrs;
#else
	static struct _seh_exception_pointers ptrs;
#endif

	ptrs.ExceptionRecord = &_seh_current_exception;
	ptrs.ContextRecord = _seh_current_context;

	return &ptrs;
}
