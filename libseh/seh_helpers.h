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

#ifndef _SEH_HELPERS_H_
#define _SEH_HELPERS_H_

#include "seh.h"

/*
 * Internal variables (defined in seh.c)
 */
#ifndef _WIN32
extern __thread struct _seh_exception_record _seh_current_exception;
#endif

/*
 * SEH Helper Macros for Manual SEH Usage
 *
 * Until the compiler has full code generation for __try/__except/__finally,
 * these macros can be used to manually implement SEH in C code.
 *
 * NOTE: These are temporary helpers. Once compiler code generation is
 * complete, the compiler will automatically emit the necessary calls.
 */

/*
 * Manual __try / __except implementation
 *
 * Usage:
 *   SEH_TRY {
 *       // Protected code
 *   } SEH_EXCEPT(filter_expression) {
 *       // Exception handler
 *   } SEH_END
 */

#define SEH_TRY \
	do { \
		struct _seh_registration _seh_reg; \
		int _seh_filter_result; \
		_seh_register(&_seh_reg, NULL, (void*)&&_seh_filter_label); \
		if (setjmp(_seh_reg.jmpbuf) == 0) {

#define SEH_EXCEPT(filter_expr) \
		} else { \
			goto _seh_handler_label; \
		} \
		goto _seh_end_label; \
		_seh_filter_label: \
		_seh_filter_result = (filter_expr); \
		return _seh_filter_result; \
		_seh_handler_label:

#define SEH_END \
		_seh_end_label: \
		_seh_unregister(&_seh_reg); \
	} while (0)

/*
 * Manual __try / __finally implementation
 *
 * Usage:
 *   SEH_TRY_FINALLY {
 *       // Protected code
 *   } SEH_FINALLY {
 *       // Cleanup code (always executes)
 *   } SEH_END_FINALLY
 */

#define SEH_TRY_FINALLY \
	do { \
		struct _seh_registration _seh_reg; \
		int _seh_abnormal = 0; \
		_seh_register(&_seh_reg, (void*)&&_seh_finally_label, NULL); \
		if (setjmp(_seh_reg.jmpbuf) != 0) \
			_seh_abnormal = 1;

#define SEH_FINALLY \
		goto _seh_finally_label; \
		_seh_finally_label: \
		{

#define SEH_END_FINALLY \
		} \
		_seh_unregister(&_seh_reg); \
		if (_seh_abnormal) \
			longjmp(_seh_reg.jmpbuf, 1); \
	} while (0)

/*
 * GetExceptionCode() / GetExceptionInformation() equivalents
 * These are callable from filter expressions and exception handlers
 */

static inline unsigned long
GetExceptionCode(void)
{
	return _seh_get_exception_code();
}

static inline struct _seh_exception_pointers *
GetExceptionInformation(void)
{
	return _seh_get_exception_info();
}

/*
 * AbnormalTermination() for __finally blocks
 * Returns non-zero if finally block is executing due to exception/goto/return
 */

#define AbnormalTermination() (_seh_abnormal)

/*
 * Simplified exception raising
 */

static inline void
RaiseException(unsigned long code, unsigned long flags)
{
	_seh_raise_exception(code, flags, 0, NULL);
}

/*
 * Exception filter return values (for convenience)
 */

#ifndef EXCEPTION_EXECUTE_HANDLER
#define EXCEPTION_EXECUTE_HANDLER      1
#define EXCEPTION_CONTINUE_SEARCH      0
#define EXCEPTION_CONTINUE_EXECUTION  (-1)
#endif

/*
 * Common exception filter patterns
 */

/* Accept all exceptions */
#define EXCEPTION_FILTER_ALL  EXCEPTION_EXECUTE_HANDLER

/* Accept specific exception code */
#define EXCEPTION_FILTER_CODE(code) \
	(GetExceptionCode() == (code) ? EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH)

/* Accept access violations only */
#define EXCEPTION_FILTER_ACCESS_VIOLATION \
	EXCEPTION_FILTER_CODE(EXCEPTION_ACCESS_VIOLATION)

/* Accept divide by zero only */
#define EXCEPTION_FILTER_DIVIDE_BY_ZERO \
	EXCEPTION_FILTER_CODE(EXCEPTION_INT_DIVIDE_BY_ZERO)

#endif /* _SEH_HELPERS_H_ */
