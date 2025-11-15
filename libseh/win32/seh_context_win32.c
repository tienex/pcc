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

#include <windows.h>
#include "../seh.h"

/*
 * Windows implementation - use native CONTEXT structure
 */

void
_seh_save_context(void *context)
{
	CONTEXT *ctx = (CONTEXT *)context;

	if (ctx == NULL)
		return;

	ctx->ContextFlags = CONTEXT_FULL;
	RtlCaptureContext(ctx);
}

void
_seh_restore_context(void *context)
{
	CONTEXT *ctx = (CONTEXT *)context;

	if (ctx == NULL)
		return;

	RtlRestoreContext(ctx, NULL);
	/* Does not return */
}

void
_seh_extract_signal_context(void *sigcontext, struct _seh_exception_record *exc)
{
	/* On Windows, context is already in the exception record */
}

void *
_seh_get_ip(void *context)
{
	CONTEXT *ctx = (CONTEXT *)context;

	if (ctx == NULL)
		return NULL;

#if defined(_M_AMD64)
	return (void *)ctx->Rip;
#elif defined(_M_IX86)
	return (void *)ctx->Eip;
#elif defined(_M_ARM64)
	return (void *)ctx->Pc;
#else
	return NULL;
#endif
}

void *
_seh_get_sp(void *context)
{
	CONTEXT *ctx = (CONTEXT *)context;

	if (ctx == NULL)
		return NULL;

#if defined(_M_AMD64)
	return (void *)ctx->Rsp;
#elif defined(_M_IX86)
	return (void *)ctx->Esp;
#elif defined(_M_ARM64)
	return (void *)ctx->Sp;
#else
	return NULL;
#endif
}

void
_seh_set_ip(void *context, void *ip)
{
	CONTEXT *ctx = (CONTEXT *)context;

	if (ctx == NULL)
		return;

#if defined(_M_AMD64)
	ctx->Rip = (DWORD64)ip;
#elif defined(_M_IX86)
	ctx->Eip = (DWORD)ip;
#elif defined(_M_ARM64)
	ctx->Pc = (DWORD64)ip;
#endif
}

unsigned long
_seh_get_register(void *context, int reg_index)
{
	CONTEXT *ctx = (CONTEXT *)context;

	if (ctx == NULL)
		return 0;

#if defined(_M_AMD64)
	switch (reg_index) {
	case 0:  return ctx->Rax;
	case 1:  return ctx->Rcx;
	case 2:  return ctx->Rdx;
	case 3:  return ctx->Rbx;
	case 4:  return ctx->Rsp;
	case 5:  return ctx->Rbp;
	case 6:  return ctx->Rsi;
	case 7:  return ctx->Rdi;
	case 8:  return ctx->R8;
	case 9:  return ctx->R9;
	case 10: return ctx->R10;
	case 11: return ctx->R11;
	case 12: return ctx->R12;
	case 13: return ctx->R13;
	case 14: return ctx->R14;
	case 15: return ctx->R15;
	default: return 0;
	}
#elif defined(_M_IX86)
	switch (reg_index) {
	case 0:  return ctx->Eax;
	case 1:  return ctx->Ecx;
	case 2:  return ctx->Edx;
	case 3:  return ctx->Ebx;
	case 4:  return ctx->Esp;
	case 5:  return ctx->Ebp;
	case 6:  return ctx->Esi;
	case 7:  return ctx->Edi;
	default: return 0;
	}
#else
	return 0;
#endif
}
