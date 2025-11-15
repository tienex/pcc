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

/* Needed for REG_* register name macros on Linux */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <ucontext.h>
#include "../seh.h"

/*
 * Exception Context Handling for Unix/POSIX
 *
 * This module provides platform-specific CPU context capture and
 * restoration for exception handling. The context includes CPU
 * registers, stack pointer, instruction pointer, etc.
 */

/*
 * Platform-specific context structure
 * This wraps the platform's native context (ucontext_t on Unix)
 */
struct _seh_cpu_context {
	ucontext_t uc;
	/* Additional platform-specific state */
#if defined(__i386__) || defined(__x86_64__)
	unsigned long trap_no;
	unsigned long error_code;
	unsigned long cr2;  /* Faulting address for SIGSEGV */
#elif defined(__arm__) || defined(__aarch64__)
	unsigned long fault_address;
	unsigned long trap_no;
#endif
};

/*
 * Save current CPU context
 */
void
_seh_save_context(void *context)
{
	struct _seh_cpu_context *ctx = (struct _seh_cpu_context *)context;

	if (ctx == NULL)
		return;

	/* Save CPU registers using getcontext() */
	if (getcontext(&ctx->uc) != 0) {
		/* Failed to get context */
		memset(ctx, 0, sizeof(*ctx));
	}
}

/*
 * Restore CPU context
 */
void
_seh_restore_context(void *context)
{
	struct _seh_cpu_context *ctx = (struct _seh_cpu_context *)context;

	if (ctx == NULL)
		return;

	/* Restore CPU registers using setcontext() */
	setcontext(&ctx->uc);
	/* Does not return */
}

/*
 * Extract exception context from signal context
 */
void
_seh_extract_signal_context(void *sigcontext, struct _seh_exception_record *exc)
{
	if (sigcontext == NULL || exc == NULL)
		return;

#ifdef __linux__
	ucontext_t *uc = (ucontext_t *)sigcontext;

#if defined(__x86_64__)
	/* AMD64/x86-64 Linux */
	exc->ExceptionAddress = (void *)uc->uc_mcontext.gregs[REG_RIP];

	if (exc->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
		/* Store faulting address */
		exc->NumberParameters = 2;
		/* Access type: 0 = read, 1 = write */
		exc->ExceptionInformation[0] =
		    (uc->uc_mcontext.gregs[REG_ERR] & 0x2) ? 1 : 0;
		/* Faulting address */
		exc->ExceptionInformation[1] = uc->uc_mcontext.gregs[REG_CR2];
	}

#elif defined(__i386__)
	/* i386 Linux */
	exc->ExceptionAddress = (void *)uc->uc_mcontext.gregs[REG_EIP];

	if (exc->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
		exc->NumberParameters = 2;
		exc->ExceptionInformation[0] =
		    (uc->uc_mcontext.gregs[REG_ERR] & 0x2) ? 1 : 0;
		exc->ExceptionInformation[1] = uc->uc_mcontext.cr2;
	}

#elif defined(__arm__)
	/* ARM Linux */
	exc->ExceptionAddress = (void *)uc->uc_mcontext.arm_pc;

#elif defined(__aarch64__)
	/* ARM64 Linux */
	exc->ExceptionAddress = (void *)uc->uc_mcontext.pc;

#endif

#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
	ucontext_t *uc = (ucontext_t *)sigcontext;

#if defined(__x86_64__)
	exc->ExceptionAddress = (void *)uc->uc_mcontext.mc_rip;
#elif defined(__i386__)
	exc->ExceptionAddress = (void *)uc->uc_mcontext.mc_eip;
#elif defined(__arm__)
	exc->ExceptionAddress = (void *)uc->uc_mcontext.__gregs[_REG_PC];
#endif

#elif defined(__APPLE__)
	/* macOS */
	ucontext_t *uc = (ucontext_t *)sigcontext;

#if defined(__x86_64__)
	exc->ExceptionAddress = (void *)uc->uc_mcontext->__ss.__rip;

	if (exc->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
		exc->NumberParameters = 2;
		/* Access type from error code */
		exc->ExceptionInformation[0] =
		    (uc->uc_mcontext->__es.__err & 0x2) ? 1 : 0;
		/* Faulting address from exception state */
		exc->ExceptionInformation[1] = uc->uc_mcontext->__es.__faultvaddr;
	}

#elif defined(__i386__)
	exc->ExceptionAddress = (void *)uc->uc_mcontext->__ss.__eip;

	if (exc->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
		exc->NumberParameters = 2;
		exc->ExceptionInformation[0] =
		    (uc->uc_mcontext->__es.__err & 0x2) ? 1 : 0;
		exc->ExceptionInformation[1] = uc->uc_mcontext->__es.__faultvaddr;
	}

#elif defined(__arm__)
	exc->ExceptionAddress = (void *)uc->uc_mcontext->__ss.__pc;

#elif defined(__aarch64__)
	exc->ExceptionAddress = (void *)uc->uc_mcontext->__ss.__pc;

	if (exc->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
		exc->NumberParameters = 2;
		exc->ExceptionInformation[0] = 0; /* Not easily available */
		exc->ExceptionInformation[1] = uc->uc_mcontext->__es.__far;
	}
#endif

#endif /* Platform checks */
}

/*
 * Get instruction pointer from context
 */
void *
_seh_get_ip(void *context)
{
	struct _seh_cpu_context *ctx = (struct _seh_cpu_context *)context;

	if (ctx == NULL)
		return NULL;

#ifdef __linux__
	#if defined(__x86_64__)
		return (void *)ctx->uc.uc_mcontext.gregs[REG_RIP];
	#elif defined(__i386__)
		return (void *)ctx->uc.uc_mcontext.gregs[REG_EIP];
	#elif defined(__arm__)
		return (void *)ctx->uc.uc_mcontext.arm_pc;
	#elif defined(__aarch64__)
		return (void *)ctx->uc.uc_mcontext.pc;
	#else
		return NULL;
	#endif
#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
	#if defined(__x86_64__)
		return (void *)ctx->uc.uc_mcontext.mc_rip;
	#elif defined(__i386__)
		return (void *)ctx->uc.uc_mcontext.mc_eip;
	#elif defined(__arm__)
		return (void *)ctx->uc.uc_mcontext.__gregs[_REG_PC];
	#else
		return NULL;
	#endif
#elif defined(__APPLE__)
	#if defined(__x86_64__)
		return (void *)ctx->uc.uc_mcontext->__ss.__rip;
	#elif defined(__i386__)
		return (void *)ctx->uc.uc_mcontext->__ss.__eip;
	#elif defined(__arm__)
		return (void *)ctx->uc.uc_mcontext->__ss.__pc;
	#elif defined(__aarch64__)
		return (void *)ctx->uc.uc_mcontext->__ss.__pc;
	#else
		return NULL;
	#endif
#else
	return NULL;
#endif
}

/*
 * Get stack pointer from context
 */
void *
_seh_get_sp(void *context)
{
	struct _seh_cpu_context *ctx = (struct _seh_cpu_context *)context;

	if (ctx == NULL)
		return NULL;

#ifdef __linux__
	#if defined(__x86_64__)
		return (void *)ctx->uc.uc_mcontext.gregs[REG_RSP];
	#elif defined(__i386__)
		return (void *)ctx->uc.uc_mcontext.gregs[REG_ESP];
	#elif defined(__arm__)
		return (void *)ctx->uc.uc_mcontext.arm_sp;
	#elif defined(__aarch64__)
		return (void *)ctx->uc.uc_mcontext.sp;
	#else
		return NULL;
	#endif
#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
	#if defined(__x86_64__)
		return (void *)ctx->uc.uc_mcontext.mc_rsp;
	#elif defined(__i386__)
		return (void *)ctx->uc.uc_mcontext.mc_esp;
	#elif defined(__arm__)
		return (void *)ctx->uc.uc_mcontext.__gregs[_REG_SP];
	#else
		return NULL;
	#endif
#elif defined(__APPLE__)
	#if defined(__x86_64__)
		return (void *)ctx->uc.uc_mcontext->__ss.__rsp;
	#elif defined(__i386__)
		return (void *)ctx->uc.uc_mcontext->__ss.__esp;
	#elif defined(__arm__)
		return (void *)ctx->uc.uc_mcontext->__ss.__sp;
	#elif defined(__aarch64__)
		return (void *)ctx->uc.uc_mcontext->__ss.__sp;
	#else
		return NULL;
	#endif
#else
	return NULL;
#endif
}

/*
 * Set instruction pointer in context
 */
void
_seh_set_ip(void *context, void *ip)
{
	struct _seh_cpu_context *ctx = (struct _seh_cpu_context *)context;

	if (ctx == NULL)
		return;

#ifdef __linux__
	#if defined(__x86_64__)
		ctx->uc.uc_mcontext.gregs[REG_RIP] = (unsigned long)ip;
	#elif defined(__i386__)
		ctx->uc.uc_mcontext.gregs[REG_EIP] = (unsigned long)ip;
	#elif defined(__arm__)
		ctx->uc.uc_mcontext.arm_pc = (unsigned long)ip;
	#elif defined(__aarch64__)
		ctx->uc.uc_mcontext.pc = (unsigned long)ip;
	#endif
#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
	#if defined(__x86_64__)
		ctx->uc.uc_mcontext.mc_rip = (unsigned long)ip;
	#elif defined(__i386__)
		ctx->uc.uc_mcontext.mc_eip = (unsigned long)ip;
	#elif defined(__arm__)
		ctx->uc.uc_mcontext.__gregs[_REG_PC] = (unsigned long)ip;
	#endif
#elif defined(__APPLE__)
	#if defined(__x86_64__)
		ctx->uc.uc_mcontext->__ss.__rip = (unsigned long)ip;
	#elif defined(__i386__)
		ctx->uc.uc_mcontext->__ss.__eip = (unsigned long)ip;
	#elif defined(__arm__)
		ctx->uc.uc_mcontext->__ss.__pc = (unsigned long)ip;
	#elif defined(__aarch64__)
		ctx->uc.uc_mcontext->__ss.__pc = (unsigned long)ip;
	#endif
#endif
}

/*
 * Get register value by index
 * This is platform-specific and matches Windows CONTEXT structure
 */
unsigned long
_seh_get_register(void *context, int reg_index)
{
	struct _seh_cpu_context *ctx = (struct _seh_cpu_context *)context;

	if (ctx == NULL)
		return 0;

#ifdef __linux__
	#if defined(__x86_64__)
		/* AMD64 register indices (Windows CONTEXT compatible) */
		switch (reg_index) {
		case 0:  return ctx->uc.uc_mcontext.gregs[REG_RAX];
		case 1:  return ctx->uc.uc_mcontext.gregs[REG_RCX];
		case 2:  return ctx->uc.uc_mcontext.gregs[REG_RDX];
		case 3:  return ctx->uc.uc_mcontext.gregs[REG_RBX];
		case 4:  return ctx->uc.uc_mcontext.gregs[REG_RSP];
		case 5:  return ctx->uc.uc_mcontext.gregs[REG_RBP];
		case 6:  return ctx->uc.uc_mcontext.gregs[REG_RSI];
		case 7:  return ctx->uc.uc_mcontext.gregs[REG_RDI];
		case 8:  return ctx->uc.uc_mcontext.gregs[REG_R8];
		case 9:  return ctx->uc.uc_mcontext.gregs[REG_R9];
		case 10: return ctx->uc.uc_mcontext.gregs[REG_R10];
		case 11: return ctx->uc.uc_mcontext.gregs[REG_R11];
		case 12: return ctx->uc.uc_mcontext.gregs[REG_R12];
		case 13: return ctx->uc.uc_mcontext.gregs[REG_R13];
		case 14: return ctx->uc.uc_mcontext.gregs[REG_R14];
		case 15: return ctx->uc.uc_mcontext.gregs[REG_R15];
		default: return 0;
		}
	#elif defined(__i386__)
		/* i386 register indices */
		switch (reg_index) {
		case 0:  return ctx->uc.uc_mcontext.gregs[REG_EAX];
		case 1:  return ctx->uc.uc_mcontext.gregs[REG_ECX];
		case 2:  return ctx->uc.uc_mcontext.gregs[REG_EDX];
		case 3:  return ctx->uc.uc_mcontext.gregs[REG_EBX];
		case 4:  return ctx->uc.uc_mcontext.gregs[REG_ESP];
		case 5:  return ctx->uc.uc_mcontext.gregs[REG_EBP];
		case 6:  return ctx->uc.uc_mcontext.gregs[REG_ESI];
		case 7:  return ctx->uc.uc_mcontext.gregs[REG_EDI];
		default: return 0;
		}
	#else
		return 0;
	#endif
#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
	#if defined(__x86_64__)
		/* BSD AMD64 register access */
		switch (reg_index) {
		case 0:  return ctx->uc.uc_mcontext.mc_rax;
		case 1:  return ctx->uc.uc_mcontext.mc_rcx;
		case 2:  return ctx->uc.uc_mcontext.mc_rdx;
		case 3:  return ctx->uc.uc_mcontext.mc_rbx;
		case 4:  return ctx->uc.uc_mcontext.mc_rsp;
		case 5:  return ctx->uc.uc_mcontext.mc_rbp;
		case 6:  return ctx->uc.uc_mcontext.mc_rsi;
		case 7:  return ctx->uc.uc_mcontext.mc_rdi;
		case 8:  return ctx->uc.uc_mcontext.mc_r8;
		case 9:  return ctx->uc.uc_mcontext.mc_r9;
		case 10: return ctx->uc.uc_mcontext.mc_r10;
		case 11: return ctx->uc.uc_mcontext.mc_r11;
		case 12: return ctx->uc.uc_mcontext.mc_r12;
		case 13: return ctx->uc.uc_mcontext.mc_r13;
		case 14: return ctx->uc.uc_mcontext.mc_r14;
		case 15: return ctx->uc.uc_mcontext.mc_r15;
		default: return 0;
		}
	#elif defined(__i386__)
		/* BSD i386 register access */
		switch (reg_index) {
		case 0:  return ctx->uc.uc_mcontext.mc_eax;
		case 1:  return ctx->uc.uc_mcontext.mc_ecx;
		case 2:  return ctx->uc.uc_mcontext.mc_edx;
		case 3:  return ctx->uc.uc_mcontext.mc_ebx;
		case 4:  return ctx->uc.uc_mcontext.mc_esp;
		case 5:  return ctx->uc.uc_mcontext.mc_ebp;
		case 6:  return ctx->uc.uc_mcontext.mc_esi;
		case 7:  return ctx->uc.uc_mcontext.mc_edi;
		default: return 0;
		}
	#else
		return 0;
	#endif
#elif defined(__APPLE__)
	#if defined(__x86_64__)
		/* macOS AMD64 register access */
		switch (reg_index) {
		case 0:  return ctx->uc.uc_mcontext->__ss.__rax;
		case 1:  return ctx->uc.uc_mcontext->__ss.__rcx;
		case 2:  return ctx->uc.uc_mcontext->__ss.__rdx;
		case 3:  return ctx->uc.uc_mcontext->__ss.__rbx;
		case 4:  return ctx->uc.uc_mcontext->__ss.__rsp;
		case 5:  return ctx->uc.uc_mcontext->__ss.__rbp;
		case 6:  return ctx->uc.uc_mcontext->__ss.__rsi;
		case 7:  return ctx->uc.uc_mcontext->__ss.__rdi;
		case 8:  return ctx->uc.uc_mcontext->__ss.__r8;
		case 9:  return ctx->uc.uc_mcontext->__ss.__r9;
		case 10: return ctx->uc.uc_mcontext->__ss.__r10;
		case 11: return ctx->uc.uc_mcontext->__ss.__r11;
		case 12: return ctx->uc.uc_mcontext->__ss.__r12;
		case 13: return ctx->uc.uc_mcontext->__ss.__r13;
		case 14: return ctx->uc.uc_mcontext->__ss.__r14;
		case 15: return ctx->uc.uc_mcontext->__ss.__r15;
		default: return 0;
		}
	#elif defined(__i386__)
		/* macOS i386 register access */
		switch (reg_index) {
		case 0:  return ctx->uc.uc_mcontext->__ss.__eax;
		case 1:  return ctx->uc.uc_mcontext->__ss.__ecx;
		case 2:  return ctx->uc.uc_mcontext->__ss.__edx;
		case 3:  return ctx->uc.uc_mcontext->__ss.__ebx;
		case 4:  return ctx->uc.uc_mcontext->__ss.__esp;
		case 5:  return ctx->uc.uc_mcontext->__ss.__ebp;
		case 6:  return ctx->uc.uc_mcontext->__ss.__esi;
		case 7:  return ctx->uc.uc_mcontext->__ss.__edi;
		default: return 0;
		}
	#else
		return 0;
	#endif
#else
	return 0;
#endif
}
