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
 * Exception Context Handling for DOS
 *
 * DOS has limited context support - we provide minimal stubs.
 */

void
_seh_save_context(void *context)
{
	/* Limited context support on DOS */
	if (context)
		memset(context, 0, sizeof(jmp_buf));
}

void
_seh_restore_context(void *context)
{
	/* Cannot restore context on DOS */
}

void
_seh_extract_signal_context(void *sigcontext, struct _seh_exception_record *exc)
{
	/* No signal context on DOS */
}

void *
_seh_get_ip(void *context)
{
	/* Cannot get IP from context on DOS */
	return NULL;
}

void *
_seh_get_sp(void *context)
{
	/* Cannot get SP from context on DOS */
	return NULL;
}

void
_seh_set_ip(void *context, void *ip)
{
	/* Cannot set IP in context on DOS */
}

unsigned long
_seh_get_register(void *context, int reg_index)
{
	/* Cannot get registers from context on DOS */
	return 0;
}
