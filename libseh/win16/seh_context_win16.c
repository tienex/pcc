/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Exception Context Handling for Windows 16-bit
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../seh.h"

/* Limited context support in Win16 */

void
_seh_save_context(void *context)
{
	if (context)
		memset(context, 0, sizeof(jmp_buf));
}

void
_seh_restore_context(void *context)
{
	/* Cannot restore context on Win16 */
}

void
_seh_extract_signal_context(void *sigcontext, struct _seh_exception_record *exc)
{
	/* No signal context on Win16 */
}

void *
_seh_get_ip(void *context)
{
	return NULL;
}

void *
_seh_get_sp(void *context)
{
	return NULL;
}

void
_seh_set_ip(void *context, void *ip)
{
	/* Cannot set IP on Win16 */
}

unsigned long
_seh_get_register(void *context, int reg_index)
{
	return 0;
}
