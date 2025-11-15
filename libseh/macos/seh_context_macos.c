/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Exception Context Handling for Mac OS Classic
 */

#include <string.h>
#include "../seh.h"

/* Mac OS Classic 68k/PowerPC context - limited support */

void
_seh_save_context(void *context)
{
	if (context)
		memset(context, 0, 512);
}

void
_seh_restore_context(void *context)
{
	/* Limited restore capability */
}

void
_seh_extract_signal_context(void *sigcontext, struct _seh_exception_record *exc)
{
	/* Mac OS doesn't provide standard exception context */
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
	/* Not supported */
}

unsigned long
_seh_get_register(void *context, int reg_index)
{
	return 0;
}
