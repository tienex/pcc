/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Exception Context Handling for UEFI
 */

#include <string.h>
#include "../seh.h"

/* UEFI has limited context support */

void
_seh_save_context(void *context)
{
	if (context)
		memset(context, 0, 512);
}

void
_seh_restore_context(void *context)
{
	/* Limited restore capability in UEFI */
}

void
_seh_extract_signal_context(void *sigcontext, struct _seh_exception_record *exc)
{
	/* UEFI doesn't have signal-based exceptions */
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
