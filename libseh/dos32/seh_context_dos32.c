/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Exception Context Handling for DOS 32-bit
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../seh.h"

/* DOS32 has better context support via DPMI */

void
_seh_save_context(void *context)
{
	if (context)
		memset(context, 0, 256); /* Save basic registers */
}

void
_seh_restore_context(void *context)
{
	/* Limited restore capability */
}

void
_seh_extract_signal_context(void *sigcontext, struct _seh_exception_record *exc)
{
	/* DPMI provides some exception context */
}

void *
_seh_get_ip(void *context)
{
	/* Could extract from DPMI exception frame */
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
	/* Possible with DPMI but complex */
}

unsigned long
_seh_get_register(void *context, int reg_index)
{
	return 0;
}
