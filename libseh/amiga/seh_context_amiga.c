/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Exception Context Handling for AmigaOS
 */

#include <string.h>
#include "../seh.h"

/*
 * AmigaOS context handling
 * Context structure varies between 68k and PowerPC
 */

void
_seh_save_context(void *context)
{
	if (context)
		memset(context, 0, 512);
}

void
_seh_restore_context(void *context)
{
	/* Limited restore capability on AmigaOS */
}

void
_seh_extract_signal_context(void *sigcontext, struct _seh_exception_record *exc)
{
	/* AmigaOS Exec signals provide limited context */
}

void *
_seh_get_ip(void *context)
{
	/* Would need to access task's context structure */
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
	/* Limited capability */
}

unsigned long
_seh_get_register(void *context, int reg_index)
{
	return 0;
}
