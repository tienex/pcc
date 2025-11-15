/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Exception Context Handling for OS/2 32-bit
 */

#define INCL_BASE
#include <os2.h>
#include "../seh.h"
#include <string.h>

void _seh_save_context(void *context) { if (context) memset(context, 0, 256); }
void _seh_restore_context(void *context) { }
void _seh_extract_signal_context(void *sigcontext, struct _seh_exception_record *exc) { }
void *_seh_get_ip(void *context) { return NULL; }
void *_seh_get_sp(void *context) { return NULL; }
void _seh_set_ip(void *context, void *ip) { }
unsigned long _seh_get_register(void *context, int reg_index) { return 0; }
