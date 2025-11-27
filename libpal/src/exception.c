/*
 * Copyright (c) 2025 PCC Paradox PAL Runtime Library
 *
 * Exception handling support
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../include/palrt.h"

/* Thread-local exception (simplified - not thread-safe) */
static PAL_Exception current_exception = {0, "", NULL, 0};
static int exception_set = 0;

PAL_Exception *pal_get_exception(void)
{
	if (!exception_set)
		return NULL;

	return &current_exception;
}

void pal_set_exception(int code, const char *message, const char *file, int line)
{
	current_exception.code = code;
	strncpy(current_exception.message, message ? message : "",
	        sizeof(current_exception.message) - 1);
	current_exception.message[sizeof(current_exception.message) - 1] = '\0';
	current_exception.file = file;
	current_exception.line = line;
	exception_set = 1;
}

void pal_clear_exception(void)
{
	exception_set = 0;
	current_exception.code = 0;
	current_exception.message[0] = '\0';
	current_exception.file = NULL;
	current_exception.line = 0;
}
