/*
 * COBOL Runtime Library - Error Handling
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cobolrt.h"

static int current_exception = 0;
static char exception_message[256] = "";
static int trace_enabled = 0;

void
__cobol_set_exception(int code, const char *message)
{
	current_exception = code;

	if (message) {
		strncpy(exception_message, message, sizeof(exception_message) - 1);
		exception_message[sizeof(exception_message) - 1] = '\0';
	} else {
		exception_message[0] = '\0';
	}

	if (trace_enabled) {
		fprintf(stderr, "COBOL Exception %d: %s\n", code, message ? message : "");
	}
}

int
__cobol_get_exception(void)
{
	return current_exception;
}

const char *
__cobol_get_exception_message(void)
{
	return exception_message;
}

void
__cobol_fatal_error(const char *message)
{
	fprintf(stderr, "COBOL Fatal Error: %s\n", message);
	exit(1);
}

void
__cobol_init(void)
{
	/* Initialize runtime */
	current_exception = 0;
	exception_message[0] = '\0';
	trace_enabled = 0;
}

void
__cobol_cleanup(void)
{
	/* Cleanup runtime resources */
}

void
__cobol_set_trace(int enabled)
{
	trace_enabled = enabled;
}
