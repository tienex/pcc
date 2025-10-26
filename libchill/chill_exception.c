/*
 * Copyright (c) 2025 PCC CHILL Runtime Library
 *
 * CHILL exception handling (simplified)
 */

#include "chill.h"
#include <stdio.h>
#include <stdlib.h>

/*
 * Exception name table
 */
static const char *exception_names[] = {
	"NONE",
	"RANGEFAIL",
	"EMPTY",
	"OVERFLOW",
	"ZERODIVIDE",
	"INVALIDOP",
	"OUTOFMEM",
	"IOERROR"
};

/*
 * Get exception name from code
 */
const char *
chill_exception_name(chill_exception_t exc)
{
	if (exc < 0 || exc >= (sizeof(exception_names) / sizeof(exception_names[0]))) {
		return "UNKNOWN";
	}
	return exception_names[exc];
}

/*
 * Raise an exception
 * In a full implementation, this would support proper exception handling
 * with ONEXCEPTION clauses. For now, we just print an error and exit.
 */
void
chill_raise(chill_exception_t exc, const char *msg)
{
	fprintf(stderr, "CHILL Exception: %s", chill_exception_name(exc));
	if (msg != NULL) {
		fprintf(stderr, ": %s", msg);
	}
	fprintf(stderr, "\n");

	/* In a real implementation, we would:
	 * 1. Check for ONEXCEPTION handlers in the current scope
	 * 2. Unwind the stack if no handler found
	 * 3. Eventually call a default handler or abort
	 *
	 * For this simplified version, we just exit.
	 */
	exit(1);
}
