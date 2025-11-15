/*
 * Copyright (c) 2025 PCC DEC MACRO Compiler
 *
 * Error handling and diagnostics
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "pass1.h"

/* Global error counters */
int nerrors = 0;
int nwarnings = 0;

/* Maximum errors before abort */
static int max_errors = 20;

/*
 * Initialize error handling
 */
void
error_init(void)
{
	nerrors = 0;
	nwarnings = 0;
}

/*
 * Report an error at current location
 */
void
error(const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d: error: ", ftitle, lineno);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	nerrors++;
	if (nerrors >= max_errors) {
		fprintf(stderr, "Too many errors, aborting.\n");
		exit(1);
	}
}

/*
 * Report an error at specific line
 */
void
error_at(int line, const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d: error: ", ftitle, line);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	nerrors++;
	if (nerrors >= max_errors) {
		fprintf(stderr, "Too many errors, aborting.\n");
		exit(1);
	}
}

/*
 * Report a warning
 */
void
warning(const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d: warning: ", ftitle, lineno);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	nwarnings++;
}

/*
 * Report a fatal error and abort
 */
void
fatal(const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d: fatal error: ", ftitle, lineno);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	exit(1);
}
