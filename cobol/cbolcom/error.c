/*
 * COBOL error handling
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "pass1.h"

void
error_at(int line, const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "Error at line %d: ", line);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	nerrors++;
}

void
warning_at(int line, const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "Warning at line %d: ", line);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	nwarnings++;
}
