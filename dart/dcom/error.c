/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart error reporting
 */

#include "pass1.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

static int error_count = 0;
static int warning_count = 0;
static int max_errors = 100;

void
dart_error(int lineno, int column, const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "error: line %d, column %d: ", lineno, column);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	error_count++;
	if (error_count >= max_errors) {
		fprintf(stderr, "too many errors, aborting compilation\n");
		exit(1);
	}
}

void
dart_warning(int lineno, int column, const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "warning: line %d, column %d: ", lineno, column);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	warning_count++;
}

int
dart_get_error_count(void)
{
	return error_count;
}

int
dart_get_warning_count(void)
{
	return warning_count;
}

void
dart_reset_error_count(void)
{
	error_count = 0;
	warning_count = 0;
}
