/*
 * Copyright (c) 2025 PCC Prolog Compiler
 *
 * Error handling functions
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "pass1.h"

/* Error counters */
static int error_count = 0;
static int warning_count = 0;

/* Maximum errors before stopping */
#define MAX_ERRORS 25

/* Print error message */
void error(const char *fmt, ...) {
	va_list ap;

	fprintf(stderr, "Error at line %d, column %d: ",
	        lineno, current_column);

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);

	fprintf(stderr, "\n");

	error_count++;

	if (error_count >= MAX_ERRORS) {
		fprintf(stderr, "Too many errors, stopping compilation\n");
		exit(1);
	}
}

/* Print warning message */
void warning(const char *fmt, ...) {
	va_list ap;

	if (options.warnings == 0)
		return;

	fprintf(stderr, "Warning at line %d, column %d: ",
	        lineno, current_column);

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);

	fprintf(stderr, "\n");

	warning_count++;
}

/* Print fatal error and exit */
void fatal(const char *fmt, ...) {
	va_list ap;

	fprintf(stderr, "Fatal error: ");

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);

	fprintf(stderr, "\n");

	exit(1);
}

/* Get error count */
int get_error_count(void) {
	return error_count;
}

/* Get warning count */
int get_warning_count(void) {
	return warning_count;
}
