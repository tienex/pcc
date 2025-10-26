/*
 * Copyright (c) 2025 PCC OCaml Compiler
 *
 * Error reporting implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "pass1.h"

/* Global error tracking */
int nerrors = 0;
int nwarnings = 0;
int max_errors = 20;
int use_color = 0;
int show_caret = 1;
char *current_file = "<unknown>";
int current_column = 1;

/*
 * Initialize error reporting system
 */
void
error_init(void)
{
	nerrors = 0;
	nwarnings = 0;
}

/*
 * Error at current location
 */
void
error(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	fprintf(stderr, "%s:%d: error: ", current_file, lineno);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);

	nerrors++;

	if (nerrors >= max_errors) {
		fprintf(stderr, "Too many errors, compilation aborted.\n");
		exit(1);
	}
}

/*
 * Warning at current location
 */
void
warning(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	fprintf(stderr, "%s:%d: warning: ", current_file, lineno);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);

	nwarnings++;
}

/*
 * Fatal error (terminates compilation)
 */
void
fatal(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	fprintf(stderr, "%s:%d: fatal error: ", current_file, lineno);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);

	exit(1);
}
