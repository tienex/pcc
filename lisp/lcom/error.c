/*
 * Copyright (c) 2025 PCC Common LISP Compiler
 *
 * Error handling and diagnostic messages
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "pass1.h"

/* Warning control flags */
static int warn_all = 1;
static int warn_unused = 1;
static int warn_strict = 0;

/*
 * Initialize error system
 */
void
error_init(void)
{
	nerrors = 0;
	nwarnings = 0;
}

/*
 * Enable a warning category
 */
void
enable_warning(const char *name)
{
	if (strcmp(name, "all") == 0)
		warn_all = 1;
	else if (strcmp(name, "unused") == 0)
		warn_unused = 1;
	else if (strcmp(name, "strict") == 0)
		warn_strict = 1;
}

/*
 * Disable a warning category
 */
void
disable_warning(const char *name)
{
	if (strcmp(name, "all") == 0)
		warn_all = 0;
	else if (strcmp(name, "unused") == 0)
		warn_unused = 0;
	else if (strcmp(name, "strict") == 0)
		warn_strict = 0;
}

/*
 * Report an error message
 */
void
error_msg(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);

	if (use_color)
		fprintf(stderr, "\033[1m%s:%d: \033[31merror:\033[0m ",
		    current_file, lineno);
	else
		fprintf(stderr, "%s:%d: error: ", current_file, lineno);

	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);

	nerrors++;
	if (nerrors >= max_errors) {
		fprintf(stderr, "Too many errors, aborting.\n");
		exit(1);
	}
}

/*
 * Report a warning message
 */
void
warning_msg(const char *fmt, ...)
{
	va_list ap;

	if (!warn_all)
		return;

	va_start(ap, fmt);

	if (use_color)
		fprintf(stderr, "\033[1m%s:%d: \033[35mwarning:\033[0m ",
		    current_file, lineno);
	else
		fprintf(stderr, "%s:%d: warning: ", current_file, lineno);

	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);

	nwarnings++;
}
