/*
 * Copyright (c) 2025 PCC ALGOL 60+ Compiler
 *
 * Error reporting implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "error.h"

/* Global error tracking */
int nerrors = 0;
int nwarnings = 0;
int max_errors = 20;  /* Stop after 20 errors by default */
const char *current_file = "<unknown>";
int current_line = 1;
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
 * Set current source location
 */
void
set_location(const char *file, int line, int col)
{
	current_file = file;
	current_line = line;
	current_column = col;
}

/*
 * Create source location from current position
 */
source_loc_t
make_loc(void)
{
	source_loc_t loc;
	loc.filename = current_file;
	loc.line = current_line;
	loc.column = current_column;
	loc.end_column = current_column;
	return loc;
}

/*
 * Print formatted error message
 */
void
error(const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d: error: ", current_file, current_line);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	nerrors++;
	if (max_errors > 0 && nerrors >= max_errors) {
		fprintf(stderr, "Too many errors, aborting compilation\n");
		exit(1);
	}
}

/*
 * Print formatted error message at specific location
 */
void
error_at(source_loc_t loc, const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d:%d: error: ", loc.filename, loc.line, loc.column);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	nerrors++;
	if (max_errors > 0 && nerrors >= max_errors) {
		fprintf(stderr, "Too many errors, aborting compilation\n");
		exit(1);
	}
}

/*
 * Print formatted warning message
 */
void
warning(const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d: warning: ", current_file, current_line);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	nwarnings++;
}

/*
 * Print formatted warning message at specific location
 */
void
warning_at(source_loc_t loc, const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d:%d: warning: ", loc.filename, loc.line, loc.column);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	nwarnings++;
}

/*
 * Print formatted note message
 */
void
note(const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d: note: ", current_file, current_line);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
}

/*
 * Print formatted note message at specific location
 */
void
note_at(source_loc_t loc, const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d:%d: note: ", loc.filename, loc.line, loc.column);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
}

/*
 * Print fatal error and exit
 */
void
fatal(const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d: fatal error: ", current_file, current_line);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	exit(1);
}

/*
 * Parser error handler (called by yacc/bison)
 */
void
yyerror(const char *s)
{
	error("parse error: %s", s);
}

/*
 * Check if error limit reached
 */
int
too_many_errors(void)
{
	return (max_errors > 0 && nerrors >= max_errors);
}
