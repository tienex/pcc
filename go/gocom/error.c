/*
 * Copyright (c) 2025 PCC Go Compiler
 *
 * Error reporting implementation
 * Provides Clang-style diagnostics
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include "error.h"
#include "pass1.h"

/* Global error counters */
int nerrors = 0;
int nwarnings = 0;
int max_errors = 20;

/* Diagnostic options */
int use_color = 1;      /* Auto-detect terminal support */
int show_caret = 1;

/* ANSI color codes */
#define COLOR_RESET   "\033[0m"
#define COLOR_BOLD    "\033[1m"
#define COLOR_RED     "\033[31m"
#define COLOR_YELLOW  "\033[33m"
#define COLOR_GREEN   "\033[32m"
#define COLOR_CYAN    "\033[36m"
#define COLOR_MAGENTA "\033[35m"

/*
 * Initialize error reporting system
 */
void
error_init(void)
{
	nerrors = 0;
	nwarnings = 0;

	/* Auto-detect color support */
	if (!isatty(STDERR_FILENO))
		use_color = 0;
}

/*
 * Helper: Get color code for diagnostic level
 */
static const char *
diag_color(diag_level_t level)
{
	if (!use_color)
		return "";

	switch (level) {
	case DIAG_ERROR:
	case DIAG_FATAL:
		return COLOR_RED COLOR_BOLD;
	case DIAG_WARNING:
		return COLOR_YELLOW COLOR_BOLD;
	case DIAG_NOTE:
		return COLOR_CYAN COLOR_BOLD;
	default:
		return "";
	}
}

/*
 * Helper: Get diagnostic level name
 */
static const char *
diag_name(diag_level_t level)
{
	switch (level) {
	case DIAG_ERROR:
		return "error";
	case DIAG_WARNING:
		return "warning";
	case DIAG_NOTE:
		return "note";
	case DIAG_FATAL:
		return "fatal error";
	default:
		return "diagnostic";
	}
}

/*
 * Core diagnostic function
 */
static void
vdiag(diag_level_t level, source_loc_t loc, const char *fmt, va_list ap)
{
	/* Update counters */
	if (level == DIAG_ERROR || level == DIAG_FATAL)
		nerrors++;
	else if (level == DIAG_WARNING)
		nwarnings++;

	/* Print location and level */
	if (loc.filename && loc.line > 0) {
		fprintf(stderr, "%s%s:%d:%d: %s%s:%s ",
		    COLOR_BOLD, loc.filename, loc.line, loc.column,
		    diag_color(level), diag_name(level),
		    use_color ? COLOR_RESET : "");
	} else {
		fprintf(stderr, "%s%s:%s ",
		    diag_color(level), diag_name(level),
		    use_color ? COLOR_RESET : "");
	}

	/* Print message */
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");

	/* Check error limit */
	if (nerrors >= max_errors && max_errors > 0) {
		fprintf(stderr, "%sfatal error:%s too many errors emitted, "
		    "stopping now\n",
		    diag_color(DIAG_FATAL),
		    use_color ? COLOR_RESET : "");
		exit(1);
	}

	/* Fatal errors stop immediately */
	if (level == DIAG_FATAL)
		exit(1);
}

/*
 * Error reporting functions
 */
void
error(const char *fmt, ...)
{
	va_list ap;
	source_loc_t loc = current_loc();

	va_start(ap, fmt);
	vdiag(DIAG_ERROR, loc, fmt, ap);
	va_end(ap);
}

void
error_at(source_loc_t loc, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vdiag(DIAG_ERROR, loc, fmt, ap);
	va_end(ap);
}

void
warning(const char *fmt, ...)
{
	va_list ap;
	source_loc_t loc = current_loc();

	va_start(ap, fmt);
	vdiag(DIAG_WARNING, loc, fmt, ap);
	va_end(ap);
}

void
warning_at(source_loc_t loc, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vdiag(DIAG_WARNING, loc, fmt, ap);
	va_end(ap);
}

void
note(const char *fmt, ...)
{
	va_list ap;
	source_loc_t loc = current_loc();

	va_start(ap, fmt);
	vdiag(DIAG_NOTE, loc, fmt, ap);
	va_end(ap);
}

void
note_at(source_loc_t loc, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vdiag(DIAG_NOTE, loc, fmt, ap);
	va_end(ap);
}

void
fatal(const char *fmt, ...)
{
	va_list ap;
	source_loc_t loc = current_loc();

	va_start(ap, fmt);
	vdiag(DIAG_FATAL, loc, fmt, ap);
	va_end(ap);
}

/*
 * Create source location
 */
source_loc_t
make_loc(const char *filename, int line, int column)
{
	source_loc_t loc;
	loc.filename = filename;
	loc.line = line;
	loc.column = column;
	return loc;
}

source_loc_t
current_loc(void)
{
	extern int lineno;
	extern int current_column;
	extern char *current_file;

	return make_loc(current_file, lineno, current_column);
}

/*
 * Warning control (simplified)
 */
void
enable_warning(const char *name)
{
	/* TODO: Implement warning flags */
}

void
disable_warning(const char *name)
{
	/* TODO: Implement warning flags */
}

int
is_warning_enabled(const char *name)
{
	return 1;  /* All warnings enabled by default */
}
