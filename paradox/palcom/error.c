/*
 * Copyright (c) 2025 PCC Paradox PAL Compiler
 *
 * Error reporting implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "error.h"

/* Global error/warning counters */
int error_count = 0;
int warning_count = 0;

/* Current source location */
source_loc_t current_loc = {1, 1, NULL};

/* Source filename */
static char *source_filename = NULL;

void error_init(const char *source_file)
{
	source_filename = strdup(source_file);
	error_count = 0;
	warning_count = 0;
	current_loc.line = 1;
	current_loc.column = 1;
	current_loc.filename = source_filename;
}

void diagnostic(diag_level_t level, source_loc_t loc, const char *fmt, ...)
{
	va_list args;
	const char *level_str;
	const char *color_start = "";
	const char *color_end = "";

	/* Set level string and color */
	switch (level) {
	case DIAG_NOTE:
		level_str = "note";
		color_start = "\033[1;36m";  /* Cyan */
		color_end = "\033[0m";
		break;
	case DIAG_WARNING:
		level_str = "warning";
		color_start = "\033[1;35m";  /* Magenta */
		color_end = "\033[0m";
		warning_count++;
		break;
	case DIAG_ERROR:
		level_str = "error";
		color_start = "\033[1;31m";  /* Red */
		color_end = "\033[0m";
		error_count++;
		break;
	case DIAG_FATAL:
		level_str = "fatal error";
		color_start = "\033[1;31m";  /* Red */
		color_end = "\033[0m";
		error_count++;
		break;
	default:
		level_str = "unknown";
		break;
	}

	/* Print location and level */
	fprintf(stderr, "%s:%d:%d: %s%s:%s ",
		loc.filename ? loc.filename : "<unknown>",
		loc.line, loc.column,
		color_start, level_str, color_end);

	/* Print message */
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);

	fprintf(stderr, "\n");

	/* Exit on fatal error */
	if (level == DIAG_FATAL) {
		exit(1);
	}
}

void error(const char *fmt, ...)
{
	va_list args;
	fprintf(stderr, "%s:%d:%d: \033[1;31merror:\033[0m ",
		current_loc.filename ? current_loc.filename : "<unknown>",
		current_loc.line, current_loc.column);
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
	error_count++;
}

void error_at(source_loc_t loc, const char *fmt, ...)
{
	va_list args;
	fprintf(stderr, "%s:%d:%d: \033[1;31merror:\033[0m ",
		loc.filename ? loc.filename : "<unknown>",
		loc.line, loc.column);
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
	error_count++;
}

void warning(const char *fmt, ...)
{
	va_list args;
	fprintf(stderr, "%s:%d:%d: \033[1;35mwarning:\033[0m ",
		current_loc.filename ? current_loc.filename : "<unknown>",
		current_loc.line, current_loc.column);
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
	warning_count++;
}

void warning_at(source_loc_t loc, const char *fmt, ...)
{
	va_list args;
	fprintf(stderr, "%s:%d:%d: \033[1;35mwarning:\033[0m ",
		loc.filename ? loc.filename : "<unknown>",
		loc.line, loc.column);
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
	warning_count++;
}

void note(const char *fmt, ...)
{
	va_list args;
	fprintf(stderr, "%s:%d:%d: \033[1;36mnote:\033[0m ",
		current_loc.filename ? current_loc.filename : "<unknown>",
		current_loc.line, current_loc.column);
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
}

void note_at(source_loc_t loc, const char *fmt, ...)
{
	va_list args;
	fprintf(stderr, "%s:%d:%d: \033[1;36mnote:\033[0m ",
		loc.filename ? loc.filename : "<unknown>",
		loc.line, loc.column);
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
}

void fatal(const char *fmt, ...)
{
	va_list args;
	fprintf(stderr, "%s:%d:%d: \033[1;31mfatal error:\033[0m ",
		current_loc.filename ? current_loc.filename : "<unknown>",
		current_loc.line, current_loc.column);
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
	error_count++;
	exit(1);
}

int has_errors(void)
{
	return error_count > 0;
}

void print_error_summary(void)
{
	if (error_count > 0 || warning_count > 0) {
		fprintf(stderr, "\n");
		if (error_count > 0) {
			fprintf(stderr, "\033[1;31m%d error%s\033[0m",
				error_count, error_count == 1 ? "" : "s");
		}
		if (error_count > 0 && warning_count > 0) {
			fprintf(stderr, " and ");
		}
		if (warning_count > 0) {
			fprintf(stderr, "\033[1;35m%d warning%s\033[0m",
				warning_count, warning_count == 1 ? "" : "s");
		}
		fprintf(stderr, " generated.\n");
	}
}
