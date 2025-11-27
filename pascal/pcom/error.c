/*
 * Copyright (c) 2025 PCC Pascal Compiler
 *
 * Clang-style error reporting implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include "error.h"

/* Global error tracking */
int nerrors = 0;
int nwarnings = 0;
int max_errors = 20;  /* Stop after 20 errors by default */
int use_color = -1;   /* -1 = auto-detect, 0 = off, 1 = on */
int show_column = 1;
int show_caret = 1;
const char *current_file = "<unknown>";
int current_line = 1;
int current_column = 1;

/* Warning flags */
int warn_unused = 0;
int warn_implicit = 0;
int warn_strict = 0;
int warn_extensions = 0;
int warn_deprecated = 0;
int warn_all = 0;

/* Cache for source file reading */
#define MAX_CACHED_LINES 100
static struct {
	char *filename;
	int line_num;
	char *line_text;
} line_cache[MAX_CACHED_LINES];
static int cache_entries = 0;

/*
 * Initialize error reporting system
 */
void
error_init(void)
{
	/* Auto-detect color support if not explicitly set */
	if (use_color < 0) {
		use_color = isatty(STDERR_FILENO) ? 1 : 0;
	}

	nerrors = 0;
	nwarnings = 0;
	cache_entries = 0;
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

void
set_location_range(const char *file, int line, int start_col, int end_col)
{
	current_file = file;
	current_line = line;
	current_column = start_col;
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

source_loc_t
make_loc_range(int start_col, int end_col)
{
	source_loc_t loc;
	loc.filename = current_file;
	loc.line = current_line;
	loc.column = start_col;
	loc.end_column = end_col;
	return loc;
}

/*
 * Read a specific line from a source file
 */
char *
read_source_line(const char *filename, int line)
{
	FILE *f;
	char *buffer = NULL;
	size_t bufsize = 0;
	ssize_t len;
	int current = 1;
	int i;

	/* Check cache first */
	for (i = 0; i < cache_entries; i++) {
		if (line_cache[i].line_num == line &&
		    strcmp(line_cache[i].filename, filename) == 0) {
			return line_cache[i].line_text;
		}
	}

	/* Read from file */
	f = fopen(filename, "r");
	if (f == NULL)
		return NULL;

	while ((len = getline(&buffer, &bufsize, f)) != -1) {
		if (current == line) {
			/* Remove trailing newline */
			if (len > 0 && buffer[len-1] == '\n')
				buffer[len-1] = '\0';
			if (len > 1 && buffer[len-2] == '\r')
				buffer[len-2] = '\0';

			/* Add to cache */
			if (cache_entries < MAX_CACHED_LINES) {
				line_cache[cache_entries].filename = strdup(filename);
				line_cache[cache_entries].line_num = line;
				line_cache[cache_entries].line_text = strdup(buffer);
				cache_entries++;
			}

			fclose(f);
			return buffer;
		}
		current++;
	}

	fclose(f);
	if (buffer)
		free(buffer);
	return NULL;
}

/*
 * Get color code based on diagnostic level
 */
static const char *
get_color(diag_level_t level)
{
	if (!use_color)
		return "";

	switch (level) {
	case DIAG_NOTE:
		return COLOR_CYAN;
	case DIAG_WARNING:
		return COLOR_YELLOW;
	case DIAG_ERROR:
	case DIAG_FATAL:
		return COLOR_RED;
	default:
		return "";
	}
}

/*
 * Get level name string
 */
static const char *
get_level_name(diag_level_t level)
{
	switch (level) {
	case DIAG_NOTE:
		return "note";
	case DIAG_WARNING:
		return "warning";
	case DIAG_ERROR:
		return "error";
	case DIAG_FATAL:
		return "fatal error";
	default:
		return "diagnostic";
	}
}

/*
 * Print diagnostic in clang style:
 *
 * filename:line:col: error: message
 *   source line
 *   ^~~~~
 */
void
print_diagnostic(const diag_context_t *ctx)
{
	const char *color = get_color(ctx->level);
	const char *reset = use_color ? COLOR_RESET : "";
	const char *bold = use_color ? COLOR_BOLD : "";
	int i, start, end;

	/* Print: filename:line:col: level: message */
	if (show_column && ctx->loc.column > 0) {
		fprintf(stderr, "%s%s:%d:%d:%s %s%s:%s %s\n",
		        bold, ctx->loc.filename, ctx->loc.line, ctx->loc.column, reset,
		        bold, color, get_level_name(ctx->level), reset,
		        ctx->message);
	} else {
		fprintf(stderr, "%s%s:%d:%s %s%s:%s %s\n",
		        bold, ctx->loc.filename, ctx->loc.line, reset,
		        bold, color, get_level_name(ctx->level), reset,
		        ctx->message);
	}

	/* Print source line and caret if available */
	if (show_caret && ctx->source_line != NULL && ctx->loc.column > 0) {
		fprintf(stderr, "  %s\n", ctx->source_line);

		/* Print caret indicator */
		fprintf(stderr, "  ");

		/* Spaces before caret */
		for (i = 1; i < ctx->loc.column; i++) {
			if (ctx->source_line[i-1] == '\t')
				fputc('\t', stderr);
			else
				fputc(' ', stderr);
		}

		/* Print caret and underline */
		start = ctx->loc.column;
		end = ctx->loc.end_column > start ? ctx->loc.end_column : start;

		fprintf(stderr, "%s%s^", bold, color);
		for (i = start; i < end; i++) {
			fputc('~', stderr);
		}
		fprintf(stderr, "%s\n", reset);
	}
}

/*
 * Emit a diagnostic with source context
 */
void
emit_diagnostic(diag_level_t level, source_loc_t loc, const char *fmt, va_list ap)
{
	diag_context_t ctx;
	char message[1024];

	/* Format message */
	vsnprintf(message, sizeof(message), fmt, ap);

	/* Build diagnostic context */
	ctx.loc = loc;
	ctx.level = level;
	ctx.message = message;
	ctx.source_line = read_source_line(loc.filename, loc.line);

	/* Print diagnostic */
	print_diagnostic(&ctx);

	/* Update counters */
	if (level == DIAG_ERROR || level == DIAG_FATAL)
		nerrors++;
	else if (level == DIAG_WARNING)
		nwarnings++;

	/* Check error limit */
	if (level == DIAG_FATAL || too_many_errors()) {
		fprintf(stderr, "compilation terminated.\n");
		exit(1);
	}
}

/*
 * Error at current location
 */
void
error(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	emit_diagnostic(DIAG_ERROR, make_loc(), fmt, ap);
	va_end(ap);
}

/*
 * Error at specific location
 */
void
error_at(source_loc_t loc, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	emit_diagnostic(DIAG_ERROR, loc, fmt, ap);
	va_end(ap);
}

/*
 * Warning at current location
 */
void
warning(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	emit_diagnostic(DIAG_WARNING, make_loc(), fmt, ap);
	va_end(ap);
}

/*
 * Warning at specific location
 */
void
warning_at(source_loc_t loc, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	emit_diagnostic(DIAG_WARNING, loc, fmt, ap);
	va_end(ap);
}

/*
 * Note at current location
 */
void
note(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	emit_diagnostic(DIAG_NOTE, make_loc(), fmt, ap);
	va_end(ap);
}

/*
 * Note at specific location
 */
void
note_at(source_loc_t loc, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	emit_diagnostic(DIAG_NOTE, loc, fmt, ap);
	va_end(ap);
}

/*
 * Fatal error (terminates compilation)
 */
void
fatal(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	emit_diagnostic(DIAG_FATAL, make_loc(), fmt, ap);
	va_end(ap);
	/* Never returns */
}

/*
 * Parser error handler
 */
void
yyerror(const char *s)
{
	error("%s", s);
}

/*
 * Check if too many errors
 */
int
too_many_errors(void)
{
	return max_errors > 0 && nerrors >= max_errors;
}

/*
 * Warning control - enable specific warning
 */
void
enable_warning(const char *name)
{
	if (strcmp(name, "all") == 0) {
		warn_all = 1;
		warn_unused = 1;
		warn_implicit = 1;
		warn_extensions = 1;
		warn_deprecated = 1;
		return;
	}

	if (strcmp(name, "unused") == 0)
		warn_unused = 1;
	else if (strcmp(name, "implicit") == 0)
		warn_implicit = 1;
	else if (strcmp(name, "strict") == 0)
		warn_strict = 1;
	else if (strcmp(name, "extensions") == 0)
		warn_extensions = 1;
	else if (strcmp(name, "deprecated") == 0)
		warn_deprecated = 1;
}

/*
 * Warning control - disable specific warning
 */
void
disable_warning(const char *name)
{
	if (strcmp(name, "all") == 0) {
		warn_all = 0;
		warn_unused = 0;
		warn_implicit = 0;
		warn_extensions = 0;
		warn_deprecated = 0;
		return;
	}

	if (strcmp(name, "unused") == 0)
		warn_unused = 0;
	else if (strcmp(name, "implicit") == 0)
		warn_implicit = 0;
	else if (strcmp(name, "strict") == 0)
		warn_strict = 0;
	else if (strcmp(name, "extensions") == 0)
		warn_extensions = 0;
	else if (strcmp(name, "deprecated") == 0)
		warn_deprecated = 0;
}

/*
 * Check if warning is enabled
 */
int
is_warning_enabled(const char *name)
{
	if (strcmp(name, "unused") == 0)
		return warn_unused || warn_all;
	if (strcmp(name, "implicit") == 0)
		return warn_implicit || warn_all;
	if (strcmp(name, "strict") == 0)
		return warn_strict;
	if (strcmp(name, "extensions") == 0)
		return warn_extensions || warn_all;
	if (strcmp(name, "deprecated") == 0)
		return warn_deprecated || warn_all;
	return 0;
}
