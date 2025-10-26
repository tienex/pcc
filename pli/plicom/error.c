/*
 * Copyright (c) 2025 PCC PL/I Compiler
 *
 * Clang-style error reporting implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "error.h"

/* Global error counters */
int nerrors = 0;
int nwarnings = 0;
int max_errors = 0;  /* 0 = unlimited */

/* Output options */
int use_color = 1;   /* Enable by default if tty */
int show_column = 1;
int show_caret = 1;

/* Current source location */
const char *current_file = NULL;
int current_line = 1;
int current_column = 1;

/* Warning flags */
int warn_unused = 0;
int warn_conversion = 0;
int warn_strict = 0;
int warn_extensions = 0;
int warn_deprecated = 0;
int warn_all = 0;

/* Initialize error reporting */
void error_init(void) {
	nerrors = 0;
	nwarnings = 0;
	current_line = 1;
	current_column = 1;

	/* Check if stdout is a tty for color support */
	use_color = isatty(fileno(stderr));
}

/* Set current location */
void set_location(const char *file, int line, int col) {
	current_file = file;
	current_line = line;
	current_column = col;
}

void set_location_range(const char *file, int line, int start_col, int end_col) {
	current_file = file;
	current_line = line;
	current_column = start_col;
}

/* Make location from current position */
source_loc_t make_loc(void) {
	source_loc_t loc;
	loc.filename = current_file;
	loc.line = current_line;
	loc.column = current_column;
	loc.end_column = current_column;
	return loc;
}

source_loc_t make_loc_range(int start_col, int end_col) {
	source_loc_t loc;
	loc.filename = current_file;
	loc.line = current_line;
	loc.column = start_col;
	loc.end_column = end_col;
	return loc;
}

/* Print diagnostic message */
void print_diagnostic(const diag_context_t *ctx) {
	const char *level_str;
	const char *color_code = "";
	const char *reset = "";

	if (use_color) {
		reset = COLOR_RESET;
		switch (ctx->level) {
		case DIAG_NOTE:
			level_str = "note";
			color_code = COLOR_CYAN;
			break;
		case DIAG_WARNING:
			level_str = "warning";
			color_code = COLOR_YELLOW;
			break;
		case DIAG_ERROR:
			level_str = "error";
			color_code = COLOR_RED;
			break;
		case DIAG_FATAL:
			level_str = "fatal error";
			color_code = COLOR_RED COLOR_BOLD;
			break;
		default:
			level_str = "message";
			break;
		}
	} else {
		switch (ctx->level) {
		case DIAG_NOTE:    level_str = "note"; break;
		case DIAG_WARNING: level_str = "warning"; break;
		case DIAG_ERROR:   level_str = "error"; break;
		case DIAG_FATAL:   level_str = "fatal error"; break;
		default:           level_str = "message"; break;
		}
	}

	/* Print location and message */
	if (ctx->loc.filename && show_column) {
		fprintf(stderr, "%s:%d:%d: %s%s:%s %s\n",
			ctx->loc.filename, ctx->loc.line, ctx->loc.column,
			color_code, level_str, reset, ctx->message);
	} else if (ctx->loc.filename) {
		fprintf(stderr, "%s:%d: %s%s:%s %s\n",
			ctx->loc.filename, ctx->loc.line,
			color_code, level_str, reset, ctx->message);
	} else {
		fprintf(stderr, "%s%s:%s %s\n",
			color_code, level_str, reset, ctx->message);
	}

	/* Show source line with caret if available */
	if (show_caret && ctx->source_line && ctx->loc.column > 0) {
		fprintf(stderr, "%s\n", ctx->source_line);
		/* Print spaces and caret */
		for (int i = 1; i < ctx->loc.column; i++) {
			fprintf(stderr, " ");
		}
		if (use_color) {
			fprintf(stderr, "%s^%s\n", COLOR_GREEN, COLOR_RESET);
		} else {
			fprintf(stderr, "^\n");
		}
	}
}

/* Emit diagnostic */
void emit_diagnostic(diag_level_t level, source_loc_t loc, const char *fmt, va_list ap) {
	char message[1024];
	vsnprintf(message, sizeof(message), fmt, ap);

	diag_context_t ctx;
	ctx.loc = loc;
	ctx.level = level;
	ctx.message = message;
	ctx.source_line = NULL;

	/* Try to read source line for context */
	if (loc.filename && loc.line > 0) {
		ctx.source_line = read_source_line(loc.filename, loc.line);
	}

	print_diagnostic(&ctx);

	if (ctx.source_line) {
		free(ctx.source_line);
	}

	/* Update counters */
	if (level == DIAG_ERROR || level == DIAG_FATAL) {
		nerrors++;
	} else if (level == DIAG_WARNING) {
		nwarnings++;
	}
}

/* Main diagnostic functions */
void error(const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	emit_diagnostic(DIAG_ERROR, make_loc(), fmt, ap);
	va_end(ap);
}

void error_at(source_loc_t loc, const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	emit_diagnostic(DIAG_ERROR, loc, fmt, ap);
	va_end(ap);
}

void warning(const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	emit_diagnostic(DIAG_WARNING, make_loc(), fmt, ap);
	va_end(ap);
}

void warning_at(source_loc_t loc, const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	emit_diagnostic(DIAG_WARNING, loc, fmt, ap);
	va_end(ap);
}

void note(const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	emit_diagnostic(DIAG_NOTE, make_loc(), fmt, ap);
	va_end(ap);
}

void note_at(source_loc_t loc, const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	emit_diagnostic(DIAG_NOTE, loc, fmt, ap);
	va_end(ap);
}

void fatal(const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	emit_diagnostic(DIAG_FATAL, make_loc(), fmt, ap);
	va_end(ap);
	exit(1);
}

/* Read source line */
char *read_source_line(const char *filename, int line) {
	FILE *fp = fopen(filename, "r");
	if (!fp) return NULL;

	char *buffer = NULL;
	size_t bufsize = 0;
	int current = 0;

	while (getline(&buffer, &bufsize, fp) != -1) {
		current++;
		if (current == line) {
			fclose(fp);
			/* Remove trailing newline */
			size_t len = strlen(buffer);
			if (len > 0 && buffer[len-1] == '\n') {
				buffer[len-1] = '\0';
			}
			return buffer;
		}
	}

	fclose(fp);
	free(buffer);
	return NULL;
}

/* Parser error handler */
void yyerror(const char *s) {
	error("%s", s);
}

/* Check if too many errors */
int too_many_errors(void) {
	return max_errors > 0 && nerrors >= max_errors;
}

/* Warning control */
void enable_warning(const char *name) {
	if (strcmp(name, "all") == 0) warn_all = 1;
	else if (strcmp(name, "unused") == 0) warn_unused = 1;
	else if (strcmp(name, "conversion") == 0) warn_conversion = 1;
	else if (strcmp(name, "strict") == 0) warn_strict = 1;
	else if (strcmp(name, "extensions") == 0) warn_extensions = 1;
	else if (strcmp(name, "deprecated") == 0) warn_deprecated = 1;
}

void disable_warning(const char *name) {
	if (strcmp(name, "all") == 0) warn_all = 0;
	else if (strcmp(name, "unused") == 0) warn_unused = 0;
	else if (strcmp(name, "conversion") == 0) warn_conversion = 0;
	else if (strcmp(name, "strict") == 0) warn_strict = 0;
	else if (strcmp(name, "extensions") == 0) warn_extensions = 0;
	else if (strcmp(name, "deprecated") == 0) warn_deprecated = 0;
}

int is_warning_enabled(const char *name) {
	if (strcmp(name, "unused") == 0) return warn_unused;
	if (strcmp(name, "conversion") == 0) return warn_conversion;
	if (strcmp(name, "strict") == 0) return warn_strict;
	if (strcmp(name, "extensions") == 0) return warn_extensions;
	if (strcmp(name, "deprecated") == 0) return warn_deprecated;
	return 0;
}
