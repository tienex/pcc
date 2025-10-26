/*
 * Copyright (c) 2025 PCC Pascal Compiler
 *
 * Clang-style error reporting for Pascal compiler
 * This provides detailed, user-friendly diagnostics with source context
 */

#ifndef ERROR_H
#define ERROR_H

#include <stdarg.h>

/* Diagnostic severity levels */
typedef enum {
	DIAG_NOTE,      /* Informational note */
	DIAG_WARNING,   /* Warning (non-fatal) */
	DIAG_ERROR,     /* Error (compilation continues) */
	DIAG_FATAL      /* Fatal error (abort compilation) */
} diag_level_t;

/* Source location information */
typedef struct source_loc {
	const char *filename;   /* Source file name */
	int line;               /* Line number (1-based) */
	int column;             /* Column number (1-based) */
	int end_column;         /* End column for ranges */
} source_loc_t;

/* Diagnostic context */
typedef struct diag_context {
	source_loc_t loc;       /* Primary location */
	diag_level_t level;     /* Severity level */
	const char *message;    /* Diagnostic message */
	char *source_line;      /* Copy of source line for display */
} diag_context_t;

/* Global error tracking */
extern int nerrors;         /* Total error count */
extern int nwarnings;       /* Total warning count */
extern int max_errors;      /* Maximum errors before abort (0 = unlimited) */
extern int use_color;       /* Enable color output */
extern int show_column;     /* Show column numbers */
extern int show_caret;      /* Show caret (^) under errors */
extern const char *current_file;  /* Current source file being compiled */
extern int current_line;    /* Current line number */
extern int current_column;  /* Current column number */

/* Diagnostic flags (similar to clang's -W flags) */
extern int warn_unused;     /* Warn about unused variables/functions */
extern int warn_implicit;   /* Warn about implicit declarations */
extern int warn_strict;     /* Strict ISO Pascal compliance warnings */
extern int warn_extensions; /* Warn about dialect extensions */
extern int warn_deprecated; /* Warn about deprecated features */
extern int warn_all;        /* Enable all warnings */

/* Initialize error reporting system */
void error_init(void);

/* Set current source location */
void set_location(const char *file, int line, int col);
void set_location_range(const char *file, int line, int start_col, int end_col);

/* Main diagnostic functions - use these from compiler code */
void error(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
void error_at(source_loc_t loc, const char *fmt, ...) __attribute__((format(printf, 2, 3)));
void warning(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
void warning_at(source_loc_t loc, const char *fmt, ...) __attribute__((format(printf, 2, 3)));
void note(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
void note_at(source_loc_t loc, const char *fmt, ...) __attribute__((format(printf, 2, 3)));
void fatal(const char *fmt, ...) __attribute__((format(printf, 1, 2), noreturn));

/* Emit a complete diagnostic with source context */
void emit_diagnostic(diag_level_t level, source_loc_t loc, const char *fmt, va_list ap);

/* Helper to create source location from current position */
source_loc_t make_loc(void);
source_loc_t make_loc_range(int start_col, int end_col);

/* Read source line from file for context display */
char *read_source_line(const char *filename, int line);

/* Display formatted diagnostic (clang-style) */
void print_diagnostic(const diag_context_t *ctx);

/* Parser error handler (called by yacc/bison) */
void yyerror(const char *s);

/* Check if error limit reached */
int too_many_errors(void);

/* Warning control functions */
void enable_warning(const char *name);
void disable_warning(const char *name);
int is_warning_enabled(const char *name);

/* ANSI color codes for diagnostics */
#define COLOR_RESET     "\033[0m"
#define COLOR_BOLD      "\033[1m"
#define COLOR_RED       "\033[31m"
#define COLOR_YELLOW    "\033[33m"
#define COLOR_GREEN     "\033[32m"
#define COLOR_CYAN      "\033[36m"
#define COLOR_MAGENTA   "\033[35m"

/* Convenience macros for common error patterns */
#define ERROR_SYNTAX(msg) error("syntax error: %s", msg)
#define ERROR_TYPE(msg) error("type error: %s", msg)
#define ERROR_SEMANTIC(msg) error("semantic error: %s", msg)
#define WARNING_UNUSED(name) if (warn_unused) warning("unused identifier '%s'", name)
#define WARNING_EXTENSION(msg) if (warn_extensions) warning("extension: %s", msg)

#endif /* ERROR_H */
