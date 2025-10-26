/*
 * Copyright (c) 2025 PCC ALGOL 60+ Compiler
 *
 * Error reporting for ALGOL 60+ compiler
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

/* Global error tracking */
extern int nerrors;         /* Total error count */
extern int nwarnings;       /* Total warning count */
extern int max_errors;      /* Maximum errors before abort (0 = unlimited) */
extern const char *current_file;  /* Current source file being compiled */
extern int current_line;    /* Current line number */
extern int current_column;  /* Current column number */

/* Initialize error reporting system */
void error_init(void);

/* Set current source location */
void set_location(const char *file, int line, int col);

/* Main diagnostic functions */
void error(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
void error_at(source_loc_t loc, const char *fmt, ...) __attribute__((format(printf, 2, 3)));
void warning(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
void warning_at(source_loc_t loc, const char *fmt, ...) __attribute__((format(printf, 2, 3)));
void note(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
void note_at(source_loc_t loc, const char *fmt, ...) __attribute__((format(printf, 2, 3)));
void fatal(const char *fmt, ...) __attribute__((format(printf, 1, 2), noreturn));

/* Helper to create source location from current position */
source_loc_t make_loc(void);

/* Parser error handler (called by yacc/bison) */
void yyerror(const char *s);

/* Check if error limit reached */
int too_many_errors(void);

#endif /* ERROR_H */
