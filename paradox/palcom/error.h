/*
 * Copyright (c) 2025 PCC Paradox PAL Compiler
 *
 * Error reporting and diagnostics
 * Clang-style error messages with source location
 */

#ifndef ERROR_H
#define ERROR_H

#include <stdio.h>
#include <stdarg.h>

/* Source location tracking */
typedef struct source_loc {
	int line;
	int column;
	char *filename;
} source_loc_t;

/* Diagnostic levels */
typedef enum {
	DIAG_NOTE,
	DIAG_WARNING,
	DIAG_ERROR,
	DIAG_FATAL
} diag_level_t;

/* Global error/warning counters */
extern int error_count;
extern int warning_count;

/* Current source location */
extern source_loc_t current_loc;

/* Initialize error system */
void error_init(const char *source_file);

/* Error reporting functions */
void error(const char *fmt, ...);
void error_at(source_loc_t loc, const char *fmt, ...);
void warning(const char *fmt, ...);
void warning_at(source_loc_t loc, const char *fmt, ...);
void note(const char *fmt, ...);
void note_at(source_loc_t loc, const char *fmt, ...);
void fatal(const char *fmt, ...);

/* Diagnostic with level */
void diagnostic(diag_level_t level, source_loc_t loc, const char *fmt, ...);

/* Check if compilation should abort */
int has_errors(void);

/* Print error summary */
void print_error_summary(void);

#endif /* ERROR_H */
