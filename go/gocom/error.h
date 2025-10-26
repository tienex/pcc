/*
 * Copyright (c) 2025 PCC Go Compiler
 *
 * Error reporting system
 * Provides Clang-style diagnostics with colors and carets
 */

#ifndef ERROR_H
#define ERROR_H

#include <stdarg.h>

/* Source location */
typedef struct {
	const char *filename;
	int line;
	int column;
} source_loc_t;

/* Error severity levels */
typedef enum {
	DIAG_NOTE,
	DIAG_WARNING,
	DIAG_ERROR,
	DIAG_FATAL
} diag_level_t;

/* Global error counters */
extern int nerrors;
extern int nwarnings;
extern int max_errors;

/* Diagnostic options */
extern int use_color;      /* Use colored output */
extern int show_caret;     /* Show caret diagnostics */

/* Initialize error reporting system */
void error_init(void);

/* Report diagnostics */
void error(const char *fmt, ...);
void error_at(source_loc_t loc, const char *fmt, ...);
void warning(const char *fmt, ...);
void warning_at(source_loc_t loc, const char *fmt, ...);
void note(const char *fmt, ...);
void note_at(source_loc_t loc, const char *fmt, ...);
void fatal(const char *fmt, ...);

/* Create source location */
source_loc_t make_loc(const char *filename, int line, int column);
source_loc_t current_loc(void);

/* Warning control */
void enable_warning(const char *name);
void disable_warning(const char *name);
int is_warning_enabled(const char *name);

#endif /* ERROR_H */
