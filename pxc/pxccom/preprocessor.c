/*
 * Copyright (c) 2025 PCC Xbase++ Compiler
 *
 * Preprocessor for Xbase++ (#include, #define, #ifdef, etc.)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "pass1.h"

#define MAX_INCLUDES 100
#define MAX_DEFINES 1000
#define MAX_MACRO_LEN 4096

/* Macro definition */
typedef struct macro {
	char *name;
	char *value;
	struct macro *next;
} MACRO;

/* Include path list */
static char *include_paths[MAX_INCLUDES];
static int num_include_paths = 0;

/* Macro table */
static MACRO *macros[256];  /* Hash table */

/* Include stack */
static FILE *include_stack[MAX_INCLUDES];
static char *filename_stack[MAX_INCLUDES];
static int line_stack[MAX_INCLUDES];
static int include_depth = 0;

/*
 * Hash function for macros
 */
static unsigned int
macro_hash(const char *name)
{
	unsigned int h = 0;
	while (*name)
		h = (h << 4) + *name++;
	return h % 256;
}

/*
 * Define a macro
 */
void
pp_define(const char *name, const char *value)
{
	unsigned int h = macro_hash(name);
	MACRO *m = malloc(sizeof(MACRO));

	if (!m) return;

	m->name = strdup(name);
	m->value = strdup(value ? value : "");
	m->next = macros[h];
	macros[h] = m;
}

/*
 * Look up macro
 */
const char *
pp_lookup(const char *name)
{
	unsigned int h = macro_hash(name);
	MACRO *m;

	for (m = macros[h]; m; m = m->next) {
		if (strcmp(m->name, name) == 0)
			return m->value;
	}

	return NULL;
}

/*
 * Undefine macro
 */
void
pp_undef(const char *name)
{
	unsigned int h = macro_hash(name);
	MACRO *m, *prev = NULL;

	for (m = macros[h]; m; prev = m, m = m->next) {
		if (strcmp(m->name, name) == 0) {
			if (prev)
				prev->next = m->next;
			else
				macros[h] = m->next;
			free(m->name);
			free(m->value);
			free(m);
			return;
		}
	}
}

/*
 * Add include path
 */
void
pp_add_include_path(const char *path)
{
	if (num_include_paths < MAX_INCLUDES) {
		include_paths[num_include_paths++] = strdup(path);
	}
}

/*
 * Find include file
 */
static FILE *
find_include(const char *filename, int is_system)
{
	FILE *fp;
	char path[1024];
	int i;

	/* Try current directory first for "" includes */
	if (!is_system) {
		fp = fopen(filename, "r");
		if (fp) return fp;
	}

	/* Try include paths */
	for (i = 0; i < num_include_paths; i++) {
		snprintf(path, sizeof(path), "%s/%s", include_paths[i], filename);
		fp = fopen(path, "r");
		if (fp) return fp;
	}

	/* Try system paths */
	snprintf(path, sizeof(path), "/usr/include/%s", filename);
	fp = fopen(path, "r");
	if (fp) return fp;

	snprintf(path, sizeof(path), "/usr/local/include/%s", filename);
	fp = fopen(path, "r");
	if (fp) return fp;

	return NULL;
}

/*
 * Process #include directive
 */
int
pp_include(const char *filename, int is_system)
{
	FILE *fp;

	if (include_depth >= MAX_INCLUDES) {
		error("include nesting too deep");
		return -1;
	}

	fp = find_include(filename, is_system);
	if (!fp) {
		error("cannot open include file: %s", filename);
		return -1;
	}

	/* Push current file */
	include_stack[include_depth] = yyin;
	filename_stack[include_depth] = strdup(filename);
	line_stack[include_depth] = lineno;
	include_depth++;

	/* Switch to new file */
	yyin = fp;
	lineno = 1;

	return 0;
}

/*
 * Pop include file
 */
int
pp_pop_include(void)
{
	if (include_depth == 0)
		return -1;

	fclose(yyin);
	include_depth--;

	yyin = include_stack[include_depth];
	lineno = line_stack[include_depth];
	free(filename_stack[include_depth]);

	return 0;
}

/*
 * Expand macros in a line
 */
char *
pp_expand_macros(const char *line)
{
	static char expanded[MAX_MACRO_LEN];
	char token[256];
	const char *p = line;
	char *out = expanded;
	int in_token = 0;
	int token_len = 0;

	while (*p && (out - expanded) < MAX_MACRO_LEN - 1) {
		if (isalnum(*p) || *p == '_') {
			/* Part of identifier */
			if (!in_token) {
				in_token = 1;
				token_len = 0;
			}
			if (token_len < 255) {
				token[token_len++] = *p;
			}
			p++;
		} else {
			/* Not part of identifier */
			if (in_token) {
				token[token_len] = '\0';
				const char *value = pp_lookup(token);
				if (value) {
					/* Expand macro */
					while (*value && (out - expanded) < MAX_MACRO_LEN - 1) {
						*out++ = *value++;
					}
				} else {
					/* Not a macro, copy token */
					int i;
					for (i = 0; i < token_len && (out - expanded) < MAX_MACRO_LEN - 1; i++) {
						*out++ = token[i];
					}
				}
				in_token = 0;
			}
			*out++ = *p++;
		}
	}

	/* Handle final token */
	if (in_token) {
		token[token_len] = '\0';
		const char *value = pp_lookup(token);
		if (value) {
			while (*value && (out - expanded) < MAX_MACRO_LEN - 1) {
				*out++ = *value++;
			}
		} else {
			int i;
			for (i = 0; i < token_len && (out - expanded) < MAX_MACRO_LEN - 1; i++) {
				*out++ = token[i];
			}
		}
	}

	*out = '\0';
	return expanded;
}

/*
 * Initialize preprocessor
 */
void
pp_init(void)
{
	int i;

	/* Clear macro table */
	for (i = 0; i < 256; i++) {
		macros[i] = NULL;
	}

	/* Add default include paths */
	pp_add_include_path(".");
	pp_add_include_path("/usr/local/include/pxc");

	/* Define standard macros */
	pp_define("__XBASEPP__", "1");
	pp_define("__PXC__", "1");

	include_depth = 0;
}

/*
 * Cleanup preprocessor
 */
void
pp_cleanup(void)
{
	int i;
	MACRO *m, *next;

	/* Free macros */
	for (i = 0; i < 256; i++) {
		for (m = macros[i]; m; m = next) {
			next = m->next;
			free(m->name);
			free(m->value);
			free(m);
		}
		macros[i] = NULL;
	}

	/* Free include paths */
	for (i = 0; i < num_include_paths; i++) {
		free(include_paths[i]);
	}
	num_include_paths = 0;
}
