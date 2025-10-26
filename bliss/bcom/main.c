/*
 * Copyright (c) 2025 PCC BLISS Compiler
 *
 * Main driver for BLISS frontend (bcom)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "pass1.h"

/* Global variables */
int lineno = 1;
int current_column = 1;
char *ftitle = "<stdin>";
FILE *outfile = NULL;
int blevel = 0;
int errors = 0;
int warnings = 0;

/* External from lexer */
extern FILE *yyin;
extern int yyparse(void);
extern int yylex_destroy(void);

/* Options */
static int verbose = 0;
static int debug = 0;

/* Error handling */
void error(const char *fmt, ...) {
	va_list ap;
	fprintf(stderr, "%s:%d:%d: error: ", ftitle, lineno, current_column);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
	errors++;
}

void warning(const char *fmt, ...) {
	va_list ap;
	fprintf(stderr, "%s:%d:%d: warning: ", ftitle, lineno, current_column);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
	warnings++;
}

void yyerror(const char *s) {
	error("%s", s);
}

/* Usage message */
static void usage(void) {
	fprintf(stderr, "Usage: bcom [options] [input-file]\n");
	fprintf(stderr, "Options:\n");
	fprintf(stderr, "  -o file    Write output to file\n");
	fprintf(stderr, "  -v         Verbose mode\n");
	fprintf(stderr, "  -g         Generate debug information\n");
	fprintf(stderr, "  -h         Show this help message\n");
	exit(1);
}

/* Initialize compiler */
void init_compiler(void) {
	symtab_init();
	init_builtins();
	blevel = 0;
	errors = 0;
	warnings = 0;
	lineno = 1;
	current_column = 1;
}

/* Finish compilation */
void finish_compilation(void) {
	if (outfile && outfile != stdout) {
		fclose(outfile);
	}

	if (errors > 0) {
		fprintf(stderr, "%d error%s", errors, errors == 1 ? "" : "s");
		if (warnings > 0)
			fprintf(stderr, ", %d warning%s", warnings, warnings == 1 ? "" : "s");
		fprintf(stderr, "\n");
		exit(1);
	} else if (warnings > 0) {
		fprintf(stderr, "%d warning%s\n", warnings, warnings == 1 ? "" : "s");
	}

	if (verbose) {
		fprintf(stderr, "Compilation successful\n");
	}
}

/* Main entry point */
int main(int argc, char *argv[]) {
	int i;
	char *input_file = NULL;
	char *output_file = NULL;

	/* Parse command-line arguments */
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			switch (argv[i][1]) {
			case 'o':
				if (i + 1 >= argc) {
					fprintf(stderr, "error: -o requires an argument\n");
					usage();
				}
				output_file = argv[++i];
				break;
			case 'v':
				verbose = 1;
				break;
			case 'g':
				debug = 1;
				break;
			case 'h':
				usage();
				break;
			default:
				fprintf(stderr, "error: unknown option '%s'\n", argv[i]);
				usage();
			}
		} else {
			if (input_file != NULL) {
				fprintf(stderr, "error: multiple input files specified\n");
				usage();
			}
			input_file = argv[i];
		}
	}

	/* Open input file */
	if (input_file != NULL) {
		yyin = fopen(input_file, "r");
		if (yyin == NULL) {
			fprintf(stderr, "error: cannot open '%s'\n", input_file);
			exit(1);
		}
		ftitle = input_file;
	} else {
		yyin = stdin;
		ftitle = "<stdin>";
	}

	/* Open output file */
	if (output_file != NULL) {
		outfile = fopen(output_file, "w");
		if (outfile == NULL) {
			fprintf(stderr, "error: cannot open '%s' for writing\n", output_file);
			exit(1);
		}
	} else {
		outfile = stdout;
	}

	if (verbose) {
		fprintf(stderr, "BLISS Compiler (bcom) - PCC Frontend\n");
		fprintf(stderr, "Compiling: %s\n", ftitle);
	}

	/* Initialize compiler */
	init_compiler();

	/* Parse the input */
	if (yyparse() != 0) {
		errors++;
	}

	/* Clean up */
	if (input_file != NULL) {
		fclose(yyin);
	}

	/* Cleanup lexer resources */
	yylex_destroy();

	/* Finish compilation */
	finish_compilation();

	return 0;
}
