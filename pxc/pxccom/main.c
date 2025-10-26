/*
 * Copyright (c) 2025 PCC Xbase++ Compiler
 *
 * Main entry point for pxccom (Xbase++ compiler proper)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include "pass1.h"

/* Global variables */
int lineno = 1;
char *filename = "<stdin>";
int errors = 0;
int warnings = 0;
int debug = 0;

/* Command line options */
static int opt_verbose = 0;
static int opt_optimize = 0;
static char *output_file = NULL;

/* External declarations */
extern int yyparse(void);
extern int yylex(void);
extern FILE *yyin;
void reset_lexer(void);

/* Function prototypes */
static void usage(void);
static void parse_options(int argc, char **argv);

int
main(int argc, char **argv)
{
	int i;
	FILE *fp;

	/* Initialize subsystems */
	symtab_init();
	init_builtins();

	/* Parse command line options */
	parse_options(argc, argv);

	/* If no input files specified, read from stdin */
	if (optind >= argc) {
		filename = "<stdin>";
		yyin = stdin;
		reset_lexer();

		if (opt_verbose)
			fprintf(stderr, "Parsing %s...\n", filename);

		if (yyparse() != 0) {
			errors++;
		}
	} else {
		/* Process each input file */
		for (i = optind; i < argc; i++) {
			filename = argv[i];

			fp = fopen(filename, "r");
			if (fp == NULL) {
				fprintf(stderr, "pxccom: cannot open %s\n", filename);
				errors++;
				continue;
			}

			yyin = fp;
			reset_lexer();

			if (opt_verbose)
				fprintf(stderr, "Parsing %s...\n", filename);

			if (yyparse() != 0) {
				errors++;
			}

			fclose(fp);
		}
	}

	/* Print summary */
	if (opt_verbose) {
		fprintf(stderr, "Compilation complete: ");
		fprintf(stderr, "%d error%s, %d warning%s\n",
			errors, errors == 1 ? "" : "s",
			warnings, warnings == 1 ? "" : "s");
	}

	/* Return exit code */
	return (errors > 0) ? 1 : 0;
}

/*
 * Parse command line options
 */
static void
parse_options(int argc, char **argv)
{
	int c;
	extern char *optarg;
	extern int optind;

	while ((c = getopt(argc, argv, "vdo:O")) != -1) {
		switch (c) {
		case 'v':
			opt_verbose = 1;
			break;
		case 'd':
			debug = 1;
			break;
		case 'o':
			output_file = optarg;
			break;
		case 'O':
			opt_optimize = 1;
			break;
		case '?':
		default:
			usage();
			exit(1);
		}
	}
}

/*
 * Print usage message
 */
static void
usage(void)
{
	fprintf(stderr, "Usage: pxccom [options] [file ...]\n");
	fprintf(stderr, "Options:\n");
	fprintf(stderr, "  -v          Verbose output\n");
	fprintf(stderr, "  -d          Debug mode\n");
	fprintf(stderr, "  -o file     Output file\n");
	fprintf(stderr, "  -O          Enable optimizations\n");
}

/*
 * Error reporting
 */
void
error(const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d: error: ", filename, lineno);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	errors++;
}

void
warning(const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d: warning: ", filename, lineno);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	warnings++;
}
