/*
 * Copyright (c) 2025 PCC ALGOL 60+ Compiler
 *
 * Main entry point for ALGOL 60+ compiler (acom)
 * Pass 1: Lexical analysis, parsing, semantic analysis, IR generation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "pass1.h"

/* External parser function */
extern int yyparse(void);
extern FILE *yyin;
extern int yylineno;

/* Global variables */
int lineno = 1;
char *ftitle = "<stdin>";
FILE *outfile = NULL;
int blevel = 0;
int current_line = 1;

/* Command-line options */
static int verbose = 0;

/*
 * Print usage information
 */
static void
usage(void)
{
	fprintf(stderr,
	    "Usage: acom [options] [file]\n"
	    "Options:\n"
	    "  -o file       Write output to file\n"
	    "  -v            Verbose output\n"
	    "  -h, --help    Show this help\n");
	exit(1);
}

/*
 * Parse command-line arguments
 */
static void
parse_args(int argc, char **argv)
{
	int i;

	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			/* Option */
			if (strcmp(argv[i], "-o") == 0) {
				/* Output file */
				if (++i >= argc) {
					fprintf(stderr, "error: -o requires argument\n");
					usage();
				}
				outfile = fopen(argv[i], "w");
				if (outfile == NULL) {
					perror(argv[i]);
					exit(1);
				}
			} else if (strcmp(argv[i], "-v") == 0) {
				verbose = 1;
			} else if (strcmp(argv[i], "-h") == 0 ||
			           strcmp(argv[i], "--help") == 0) {
				usage();
			} else {
				fprintf(stderr, "error: unknown option '%s'\n", argv[i]);
				usage();
			}
		} else {
			/* Input file */
			ftitle = argv[i];
			yyin = fopen(ftitle, "r");
			if (yyin == NULL) {
				perror(ftitle);
				exit(1);
			}
			current_file = ftitle;
		}
	}

	/* Default output to stdout */
	if (outfile == NULL)
		outfile = stdout;
}

/*
 * Main entry point
 */
int
main(int argc, char **argv)
{
	int ret;

	/* Initialize subsystems */
	error_init();
	symtab_init();
	init_types();
	init_builtins();

	/* Parse arguments */
	parse_args(argc, argv);

	if (verbose) {
		fprintf(stderr, "PCC ALGOL 60+ Compiler\n");
		fprintf(stderr, "Input: %s\n", ftitle);
	}

	/* Parse input */
	ret = yyparse();

	/* Check for errors */
	if (nerrors > 0) {
		fprintf(stderr, "%d error%s generated.\n",
		    nerrors, nerrors == 1 ? "" : "s");
		return 1;
	}

	if (nwarnings > 0 && verbose) {
		fprintf(stderr, "%d warning%s generated.\n",
		    nwarnings, nwarnings == 1 ? "" : "s");
	}

	if (verbose) {
		fprintf(stderr, "Compilation successful.\n");
	}

	/* Close files */
	if (yyin != stdin)
		fclose(yyin);
	if (outfile != stdout)
		fclose(outfile);

	return ret;
}
