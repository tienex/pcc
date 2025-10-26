/*
 * Copyright (c) 2025 PCC Go Compiler
 *
 * Main entry point for Go compiler (gocom)
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

/* Global variables */
int lineno = 1;
int current_column = 1;
char *ftitle = "<stdin>";
char *current_file = "<stdin>";
char *package_name = "main";
FILE *outfile = NULL;
int blevel = 0;

/* Command-line options */
static int verbose = 0;
static int dump_ast = 0;
static int dump_symtab = 0;
static char *output_file = NULL;

/*
 * Print usage information
 */
static void
usage(void)
{
	fprintf(stderr,
	    "Usage: gocom [options] [file.go]\n"
	    "Options:\n"
	    "  -o file       Write output to file\n"
	    "  -v            Verbose output\n"
	    "  -ferror-limit=N  Set maximum errors before abort (default: 20)\n"
	    "  -fno-color    Disable colored diagnostics\n"
	    "  -fno-caret    Disable caret diagnostics\n"
	    "  --dump-ast    Dump abstract syntax tree\n"
	    "  --dump-symtab Dump symbol table\n"
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
				output_file = argv[i];
			} else if (strncmp(argv[i], "-ferror-limit=", 14) == 0) {
				max_errors = atoi(&argv[i][14]);
			} else if (strcmp(argv[i], "-fno-color") == 0) {
				use_color = 0;
			} else if (strcmp(argv[i], "-fno-caret") == 0) {
				show_caret = 0;
			} else if (strcmp(argv[i], "-v") == 0) {
				verbose = 1;
			} else if (strcmp(argv[i], "--dump-ast") == 0) {
				dump_ast = 1;
			} else if (strcmp(argv[i], "--dump-symtab") == 0) {
				dump_symtab = 1;
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
			current_file = argv[i];
			yyin = fopen(ftitle, "r");
			if (yyin == NULL) {
				perror(ftitle);
				exit(1);
			}
		}
	}

	/* Open output file */
	if (output_file != NULL) {
		outfile = fopen(output_file, "w");
		if (outfile == NULL) {
			perror(output_file);
			exit(1);
		}
	} else {
		outfile = stdout;
	}
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
		fprintf(stderr, "PCC Go Compiler\n");
		fprintf(stderr, "Input: %s\n", ftitle);
		if (output_file)
			fprintf(stderr, "Output: %s\n", output_file);
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
		fprintf(stderr, "Package: %s\n", package_name);
		fprintf(stderr, "Compilation successful.\n");
	}

	/* Close files */
	if (yyin != stdin)
		fclose(yyin);
	if (outfile != stdout)
		fclose(outfile);

	return ret;
}
