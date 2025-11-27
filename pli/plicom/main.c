/*
 * Copyright (c) 2025 PCC PL/I Compiler
 *
 * Main entry point for plicom (PL/I compiler proper)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "pass1.h"

/* External from parser */
extern int yyparse(void);
extern FILE *yyin;

/* Usage message */
static void usage(void) {
	fprintf(stderr, "Usage: plicom [options] <file>\n");
	fprintf(stderr, "Options:\n");
	fprintf(stderr, "  -o <file>    Set output file\n");
	fprintf(stderr, "  -d <dialect> Set PL/I dialect (pli, plm, plm86, plm386, plc)\n");
	fprintf(stderr, "  -W<warning>  Enable warning\n");
	fprintf(stderr, "  -w           Disable all warnings\n");
	fprintf(stderr, "  -v           Verbose mode\n");
	fprintf(stderr, "  -h           Show this help\n");
	exit(1);
}

int main(int argc, char **argv) {
	char *input_file = NULL;
	char *output_file = NULL;
	char *dialect_str = NULL;
	int verbose = 0;
	int opt;

	/* Initialize subsystems */
	error_init();
	dialect_init();
	symtab_init();

	/* Parse command-line arguments */
	while ((opt = getopt(argc, argv, "o:d:W:wvh")) != -1) {
		switch (opt) {
		case 'o':
			output_file = optarg;
			break;
		case 'd':
			dialect_str = optarg;
			break;
		case 'W':
			enable_warning(optarg);
			break;
		case 'w':
			warn_all = 0;
			warn_unused = 0;
			warn_conversion = 0;
			warn_strict = 0;
			warn_extensions = 0;
			warn_deprecated = 0;
			break;
		case 'v':
			verbose = 1;
			break;
		case 'h':
		default:
			usage();
		}
	}

	/* Get input file */
	if (optind >= argc) {
		fprintf(stderr, "Error: No input file specified\n");
		usage();
	}
	input_file = argv[optind];

	/* Set dialect if specified */
	if (dialect_str) {
		pli_dialect_t dialect = parse_dialect(dialect_str);
		set_dialect(dialect);
		if (verbose) {
			fprintf(stderr, "Using dialect: %s\n", get_dialect_name(dialect));
		}
	}

	/* Open input file */
	yyin = fopen(input_file, "r");
	if (!yyin) {
		fatal("cannot open input file '%s'", input_file);
	}

	/* Set current file for error messages */
	current_file = input_file;
	ftitle = input_file;

	/* Open output file */
	if (output_file) {
		outfile = fopen(output_file, "w");
		if (!outfile) {
			fatal("cannot open output file '%s'", output_file);
		}
	} else {
		outfile = stdout;
	}

	if (verbose) {
		fprintf(stderr, "Compiling %s...\n", input_file);
	}

	/* Initialize built-in functions */
	init_builtins();

	/* Parse the input */
	int result = yyparse();

	/* Close files */
	fclose(yyin);
	if (outfile != stdout) {
		fclose(outfile);
	}

	/* Report results */
	if (verbose || nerrors > 0 || nwarnings > 0) {
		if (nerrors > 0) {
			fprintf(stderr, "%d error%s generated.\n",
				nerrors, nerrors == 1 ? "" : "s");
		}
		if (nwarnings > 0) {
			fprintf(stderr, "%d warning%s generated.\n",
				nwarnings, nwarnings == 1 ? "" : "s");
		}
	}

	/* Return error code */
	if (result != 0 || nerrors > 0) {
		return 1;
	}

	return 0;
}
