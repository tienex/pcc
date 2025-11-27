/*
 * Copyright (c) 2025 PCC Paradox PAL Compiler
 *
 * Main entry point for PAL/ObjectPAL compiler
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "pass1.h"

/* External functions from lexer/parser */
extern int yyparse(void);
extern FILE *yyin;
extern int yylineno;

/* Global variables */
int lineno = 1;
int current_column = 1;
char *ftitle = NULL;
FILE *outfile = NULL;
int blevel = 0;

/* Command-line options */
static int opt_verbose = 0;
static int opt_debug = 0;
static char *opt_output = NULL;
static char *opt_dialect = NULL;

/* Function prototypes */
static void usage(const char *progname);
static void version(void);

int main(int argc, char **argv)
{
	int opt;
	int result;
	const char *input_file = NULL;

	static struct option long_options[] = {
		{"help",     no_argument,       0, 'h'},
		{"version",  no_argument,       0, 'v'},
		{"output",   required_argument, 0, 'o'},
		{"dialect",  required_argument, 0, 'd'},
		{"verbose",  no_argument,       0, 'V'},
		{"debug",    no_argument,       0, 'g'},
		{0, 0, 0, 0}
	};

	/* Parse command-line options */
	while ((opt = getopt_long(argc, argv, "hvo:d:Vg", long_options, NULL)) != -1) {
		switch (opt) {
		case 'h':
			usage(argv[0]);
			return 0;
		case 'v':
			version();
			return 0;
		case 'o':
			opt_output = optarg;
			break;
		case 'd':
			opt_dialect = optarg;
			break;
		case 'V':
			opt_verbose = 1;
			break;
		case 'g':
			opt_debug = 1;
			break;
		default:
			usage(argv[0]);
			return 1;
		}
	}

	/* Get input file */
	if (optind < argc) {
		input_file = argv[optind];
	} else {
		fprintf(stderr, "Error: No input file specified\n");
		usage(argv[0]);
		return 1;
	}

	/* Initialize dialect system */
	dialect_init();

	/* Set dialect if specified */
	if (opt_dialect) {
		pal_dialect_t dialect = parse_dialect(opt_dialect);
		if (dialect == -1) {
			fprintf(stderr, "Error: Unknown dialect: %s\n", opt_dialect);
			return 1;
		}
		set_dialect(dialect);
	} else {
		/* Default to ObjectPAL latest */
		set_dialect(DIALECT_OBJECTPAL_LATEST);
	}

	if (opt_verbose) {
		printf("PAL Compiler - Compiling for %s\n",
		       get_dialect_name(current_dialect));
	}

	/* Initialize error system */
	ftitle = strdup(input_file);
	error_init(input_file);

	/* Open input file */
	yyin = fopen(input_file, "r");
	if (!yyin) {
		perror(input_file);
		return 1;
	}

	/* Open output file */
	if (opt_output) {
		outfile = fopen(opt_output, "w");
		if (!outfile) {
			perror(opt_output);
			fclose(yyin);
			return 1;
		}
	} else {
		outfile = stdout;
	}

	/* Initialize symbol table */
	symtab_init();

	/* Initialize built-in functions */
	init_builtins();

	/* Parse input */
	if (opt_verbose) {
		printf("Parsing %s...\n", input_file);
	}

	result = yyparse();

	/* Close files */
	fclose(yyin);
	if (outfile != stdout) {
		fclose(outfile);
	}

	/* Print error summary */
	if (opt_verbose || has_errors()) {
		print_error_summary();
	}

	/* Return error code */
	if (has_errors()) {
		return 1;
	}

	if (opt_verbose) {
		printf("Compilation successful.\n");
	}

	return 0;
}

static void usage(const char *progname)
{
	printf("Usage: %s [options] <input-file>\n", progname);
	printf("\nOptions:\n");
	printf("  -h, --help              Show this help message\n");
	printf("  -v, --version           Show version information\n");
	printf("  -o, --output <file>     Output file (default: stdout)\n");
	printf("  -d, --dialect <dialect> Target dialect (default: objectpal-latest)\n");
	printf("  -V, --verbose           Verbose output\n");
	printf("  -g, --debug             Enable debug output\n");
	printf("\nSupported dialects:\n");
	printf("  pal-1.0                 Paradox 1.0 PAL\n");
	printf("  pal-3.0                 Paradox 3.0 PAL\n");
	printf("  pal-4.5                 Paradox 4.5 PAL\n");
	printf("  objectpal-1.0           ObjectPAL 1.0\n");
	printf("  objectpal-7.0           ObjectPAL 7.0\n");
	printf("  objectpal-latest        Latest ObjectPAL (default)\n");
}

static void version(void)
{
	printf("PCC PAL Compiler version 0.1\n");
	printf("Copyright (c) 2025 PCC Project\n");
	printf("Supports Paradox PAL and ObjectPAL\n");
}
